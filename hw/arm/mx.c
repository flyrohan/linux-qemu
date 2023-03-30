/*
 * MX emulation (c) 2023 Junghyun Kim
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#include "qemu/osdep.h"
#include "qemu-common.h"
#include "qemu/datadir.h"
#include "qemu/units.h"
#include "qemu/option.h"
#include "monitor/qdev.h"
#include "qapi/error.h"
#include "hw/sysbus.h"
#include "hw/arm/boot.h"
#include "hw/arm/primecell.h"
#include "hw/block/flash.h"
#include "hw/vfio/vfio-calxeda-xgmac.h"
#include "hw/vfio/vfio-amd-xgbe.h"
#include "hw/display/ramfb.h"
#include "net/net.h"
#include "sysemu/device_tree.h"
#include "sysemu/numa.h"
#include "sysemu/runstate.h"
#include "sysemu/tpm.h"
#include "sysemu/kvm.h"
#include "hw/loader.h"
#include "qapi/error.h"
#include "qemu/bitops.h"
#include "qemu/error-report.h"
#include "qemu/module.h"
#include "hw/pci-host/gpex.h"
#include "hw/virtio/virtio-pci.h"
#include "hw/arm/sysbus-fdt.h"
#include "hw/platform-bus.h"
#include "hw/qdev-properties.h"
#include "hw/arm/fdt.h"
#include "hw/intc/arm_gic.h"
#include "hw/intc/arm_gicv3_common.h"
#include "hw/irq.h"
#include "kvm_arm.h"
#include "hw/firmware/smbios.h"
#include "qapi/visitor.h"
#include "qapi/qapi-visit-common.h"
#include "standard-headers/linux/input.h"
#include "hw/arm/smmuv3.h"
#include "target/arm/internals.h"
#include "hw/mem/pc-dimm.h"
#include "hw/mem/nvdimm.h"
#include "hw/char/pl011.h"
#include "qemu/guest-random.h"

#include "mx.h"
#include "mx_map.h"

#define TYPE_MX_MACHINE   MACHINE_TYPE_NAME("mx")

typedef struct MX_MachineClass {
    MachineClass parent;
    bool no_pmu;
    bool claim_edge_triggered_timers;
    bool smbios_old_sys_ver;
    bool no_highmem_ecam;
    bool no_secure_gpio;
	MX_CORE_TYPE core;
	MX_MACHINE_TYPE machine;
    uint32_t revision;
} MX_MachineClass;

typedef struct MX_CoreType {
    const char *code_name;
	const char *cpu_type;
    int cores;
    VirtGICType gic_version;
    bool highmem;	/* physical address space above 32 bits */
	const int *irqmap;
	const MemMapEntry *memmap;
} MX_CoreType;

typedef struct MX_MachineType {
    const char *desc;
	const MX_CoreType *core_type;
	uint64_t memory_size;
} MX_MachineType;

typedef struct MX_MachineState {
    MachineState parent;
    Notifier machine_done;
    DeviceState *platform_bus_dev;
    FWCfgState *fw_cfg;
    PFlashCFI01 *flash[2];
    bool secure;
    bool highmem;
    bool highmem_ecam;
    bool virt;
    struct arm_boot_info bootinfo;
    char *pciehb_nodename;
    int fdt_size;
    uint32_t clock_phandle;
    uint32_t gic_phandle;
    int psci_conduit;
    hwaddr highest_gpa;
    DeviceState *gic;
    Notifier powerdown_notifier;
    PCIBus *bus;
	const MX_MachineType *machine_type;
} MX_MachineState;

#define VIRT_ECAM_ID(high) (high ? VIRT_HIGH_PCIE_ECAM : VIRT_PCIE_ECAM)

OBJECT_DECLARE_TYPE(MX_MachineState, MX_MachineClass, MX_MACHINE)

static const MX_CoreType mx_core_types[] = {
    [MX_CORE_S1_STD] = {
		.code_name   = "st",
		.cpu_type    = "cortex-a53",
		.cores       = 4,
		.gic_version = 2, /* gicv2 */
		.irqmap      = mx_irqmap[MX_CORE_S1_STD],
		.memmap		 = mx_memmap[MX_CORE_S1_STD],
	},
};
#define MX_CORE_REVISON(rev)	(rev & 0xF)

static const MX_MachineType mx_mach_types[] = {
    [MX_MACHINE_S1_EMU] = { "emu", &mx_core_types[MX_CORE_S1_STD], (1 * GiB) },
    [MX_MACHINE_S1_EVB] = { "evb", &mx_core_types[MX_CORE_S1_STD], (512 * MiB) },
};

static const MX_CoreType *mx_mach_coretype(MX_MACHINE_TYPE type)
{
    return mx_mach_types[type].core_type;
}

static const MX_MachineType *mx_mach_machtype(MX_MACHINE_TYPE type)
{
    return &mx_mach_types[type];
}

/* Return the number of used redistributor regions  */
static inline int mx_gicv3_redist_region_count(MX_MachineState *vms)
{
	const MX_CoreType *ct = vms->machine_type->core_type;
    uint32_t redist0_capacity =
                ct->memmap[VIRT_GIC_REDIST].size / GICV3_REDIST_SIZE;

    assert(ct->gic_version == VIRT_GIC_VERSION_3);

    return MACHINE(vms)->smp.cpus > redist0_capacity ? 2 : 1;
}

static void dev_kaslr_seed_create(MachineState *ms, const char *node)
{
    uint64_t seed;

    if (qemu_guest_getrandom(&seed, sizeof(seed), NULL))
        return;

    qemu_fdt_setprop_u64(ms->fdt, node, "kaslr-seed", seed);
}

static void fdt_create(MX_MachineState *vms)
{
    MachineState *ms = MACHINE(vms);
    int nb_numa_nodes = ms->numa_state->num_nodes;
    void *fdt = create_device_tree(&vms->fdt_size);

    if (!fdt) {
        error_report("create_device_tree() failed");
        exit(1);
    }

    ms->fdt = fdt;

    /* Header */
    qemu_fdt_setprop_string(fdt, "/", "compatible", "linux,dummy-virt");
    qemu_fdt_setprop_cell(fdt, "/", "#address-cells", 0x2);
    qemu_fdt_setprop_cell(fdt, "/", "#size-cells", 0x2);

    /* /chosen must exist for load_dtb to fill in necessary properties later */
    qemu_fdt_add_subnode(fdt, "/chosen");
    dev_kaslr_seed_create(ms, "/chosen");

    if (vms->secure) {
        qemu_fdt_add_subnode(fdt, "/secure-chosen");
        dev_kaslr_seed_create(ms, "/secure-chosen");
    }

    /* Clock node, for the benefit of the UART. The kernel device tree
     * binding documentation claims the PL011 node clock properties are
     * optional but in practice if you omit them the kernel refuses to
     * probe for the device.
     */
    vms->clock_phandle = qemu_fdt_alloc_phandle(fdt);
    qemu_fdt_add_subnode(fdt, "/apb-pclk");
    qemu_fdt_setprop_string(fdt, "/apb-pclk", "compatible", "fixed-clock");
    qemu_fdt_setprop_cell(fdt, "/apb-pclk", "#clock-cells", 0x0);
    qemu_fdt_setprop_cell(fdt, "/apb-pclk", "clock-frequency", 24000000);
    qemu_fdt_setprop_string(fdt, "/apb-pclk", "clock-output-names",
                                "clk24mhz");
    qemu_fdt_setprop_cell(fdt, "/apb-pclk", "phandle", vms->clock_phandle);

    if (nb_numa_nodes > 0 && ms->numa_state->have_numa_distance) {
        int size = nb_numa_nodes * nb_numa_nodes * 3 * sizeof(uint32_t);
        uint32_t *matrix = g_malloc0(size);
        int idx, i, j;

        for (i = 0; i < nb_numa_nodes; i++) {
            for (j = 0; j < nb_numa_nodes; j++) {
                idx = (i * nb_numa_nodes + j) * 3;
                matrix[idx + 0] = cpu_to_be32(i);
                matrix[idx + 1] = cpu_to_be32(j);
                matrix[idx + 2] =
                    cpu_to_be32(ms->numa_state->nodes[i].distance[j]);
            }
        }

        qemu_fdt_add_subnode(fdt, "/distance-map");
        qemu_fdt_setprop_string(fdt, "/distance-map", "compatible",
                                "numa-distance-map-v1");
        qemu_fdt_setprop(fdt, "/distance-map", "distance-matrix",
                         matrix, size);
        g_free(matrix);
    }
}

static void fdt_add_timer_nodes(const MX_MachineState *vms)
{
	const MX_CoreType *ct = vms->machine_type->core_type;

    /* On real hardware these interrupts are level-triggered.
     * On KVM they were edge-triggered before host kernel version 4.4,
     * and level-triggered afterwards.
     * On emulated QEMU they are level-triggered.
     *
     * Getting the DTB info about them wrong is awkward for some
     * guest kernels:
     *  pre-4.8 ignore the DT and leave the interrupt configured
     *   with whatever the GIC reset value (or the bootloader) left it at
     *  4.8 before rc6 honour the incorrect data by programming it back
     *   into the GIC, causing problems
     *  4.8rc6 and later ignore the DT and always write "level triggered"
     *   into the GIC
     *
     * For backwards-compatibility, virt-2.8 and earlier will continue
     * to say these are edge-triggered, but later machines will report
     * the correct information.
     */
    ARMCPU *armcpu;
    MX_MachineClass *vmc = MX_MACHINE_GET_CLASS(vms);
    uint32_t irqflags = GIC_FDT_IRQ_FLAGS_LEVEL_HI;
    MachineState *ms = MACHINE(vms);

    if (vmc->claim_edge_triggered_timers)
        irqflags = GIC_FDT_IRQ_FLAGS_EDGE_LO_HI;

    if (ct->gic_version == VIRT_GIC_VERSION_2)
        irqflags = deposit32(irqflags, GIC_FDT_IRQ_PPI_CPU_START,
                             GIC_FDT_IRQ_PPI_CPU_WIDTH,
                             (1 << MACHINE(vms)->smp.cpus) - 1);

    qemu_fdt_add_subnode(ms->fdt, "/timer");

    armcpu = ARM_CPU(qemu_get_cpu(0));
    if (arm_feature(&armcpu->env, ARM_FEATURE_V8)) {
        const char compat[] = "arm,armv8-timer\0arm,armv7-timer";
        qemu_fdt_setprop(ms->fdt, "/timer", "compatible",
                         compat, sizeof(compat));
    } else {
        qemu_fdt_setprop_string(ms->fdt, "/timer", "compatible",
                                "arm,armv7-timer");
    }
    qemu_fdt_setprop(ms->fdt, "/timer", "always-on", NULL, 0);
    qemu_fdt_setprop_cells(ms->fdt, "/timer", "interrupts",
                       GIC_FDT_IRQ_TYPE_PPI, ARCH_TIMER_S_EL1_IRQ, irqflags,
                       GIC_FDT_IRQ_TYPE_PPI, ARCH_TIMER_NS_EL1_IRQ, irqflags,
                       GIC_FDT_IRQ_TYPE_PPI, ARCH_TIMER_VIRT_IRQ, irqflags,
                       GIC_FDT_IRQ_TYPE_PPI, ARCH_TIMER_NS_EL2_IRQ, irqflags);
}

static void fdt_add_cpu_nodes(const MX_MachineState *vms)
{
    int cpu;
    int addr_cells = 1;
    const MachineState *ms = MACHINE(vms);
    int smp_cpus = ms->smp.cpus;

    /*
     * From Documentation/devicetree/bindings/arm/cpus.txt
     *  On ARM v8 64-bit systems value should be set to 2,
     *  that corresponds to the MPIDR_EL1 register size.
     *  If MPIDR_EL1[63:32] value is equal to 0 on all CPUs
     *  in the system, #address-cells can be set to 1, since
     *  MPIDR_EL1[63:32] bits are not used for CPUs
     *  identification.
     *
     *  Here we actually don't know whether our system is 32- or 64-bit one.
     *  The simplest way to go is to examine affinity IDs of all our CPUs. If
     *  at least one of them has Aff3 populated, we set #address-cells to 2.
     */
    for (cpu = 0; cpu < smp_cpus; cpu++) {
        ARMCPU *armcpu = ARM_CPU(qemu_get_cpu(cpu));

        if (armcpu->mp_affinity & ARM_AFF3_MASK) {
            addr_cells = 2;
            break;
        }
    }

    qemu_fdt_add_subnode(ms->fdt, "/cpus");
    qemu_fdt_setprop_cell(ms->fdt, "/cpus", "#address-cells", addr_cells);
    qemu_fdt_setprop_cell(ms->fdt, "/cpus", "#size-cells", 0x0);

    for (cpu = smp_cpus - 1; cpu >= 0; cpu--) {
        char *nodename = g_strdup_printf("/cpus/cpu@%d", cpu);
        ARMCPU *armcpu = ARM_CPU(qemu_get_cpu(cpu));
        CPUState *cs = CPU(armcpu);

        qemu_fdt_add_subnode(ms->fdt, nodename);
        qemu_fdt_setprop_string(ms->fdt, nodename, "device_type", "cpu");
        qemu_fdt_setprop_string(ms->fdt, nodename, "compatible",
                                    armcpu->dtb_compatible);

        if (vms->psci_conduit != QEMU_PSCI_CONDUIT_DISABLED && smp_cpus > 1)
            qemu_fdt_setprop_string(ms->fdt, nodename,
                                        "enable-method", "psci");

        if (addr_cells == 2)
            qemu_fdt_setprop_u64(ms->fdt, nodename, "reg",
                                 armcpu->mp_affinity);
		else
            qemu_fdt_setprop_cell(ms->fdt, nodename, "reg",
                                  armcpu->mp_affinity);

        if (ms->possible_cpus->cpus[cs->cpu_index].props.has_node_id)
            qemu_fdt_setprop_cell(ms->fdt, nodename, "numa-node-id",
                ms->possible_cpus->cpus[cs->cpu_index].props.node_id);

        g_free(nodename);
    }
}

static void fdt_add_gic_node(MX_MachineState *vms)
{
    MachineState *ms = MACHINE(vms);
    char *nodename;
	const MX_CoreType *ct = vms->machine_type->core_type;

    vms->gic_phandle = qemu_fdt_alloc_phandle(ms->fdt);
    qemu_fdt_setprop_cell(ms->fdt, "/", "interrupt-parent", vms->gic_phandle);

    nodename = g_strdup_printf("/intc@%" PRIx64,
                               ct->memmap[VIRT_GIC_DIST].base);
    qemu_fdt_add_subnode(ms->fdt, nodename);
    qemu_fdt_setprop_cell(ms->fdt, nodename, "#interrupt-cells", 3);
    qemu_fdt_setprop(ms->fdt, nodename, "interrupt-controller", NULL, 0);
    qemu_fdt_setprop_cell(ms->fdt, nodename, "#address-cells", 0x2);
    qemu_fdt_setprop_cell(ms->fdt, nodename, "#size-cells", 0x2);
    qemu_fdt_setprop(ms->fdt, nodename, "ranges", NULL, 0);
    if (ct->gic_version == VIRT_GIC_VERSION_3) {
        int nb_redist_regions = mx_gicv3_redist_region_count(vms);

        qemu_fdt_setprop_string(ms->fdt, nodename, "compatible",
                                "arm,gic-v3");

        qemu_fdt_setprop_cell(ms->fdt, nodename,
                              "#redistributor-regions", nb_redist_regions);

        if (nb_redist_regions == 1) {
            qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                         2, ct->memmap[VIRT_GIC_DIST].base,
                                         2, ct->memmap[VIRT_GIC_DIST].size,
                                         2, ct->memmap[VIRT_GIC_REDIST].base,
                                         2, ct->memmap[VIRT_GIC_REDIST].size);
        } else {
            qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                 2, ct->memmap[VIRT_GIC_DIST].base,
                                 2, ct->memmap[VIRT_GIC_DIST].size,
                                 2, ct->memmap[VIRT_GIC_REDIST].base,
                                 2, ct->memmap[VIRT_GIC_REDIST].size,
                                 2, ct->memmap[VIRT_HIGH_GIC_REDIST2].base,
                                 2, ct->memmap[VIRT_HIGH_GIC_REDIST2].size);
        }

        if (vms->virt) {
            qemu_fdt_setprop_cells(ms->fdt, nodename, "interrupts",
                                   GIC_FDT_IRQ_TYPE_PPI, ARCH_GIC_MAINT_IRQ,
                                   GIC_FDT_IRQ_FLAGS_LEVEL_HI);
        }
    } else {
        /* 'cortex-a15-gic' means 'GIC v2' */
        qemu_fdt_setprop_string(ms->fdt, nodename, "compatible",
                                "arm,cortex-a15-gic");
        if (!vms->virt) {
            qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                         2, ct->memmap[VIRT_GIC_DIST].base,
                                         2, ct->memmap[VIRT_GIC_DIST].size,
                                         2, ct->memmap[VIRT_GIC_CPU].base,
                                         2, ct->memmap[VIRT_GIC_CPU].size);
        } else {
            qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                         2, ct->memmap[VIRT_GIC_DIST].base,
                                         2, ct->memmap[VIRT_GIC_DIST].size,
                                         2, ct->memmap[VIRT_GIC_CPU].base,
                                         2, ct->memmap[VIRT_GIC_CPU].size,
                                         2, ct->memmap[VIRT_GIC_HYP].base,
                                         2, ct->memmap[VIRT_GIC_HYP].size,
                                         2, ct->memmap[VIRT_GIC_VCPU].base,
                                         2, ct->memmap[VIRT_GIC_VCPU].size);
            qemu_fdt_setprop_cells(ms->fdt, nodename, "interrupts",
                                   GIC_FDT_IRQ_TYPE_PPI, ARCH_GIC_MAINT_IRQ,
                                   GIC_FDT_IRQ_FLAGS_LEVEL_HI);
        }
    }

    qemu_fdt_setprop_cell(ms->fdt, nodename, "phandle", vms->gic_phandle);
    g_free(nodename);
}

static void fdt_add_pmu_nodes(const MX_MachineState *vms)
{
    ARMCPU *armcpu = ARM_CPU(first_cpu);
    uint32_t irqflags = GIC_FDT_IRQ_FLAGS_LEVEL_HI;
    MachineState *ms = MACHINE(vms);
	const MX_CoreType *ct = vms->machine_type->core_type;

    if (!arm_feature(&armcpu->env, ARM_FEATURE_PMU)) {
        assert(!object_property_get_bool(OBJECT(armcpu), "pmu", NULL));
        return;
    }

    if (ct->gic_version == VIRT_GIC_VERSION_2) {
        irqflags = deposit32(irqflags, GIC_FDT_IRQ_PPI_CPU_START,
                             GIC_FDT_IRQ_PPI_CPU_WIDTH,
                             (1 << MACHINE(vms)->smp.cpus) - 1);
    }

    qemu_fdt_add_subnode(ms->fdt, "/pmu");
    if (arm_feature(&armcpu->env, ARM_FEATURE_V8)) {
        const char compat[] = "arm,armv8-pmuv3";
        qemu_fdt_setprop(ms->fdt, "/pmu", "compatible",
                         compat, sizeof(compat));
        qemu_fdt_setprop_cells(ms->fdt, "/pmu", "interrupts",
                               GIC_FDT_IRQ_TYPE_PPI, VIRTUAL_PMU_IRQ, irqflags);
    }
}

static void dev_gic_create(MX_MachineState *vms)
{
    MachineState *ms = MACHINE(vms);
    /* We create a standalone GIC */
    SysBusDevice *gicbusdev;
    const char *gictype;
	const MX_CoreType *ct = vms->machine_type->core_type;
    int type = ct->gic_version, i;
    unsigned int smp_cpus = ms->smp.cpus;
    uint32_t nb_redist_regions = 0;

    gictype = (type == 3) ? gicv3_class_name() : gic_class_name();

    vms->gic = qdev_new(gictype);
    qdev_prop_set_uint32(vms->gic, "revision", type);
    qdev_prop_set_uint32(vms->gic, "num-cpu", smp_cpus);
    /* Note that the num-irq property counts both internal and external
     * interrupts; there are always 32 of the former (mandated by GIC spec).
     */
    qdev_prop_set_uint32(vms->gic, "num-irq", NUM_IRQS + 32);

    if (type == 3) {
        uint32_t redist0_capacity =
                    ct->memmap[VIRT_GIC_REDIST].size / GICV3_REDIST_SIZE;
        uint32_t redist0_count = MIN(smp_cpus, redist0_capacity);

        nb_redist_regions = mx_gicv3_redist_region_count(vms);

        qdev_prop_set_uint32(vms->gic, "len-redist-region-count",
                             nb_redist_regions);
        qdev_prop_set_uint32(vms->gic, "redist-region-count[0]", redist0_count);

        if (nb_redist_regions == 2) {
            uint32_t redist1_capacity =
                    ct->memmap[VIRT_HIGH_GIC_REDIST2].size / GICV3_REDIST_SIZE;

            qdev_prop_set_uint32(vms->gic, "redist-region-count[1]",
                MIN(smp_cpus - redist0_count, redist1_capacity));
        }
    }

    gicbusdev = SYS_BUS_DEVICE(vms->gic);
    sysbus_realize_and_unref(gicbusdev, &error_fatal);
    sysbus_mmio_map(gicbusdev, 0, ct->memmap[VIRT_GIC_DIST].base);
    if (type == 3) {
        sysbus_mmio_map(gicbusdev, 1, ct->memmap[VIRT_GIC_REDIST].base);
        if (nb_redist_regions == 2) {
            sysbus_mmio_map(gicbusdev, 2,
                            ct->memmap[VIRT_HIGH_GIC_REDIST2].base);
        }
    } else {
        sysbus_mmio_map(gicbusdev, 1, ct->memmap[VIRT_GIC_CPU].base);
        if (vms->virt) {
            sysbus_mmio_map(gicbusdev, 2, ct->memmap[VIRT_GIC_HYP].base);
            sysbus_mmio_map(gicbusdev, 3, ct->memmap[VIRT_GIC_VCPU].base);
        }
    }

    /* Wire the outputs from each CPU's generic timer and the GICv3
     * maintenance interrupt signal to the appropriate GIC PPI inputs,
     * and the GIC's IRQ/FIQ/VIRQ/VFIQ interrupt outputs to the CPU's inputs.
     */
    for (i = 0; i < smp_cpus; i++) {
        DeviceState *cpudev = DEVICE(qemu_get_cpu(i));
        int ppibase = NUM_IRQS + i * GIC_INTERNAL + GIC_NR_SGIS;
        int irq;
        /* Mapping from the output timer irq lines from the CPU to the
         * GIC PPI inputs we use for the virt board.
         */
        const int timer_irq[] = {
            [GTIMER_PHYS] = ARCH_TIMER_NS_EL1_IRQ,
            [GTIMER_VIRT] = ARCH_TIMER_VIRT_IRQ,
            [GTIMER_HYP]  = ARCH_TIMER_NS_EL2_IRQ,
            [GTIMER_SEC]  = ARCH_TIMER_S_EL1_IRQ,
        };

        for (irq = 0; irq < ARRAY_SIZE(timer_irq); irq++) {
            qdev_connect_gpio_out(cpudev, irq,
                                  qdev_get_gpio_in(vms->gic,
                                                   ppibase + timer_irq[irq]));
        }

        if (type == 3) {
            qemu_irq irq = qdev_get_gpio_in(vms->gic,
                                            ppibase + ARCH_GIC_MAINT_IRQ);
            qdev_connect_gpio_out_named(cpudev, "gicv3-maintenance-interrupt",
                                        0, irq);
        } else if (vms->virt) {
            qemu_irq irq = qdev_get_gpio_in(vms->gic,
                                            ppibase + ARCH_GIC_MAINT_IRQ);
            sysbus_connect_irq(gicbusdev, i + 4 * smp_cpus, irq);
        }

        qdev_connect_gpio_out_named(cpudev, "pmu-interrupt", 0,
                                    qdev_get_gpio_in(vms->gic, ppibase
                                                     + VIRTUAL_PMU_IRQ));

        sysbus_connect_irq(gicbusdev, i, qdev_get_gpio_in(cpudev, ARM_CPU_IRQ));
        sysbus_connect_irq(gicbusdev, i + smp_cpus,
                           qdev_get_gpio_in(cpudev, ARM_CPU_FIQ));
        sysbus_connect_irq(gicbusdev, i + 2 * smp_cpus,
                           qdev_get_gpio_in(cpudev, ARM_CPU_VIRQ));
        sysbus_connect_irq(gicbusdev, i + 3 * smp_cpus,
                           qdev_get_gpio_in(cpudev, ARM_CPU_VFIQ));
    }

    fdt_add_gic_node(vms);
}

static void dev_uart_create(const MX_MachineState *vms, int uart,
                        MemoryRegion *mem, Chardev *chr)
{
    char *nodename;
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr base = ct->memmap[uart].base;
    hwaddr size = ct->memmap[uart].size;
    int irq = ct->irqmap[uart];
    const char compat[] = "arm,pl011\0arm,primecell";
    const char clocknames[] = "uartclk\0apb_pclk";
    DeviceState *dev = qdev_new(TYPE_PL011);
    SysBusDevice *s = SYS_BUS_DEVICE(dev);
    MachineState *ms = MACHINE(vms);

    qdev_prop_set_chr(dev, "chardev", chr);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);
    memory_region_add_subregion(mem, base,
                                sysbus_mmio_get_region(s, 0));
    sysbus_connect_irq(s, 0, qdev_get_gpio_in(vms->gic, irq));

    nodename = g_strdup_printf("/pl011@%" PRIx64, base);
    qemu_fdt_add_subnode(ms->fdt, nodename);
    /* Note that we can't use setprop_string because of the embedded NUL */
    qemu_fdt_setprop(ms->fdt, nodename, "compatible",
                         compat, sizeof(compat));
    qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                     2, base, 2, size);
    qemu_fdt_setprop_cells(ms->fdt, nodename, "interrupts",
                               GIC_FDT_IRQ_TYPE_SPI, irq,
                               GIC_FDT_IRQ_FLAGS_LEVEL_HI);
    qemu_fdt_setprop_cells(ms->fdt, nodename, "clocks",
                               vms->clock_phandle, vms->clock_phandle);
    qemu_fdt_setprop(ms->fdt, nodename, "clock-names",
                         clocknames, sizeof(clocknames));

    if (uart == VIRT_UART) {
        qemu_fdt_setprop_string(ms->fdt, "/chosen", "stdout-path", nodename);
    } else {
        /* Mark as not usable by the normal world */
        qemu_fdt_setprop_string(ms->fdt, nodename, "status", "disabled");
        qemu_fdt_setprop_string(ms->fdt, nodename, "secure-status", "okay");

        qemu_fdt_setprop_string(ms->fdt, "/secure-chosen", "stdout-path",
                                nodename);
    }

    g_free(nodename);
}

static void dev_rtc_create(const MX_MachineState *vms)
{
    char *nodename;
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr base = ct->memmap[VIRT_RTC].base;
    hwaddr size = ct->memmap[VIRT_RTC].size;
    int irq = ct->irqmap[VIRT_RTC];
    const char compat[] = "arm,pl031\0arm,primecell";
    MachineState *ms = MACHINE(vms);

    sysbus_create_simple("pl031", base, qdev_get_gpio_in(vms->gic, irq));

    nodename = g_strdup_printf("/pl031@%" PRIx64, base);
    qemu_fdt_add_subnode(ms->fdt, nodename);
    qemu_fdt_setprop(ms->fdt, nodename, "compatible", compat, sizeof(compat));
    qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                 2, base, 2, size);
    qemu_fdt_setprop_cells(ms->fdt, nodename, "interrupts",
                           GIC_FDT_IRQ_TYPE_SPI, irq,
                           GIC_FDT_IRQ_FLAGS_LEVEL_HI);
    qemu_fdt_setprop_cell(ms->fdt, nodename, "clocks", vms->clock_phandle);
    qemu_fdt_setprop_string(ms->fdt, nodename, "clock-names", "apb_pclk");
    g_free(nodename);
}

static DeviceState *gpio_key_dev;
static void mx_powerdown_req(Notifier *n, void *opaque)
{
	/* use gpio Pin 3 for power button event */
	qemu_set_irq(qdev_get_gpio_in(gpio_key_dev, 0), 1);
}

static void create_gpio_keys(char *fdt, DeviceState *pl061_dev,
                             uint32_t phandle)
{
    gpio_key_dev = sysbus_create_simple("gpio-key", -1,
                                        qdev_get_gpio_in(pl061_dev, 3));

    qemu_fdt_add_subnode(fdt, "/gpio-keys");
    qemu_fdt_setprop_string(fdt, "/gpio-keys", "compatible", "gpio-keys");
    qemu_fdt_setprop_cell(fdt, "/gpio-keys", "#size-cells", 0);
    qemu_fdt_setprop_cell(fdt, "/gpio-keys", "#address-cells", 1);

    qemu_fdt_add_subnode(fdt, "/gpio-keys/poweroff");
    qemu_fdt_setprop_string(fdt, "/gpio-keys/poweroff",
                            "label", "GPIO Key Poweroff");
    qemu_fdt_setprop_cell(fdt, "/gpio-keys/poweroff", "linux,code",
                          KEY_POWER);
    qemu_fdt_setprop_cells(fdt, "/gpio-keys/poweroff",
                           "gpios", phandle, 3, 0);
}

#define SECURE_GPIO_POWEROFF 0
#define SECURE_GPIO_RESET    1

static void create_secure_gpio_pwr(char *fdt, DeviceState *pl061_dev,
                                   uint32_t phandle)
{
    DeviceState *gpio_pwr_dev;

    /* gpio-pwr */
    gpio_pwr_dev = sysbus_create_simple("gpio-pwr", -1, NULL);

    /* connect secure pl061 to gpio-pwr */
    qdev_connect_gpio_out(pl061_dev, SECURE_GPIO_RESET,
                          qdev_get_gpio_in_named(gpio_pwr_dev, "reset", 0));
    qdev_connect_gpio_out(pl061_dev, SECURE_GPIO_POWEROFF,
                          qdev_get_gpio_in_named(gpio_pwr_dev, "shutdown", 0));

    qemu_fdt_add_subnode(fdt, "/gpio-poweroff");
    qemu_fdt_setprop_string(fdt, "/gpio-poweroff", "compatible",
                            "gpio-poweroff");
    qemu_fdt_setprop_cells(fdt, "/gpio-poweroff",
                           "gpios", phandle, SECURE_GPIO_POWEROFF, 0);
    qemu_fdt_setprop_string(fdt, "/gpio-poweroff", "status", "disabled");
    qemu_fdt_setprop_string(fdt, "/gpio-poweroff", "secure-status",
                            "okay");

    qemu_fdt_add_subnode(fdt, "/gpio-restart");
    qemu_fdt_setprop_string(fdt, "/gpio-restart", "compatible",
                            "gpio-restart");
    qemu_fdt_setprop_cells(fdt, "/gpio-restart",
                           "gpios", phandle, SECURE_GPIO_RESET, 0);
    qemu_fdt_setprop_string(fdt, "/gpio-restart", "status", "disabled");
    qemu_fdt_setprop_string(fdt, "/gpio-restart", "secure-status",
                            "okay");
}

static void create_gpio_devices(const MX_MachineState *vms, int gpio,
                                MemoryRegion *mem)
{
    char *nodename;
    DeviceState *pl061_dev;
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr base = ct->memmap[gpio].base;
    hwaddr size = ct->memmap[gpio].size;
    int irq = ct->irqmap[gpio];
    const char compat[] = "arm,pl061\0arm,primecell";
    SysBusDevice *s;
    MachineState *ms = MACHINE(vms);

    pl061_dev = qdev_new("pl061");
    /* Pull lines down to 0 if not driven by the PL061 */
    qdev_prop_set_uint32(pl061_dev, "pullups", 0);
    qdev_prop_set_uint32(pl061_dev, "pulldowns", 0xff);
    s = SYS_BUS_DEVICE(pl061_dev);
    sysbus_realize_and_unref(s, &error_fatal);
    memory_region_add_subregion(mem, base, sysbus_mmio_get_region(s, 0));
    sysbus_connect_irq(s, 0, qdev_get_gpio_in(vms->gic, irq));

    uint32_t phandle = qemu_fdt_alloc_phandle(ms->fdt);
    nodename = g_strdup_printf("/pl061@%" PRIx64, base);
    qemu_fdt_add_subnode(ms->fdt, nodename);
    qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                 2, base, 2, size);
    qemu_fdt_setprop(ms->fdt, nodename, "compatible", compat, sizeof(compat));
    qemu_fdt_setprop_cell(ms->fdt, nodename, "#gpio-cells", 2);
    qemu_fdt_setprop(ms->fdt, nodename, "gpio-controller", NULL, 0);
    qemu_fdt_setprop_cells(ms->fdt, nodename, "interrupts",
                           GIC_FDT_IRQ_TYPE_SPI, irq,
                           GIC_FDT_IRQ_FLAGS_LEVEL_HI);
    qemu_fdt_setprop_cell(ms->fdt, nodename, "clocks", vms->clock_phandle);
    qemu_fdt_setprop_string(ms->fdt, nodename, "clock-names", "apb_pclk");
    qemu_fdt_setprop_cell(ms->fdt, nodename, "phandle", phandle);

    if (gpio != VIRT_GPIO) {
        /* Mark as not usable by the normal world */
        qemu_fdt_setprop_string(ms->fdt, nodename, "status", "disabled");
        qemu_fdt_setprop_string(ms->fdt, nodename, "secure-status", "okay");
    }
    g_free(nodename);

    /* Child gpio devices */
    if (gpio == VIRT_GPIO) {
        create_gpio_keys(ms->fdt, pl061_dev, phandle);
    } else {
        create_secure_gpio_pwr(ms->fdt, pl061_dev, phandle);
    }
}

static void create_virtio_devices(const MX_MachineState *vms)
{
    int i;
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr size = ct->memmap[VIRT_MMIO].size;
    MachineState *ms = MACHINE(vms);

    /* We create the transports in forwards order. Since qbus_realize()
     * prepends (not appends) new child buses, the incrementing loop below will
     * create a list of virtio-mmio buses with decreasing base addresses.
     *
     * When a -device option is processed from the command line,
     * qbus_find_recursive() picks the next free virtio-mmio bus in forwards
     * order. The upshot is that -device options in increasing command line
     * order are mapped to virtio-mmio buses with decreasing base addresses.
     *
     * When this code was originally written, that arrangement ensured that the
     * guest Linux kernel would give the lowest "name" (/dev/vda, eth0, etc) to
     * the first -device on the command line. (The end-to-end order is a
     * function of this loop, qbus_realize(), qbus_find_recursive(), and the
     * guest kernel's name-to-address assignment strategy.)
     *
     * Meanwhile, the kernel's traversal seems to have been reversed; see eg.
     * the message, if not necessarily the code, of commit 70161ff336.
     * Therefore the loop now establishes the inverse of the original intent.
     *
     * Unfortunately, we can't counteract the kernel change by reversing the
     * loop; it would break existing command lines.
     *
     * In any case, the kernel makes no guarantee about the stability of
     * enumeration order of virtio devices (as demonstrated by it changing
     * between kernel versions). For reliable and stable identification
     * of disks users must use UUIDs or similar mechanisms.
     */
    for (i = 0; i < NUM_VIRTIO_TRANSPORTS; i++) {
        int irq = ct->irqmap[VIRT_MMIO] + i;
        hwaddr base = ct->memmap[VIRT_MMIO].base + i * size;

        sysbus_create_simple("virtio-mmio", base,
                             qdev_get_gpio_in(vms->gic, irq));
    }

    /* We add dtb nodes in reverse order so that they appear in the finished
     * device tree lowest address first.
     *
     * Note that this mapping is independent of the loop above. The previous
     * loop influences virtio device to virtio transport assignment, whereas
     * this loop controls how virtio transports are laid out in the dtb.
     */
    for (i = NUM_VIRTIO_TRANSPORTS - 1; i >= 0; i--) {
        char *nodename;
        int irq = ct->irqmap[VIRT_MMIO] + i;
        hwaddr base = ct->memmap[VIRT_MMIO].base + i * size;

        nodename = g_strdup_printf("/virtio_mmio@%" PRIx64, base);
        qemu_fdt_add_subnode(ms->fdt, nodename);
        qemu_fdt_setprop_string(ms->fdt, nodename,
                                "compatible", "virtio,mmio");
        qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                     2, base, 2, size);
        qemu_fdt_setprop_cells(ms->fdt, nodename, "interrupts",
                               GIC_FDT_IRQ_TYPE_SPI, irq,
                               GIC_FDT_IRQ_FLAGS_EDGE_LO_HI);
        qemu_fdt_setprop(ms->fdt, nodename, "dma-coherent", NULL, 0);
        g_free(nodename);
    }
}

#define VIRT_FLASH_SECTOR_SIZE (256 * KiB)

static PFlashCFI01 *mx_flash_create1(MX_MachineState *vms,
                                        const char *name,
                                        const char *alias_prop_name)
{
    /*
     * Create a single flash device.  We use the same parameters as
     * the flash devices on the Versatile Express board.
     */
    DeviceState *dev = qdev_new(TYPE_PFLASH_CFI01);

    qdev_prop_set_uint64(dev, "sector-length", VIRT_FLASH_SECTOR_SIZE);
    qdev_prop_set_uint8(dev, "width", 4);
    qdev_prop_set_uint8(dev, "device-width", 2);
    qdev_prop_set_bit(dev, "big-endian", false);
    qdev_prop_set_uint16(dev, "id0", 0x89);
    qdev_prop_set_uint16(dev, "id1", 0x18);
    qdev_prop_set_uint16(dev, "id2", 0x00);
    qdev_prop_set_uint16(dev, "id3", 0x00);
    qdev_prop_set_string(dev, "name", name);
    object_property_add_child(OBJECT(vms), name, OBJECT(dev));
    object_property_add_alias(OBJECT(vms), alias_prop_name,
                              OBJECT(dev), "drive");
    return PFLASH_CFI01(dev);
}

static void mx_flash_create(MX_MachineState *vms)
{
    vms->flash[0] = mx_flash_create1(vms, "virt.flash0", "pflash0");
    vms->flash[1] = mx_flash_create1(vms, "virt.flash1", "pflash1");
}

static void mx_flash_map1(PFlashCFI01 *flash,
                            hwaddr base, hwaddr size,
                            MemoryRegion *sysmem)
{
    DeviceState *dev = DEVICE(flash);

    assert(QEMU_IS_ALIGNED(size, VIRT_FLASH_SECTOR_SIZE));
    assert(size / VIRT_FLASH_SECTOR_SIZE <= UINT32_MAX);
    qdev_prop_set_uint32(dev, "num-blocks", size / VIRT_FLASH_SECTOR_SIZE);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

    memory_region_add_subregion(sysmem, base,
                                sysbus_mmio_get_region(SYS_BUS_DEVICE(dev),
                                                       0));
}

static void mx_flash_map(MX_MachineState *vms,
                           MemoryRegion *sysmem,
                           MemoryRegion *secure_sysmem)
{
	const MX_CoreType *ct = vms->machine_type->core_type;
    /*
     * Map two flash devices to fill the VIRT_FLASH space in the memmap.
     * sysmem is the system memory space. secure_sysmem is the secure view
     * of the system, and the first flash device should be made visible only
     * there. The second flash device is visible to both secure and nonsecure.
     * If sysmem == secure_sysmem this means there is no separate Secure
     * address space and both flash devices are generally visible.
     */
    hwaddr flashsize = ct->memmap[VIRT_FLASH].size / 2;
    hwaddr flashbase = ct->memmap[VIRT_FLASH].base;

    mx_flash_map1(vms->flash[0], flashbase, flashsize,
                    secure_sysmem);
    mx_flash_map1(vms->flash[1], flashbase + flashsize, flashsize,
                    sysmem);
}

static void mx_flash_fdt(MX_MachineState *vms,
                           MemoryRegion *sysmem,
                           MemoryRegion *secure_sysmem)
{
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr flashsize = ct->memmap[VIRT_FLASH].size / 2;
    hwaddr flashbase = ct->memmap[VIRT_FLASH].base;
    MachineState *ms = MACHINE(vms);
    char *nodename;

    if (sysmem == secure_sysmem) {
        /* Report both flash devices as a single node in the DT */
        nodename = g_strdup_printf("/flash@%" PRIx64, flashbase);
        qemu_fdt_add_subnode(ms->fdt, nodename);
        qemu_fdt_setprop_string(ms->fdt, nodename, "compatible", "cfi-flash");
        qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                     2, flashbase, 2, flashsize,
                                     2, flashbase + flashsize, 2, flashsize);
        qemu_fdt_setprop_cell(ms->fdt, nodename, "bank-width", 4);
        g_free(nodename);
    } else {
        /*
         * Report the devices as separate nodes so we can mark one as
         * only visible to the secure world.
         */
        nodename = g_strdup_printf("/secflash@%" PRIx64, flashbase);
        qemu_fdt_add_subnode(ms->fdt, nodename);
        qemu_fdt_setprop_string(ms->fdt, nodename, "compatible", "cfi-flash");
        qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                     2, flashbase, 2, flashsize);
        qemu_fdt_setprop_cell(ms->fdt, nodename, "bank-width", 4);
        qemu_fdt_setprop_string(ms->fdt, nodename, "status", "disabled");
        qemu_fdt_setprop_string(ms->fdt, nodename, "secure-status", "okay");
        g_free(nodename);

        nodename = g_strdup_printf("/flash@%" PRIx64, flashbase);
        qemu_fdt_add_subnode(ms->fdt, nodename);
        qemu_fdt_setprop_string(ms->fdt, nodename, "compatible", "cfi-flash");
        qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                     2, flashbase + flashsize, 2, flashsize);
        qemu_fdt_setprop_cell(ms->fdt, nodename, "bank-width", 4);
        g_free(nodename);
    }
}

static bool mx_firmware_init(MX_MachineState *vms,
                               MemoryRegion *sysmem,
                               MemoryRegion *secure_sysmem)
{
    int i;
    const char *bios_name;
    BlockBackend *pflash_blk0;

    /* Map legacy -drive if=pflash to machine properties */
    for (i = 0; i < ARRAY_SIZE(vms->flash); i++) {
        pflash_cfi01_legacy_drive(vms->flash[i],
                                  drive_get(IF_PFLASH, 0, i));
    }

    mx_flash_map(vms, sysmem, secure_sysmem);

    pflash_blk0 = pflash_cfi01_get_blk(vms->flash[0]);

    bios_name = MACHINE(vms)->firmware;
    if (bios_name) {
        char *fname;
        MemoryRegion *mr;
        int image_size;

        if (pflash_blk0) {
            error_report("The contents of the first flash device may be "
                         "specified with -bios or with -drive if=pflash... "
                         "but you cannot use both options at once");
            exit(1);
        }

        /* Fall back to -bios */

        fname = qemu_find_file(QEMU_FILE_TYPE_BIOS, bios_name);
        if (!fname) {
            error_report("Could not find ROM image '%s'", bios_name);
            exit(1);
        }
        mr = sysbus_mmio_get_region(SYS_BUS_DEVICE(vms->flash[0]), 0);
        image_size = load_image_mr(fname, mr);
        g_free(fname);
        if (image_size < 0) {
            error_report("Could not load ROM image '%s'", bios_name);
            exit(1);
        }
    }

    return pflash_blk0 || bios_name;
}

static FWCfgState *dev_fw_cfg_create(const MX_MachineState *vms, AddressSpace *as)
{
    MachineState *ms = MACHINE(vms);
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr base = ct->memmap[VIRT_FW_CFG].base;
    hwaddr size = ct->memmap[VIRT_FW_CFG].size;
    FWCfgState *fw_cfg;
    char *nodename;

    fw_cfg = fw_cfg_init_mem_wide(base + 8, base, 8, base + 16, as);
    fw_cfg_add_i16(fw_cfg, FW_CFG_NB_CPUS, (uint16_t)ms->smp.cpus);

    nodename = g_strdup_printf("/fw-cfg@%" PRIx64, base);
    qemu_fdt_add_subnode(ms->fdt, nodename);
    qemu_fdt_setprop_string(ms->fdt, nodename,
                            "compatible", "qemu,fw-cfg-mmio");
    qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                 2, base, 2, size);
    qemu_fdt_setprop(ms->fdt, nodename, "dma-coherent", NULL, 0);
    g_free(nodename);
    return fw_cfg;
}

static void dev_pcie_create_irq_map(const MachineState *ms,
                                uint32_t gic_phandle,
                                int first_irq, const char *nodename)
{
    int devfn, pin;
    uint32_t full_irq_map[4 * 4 * 10] = { 0 };
    uint32_t *irq_map = full_irq_map;

    for (devfn = 0; devfn <= 0x18; devfn += 0x8) {
        for (pin = 0; pin < 4; pin++) {
            int irq_type = GIC_FDT_IRQ_TYPE_SPI;
            int irq_nr = first_irq + ((pin + PCI_SLOT(devfn)) % PCI_NUM_PINS);
            int irq_level = GIC_FDT_IRQ_FLAGS_LEVEL_HI;
            int i;

            uint32_t map[] = {
                devfn << 8, 0, 0,                           /* devfn */
                pin + 1,                                    /* PCI pin */
                gic_phandle, 0, 0, irq_type, irq_nr, irq_level }; /* GIC irq */

            /* Convert map to big endian */
            for (i = 0; i < 10; i++) {
                irq_map[i] = cpu_to_be32(map[i]);
            }
            irq_map += 10;
        }
    }

    qemu_fdt_setprop(ms->fdt, nodename, "interrupt-map",
                     full_irq_map, sizeof(full_irq_map));

    qemu_fdt_setprop_cells(ms->fdt, nodename, "interrupt-map-mask",
                           cpu_to_be16(PCI_DEVFN(3, 0)), /* Slot 3 */
                           0, 0,
                           0x7           /* PCI irq */);
}

static void dev_pcie_create(MX_MachineState *vms)
{
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr base_mmio = ct->memmap[VIRT_PCIE_MMIO].base;
    hwaddr size_mmio = ct->memmap[VIRT_PCIE_MMIO].size;
    hwaddr base_mmio_high = ct->memmap[VIRT_HIGH_PCIE_MMIO].base;
    hwaddr size_mmio_high = ct->memmap[VIRT_HIGH_PCIE_MMIO].size;
    hwaddr base_pio = ct->memmap[VIRT_PCIE_PIO].base;
    hwaddr size_pio = ct->memmap[VIRT_PCIE_PIO].size;
    hwaddr base_ecam, size_ecam;
    hwaddr base = base_mmio;
    int nr_pcie_buses;
    int irq = ct->irqmap[VIRT_PCIE];
    MemoryRegion *mmio_alias;
    MemoryRegion *mmio_reg;
    MemoryRegion *ecam_alias;
    MemoryRegion *ecam_reg;
    DeviceState *dev;
    char *nodename;
    int i, ecam_id;
    PCIHostState *pci;
    MachineState *ms = MACHINE(vms);

    dev = qdev_new(TYPE_GPEX_HOST);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);

    ecam_id = VIRT_ECAM_ID(vms->highmem_ecam);
    base_ecam = ct->memmap[ecam_id].base;
    size_ecam = ct->memmap[ecam_id].size;
    nr_pcie_buses = size_ecam / PCIE_MMCFG_SIZE_MIN;
    /* Map only the first size_ecam bytes of ECAM space */
    ecam_alias = g_new0(MemoryRegion, 1);
    ecam_reg = sysbus_mmio_get_region(SYS_BUS_DEVICE(dev), 0);
    memory_region_init_alias(ecam_alias, OBJECT(dev), "pcie-ecam",
                             ecam_reg, 0, size_ecam);
    memory_region_add_subregion(get_system_memory(), base_ecam, ecam_alias);

    /* Map the MMIO window into system address space so as to expose
     * the section of PCI MMIO space which starts at the same base address
     * (ie 1:1 mapping for that part of PCI MMIO space visible through
     * the window).
     */
    mmio_alias = g_new0(MemoryRegion, 1);
    mmio_reg = sysbus_mmio_get_region(SYS_BUS_DEVICE(dev), 1);
    memory_region_init_alias(mmio_alias, OBJECT(dev), "pcie-mmio",
                             mmio_reg, base_mmio, size_mmio);
    memory_region_add_subregion(get_system_memory(), base_mmio, mmio_alias);

    if (vms->highmem) {
        /* Map high MMIO space */
        MemoryRegion *high_mmio_alias = g_new0(MemoryRegion, 1);

        memory_region_init_alias(high_mmio_alias, OBJECT(dev), "pcie-mmio-high",
                                 mmio_reg, base_mmio_high, size_mmio_high);
        memory_region_add_subregion(get_system_memory(), base_mmio_high,
                                    high_mmio_alias);
    }

    /* Map IO port space */
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 2, base_pio);

    for (i = 0; i < GPEX_NUM_IRQS; i++) {
        sysbus_connect_irq(SYS_BUS_DEVICE(dev), i,
                           qdev_get_gpio_in(vms->gic, irq + i));
        gpex_set_irq_num(GPEX_HOST(dev), i, irq + i);
    }

    pci = PCI_HOST_BRIDGE(dev);
    vms->bus = pci->bus;
    if (vms->bus) {
        for (i = 0; i < nb_nics; i++) {
            NICInfo *nd = &nd_table[i];

            if (!nd->model) {
                nd->model = g_strdup("virtio");
            }

            pci_nic_init_nofail(nd, pci->bus, nd->model, NULL);
        }
    }

    nodename = vms->pciehb_nodename = g_strdup_printf("/pcie@%" PRIx64, base);
    qemu_fdt_add_subnode(ms->fdt, nodename);
    qemu_fdt_setprop_string(ms->fdt, nodename,
                            "compatible", "pci-host-ecam-generic");
    qemu_fdt_setprop_string(ms->fdt, nodename, "device_type", "pci");
    qemu_fdt_setprop_cell(ms->fdt, nodename, "#address-cells", 3);
    qemu_fdt_setprop_cell(ms->fdt, nodename, "#size-cells", 2);
    qemu_fdt_setprop_cell(ms->fdt, nodename, "linux,pci-domain", 0);
    qemu_fdt_setprop_cells(ms->fdt, nodename, "bus-range", 0,
                           nr_pcie_buses - 1);
    qemu_fdt_setprop(ms->fdt, nodename, "dma-coherent", NULL, 0);

    qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg",
                                 2, base_ecam, 2, size_ecam);

    if (vms->highmem) {
        qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "ranges",
                                     1, FDT_PCI_RANGE_IOPORT, 2, 0,
                                     2, base_pio, 2, size_pio,
                                     1, FDT_PCI_RANGE_MMIO, 2, base_mmio,
                                     2, base_mmio, 2, size_mmio,
                                     1, FDT_PCI_RANGE_MMIO_64BIT,
                                     2, base_mmio_high,
                                     2, base_mmio_high, 2, size_mmio_high);
    } else {
        qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "ranges",
                                     1, FDT_PCI_RANGE_IOPORT, 2, 0,
                                     2, base_pio, 2, size_pio,
                                     1, FDT_PCI_RANGE_MMIO, 2, base_mmio,
                                     2, base_mmio, 2, size_mmio);
    }

    qemu_fdt_setprop_cell(ms->fdt, nodename, "#interrupt-cells", 1);
    dev_pcie_create_irq_map(ms, vms->gic_phandle, irq, nodename);
}

static void dev_platform_bus_create(MX_MachineState *vms)
{
    DeviceState *dev;
    SysBusDevice *s;
    int i;
    MemoryRegion *sysmem = get_system_memory();
	const MX_CoreType *ct = vms->machine_type->core_type;

    dev = qdev_new(TYPE_PLATFORM_BUS_DEVICE);
    dev->id = TYPE_PLATFORM_BUS_DEVICE;
    qdev_prop_set_uint32(dev, "num_irqs", PLATFORM_BUS_NUM_IRQS);
    qdev_prop_set_uint32(dev, "mmio_size", ct->memmap[VIRT_PLATFORM_BUS].size);
    sysbus_realize_and_unref(SYS_BUS_DEVICE(dev), &error_fatal);
    vms->platform_bus_dev = dev;

    s = SYS_BUS_DEVICE(dev);
    for (i = 0; i < PLATFORM_BUS_NUM_IRQS; i++) {
        int irq = ct->irqmap[VIRT_PLATFORM_BUS] + i;
        sysbus_connect_irq(s, i, qdev_get_gpio_in(vms->gic, irq));
    }

    memory_region_add_subregion(sysmem,
                                ct->memmap[VIRT_PLATFORM_BUS].base,
                                sysbus_mmio_get_region(s, 0));
}

static void dev_tag_ram_create(MemoryRegion *tag_sysmem,
                           hwaddr base, hwaddr size,
                           const char *name)
{
    MemoryRegion *tagram = g_new(MemoryRegion, 1);

    memory_region_init_ram(tagram, NULL, name, size / 32, &error_fatal);
    memory_region_add_subregion(tag_sysmem, base / 32, tagram);
}

static void dev_secure_ram_create(MX_MachineState *vms,
                              MemoryRegion *secure_sysmem,
                              MemoryRegion *secure_tag_sysmem)
{
    MemoryRegion *secram = g_new(MemoryRegion, 1);
    char *nodename;
	const MX_CoreType *ct = vms->machine_type->core_type;
    hwaddr base = ct->memmap[VIRT_SECURE_MEM].base;
    hwaddr size = ct->memmap[VIRT_SECURE_MEM].size;
    MachineState *ms = MACHINE(vms);

    memory_region_init_ram(secram, NULL, "virt.secure-ram", size,
                           &error_fatal);
    memory_region_add_subregion(secure_sysmem, base, secram);

    nodename = g_strdup_printf("/secram@%" PRIx64, base);
    qemu_fdt_add_subnode(ms->fdt, nodename);
    qemu_fdt_setprop_string(ms->fdt, nodename, "device_type", "memory");
    qemu_fdt_setprop_sized_cells(ms->fdt, nodename, "reg", 2, base, 2, size);
    qemu_fdt_setprop_string(ms->fdt, nodename, "status", "disabled");
    qemu_fdt_setprop_string(ms->fdt, nodename, "secure-status", "okay");

    if (secure_tag_sysmem) {
        dev_tag_ram_create(secure_tag_sysmem, base, size, "mach-virt.secure-tag");
    }

    g_free(nodename);
}

static void *machine_dtb(const struct arm_boot_info *binfo, int *fdt_size)
{
    const MX_MachineState *board = container_of(binfo, MX_MachineState,
                                                 bootinfo);
    MachineState *ms = MACHINE(board);


    *fdt_size = board->fdt_size;
    return ms->fdt;
}

static void mx_build_smbios(MX_MachineState *vms)
{
    MachineClass *mc = MACHINE_GET_CLASS(vms);
    MX_MachineClass *vmc = MX_MACHINE_GET_CLASS(vms);
    uint8_t *smbios_tables, *smbios_anchor;
    size_t smbios_tables_len, smbios_anchor_len;
    const char *product = "QEMU Virtual Machine";

    smbios_set_defaults("QEMU", product,
                        vmc->smbios_old_sys_ver ? "1.0" : mc->name, false,
                        true, SMBIOS_ENTRY_POINT_30);

    smbios_get_tables(MACHINE(vms), NULL, 0,
                      &smbios_tables, &smbios_tables_len,
                      &smbios_anchor, &smbios_anchor_len,
                      &error_fatal);

    if (smbios_anchor) {
        fw_cfg_add_file(vms->fw_cfg, "etc/smbios/smbios-tables",
                        smbios_tables, smbios_tables_len);
        fw_cfg_add_file(vms->fw_cfg, "etc/smbios/smbios-anchor",
                        smbios_anchor, smbios_anchor_len);
    }
}

static void mx_machine_done(Notifier *notifier, void *data)
{
    MX_MachineState *vms = container_of(notifier, MX_MachineState,
                                         machine_done);
    MachineState *ms = MACHINE(vms);
    ARMCPU *cpu = ARM_CPU(first_cpu);
    struct arm_boot_info *info = &vms->bootinfo;
    AddressSpace *as = arm_boot_address_space(cpu, info);
	const MX_CoreType *ct = vms->machine_type->core_type;

    /*
     * If the user provided a dtb, we assume the dynamic sysbus nodes
     * already are integrated there. This corresponds to a use case where
     * the dynamic sysbus nodes are complex and their generation is not yet
     * supported. In that case the user can take charge of the guest dt
     * while qemu takes charge of the qom stuff.
     */
    if (info->dtb_filename == NULL) {
        platform_bus_add_all_fdt_nodes(ms->fdt, "/intc",
                                       ct->memmap[VIRT_PLATFORM_BUS].base,
                                       ct->memmap[VIRT_PLATFORM_BUS].size,
                                       ct->irqmap[VIRT_PLATFORM_BUS]);
    }
    if (arm_load_dtb(info->dtb_start, info, info->dtb_limit, as, ms) < 0) {
        exit(1);
    }

    fw_cfg_add_extra_pci_roots(vms->bus, vms->fw_cfg);

    mx_build_smbios(vms);
}

static uint64_t mx_cpu_mp_affinity(MX_MachineState *vms, int idx)
{
    uint8_t clustersz = ARM_DEFAULT_CPUS_PER_CLUSTER;
	const MX_CoreType *ct = vms->machine_type->core_type;

    /* Adjust MPIDR like 64-bit KVM hosts, which incorporate the
     * GIC's target-list limitations. 32-bit KVM hosts currently
     * always create clusters of 4 CPUs, but that is expected to
     * change when they gain support for gicv3. When KVM is enabled
     * it will override the changes we make here, therefore our
     * purposes are to make TCG consistent (with 64-bit KVM hosts)
     * and to improve SGI efficiency.
     */
    if (ct->gic_version == VIRT_GIC_VERSION_3)
        clustersz = GICV3_TARGETLIST_BITS;
	else
        clustersz = GIC_TARGETLIST_BITS;

    return arm_cpu_mp_affinity(idx, clustersz);
}

/*
 * mx_cpu_post_init() must be called after the CPUs have
 * been realized and the GIC has been created.
 */
static void mx_cpu_post_init(MX_MachineState *vms, MemoryRegion *sysmem)
{
    bool aarch64 = object_property_get_bool(OBJECT(first_cpu), "aarch64", NULL);

	if (aarch64 && vms->highmem) {
        int requested_pa_size = 64 - clz64(vms->highest_gpa);
        int pamax = arm_pamax(ARM_CPU(first_cpu));

        if (pamax < requested_pa_size) {
            error_report("VCPU supports less PA bits (%d) than "
                         "requested by the memory map (%d)",
                         pamax, requested_pa_size);
            exit(1);
        }
    }
}

static void machine_init(MachineState *machine)
{
    MX_MachineState *vms = MX_MACHINE(machine);
    MX_MachineClass *vmc = MX_MACHINE_GET_CLASS(machine);
    MachineClass *mc = MACHINE_GET_CLASS(machine);
    const CPUArchIdList *possible_cpus;
    MemoryRegion *sysmem = get_system_memory();
    MemoryRegion *secure_sysmem = NULL;
    MemoryRegion *tag_sysmem = NULL;
    MemoryRegion *secure_tag_sysmem = NULL;
    int n, mx_max_cpus;
    bool firmware_loaded;
    bool aarch64 = true;
    unsigned int smp_cpus = machine->smp.cpus;
    unsigned int max_cpus = machine->smp.max_cpus;
	const MX_CoreType *ct = NULL;

	vms->machine_type = mx_mach_machtype(vmc->machine);
	ct = vms->machine_type->core_type;

    if (vms->secure) {
        /*
         * The Secure view of the world is the same as the NonSecure,
         * but with a few extra devices. Create it as a container region
         * containing the system memory at low priority; any secure-only
         * devices go in at higher priority and take precedence.
         */
        secure_sysmem = g_new(MemoryRegion, 1);
        memory_region_init(secure_sysmem, OBJECT(machine), "secure-memory",
                           UINT64_MAX);
        memory_region_add_subregion_overlap(secure_sysmem, 0, sysmem, -1);
    }

    firmware_loaded = mx_firmware_init(vms, sysmem,
                                         secure_sysmem ?: sysmem);

    /* If we have an EL3 boot ROM then the assumption is that it will
     * implement PSCI itself, so disable QEMU's internal implementation
     * so it doesn't get in the way. Instead of starting secondary
     * CPUs in PSCI powerdown state we will start them all running and
     * let the boot ROM sort them out.
     * The usual case is that we do use QEMU's PSCI implementation;
     * if the guest has EL2 then we will use SMC as the conduit,
     * and otherwise we will use HVC (for backwards compatibility and
     * because if we're using KVM then we must use HVC).
     */
    if (vms->secure && firmware_loaded) {
        vms->psci_conduit = QEMU_PSCI_CONDUIT_DISABLED;
    } else if (vms->virt) {
        vms->psci_conduit = QEMU_PSCI_CONDUIT_SMC;
    } else {
        vms->psci_conduit = QEMU_PSCI_CONDUIT_HVC;
    }

    /* The maximum number of CPUs depends on the GIC version, or on how
     * many redistributors we can fit into the memory map.
     */
    if (ct->gic_version == VIRT_GIC_VERSION_3) {
        mx_max_cpus =
            ct->memmap[VIRT_GIC_REDIST].size / GICV3_REDIST_SIZE;
        mx_max_cpus +=
            ct->memmap[VIRT_HIGH_GIC_REDIST2].size / GICV3_REDIST_SIZE;
    } else {
        mx_max_cpus = GIC_NCPU;
    }

    if (max_cpus > mx_max_cpus) {
        error_report("Number of SMP CPUs requested (%d) exceeds max CPUs "
                     "supported by machine 'mach-virt' (%d)",
                     max_cpus, mx_max_cpus);
        exit(1);
    }

    fdt_create(vms);

    possible_cpus = mc->possible_cpu_arch_ids(machine);
    assert(possible_cpus->len == max_cpus);
    for (n = 0; n < possible_cpus->len; n++) {
        Object *cpuobj;
        CPUState *cs;

        if (n >= smp_cpus) {
            break;
        }

        cpuobj = object_new(possible_cpus->cpus[n].type);
        object_property_set_int(cpuobj, "mp-affinity",
                                possible_cpus->cpus[n].arch_id, NULL);

        cs = CPU(cpuobj);
        cs->cpu_index = n;

        numa_cpu_pre_plug(&possible_cpus->cpus[cs->cpu_index], DEVICE(cpuobj),
                          &error_fatal);

        aarch64 &= object_property_get_bool(cpuobj, "aarch64", NULL);

        if (!vms->secure) {
            object_property_set_bool(cpuobj, "has_el3", false, NULL);
        }

        if (!vms->virt && object_property_find(cpuobj, "has_el2")) {
            object_property_set_bool(cpuobj, "has_el2", false, NULL);
        }

        if (vms->psci_conduit != QEMU_PSCI_CONDUIT_DISABLED) {
            object_property_set_int(cpuobj, "psci-conduit", vms->psci_conduit,
                                    NULL);

            /* Secondary CPUs start in PSCI powered-down state */
            if (n > 0) {
                object_property_set_bool(cpuobj, "start-powered-off", true,
                                         NULL);
            }
        }

        if (vmc->no_pmu && object_property_find(cpuobj, "pmu")) {
            object_property_set_bool(cpuobj, "pmu", false, NULL);
        }

        if (object_property_find(cpuobj, "reset-cbar")) {
            object_property_set_int(cpuobj, "reset-cbar",
                                    ct->memmap[VIRT_CPUPERIPHS].base,
                                    &error_abort);
        }

        object_property_set_link(cpuobj, "memory", OBJECT(sysmem),
                                 &error_abort);
        if (vms->secure) {
            object_property_set_link(cpuobj, "secure-memory",
                                     OBJECT(secure_sysmem), &error_abort);
        }

        qdev_realize(DEVICE(cpuobj), NULL, &error_fatal);
        object_unref(cpuobj);
    }
    fdt_add_timer_nodes(vms);
    fdt_add_cpu_nodes(vms);

    memory_region_add_subregion(sysmem, ct->memmap[VIRT_MEM].base,
                                machine->ram);
    if (machine->device_memory) {
        memory_region_add_subregion(sysmem, machine->device_memory->base,
                                    &machine->device_memory->mr);
    }

    mx_flash_fdt(vms, sysmem, secure_sysmem ?: sysmem);

    dev_gic_create(vms);

    mx_cpu_post_init(vms, sysmem);

    fdt_add_pmu_nodes(vms);

    dev_uart_create(vms, VIRT_UART, sysmem, serial_hd(0));

    if (vms->secure) {
        dev_secure_ram_create(vms, secure_sysmem, secure_tag_sysmem);
        dev_uart_create(vms, VIRT_SECURE_UART, secure_sysmem, serial_hd(1));
    }

    if (tag_sysmem) {
        dev_tag_ram_create(tag_sysmem, ct->memmap[VIRT_MEM].base,
                       machine->ram_size, "mach-virt.tag");
    }

    vms->highmem_ecam &= vms->highmem && (!firmware_loaded || aarch64);

    dev_rtc_create(vms);

    dev_pcie_create(vms);

	create_gpio_devices(vms, VIRT_GPIO, sysmem);

    if (vms->secure && !vmc->no_secure_gpio) {
        create_gpio_devices(vms, VIRT_SECURE_GPIO, secure_sysmem);
    }

     /* connect powerdown request */
     vms->powerdown_notifier.notify = mx_powerdown_req;
     qemu_register_powerdown_notifier(&vms->powerdown_notifier);

    /* Create mmio transports, so the user can create virtio backends
     * (which will be automatically plugged in to the transports). If
     * no backend is created the transport will just sit harmlessly idle.
     */
    create_virtio_devices(vms);

    vms->fw_cfg = dev_fw_cfg_create(vms, &address_space_memory);
    rom_set_fw(vms->fw_cfg);

    dev_platform_bus_create(vms);

    vms->bootinfo.ram_size = machine->ram_size;
    vms->bootinfo.nb_cpus = smp_cpus;
    vms->bootinfo.board_id = -1;
    vms->bootinfo.loader_start = ct->memmap[VIRT_MEM].base;
    vms->bootinfo.get_dtb = machine_dtb;
    vms->bootinfo.skip_dtb_autoload = true;
    vms->bootinfo.firmware_loaded = firmware_loaded;
    arm_load_kernel(ARM_CPU(first_cpu), machine, &vms->bootinfo);

    vms->machine_done.notify = mx_machine_done;
    qemu_add_machine_init_done_notifier(&vms->machine_done);
}

static bool mx_get_secure(Object *obj, Error **errp)
{
    MX_MachineState *vms = MX_MACHINE(obj);

    return vms->secure;
}

static void mx_set_secure(Object *obj, bool value, Error **errp)
{
    MX_MachineState *vms = MX_MACHINE(obj);

    vms->secure = value;
}

static bool mx_get_virt(Object *obj, Error **errp)
{
    MX_MachineState *vms = MX_MACHINE(obj);

    return vms->virt;
}

static void mx_set_virt(Object *obj, bool value, Error **errp)
{
    MX_MachineState *vms = MX_MACHINE(obj);

    vms->virt = value;
}

static bool mx_get_highmem(Object *obj, Error **errp)
{
    MX_MachineState *vms = MX_MACHINE(obj);

    return vms->highmem;
}

static void mx_set_highmem(Object *obj, bool value, Error **errp)
{
    MX_MachineState *vms = MX_MACHINE(obj);

    vms->highmem = value;
}

static CpuInstanceProperties
mx_cpu_index_to_props(MachineState *ms, unsigned cpu_index)
{
    MachineClass *mc = MACHINE_GET_CLASS(ms);
    const CPUArchIdList *possible_cpus = mc->possible_cpu_arch_ids(ms);

    assert(cpu_index < possible_cpus->len);
    return possible_cpus->cpus[cpu_index].props;
}

static int64_t mx_get_default_cpu_node_id(const MachineState *ms, int idx)
{
    return idx % ms->numa_state->num_nodes;
}

static const CPUArchIdList *mx_possible_cpu_arch_ids(MachineState *ms)
{
    int n;
    unsigned int max_cpus = ms->smp.max_cpus;
    MX_MachineState *vms = MX_MACHINE(ms);

    if (ms->possible_cpus) {
        assert(ms->possible_cpus->len == max_cpus);
        return ms->possible_cpus;
    }

    ms->possible_cpus = g_malloc0(sizeof(CPUArchIdList) +
                                  sizeof(CPUArchId) * max_cpus);
    ms->possible_cpus->len = max_cpus;
    for (n = 0; n < ms->possible_cpus->len; n++) {
        ms->possible_cpus->cpus[n].type = ms->cpu_type;
        ms->possible_cpus->cpus[n].arch_id =
            mx_cpu_mp_affinity(vms, n);
        ms->possible_cpus->cpus[n].props.has_thread_id = true;
        ms->possible_cpus->cpus[n].props.thread_id = n;
    }
    return ms->possible_cpus;
}

static void mx_s1_class_property_init(MachineClass *mc)
{
	ObjectClass *oc = OBJECT_CLASS(mc);

	object_class_property_add_bool(oc, "secure", mx_get_secure,
                                   mx_set_secure);
    object_class_property_set_description(oc, "secure",
                                                "Set on/off to enable/disable the ARM "
                                                "Security Extensions (TrustZone)");

    object_class_property_add_bool(oc, "virtualization", mx_get_virt,
                                   mx_set_virt);
    object_class_property_set_description(oc, "virtualization",
                                          "Set on/off to enable/disable emulating a "
                                          "guest CPU which implements the ARM "
                                          "Virtualization Extensions");

    object_class_property_add_bool(oc, "highmem", mx_get_highmem,
                                   mx_set_highmem);
    object_class_property_set_description(oc, "highmem",
                                          "Set on/off to enable/disable using "
                                          "physical address space above 32 bits");
}

static void mx_s1_class_init(MachineClass *mc, MX_MachineClass *sc)
{
	const MX_CoreType *ct = mx_mach_coretype(sc->machine);

    mc->desc = g_strdup_printf("MX %s %s (%d cores)",
			 ct->code_name, ct->cpu_type, ct->cores);

    mc->init = machine_init;

    mc->max_cpus = ct->cores;
    mc->block_default_type = IF_VIRTIO;
    mc->no_cdrom = 1;
    mc->pci_allow_0_address = true;
    /* We know we will never create a pre-ARMv7 CPU which needs 1K pages */
    mc->minimum_page_bits = 12;
    mc->possible_cpu_arch_ids = mx_possible_cpu_arch_ids;
    mc->cpu_index_to_instance_props = mx_cpu_index_to_props;
    mc->default_cpu_type = ct->cpu_type;
    mc->get_default_cpu_node_id = mx_get_default_cpu_node_id;
    mc->nvdimm_supported = false;
    mc->auto_enable_numa_with_memhp = true;
    mc->auto_enable_numa_with_memdev = true;
    mc->default_ram_id = "mx-virt.ram";

	mx_s1_class_property_init(mc);
}

static void mx_s1_emu_class_init(ObjectClass *oc, void *data)
{
    MachineClass *mc = MACHINE_CLASS(oc);
    MX_MachineClass *sc = MX_MACHINE_CLASS(oc);

    sc->core = MX_CORE_S1_STD;
    sc->machine = MX_MACHINE_S1_EMU;
    sc->revision = 0x0;

	mx_s1_class_init(mc, MX_MACHINE_CLASS(oc));
}

static void mx_instance_init(Object *obj)
{
    MX_MachineState *vms = MX_MACHINE(obj);
    MX_MachineClass *vmc = MX_MACHINE_GET_CLASS(vms);

    /* EL3 is disabled by default on virt: this makes us consistent
     * between KVM and TCG for this board, and it also allows us to
     * boot UEFI blobs which assume no TrustZone support.
     */
    vms->secure = false;

    /* EL2 is also disabled by default, for similar reasons */
    vms->virt = false;

    /* High memory is enabled by default */
    vms->highmem = true;

    vms->highmem_ecam = !vmc->no_highmem_ecam;

    mx_flash_create(vms);
}

static const TypeInfo mx_machine_types[] = {
	{
        .name           = MACHINE_TYPE_NAME("mx-s1-emu"),
        .parent         = TYPE_MX_MACHINE,
        .class_init     = mx_s1_emu_class_init,
    },
	{
        .name           = TYPE_MX_MACHINE,
        .parent         = TYPE_MACHINE,
		.instance_size  = sizeof(MX_MachineState),
		.class_size     = sizeof(MX_MachineClass),
		.instance_init  = mx_instance_init,
		.abstract       = true,
    }
};

DEFINE_TYPES(mx_machine_types)
