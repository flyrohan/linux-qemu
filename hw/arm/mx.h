/*
 * MX emulation (c) 2023 Junghyun Kim
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */

#ifndef QEMU_ARM_MX_H
#define QEMU_ARM_MX_H

#include "exec/hwaddr.h"
#include "qemu/notify.h"
#include "hw/boards.h"
#include "hw/arm/boot.h"
#include "hw/block/flash.h"
#include "sysemu/kvm.h"
#include "hw/intc/arm_gicv3_common.h"
#include "qom/object.h"

#define NUM_GICV2M_SPIS       64
#define NUM_VIRTIO_TRANSPORTS 32
#define NUM_SMMU_IRQS          4

#define ARCH_GIC_MAINT_IRQ  9

#define ARCH_TIMER_VIRT_IRQ   11
#define ARCH_TIMER_S_EL1_IRQ  13
#define ARCH_TIMER_NS_EL1_IRQ 14
#define ARCH_TIMER_NS_EL2_IRQ 10

#define VIRTUAL_PMU_IRQ 7

#define PPI(irq) ((irq) + 16)

/* See Linux kernel arch/arm64/include/asm/pvclock-abi.h */
#define PVTIME_SIZE_PER_CPU 64

enum {
    VIRT_FLASH,
    VIRT_MEM,
    VIRT_CPUPERIPHS,
    VIRT_GIC_DIST,
    VIRT_GIC_CPU,
    VIRT_GIC_V2M,
    VIRT_GIC_HYP,
    VIRT_GIC_VCPU,
    VIRT_GIC_ITS,
    VIRT_GIC_REDIST,
    VIRT_SMMU,
    VIRT_UART,
    VIRT_MMIO,
    VIRT_RTC,
    VIRT_FW_CFG,
    VIRT_PCIE,
    VIRT_PCIE_MMIO,
    VIRT_PCIE_PIO,
    VIRT_PCIE_ECAM,
    VIRT_PLATFORM_BUS,
    VIRT_GPIO,
    VIRT_SECURE_UART,
    VIRT_SECURE_MEM,
    VIRT_SECURE_GPIO,
    VIRT_PCDIMM_ACPI,
    VIRT_ACPI_GED,
    VIRT_NVDIMM_ACPI,
    VIRT_PVTIME,
    VIRT_LOWMEMMAP_LAST,
};

/* indices of IO regions located after the RAM */
enum {
    VIRT_HIGH_GIC_REDIST2 =  VIRT_LOWMEMMAP_LAST,
    VIRT_HIGH_PCIE_ECAM,
    VIRT_HIGH_PCIE_MMIO,
};

typedef enum VirtIOMMUType {
    VIRT_IOMMU_NONE,
    VIRT_IOMMU_SMMUV3,
    VIRT_IOMMU_VIRTIO,
} VirtIOMMUType;

typedef enum VirtMSIControllerType {
    VIRT_MSI_CTRL_NONE,
    VIRT_MSI_CTRL_GICV2M,
    VIRT_MSI_CTRL_ITS,
} VirtMSIControllerType;

typedef enum VirtGICType {
    VIRT_GIC_VERSION_MAX,
    VIRT_GIC_VERSION_HOST,
    VIRT_GIC_VERSION_2,
    VIRT_GIC_VERSION_3,
    VIRT_GIC_VERSION_NOSEL,
} VirtGICType;

typedef enum MX_CORE_TYPE {
	MX_CORE_S1_STD,
} MX_CORE_TYPE;

typedef enum MX_MACHINE_TYPE {
	MX_MACHINE_S1_EMU,
	MX_MACHINE_S1_EVB,
} MX_MACHINE_TYPE;

#endif /* QEMU_ARM_MX_H */
