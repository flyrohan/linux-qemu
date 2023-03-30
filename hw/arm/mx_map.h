/*
 * MX emulation (c) 2023 Junghyun Kim
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 */
#ifndef QEMU_ARM_MX_MAP_H
#define QEMU_ARM_MX_MAP_H

#include "mx.h"

/* Legacy RAM limit in GB (< version 4.0) */
#define LEGACY_RAMLIMIT_GB 255
#define LEGACY_RAMLIMIT_BYTES (LEGACY_RAMLIMIT_GB * GiB)
/* Number of external interrupt lines to configure the GIC with */
#define NUM_IRQS 256
#define PLATFORM_BUS_NUM_IRQS 64

static const int mx_irqmap[][NUM_IRQS] = {	
    [MX_CORE_S1_STD] = {
		[VIRT_UART] = 1,
		[VIRT_RTC] = 2,
		[VIRT_PCIE] = 3, /* ... to 6 */
		[VIRT_GPIO] = 7,
		[VIRT_SECURE_UART] = 8,
		[VIRT_MMIO] = 16, /* ...to 16 + NUM_VIRTIO_TRANSPORTS - 1 */
		[VIRT_GIC_V2M] = 48, /* ...to 48 + NUM_GICV2M_SPIS - 1 */
		[VIRT_PLATFORM_BUS] = 112, /* ...to 112 + PLATFORM_BUS_NUM_IRQS -1 */
	},
};

/* Addresses and sizes of our components.
 * 0..128MB is space for a flash device so we can run bootrom code such as UEFI.
 * 128MB..256MB is used for miscellaneous device I/O.
 * 256MB..1GB is reserved for possible future PCI support (ie where the
 * PCI memory window will go if we add a PCI host controller).
 * 1GB and up is RAM (which may happily spill over into the
 * high memory region beyond 4GB).
 * This represents a compromise between how much RAM can be given to
 * a 32 bit VM and leaving space for expansion and in particular for PCI.
 * Note that devices should generally be placed at multiples of 0x10000,
 * to accommodate guests using 64K pages.
 */
static const MemMapEntry mx_memmap[][64] = {	
    [MX_CORE_S1_STD] = {
		/* Space up to 0x8000000 is reserved for a boot ROM */
		[VIRT_FLASH] =              {          0, 0x08000000 },
		[VIRT_CPUPERIPHS] =         { 0x08000000, 0x00020000 },
		/* GIC distributor and CPU interfaces sit inside the CPU peripheral space */
		[VIRT_GIC_DIST] =           { 0x08000000, 0x00010000 },
		[VIRT_GIC_CPU] =            { 0x08010000, 0x00010000 },
		[VIRT_GIC_V2M] =            { 0x08020000, 0x00001000 },
		[VIRT_GIC_HYP] =            { 0x08030000, 0x00010000 },
		[VIRT_GIC_VCPU] =           { 0x08040000, 0x00010000 },
		/* The space in between here is reserved for GICv3 CPU/vCPU/HYP */
		[VIRT_GIC_ITS] =            { 0x08080000, 0x00020000 },
		/* This redistributor space allows up to 2*64kB*123 CPUs */
		[VIRT_GIC_REDIST] =         { 0x080A0000, 0x00F60000 },
		[VIRT_UART] =               { 0x09000000, 0x00001000 },
		[VIRT_RTC] =                { 0x09010000, 0x00001000 },
		[VIRT_FW_CFG] =             { 0x09020000, 0x00000018 },
		[VIRT_GPIO] =               { 0x09030000, 0x00001000 },
		[VIRT_SECURE_UART] =        { 0x09040000, 0x00001000 },
		[VIRT_SMMU] =               { 0x09050000, 0x00020000 },
		[VIRT_PVTIME] =             { 0x090a0000, 0x00010000 },
		[VIRT_SECURE_GPIO] =        { 0x090b0000, 0x00001000 },
		[VIRT_MMIO] =               { 0x0a000000, 0x00000200 },
		/* ...repeating for a total of NUM_VIRTIO_TRANSPORTS, each of that size */
		[VIRT_PLATFORM_BUS] =       { 0x0c000000, 0x02000000 },
		[VIRT_SECURE_MEM] =         { 0x0e000000, 0x01000000 },
		[VIRT_PCIE_MMIO] =          { 0x10000000, 0x2eff0000 },
		[VIRT_PCIE_PIO] =           { 0x3eff0000, 0x00010000 },
		[VIRT_PCIE_ECAM] =          { 0x3f000000, 0x01000000 },
		/* Actual RAM size depends on initial RAM and device memory settings */
		[VIRT_MEM] =                { GiB, LEGACY_RAMLIMIT_BYTES },
#if 0
		/*
		 * Highmem IO Regions: This memory map is floating, located after the RAM.
		 * Each MemMapEntry base (GPA) will be dynamically computed, depending on the
		 * top of the RAM, so that its base get the same alignment as the size,
		 * ie. a 512GiB entry will be aligned on a 512GiB boundary. If there is
		 * less than 256GiB of RAM, the floating area starts at the 256GiB mark.
		 * Note the extended_memmap is sized so that it eventually also includes the
		 * base_memmap entries (VIRT_HIGH_GIC_REDIST2 index is greater than the last
		 * index of base_memmap).
		 */
		/* Additional 64 MB redist region (can contain up to 512 redistributors) */
		[VIRT_HIGH_GIC_REDIST2] =   { ROUND_UP(base, size), 64 * MiB },
		[VIRT_HIGH_PCIE_ECAM] =     { 0x0, 256 * MiB },
		/* Second PCIe window */
		[VIRT_HIGH_PCIE_MMIO] =     { 0x0, 512 * GiB },
#endif				
	},
};

#endif /* QEMU_ARM_MX_MAP_H */
