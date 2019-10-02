/* Machine description for AArch64 architecture.
   Copyright (C) 2012-2019 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define CTR_IDC_SHIFT           28
#define CTR_DIC_SHIFT           29

void __aarch64_sync_cache_range (const void *, const void *);

void
__aarch64_sync_cache_range (const void *base, const void *end)
{
  unsigned icache_lsize;
  unsigned dcache_lsize;
  static unsigned int cache_info = 0;
  const char *address;

  if (! cache_info)
    /* CTR_EL0 [3:0] contains log2 of icache line size in words.
       CTR_EL0 [19:16] contains log2 of dcache line size in words.  */
    asm volatile ("mrs\t%0, ctr_el0":"=r" (cache_info));

  icache_lsize = 4 << (cache_info & 0xF);
  dcache_lsize = 4 << ((cache_info >> 16) & 0xF);

  /* If CTR_EL0.IDC is enabled, Data cache clean to the Point of Unification is
     not required for instruction to data coherence.  */

  if (((cache_info >> CTR_IDC_SHIFT) & 0x1) == 0x0) {
    /* Loop over the address range, clearing one cache line at once.
       Data cache must be flushed to unification first to make sure the
       instruction cache fetches the updated data.  'end' is exclusive,
       as per the GNU definition of __clear_cache.  */

    /* Make the start address of the loop cache aligned.  */
    address = (const char*) ((__UINTPTR_TYPE__) base
			     & ~ (__UINTPTR_TYPE__) (dcache_lsize - 1));

    for (; address < (const char *) end; address += dcache_lsize)
      asm volatile ("dc\tcvau, %0"
		    :
		    : "r" (address)
		    : "memory");
  }

  asm volatile ("dsb\tish" : : : "memory");

  /* If CTR_EL0.DIC is enabled, Instruction cache cleaning to the Point of
     Unification is not required for instruction to data coherence.  */

  if (((cache_info >> CTR_DIC_SHIFT) & 0x1) == 0x0) {
    /* Make the start address of the loop cache aligned.  */
    address = (const char*) ((__UINTPTR_TYPE__) base
			     & ~ (__UINTPTR_TYPE__) (icache_lsize - 1));

    for (; address < (const char *) end; address += icache_lsize)
      asm volatile ("ic\tivau, %0"
		    :
		    : "r" (address)
		    : "memory");

    asm volatile ("dsb\tish" : : : "memory");
  }

  asm volatile("isb" : : : "memory");
}
