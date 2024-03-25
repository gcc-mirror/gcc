/* Copyright (C) 2000-2024 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@cygnus.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Locate the FDE entry for a given address, using glibc ld.so routines
   to avoid register/deregister calls at DSO load/unload.  */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include "config.h"
#ifndef inhibit_libc
#include <stddef.h>
#include <stdlib.h>
#include <link.h>
#include "unwind-ia64.h"

#if __GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 2) \
    || (__GLIBC__ == 2 && __GLIBC_MINOR__ == 2 && !defined(DT_CONFIG))
# error You need GLIBC 2.2.4 or later on IA-64 Linux
#endif

struct unw_ia64_callback_data
{
  Elf64_Addr pc;
  unsigned long *segment_base;
  unsigned long *gp;
  struct unw_table_entry *ret;
};

static int
_Unwind_IteratePhdrCallback (struct dl_phdr_info *info, size_t size, void *ptr)
{
  struct unw_ia64_callback_data *data = (struct unw_ia64_callback_data *) ptr;
  const Elf64_Phdr *phdr, *p_unwind, *p_dynamic;
  long n, match;
  Elf64_Addr load_base, seg_base;
  struct unw_table_entry *f_base, *f;
  size_t lo, hi;

  /* Make sure struct dl_phdr_info is at least as big as we need.  */
  if (size < offsetof (struct dl_phdr_info, dlpi_phnum)
	     + sizeof (info->dlpi_phnum))
    return -1;

  match = 0;
  phdr = info->dlpi_phdr;
  load_base = info->dlpi_addr;
  p_unwind = NULL;
  p_dynamic = NULL;
  seg_base = ~(Elf64_Addr) 0;

  /* See if PC falls into one of the loaded segments.  Find the unwind
     segment at the same time.  */
  for (n = info->dlpi_phnum; --n >= 0; phdr++)
    {
      if (phdr->p_type == PT_LOAD)
	{
	  Elf64_Addr vaddr = phdr->p_vaddr + load_base;
	  if (data->pc >= vaddr && data->pc < vaddr + phdr->p_memsz)
	    match = 1;
	  if (vaddr < seg_base)
	    seg_base = vaddr;
	}
      else if (phdr->p_type == PT_IA_64_UNWIND)
	p_unwind = phdr;
      else if (phdr->p_type == PT_DYNAMIC)
	p_dynamic = phdr;
    }
  if (!match || !p_unwind)
    return 0;

  /* Search for the FDE within the unwind segment.  */

  f_base = (struct unw_table_entry *) (p_unwind->p_vaddr + load_base);
  lo = 0;
  hi = p_unwind->p_memsz / sizeof (struct unw_table_entry);

  while (lo < hi)
    {
      size_t mid = (lo + hi) / 2;

      f = f_base + mid;
      if (data->pc < f->start_offset + seg_base)
	hi = mid;
      else if (data->pc >= f->end_offset + seg_base)
	lo = mid + 1;
      else
        goto found;
    }
  /* No need to search for further libraries when we know pc is contained
     in this library.  */
  return 1;

 found:
  *data->segment_base = seg_base;
  *data->gp = 0;
  data->ret = f;

  if (p_dynamic)
    {
      /* For dynamically linked executables and shared libraries,
	 DT_PLTGOT is the gp value for that object.  */
      Elf64_Dyn *dyn = (Elf64_Dyn *)(p_dynamic->p_vaddr + load_base);
      for (; dyn->d_tag != DT_NULL ; dyn++)
	if (dyn->d_tag == DT_PLTGOT)
	  {
	    /* On IA-64, _DYNAMIC is writable and GLIBC has relocated it.  */
	    *data->gp = dyn->d_un.d_ptr;
	    break;
	  }
    }
  else
    {
      /* Otherwise this is a static executable with no _DYNAMIC.
	 The gp is constant program-wide.  */
      register unsigned long gp __asm__("gp");
      *data->gp = gp;
    }

  return 1;
}

/* Return a pointer to the unwind table entry for the function
   containing PC.  */

struct unw_table_entry *
_Unwind_FindTableEntry (void *pc, unw_word *segment_base, unw_word *gp,
                        struct unw_table_entry *ent ATTRIBUTE_UNUSED)
{
  struct unw_ia64_callback_data data;

  data.pc = (Elf64_Addr) pc;
  data.segment_base = segment_base;
  data.gp = gp;
  data.ret = NULL;

  if (dl_iterate_phdr (_Unwind_IteratePhdrCallback, &data) < 0)
    return NULL;

  return data.ret;
}
#endif
