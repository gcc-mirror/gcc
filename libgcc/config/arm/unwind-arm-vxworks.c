/* Support for ARM EABI unwinding on VxWorks Downloadable Kernel Modules.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* The common unwinding code refers to __gnu_Unwind_Find_exidx and
   __cxa_type_match symbols, which are not in VxWorks kernels on ARM,
   now llvm based.

   While the common code works just fine for RTPs thanks to weak references
   and proper positioning of __exidx_start/end from linker scripts, we need
   symbol definitions for kernel modules.  */

#ifndef __RTP__

#include <private/moduleLibP.h>

/* __gnu_Unwind_Find_exidx.  See if we can use _func_moduleExidxGet to
   refine whatever we have in __exidx_start and __exidx_end.  */

typedef struct
{
  UINT32 fnoffset;
  UINT32 content;
} __EIT_entry;

extern __EIT_entry __exidx_start;
extern __EIT_entry __exidx_end;

__EIT_entry *
__gnu_Unwind_Find_exidx (void *pc, int *nrec)
{
  __EIT_entry *pstart = 0;
  __EIT_entry *pend = 0;

  if (_func_moduleExidxGet != NULL)
    _func_moduleExidxGet (pc,
			  (void *) &__exidx_start, (void *) &__exidx_end,
			  (void **) &pstart, (void **) &pend);

  if (!pstart)
    {
      pstart = &__exidx_start;
      pend = &__exidx_end;
    }

  *nrec = pend - pstart;

  return pstart;
}

/* __cxa_type_match.  A dummy version to be overridden by the libstdc++ one
 when we link with it.  */

void * __attribute__((weak))
__cxa_type_match ()
{
  return (void *) 0;
}

#endif
