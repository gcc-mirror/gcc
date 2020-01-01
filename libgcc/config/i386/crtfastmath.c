/*
 * Copyright (C) 2005-2020 Free Software Foundation, Inc.
 *
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#ifndef _SOFT_FLOAT
#define MXCSR_DAZ (1 << 6)	/* Enable denormals are zero mode */
#define MXCSR_FTZ (1 << 15)	/* Enable flush to zero mode */

#ifndef __x86_64__
/* All 64-bit targets have SSE and DAZ;
   only check them explicitly for 32-bit ones. */
#include "cpuid.h"

__attribute__ ((target("fxsr,sse")))
static void
/* The i386 ABI only requires 4-byte stack alignment, so this is necessary
   to make sure the fxsave struct gets correct alignment.
   See PR27537 and PR28621.  */
__attribute__ ((force_align_arg_pointer))
set_fast_math_sse (unsigned int edx)
{
  unsigned int mxcsr;
  
  if (edx & bit_FXSAVE)
    {
      /* Check if DAZ is available.  */
      struct
      {
	unsigned short cwd;
	unsigned short swd;
	unsigned short twd;
	unsigned short fop;
	unsigned int fip;
	unsigned int fcs;
	unsigned int foo;
	unsigned int fos;
	unsigned int mxcsr;
	unsigned int mxcsr_mask;
	unsigned int st_space[32];
	unsigned int xmm_space[32];
	unsigned int padding[56];
      } __attribute__ ((aligned (16))) fxsave;

      /* This is necessary since some implementations of FXSAVE
	 do not modify reserved areas within the image.  */
      fxsave.mxcsr_mask = 0;

      __builtin_ia32_fxsave (&fxsave);

      mxcsr = fxsave.mxcsr;

      if (fxsave.mxcsr_mask & MXCSR_DAZ)
	mxcsr |= MXCSR_DAZ;
    }
  else
    mxcsr = __builtin_ia32_stmxcsr ();

  mxcsr |= MXCSR_FTZ;
  __builtin_ia32_ldmxcsr (mxcsr);
}
#endif

static void __attribute__((constructor))
set_fast_math (void)
{
#ifndef __x86_64__
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return;

  if (edx & bit_SSE)
    set_fast_math_sse (edx);
#else
  unsigned int mxcsr = __builtin_ia32_stmxcsr ();
  mxcsr |= MXCSR_DAZ | MXCSR_FTZ;
  __builtin_ia32_ldmxcsr (mxcsr);
#endif
}
#endif
