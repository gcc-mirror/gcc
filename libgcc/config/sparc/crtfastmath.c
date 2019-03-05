/*
 * Copyright (C) 2001-2019 Free Software Foundation, Inc.
 * Contributed by David S. Miller (davem@redhat.com)
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

#define FPRS_NS		(1 << 22)	/* Non-Standard fpu results */

static void __attribute__((constructor))
set_fast_math (void)
{
  unsigned int fsr;

  /* This works for the 64-bit case because, even if 32-bit ld/st of
     the fsr register modified the upper 32-bit, the only thing up there
     are the 3 other condition codes which are "do not care" at the time
     that this runs.  */

  __asm__("st %%fsr, %0"
	  : "=m" (fsr));

  fsr |= FPRS_NS;

  __asm__("ld %0, %%fsr"
	  : : "m" (fsr));
}
