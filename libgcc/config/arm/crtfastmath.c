/*
 * Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

/* Enable flush-to-zero support for -ffast-math on VFP targets.  */
#ifndef __SOFTFP__

#define FPSCR_FZ		(1 << 24)

static void __attribute__((constructor))
__arm_set_fast_math (void)
{
  unsigned int fpscr_save;

  /* Set the FZ (flush-to-zero) bit in FPSCR.  */
  __asm__("vmrs %0, fpscr" : "=r" (fpscr_save));
  fpscr_save |= FPSCR_FZ;
  __asm__("vmsr fpscr, %0" : : "r" (fpscr_save));
}

#endif /* __SOFTFP__  */
