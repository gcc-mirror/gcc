/*
 * Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

#define _FPU_FPCR_FZ 0x1000000

#define _FPU_SETCW(fpcr)				   \
  {							   \
    __asm__ __volatile__ ("msr	fpcr, %0" : : "r" (fpcr)); \
  }

static void __attribute__((constructor))
set_fast_math (void)
{
  /* Flush to zero, round to nearest, IEEE exceptions disabled.  */
  _FPU_SETCW (_FPU_FPCR_FZ);
}
