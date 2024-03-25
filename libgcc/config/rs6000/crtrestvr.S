/* Routines for restoring vector registers.

   Copyright (C) 2012-2024 Free Software Foundation, Inc.
   Written by Alan Modra, IBM

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

/* On PowerPC64 Linux, these functions are provided by the linker.  */
#ifndef __powerpc64__

#undef __ALTIVEC__
#define __ALTIVEC__ 1
	#include "ppc-asm.h"

/* Called with r0 pointing just beyond the end of the vector save area.  */

	.machine altivec
	.section ".text"
CFI_STARTPROC
HIDDEN_FUNC(_restvr_20)
	li r11,-192
	lvx v20,r11,r0
HIDDEN_FUNC(_restvr_21)
	li r11,-176
	lvx v21,r11,r0
HIDDEN_FUNC(_restvr_22)
	li r11,-160
	lvx v22,r11,r0
HIDDEN_FUNC(_restvr_23)
	li r11,-144
	lvx v23,r11,r0
HIDDEN_FUNC(_restvr_24)
	li r11,-128
	lvx v24,r11,r0
HIDDEN_FUNC(_restvr_25)
	li r11,-112
	lvx v25,r11,r0
HIDDEN_FUNC(_restvr_26)
	li r11,-96
	lvx v26,r11,r0
HIDDEN_FUNC(_restvr_27)
	li r11,-80
	lvx v27,r11,r0
HIDDEN_FUNC(_restvr_28)
	li r11,-64
	lvx v28,r11,r0
HIDDEN_FUNC(_restvr_29)
	li r11,-48
	lvx v29,r11,r0
HIDDEN_FUNC(_restvr_30)
	li r11,-32
	lvx v30,r11,r0
HIDDEN_FUNC(_restvr_31)
	li r11,-16
	lvx v31,r11,r0
	blr
FUNC_END(_restvr_31)
FUNC_END(_restvr_30)
FUNC_END(_restvr_29)
FUNC_END(_restvr_28)
FUNC_END(_restvr_27)
FUNC_END(_restvr_26)
FUNC_END(_restvr_25)
FUNC_END(_restvr_24)
FUNC_END(_restvr_23)
FUNC_END(_restvr_22)
FUNC_END(_restvr_21)
FUNC_END(_restvr_20)
CFI_ENDPROC

#endif
