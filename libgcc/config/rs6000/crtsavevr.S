/* Routines for saving vector registers.

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
HIDDEN_FUNC(_savevr_20)
	li r11,-192
	stvx v20,r11,r0
HIDDEN_FUNC(_savevr_21)
	li r11,-176
	stvx v21,r11,r0
HIDDEN_FUNC(_savevr_22)
	li r11,-160
	stvx v22,r11,r0
HIDDEN_FUNC(_savevr_23)
	li r11,-144
	stvx v23,r11,r0
HIDDEN_FUNC(_savevr_24)
	li r11,-128
	stvx v24,r11,r0
HIDDEN_FUNC(_savevr_25)
	li r11,-112
	stvx v25,r11,r0
HIDDEN_FUNC(_savevr_26)
	li r11,-96
	stvx v26,r11,r0
HIDDEN_FUNC(_savevr_27)
	li r11,-80
	stvx v27,r11,r0
HIDDEN_FUNC(_savevr_28)
	li r11,-64
	stvx v28,r11,r0
HIDDEN_FUNC(_savevr_29)
	li r11,-48
	stvx v29,r11,r0
HIDDEN_FUNC(_savevr_30)
	li r11,-32
	stvx v30,r11,r0
HIDDEN_FUNC(_savevr_31)
	li r11,-16
	stvx v31,r11,r0
	blr
FUNC_END(_savevr_31)
FUNC_END(_savevr_30)
FUNC_END(_savevr_29)
FUNC_END(_savevr_28)
FUNC_END(_savevr_27)
FUNC_END(_savevr_26)
FUNC_END(_savevr_25)
FUNC_END(_savevr_24)
FUNC_END(_savevr_23)
FUNC_END(_savevr_22)
FUNC_END(_savevr_21)
FUNC_END(_savevr_20)
CFI_ENDPROC

#endif
