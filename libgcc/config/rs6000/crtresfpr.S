/*
 * Special support for eabi and SVR4
 *
 *   Copyright (C) 1995-2013 Free Software Foundation, Inc.
 *   Written By Michael Meissner
 *   64-bit support written by David Edelsohn
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

/* Do any initializations needed for the eabi environment */

	.section ".text"
	#include "ppc-asm.h"

/* On PowerPC64 Linux, these functions are provided by the linker.  */
#ifndef __powerpc64__

/* Routines for restoring floating point registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the floating point save area.  */

CFI_STARTPROC
HIDDEN_FUNC(_restfpr_14)	lfd	14,-144(11)	/* restore fp registers */
HIDDEN_FUNC(_restfpr_15)	lfd	15,-136(11)
HIDDEN_FUNC(_restfpr_16)	lfd	16,-128(11)
HIDDEN_FUNC(_restfpr_17)	lfd	17,-120(11)
HIDDEN_FUNC(_restfpr_18)	lfd	18,-112(11)
HIDDEN_FUNC(_restfpr_19)	lfd	19,-104(11)
HIDDEN_FUNC(_restfpr_20)	lfd	20,-96(11)
HIDDEN_FUNC(_restfpr_21)	lfd	21,-88(11)
HIDDEN_FUNC(_restfpr_22)	lfd	22,-80(11)
HIDDEN_FUNC(_restfpr_23)	lfd	23,-72(11)
HIDDEN_FUNC(_restfpr_24)	lfd	24,-64(11)
HIDDEN_FUNC(_restfpr_25)	lfd	25,-56(11)
HIDDEN_FUNC(_restfpr_26)	lfd	26,-48(11)
HIDDEN_FUNC(_restfpr_27)	lfd	27,-40(11)
HIDDEN_FUNC(_restfpr_28)	lfd	28,-32(11)
HIDDEN_FUNC(_restfpr_29)	lfd	29,-24(11)
HIDDEN_FUNC(_restfpr_30)	lfd	30,-16(11)
HIDDEN_FUNC(_restfpr_31)	lfd	31,-8(11)
			blr
FUNC_END(_restfpr_31)
FUNC_END(_restfpr_30)
FUNC_END(_restfpr_29)
FUNC_END(_restfpr_28)
FUNC_END(_restfpr_27)
FUNC_END(_restfpr_26)
FUNC_END(_restfpr_25)
FUNC_END(_restfpr_24)
FUNC_END(_restfpr_23)
FUNC_END(_restfpr_22)
FUNC_END(_restfpr_21)
FUNC_END(_restfpr_20)
FUNC_END(_restfpr_19)
FUNC_END(_restfpr_18)
FUNC_END(_restfpr_17)
FUNC_END(_restfpr_16)
FUNC_END(_restfpr_15)
FUNC_END(_restfpr_14)
CFI_ENDPROC

#endif
