/*
 * Special support for eabi and SVR4
 *
 *   Copyright (C) 1995-2020 Free Software Foundation, Inc.
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

/* Routines for saving floating point registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the floating point save area.  */

CFI_STARTPROC
HIDDEN_FUNC(_savefpr_14)	stfd	14,-144(11)	/* save fp registers */
HIDDEN_FUNC(_savefpr_15)	stfd	15,-136(11)
HIDDEN_FUNC(_savefpr_16)	stfd	16,-128(11)
HIDDEN_FUNC(_savefpr_17)	stfd	17,-120(11)
HIDDEN_FUNC(_savefpr_18)	stfd	18,-112(11)
HIDDEN_FUNC(_savefpr_19)	stfd	19,-104(11)
HIDDEN_FUNC(_savefpr_20)	stfd	20,-96(11)
HIDDEN_FUNC(_savefpr_21)	stfd	21,-88(11)
HIDDEN_FUNC(_savefpr_22)	stfd	22,-80(11)
HIDDEN_FUNC(_savefpr_23)	stfd	23,-72(11)
HIDDEN_FUNC(_savefpr_24)	stfd	24,-64(11)
HIDDEN_FUNC(_savefpr_25)	stfd	25,-56(11)
HIDDEN_FUNC(_savefpr_26)	stfd	26,-48(11)
HIDDEN_FUNC(_savefpr_27)	stfd	27,-40(11)
HIDDEN_FUNC(_savefpr_28)	stfd	28,-32(11)
HIDDEN_FUNC(_savefpr_29)	stfd	29,-24(11)
HIDDEN_FUNC(_savefpr_30)	stfd	30,-16(11)
HIDDEN_FUNC(_savefpr_31)	stfd	31,-8(11)
			blr
FUNC_END(_savefpr_31)
FUNC_END(_savefpr_30)
FUNC_END(_savefpr_29)
FUNC_END(_savefpr_28)
FUNC_END(_savefpr_27)
FUNC_END(_savefpr_26)
FUNC_END(_savefpr_25)
FUNC_END(_savefpr_24)
FUNC_END(_savefpr_23)
FUNC_END(_savefpr_22)
FUNC_END(_savefpr_21)
FUNC_END(_savefpr_20)
FUNC_END(_savefpr_19)
FUNC_END(_savefpr_18)
FUNC_END(_savefpr_17)
FUNC_END(_savefpr_16)
FUNC_END(_savefpr_15)
FUNC_END(_savefpr_14)
CFI_ENDPROC

#endif
