/*
 * Special support for eabi and SVR4
 *
 *   Copyright (C) 1995-2025 Free Software Foundation, Inc.
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

/* Routines for saving integer registers, called by the compiler.  */
/* Called with r11 pointing to the stack header word of the caller of the */
/* function, just beyond the end of the integer save area.  */

CFI_STARTPROC
HIDDEN_FUNC(_savegpr_14)	stw	14,-72(11)	/* save gp registers */
HIDDEN_FUNC(_savegpr_15)	stw	15,-68(11)
HIDDEN_FUNC(_savegpr_16)	stw	16,-64(11)
HIDDEN_FUNC(_savegpr_17)	stw	17,-60(11)
HIDDEN_FUNC(_savegpr_18)	stw	18,-56(11)
HIDDEN_FUNC(_savegpr_19)	stw	19,-52(11)
HIDDEN_FUNC(_savegpr_20)	stw	20,-48(11)
HIDDEN_FUNC(_savegpr_21)	stw	21,-44(11)
HIDDEN_FUNC(_savegpr_22)	stw	22,-40(11)
HIDDEN_FUNC(_savegpr_23)	stw	23,-36(11)
HIDDEN_FUNC(_savegpr_24)	stw	24,-32(11)
HIDDEN_FUNC(_savegpr_25)	stw	25,-28(11)
HIDDEN_FUNC(_savegpr_26)	stw	26,-24(11)
HIDDEN_FUNC(_savegpr_27)	stw	27,-20(11)
HIDDEN_FUNC(_savegpr_28)	stw	28,-16(11)
HIDDEN_FUNC(_savegpr_29)	stw	29,-12(11)
HIDDEN_FUNC(_savegpr_30)	stw	30,-8(11)
HIDDEN_FUNC(_savegpr_31)	stw	31,-4(11)
			blr
FUNC_END(_savegpr_31)
FUNC_END(_savegpr_30)
FUNC_END(_savegpr_29)
FUNC_END(_savegpr_28)
FUNC_END(_savegpr_27)
FUNC_END(_savegpr_26)
FUNC_END(_savegpr_25)
FUNC_END(_savegpr_24)
FUNC_END(_savegpr_23)
FUNC_END(_savegpr_22)
FUNC_END(_savegpr_21)
FUNC_END(_savegpr_20)
FUNC_END(_savegpr_19)
FUNC_END(_savegpr_18)
FUNC_END(_savegpr_17)
FUNC_END(_savegpr_16)
FUNC_END(_savegpr_15)
FUNC_END(_savegpr_14)
CFI_ENDPROC

#endif
