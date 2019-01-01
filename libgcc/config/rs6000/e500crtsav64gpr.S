/*
 * Special support for e500 eabi and SVR4
 *
 *   Copyright (C) 2008-2019 Free Software Foundation, Inc.
 *   Written by Nathan Froyd
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

	.section ".text"
	#include "ppc-asm.h"

#ifdef __SPE__

/* Routines for saving 64-bit integer registers, called by the compiler.  */

HIDDEN_FUNC(_save64gpr_14)	evstdd 14,0(11)
HIDDEN_FUNC(_save64gpr_15)	evstdd 15,8(11)
HIDDEN_FUNC(_save64gpr_16)	evstdd 16,16(11)
HIDDEN_FUNC(_save64gpr_17)	evstdd 17,24(11)
HIDDEN_FUNC(_save64gpr_18)	evstdd 18,32(11)
HIDDEN_FUNC(_save64gpr_19)	evstdd 19,40(11)
HIDDEN_FUNC(_save64gpr_20)	evstdd 20,48(11)
HIDDEN_FUNC(_save64gpr_21)	evstdd 21,56(11)
HIDDEN_FUNC(_save64gpr_22)	evstdd 22,64(11)
HIDDEN_FUNC(_save64gpr_23)	evstdd 23,72(11)
HIDDEN_FUNC(_save64gpr_24)	evstdd 24,80(11)
HIDDEN_FUNC(_save64gpr_25)	evstdd 25,88(11)
HIDDEN_FUNC(_save64gpr_26)	evstdd 26,96(11)
HIDDEN_FUNC(_save64gpr_27)	evstdd 27,104(11)
HIDDEN_FUNC(_save64gpr_28)	evstdd 28,112(11)
HIDDEN_FUNC(_save64gpr_29)	evstdd 29,120(11)
HIDDEN_FUNC(_save64gpr_30)	evstdd 30,128(11)
HIDDEN_FUNC(_save64gpr_31)	evstdd 31,136(11)
				blr
FUNC_END(_save64gpr_31)
FUNC_END(_save64gpr_30)
FUNC_END(_save64gpr_29)
FUNC_END(_save64gpr_28)
FUNC_END(_save64gpr_27)
FUNC_END(_save64gpr_26)
FUNC_END(_save64gpr_25)
FUNC_END(_save64gpr_24)
FUNC_END(_save64gpr_23)
FUNC_END(_save64gpr_22)
FUNC_END(_save64gpr_21)
FUNC_END(_save64gpr_20)
FUNC_END(_save64gpr_19)
FUNC_END(_save64gpr_18)
FUNC_END(_save64gpr_17)
FUNC_END(_save64gpr_16)
FUNC_END(_save64gpr_15)
FUNC_END(_save64gpr_14)

#endif
