/*
 * Special support for e500 eabi and SVR4
 *
 *   Copyright (C) 2008-2017 Free Software Foundation, Inc.
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

/* "Exit" versions that return to their caller's caller.  */

HIDDEN_FUNC(_rest64gpr_14_x)	evldd 14,0(11)
HIDDEN_FUNC(_rest64gpr_15_x)	evldd 15,8(11)
HIDDEN_FUNC(_rest64gpr_16_x)	evldd 16,16(11)
HIDDEN_FUNC(_rest64gpr_17_x)	evldd 17,24(11)
HIDDEN_FUNC(_rest64gpr_18_x)	evldd 18,32(11)
HIDDEN_FUNC(_rest64gpr_19_x)	evldd 19,40(11)
HIDDEN_FUNC(_rest64gpr_20_x)	evldd 20,48(11)
HIDDEN_FUNC(_rest64gpr_21_x)	evldd 21,56(11)
HIDDEN_FUNC(_rest64gpr_22_x)	evldd 22,64(11)
HIDDEN_FUNC(_rest64gpr_23_x)	evldd 23,72(11)
HIDDEN_FUNC(_rest64gpr_24_x)	evldd 24,80(11)
HIDDEN_FUNC(_rest64gpr_25_x)	evldd 25,88(11)
HIDDEN_FUNC(_rest64gpr_26_x)	evldd 26,96(11)
HIDDEN_FUNC(_rest64gpr_27_x)	evldd 27,104(11)
HIDDEN_FUNC(_rest64gpr_28_x)	evldd 28,112(11)
HIDDEN_FUNC(_rest64gpr_29_x)	evldd 29,120(11)
HIDDEN_FUNC(_rest64gpr_30_x)	evldd 30,128(11)
HIDDEN_FUNC(_rest64gpr_31_x)	lwz 0,148(11)
				evldd 31,136(11)
				addi 1,11,144
				mtlr 0
				blr
FUNC_END(_rest64gpr_31_x)
FUNC_END(_rest64gpr_30_x)
FUNC_END(_rest64gpr_29_x)
FUNC_END(_rest64gpr_28_x)
FUNC_END(_rest64gpr_27_x)
FUNC_END(_rest64gpr_26_x)
FUNC_END(_rest64gpr_25_x)
FUNC_END(_rest64gpr_24_x)
FUNC_END(_rest64gpr_23_x)
FUNC_END(_rest64gpr_22_x)
FUNC_END(_rest64gpr_21_x)
FUNC_END(_rest64gpr_20_x)
FUNC_END(_rest64gpr_19_x)
FUNC_END(_rest64gpr_18_x)
FUNC_END(_rest64gpr_17_x)
FUNC_END(_rest64gpr_16_x)
FUNC_END(_rest64gpr_15_x)
FUNC_END(_rest64gpr_14_x)

#endif
