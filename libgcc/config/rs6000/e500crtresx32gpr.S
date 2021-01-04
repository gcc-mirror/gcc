/*
 * Special support for e500 eabi and SVR4
 *
 *   Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

/* Routines for restoring 32-bit integer registers, called by the compiler.  */
/* "Exit" versions that return to the caller's caller.  */

HIDDEN_FUNC(_rest32gpr_14_x)	lwz 14,-72(11)
HIDDEN_FUNC(_rest32gpr_15_x)	lwz 15,-68(11)
HIDDEN_FUNC(_rest32gpr_16_x)	lwz 16,-64(11)
HIDDEN_FUNC(_rest32gpr_17_x)	lwz 17,-60(11)
HIDDEN_FUNC(_rest32gpr_18_x)	lwz 18,-56(11)
HIDDEN_FUNC(_rest32gpr_19_x)	lwz 19,-52(11)
HIDDEN_FUNC(_rest32gpr_20_x)	lwz 20,-48(11)
HIDDEN_FUNC(_rest32gpr_21_x)	lwz 21,-44(11)
HIDDEN_FUNC(_rest32gpr_22_x)	lwz 22,-40(11)
HIDDEN_FUNC(_rest32gpr_23_x)	lwz 23,-36(11)
HIDDEN_FUNC(_rest32gpr_24_x)	lwz 24,-32(11)
HIDDEN_FUNC(_rest32gpr_25_x)	lwz 25,-28(11)
HIDDEN_FUNC(_rest32gpr_26_x)	lwz 26,-24(11)
HIDDEN_FUNC(_rest32gpr_27_x)	lwz 27,-20(11)
HIDDEN_FUNC(_rest32gpr_28_x)	lwz 28,-16(11)
HIDDEN_FUNC(_rest32gpr_29_x)	lwz 29,-12(11)
HIDDEN_FUNC(_rest32gpr_30_x)	lwz 30,-8(11)
HIDDEN_FUNC(_rest32gpr_31_x)	lwz 0,4(11)
				lwz 31,-4(11)
				mr 1,11
				mtlr 0
				blr
FUNC_END(_rest32gpr_31_x)
FUNC_END(_rest32gpr_30_x)
FUNC_END(_rest32gpr_29_x)
FUNC_END(_rest32gpr_28_x)
FUNC_END(_rest32gpr_27_x)
FUNC_END(_rest32gpr_26_x)
FUNC_END(_rest32gpr_25_x)
FUNC_END(_rest32gpr_24_x)
FUNC_END(_rest32gpr_23_x)
FUNC_END(_rest32gpr_22_x)
FUNC_END(_rest32gpr_21_x)
FUNC_END(_rest32gpr_20_x)
FUNC_END(_rest32gpr_19_x)
FUNC_END(_rest32gpr_18_x)
FUNC_END(_rest32gpr_17_x)
FUNC_END(_rest32gpr_16_x)
FUNC_END(_rest32gpr_15_x)
FUNC_END(_rest32gpr_14_x)

#endif
