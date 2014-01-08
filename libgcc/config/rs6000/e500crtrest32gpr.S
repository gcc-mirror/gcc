/*
 * Special support for e500 eabi and SVR4
 *
 *   Copyright (C) 2008-2014 Free Software Foundation, Inc.
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
/* "Tail" versions that perform a tail call.  */

HIDDEN_FUNC(_rest32gpr_14_t)	lwz 14,-72(11)
HIDDEN_FUNC(_rest32gpr_15_t)	lwz 15,-68(11)
HIDDEN_FUNC(_rest32gpr_16_t)	lwz 16,-64(11)
HIDDEN_FUNC(_rest32gpr_17_t)	lwz 17,-60(11)
HIDDEN_FUNC(_rest32gpr_18_t)	lwz 18,-56(11)
HIDDEN_FUNC(_rest32gpr_19_t)	lwz 19,-52(11)
HIDDEN_FUNC(_rest32gpr_20_t)	lwz 20,-48(11)
HIDDEN_FUNC(_rest32gpr_21_t)	lwz 21,-44(11)
HIDDEN_FUNC(_rest32gpr_22_t)	lwz 22,-40(11)
HIDDEN_FUNC(_rest32gpr_23_t)	lwz 23,-36(11)
HIDDEN_FUNC(_rest32gpr_24_t)	lwz 24,-32(11)
HIDDEN_FUNC(_rest32gpr_25_t)	lwz 25,-28(11)
HIDDEN_FUNC(_rest32gpr_26_t)	lwz 26,-24(11)
HIDDEN_FUNC(_rest32gpr_27_t)	lwz 27,-20(11)
HIDDEN_FUNC(_rest32gpr_28_t)	lwz 28,-16(11)
HIDDEN_FUNC(_rest32gpr_29_t)	lwz 29,-12(11)
HIDDEN_FUNC(_rest32gpr_30_t)	lwz 30,-8(11)
HIDDEN_FUNC(_rest32gpr_31_t)	lwz 31,-4(11)
				lwz 0,4(11)
				mr 1,11
				blr
FUNC_END(_rest32gpr_31_t)
FUNC_END(_rest32gpr_30_t)
FUNC_END(_rest32gpr_29_t)
FUNC_END(_rest32gpr_28_t)
FUNC_END(_rest32gpr_27_t)
FUNC_END(_rest32gpr_26_t)
FUNC_END(_rest32gpr_25_t)
FUNC_END(_rest32gpr_24_t)
FUNC_END(_rest32gpr_23_t)
FUNC_END(_rest32gpr_22_t)
FUNC_END(_rest32gpr_21_t)
FUNC_END(_rest32gpr_20_t)
FUNC_END(_rest32gpr_19_t)
FUNC_END(_rest32gpr_18_t)
FUNC_END(_rest32gpr_17_t)
FUNC_END(_rest32gpr_16_t)
FUNC_END(_rest32gpr_15_t)
FUNC_END(_rest32gpr_14_t)

#endif
