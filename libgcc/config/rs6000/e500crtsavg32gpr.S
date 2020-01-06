/*
 * Special support for e500 eabi and SVR4
 *
 *   Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

/* Routines for saving 32-bit integer registers, called by the compiler.  */
/* "GOT" versions that load the address of the GOT into lr before returning.  */

HIDDEN_FUNC(_save32gpr_14_g)	stw 14,-72(11)
HIDDEN_FUNC(_save32gpr_15_g)	stw 15,-68(11)
HIDDEN_FUNC(_save32gpr_16_g)	stw 16,-64(11)
HIDDEN_FUNC(_save32gpr_17_g)	stw 17,-60(11)
HIDDEN_FUNC(_save32gpr_18_g)	stw 18,-56(11)
HIDDEN_FUNC(_save32gpr_19_g)	stw 19,-52(11)
HIDDEN_FUNC(_save32gpr_20_g)	stw 20,-48(11)
HIDDEN_FUNC(_save32gpr_21_g)	stw 21,-44(11)
HIDDEN_FUNC(_save32gpr_22_g)	stw 22,-40(11)
HIDDEN_FUNC(_save32gpr_23_g)	stw 23,-36(11)
HIDDEN_FUNC(_save32gpr_24_g)	stw 24,-32(11)
HIDDEN_FUNC(_save32gpr_25_g)	stw 25,-28(11)
HIDDEN_FUNC(_save32gpr_26_g)	stw 26,-24(11)
HIDDEN_FUNC(_save32gpr_27_g)	stw 27,-20(11)
HIDDEN_FUNC(_save32gpr_28_g)	stw 28,-16(11)
HIDDEN_FUNC(_save32gpr_29_g)	stw 29,-12(11)
HIDDEN_FUNC(_save32gpr_30_g)	stw 30,-8(11)
HIDDEN_FUNC(_save32gpr_31_g)	stw 31,-4(11)
				b _GLOBAL_OFFSET_TABLE_-4
FUNC_END(_save32gpr_31_g)
FUNC_END(_save32gpr_30_g)
FUNC_END(_save32gpr_29_g)
FUNC_END(_save32gpr_28_g)
FUNC_END(_save32gpr_27_g)
FUNC_END(_save32gpr_26_g)
FUNC_END(_save32gpr_25_g)
FUNC_END(_save32gpr_24_g)
FUNC_END(_save32gpr_23_g)
FUNC_END(_save32gpr_22_g)
FUNC_END(_save32gpr_21_g)
FUNC_END(_save32gpr_20_g)
FUNC_END(_save32gpr_19_g)
FUNC_END(_save32gpr_18_g)
FUNC_END(_save32gpr_17_g)
FUNC_END(_save32gpr_16_g)
FUNC_END(_save32gpr_15_g)
FUNC_END(_save32gpr_14_g)

#endif
