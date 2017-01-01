/* c-isr library stuff of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

.macro RESTORE_PARTIAL
#if defined(NDS32_NESTED) || defined(NDS32_NESTED_READY)
	setgie.d
	dsb
	lmw.bim $r1, [$sp], $r2, #0x0	/* Restore IPC, IPSW.  */
	mtsr $r1, $IPC	/* Set IPC.  */
	mtsr $r2, $IPSW	/* Set IPSW.  */
#endif
	RESTORE_FPU_REGS
	RESTORE_MAC_REGS
#ifdef NDS32_EXT_IFC
	lmw.bim	$r1, [$sp], $r2, #0x0	/* Restore extra $r2 to keep
					   stack 8-byte alignment.  */
	mtusr   $r1, $IFC_LP
#endif
	lmw.bim $r0, [$sp], $r5, #0x0	/* Restore all regs.  */
#ifdef __NDS32_REDUCED_REGS__
	lmw.bim $r15, [$sp], $r15, #0x2
#else
	lmw.bim $r15, [$sp], $r27, #0x2	/* Restore all regs.  */
#endif
.endm
