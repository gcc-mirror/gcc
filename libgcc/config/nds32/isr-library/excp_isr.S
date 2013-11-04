/* c-isr library stuff of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#include "save_mac_regs.inc"
#include "save_fpu_regs.inc"
#include "save_fpu_regs_00.inc"
#include "save_fpu_regs_01.inc"
#include "save_fpu_regs_02.inc"
#include "save_fpu_regs_03.inc"
#include "save_all.inc"
#include "save_partial.inc"
#include "adj_intr_lvl.inc"
#include "restore_mac_regs.inc"
#include "restore_fpu_regs_00.inc"
#include "restore_fpu_regs_01.inc"
#include "restore_fpu_regs_02.inc"
#include "restore_fpu_regs_03.inc"
#include "restore_fpu_regs.inc"
#include "restore_all.inc"
#include "restore_partial.inc"
	.section .nds32_isr, "ax"       /* Put it in the section of 1st level handler. */
	.align	1
/*
  First Level Handlers
  1. First Level Handlers are invokded in vector section via jump instruction
  with specific names for different configurations.
  2. Naming Format: _nds32_e_SR_NT for exception handlers.
		    _nds32_i_SR_NT for interrupt handlers.
  2.1 All upper case letters are replaced with specific lower case letters encodings.
  2.2 SR: Saved Registers
      sa: Save All regs (context)
      ps: Partial Save (all caller-saved regs)
  2.3 NT: Nested Type
      ns: nested
      nn: not nested
      nr: nested ready
*/

/*
  This is original 16-byte vector size version.
*/
#ifdef NDS32_SAVE_ALL_REGS
#if defined(NDS32_NESTED)
	.globl	_nds32_e_sa_ns
	.type	_nds32_e_sa_ns, @function
_nds32_e_sa_ns:
#elif defined(NDS32_NESTED_READY)
	.globl	_nds32_e_sa_nr
	.type	_nds32_e_sa_nr, @function
_nds32_e_sa_nr:
#else /* Not nested handler. */
	.globl	_nds32_e_sa_nn
	.type	_nds32_e_sa_nn, @function
_nds32_e_sa_nn:
#endif /* endif for Nest Type */
#else /* not NDS32_SAVE_ALL_REGS */
#if defined(NDS32_NESTED)
	.globl	_nds32_e_ps_ns
	.type	_nds32_e_ps_ns, @function
_nds32_e_ps_ns:
#elif defined(NDS32_NESTED_READY)
	.globl	_nds32_e_ps_nr
	.type	_nds32_e_ps_nr, @function
_nds32_e_ps_nr:
#else /* Not nested handler. */
	.globl	_nds32_e_ps_nn
	.type	_nds32_e_ps_nn, @function
_nds32_e_ps_nn:
#endif /* endif for Nest Type */
#endif /* not NDS32_SAVE_ALL_REGS */

/*
  This is 16-byte vector size version.
  The vector id was restored into $r0 in vector by compiler.
*/
#ifdef NDS32_SAVE_ALL_REGS
        SAVE_ALL
#else
        SAVE_PARTIAL
#endif
	/* Prepare to call 2nd level handler. */
	la	$r2, _nds32_jmptbl_00
	lw	$r2, [$r2 + $r0 << #2]
	ADJ_INTR_LVL	/* Adjust INTR level. $r3 is clobbered.  */
	jral    $r2
	/* Restore used registers. */
#ifdef NDS32_SAVE_ALL_REGS
	RESTORE_ALL
#else
	RESTORE_PARTIAL
#endif
	iret

#ifdef NDS32_SAVE_ALL_REGS
#if defined(NDS32_NESTED)
	.size	_nds32_e_sa_ns, .-_nds32_e_sa_ns
#elif defined(NDS32_NESTED_READY)
	.size	_nds32_e_sa_nr, .-_nds32_e_sa_nr
#else /* Not nested handler. */
	.size	_nds32_e_sa_nn, .-_nds32_e_sa_nn
#endif /* endif for Nest Type */
#else /* not NDS32_SAVE_ALL_REGS */
#if defined(NDS32_NESTED)
	.size	_nds32_e_ps_ns, .-_nds32_e_ps_ns
#elif defined(NDS32_NESTED_READY)
	.size	_nds32_e_ps_nr, .-_nds32_e_ps_nr
#else /* Not nested handler. */
	.size	_nds32_e_ps_nn, .-_nds32_e_ps_nn
#endif /* endif for Nest Type */
#endif /* not NDS32_SAVE_ALL_REGS */
