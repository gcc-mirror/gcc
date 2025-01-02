/* Support routine for SME.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#include "aarch64-asm.h"

/* Disable ZA.  Call ABI:
   - Private ZA, streaming-compatible.
   - x0-x13, x19-x29, sp and fp regs are call preserved.
   - Takes no argument.
   - Does not return a value.
   - Can abort on failure (then registers are not preserved).  */

HIDDEN (__aarch64_have_sme)

HIDDEN (__libgcc_arm_tpidr2_save)

variant_pcs (__arm_za_disable)

ENTRY (__arm_za_disable)
	/* Check if SME is available.  */
	adrp	x14, __aarch64_have_sme
	ldrb	w14, [x14, :lo12:__aarch64_have_sme]
	cbz	w14, L(end)

	.inst	0xd53bd0ae  /* mrs	x14, tpidr2_el0  */
	cbz	x14, L(end)

	PACIASP
	stp	x29, x30, [sp, -16]!
	.cfi_adjust_cfa_offset 16
	.cfi_rel_offset x29, 0
	.cfi_rel_offset x30, 8
	mov	x29, sp
	bl	__libgcc_arm_tpidr2_save
	.inst	0xd51bd0bf  /* msr	tpidr2_el0, xzr  */
	.inst	0xd503447f  /* smstop	za  */
	ldp	x29, x30, [sp], 16
	.cfi_adjust_cfa_offset -16
	.cfi_restore x29
	.cfi_restore x30
	AUTIASP
L(end):
	ret
END (__arm_za_disable)

/* Hidden alias used by the unwinder.  */
.global __libgcc_arm_za_disable
HIDDEN (__libgcc_arm_za_disable)
.set __libgcc_arm_za_disable, __arm_za_disable
