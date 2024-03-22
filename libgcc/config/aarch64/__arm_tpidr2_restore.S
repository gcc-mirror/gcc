/* Support routine for SME.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

/* Used for lazy ZA restore.  Call ABI:
   - Shared ZA, streaming-compatible.
   - x0 is a pointer to a TPIDR2 block.
   - x0-x13, x19-x29, sp and fp regs are call preserved.
   - Does not return a value.
   - Can abort on failure (then registers are not preserved).  */

variant_pcs (__arm_tpidr2_restore)

ENTRY (__arm_tpidr2_restore)
	.inst	0xd53bd0ae  /* mrs	x14, tpidr2_el0  */
	cbnz	x14, L(fail)

	/* check reserved bytes.  */
	ldrh	w15, [x0, 10]
	ldr	w16, [x0, 12]
	orr	w15, w15, w16
	cbnz	w15, L(fail)

	ldr	x16, [x0]
	cbz	x16, L(end)
	ldrh	w17, [x0, 8]
	cbz	w17, L(end)

	/* x0: blk, x14: 0, x15: 0,
	   x16: za_save_buffer, x17: num_za_save_slices.  */

L(restore_loop):
	.inst	0xe1006200  /* ldr	za[w15, 0], [x16]  */
	.inst	0xe1006201  /* ldr	za[w15, 1], [x16, 1, mul vl]  */
	.inst	0xe1006202  /* ldr	za[w15, 2], [x16, 2, mul vl]  */
	.inst	0xe1006203  /* ldr	za[w15, 3], [x16, 3, mul vl]  */
	.inst	0xe1006204  /* ldr	za[w15, 4], [x16, 4, mul vl]  */
	.inst	0xe1006205  /* ldr	za[w15, 5], [x16, 5, mul vl]  */
	.inst	0xe1006206  /* ldr	za[w15, 6], [x16, 6, mul vl]  */
	.inst	0xe1006207  /* ldr	za[w15, 7], [x16, 7, mul vl]  */
	.inst	0xe1006208  /* ldr	za[w15, 8], [x16, 8, mul vl]  */
	.inst	0xe1006209  /* ldr	za[w15, 9], [x16, 9, mul vl]  */
	.inst	0xe100620a  /* ldr	za[w15, 10], [x16, 10, mul vl]  */
	.inst	0xe100620b  /* ldr	za[w15, 11], [x16, 11, mul vl]  */
	.inst	0xe100620c  /* ldr	za[w15, 12], [x16, 12, mul vl]  */
	.inst	0xe100620d  /* ldr	za[w15, 13], [x16, 13, mul vl]  */
	.inst	0xe100620e  /* ldr	za[w15, 14], [x16, 14, mul vl]  */
	.inst	0xe100620f  /* ldr	za[w15, 15], [x16, 15, mul vl]  */
	add	w15, w15, 16
	.inst	0x04305a10  /* addsvl	x16, x16, 16  */
	cmp	w17, w15
	bhi	L(restore_loop)
L(end):
	ret
L(fail):
	PACIASP
	stp	x29, x30, [sp, -32]!
	.cfi_adjust_cfa_offset 32
	.cfi_rel_offset x29, 0
	.cfi_rel_offset x30, 8
	mov	x29, sp
	.inst	0x04e0e3f0  /* cntd	x16  */
	str	x16, [sp, 16]
	.cfi_rel_offset 46, 16
	.inst	0xd503467f  /* smstop  */
	bl	abort
END (__arm_tpidr2_restore)
