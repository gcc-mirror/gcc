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

/* Query SME state.  Call ABI:
   - Private ZA, streaming-compatible.
   - x2-x15, x19-x29, sp and fp regs are call preserved.
   - Takes no argument.
   - Returns SME state in x0 and TPIDR2_EL0 in x1.  */

HIDDEN (__aarch64_have_sme)

variant_pcs (__arm_sme_state)

ENTRY (__arm_sme_state)
	/* Check if SME is available.  */
	adrp	x1, __aarch64_have_sme
	ldrb	w1, [x1, :lo12:__aarch64_have_sme]
	cbz	w1, L(nosme)

	/* Expose the bottom 2 bits of svcr (SM, ZA) in x0 and set the
	   top 2 bits indicating that SME and TPIDR2_EL0 are available.  */
	.inst	0xd53b4240  /* mrs	x0, svcr  */
	.inst	0xd53bd0a1  /* mrs	x1, tpidr2_el0  */
	and	x0, x0, 3
	orr	x0, x0, 0xc000000000000000
	ret

L(nosme):
	mov	x0, 0
	mov	x1, 0
	ret
END (__arm_sme_state)
