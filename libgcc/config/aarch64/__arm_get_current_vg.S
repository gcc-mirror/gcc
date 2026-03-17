/* Support routine for SME.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.

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
   - x1-x15, x19-x29, sp and fp regs are call preserved.
   - Takes no argument.
   - Returns the value of the current VG in x0 if this value exists, and
     returns zero otherwise.  */

HIDDEN (__aarch64_cpu_features)

variant_pcs (__arm_get_current_vg)

ENTRY (__arm_get_current_vg)
	/* Check if SVE is available.  */
	adrp	x16, __aarch64_cpu_features
	ldr	x16, [x16, :lo12:__aarch64_cpu_features]
	tbnz	x16, #30, L(end_cntd)

	/* Check if SME is available.  */
	tbz	x16, #42, L(end_zero)

	/* Check if we're in streaming mode  */
	.inst	0xd53b4250  /* mrs	x16, svcr  */
	tbz	x16, #0, L(end_zero)
L(end_cntd):
	.inst	0x04e0e3e0  /* cntd	x0  */
	ret
L(end_zero):
	mov	x0, 0
	ret
END (__arm_get_current_vg)
