/*  This file contains the GPR save and restore routines for Darwin.
 *
 *   Copyright (C) 2011 Free Software Foundation, Inc.
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

/* Contributed by Iain Sandoe  <iains@gcc.gnu.org> */

/* Like their FP and VEC counterparts, these routines have only one externally
   visible entry point.  Calls have to be constructed as offsets from this.
   (I.E. callers have to jump to "saveGPR+((x-13)*4" to save registers x..31).

   Each save/load instruction is 4 bytes long (for both m32 and m64 builds).

   The save/restores here are done w.r.t r11.

   restGPRx restores the link reg from the stack and returns to the saved
   address.

   */

#include "darwin-asm.h"

	.text
	.align 2

	.private_extern saveGPR
saveGPR:
	stg r13,(-19 * GPR_BYTES)(r11)
	stg r14,(-18 * GPR_BYTES)(r11)
	stg r15,(-17 * GPR_BYTES)(r11)
	stg r16,(-16 * GPR_BYTES)(r11)
	stg r17,(-15 * GPR_BYTES)(r11)
	stg r18,(-14 * GPR_BYTES)(r11)
	stg r19,(-13 * GPR_BYTES)(r11)
	stg r20,(-12 * GPR_BYTES)(r11)
	stg r21,(-11 * GPR_BYTES)(r11)
	stg r22,(-10 * GPR_BYTES)(r11)
	stg r23,( -9 * GPR_BYTES)(r11)
	stg r24,( -8 * GPR_BYTES)(r11)
	stg r25,( -7 * GPR_BYTES)(r11)
	stg r26,( -6 * GPR_BYTES)(r11)
	stg r27,( -5 * GPR_BYTES)(r11)
	stg r28,( -4 * GPR_BYTES)(r11)
	stg r29,( -3 * GPR_BYTES)(r11)
	stg r30,( -2 * GPR_BYTES)(r11)
	stg r31,( -1 * GPR_BYTES)(r11)
	blr

/* */

	.private_extern restGPR
restGPR:
	lg r13,(-19 * GPR_BYTES)(r11)
	lg r14,(-18 * GPR_BYTES)(r11)
	lg r15,(-17 * GPR_BYTES)(r11)
	lg r16,(-16 * GPR_BYTES)(r11)
	lg r17,(-15 * GPR_BYTES)(r11)
	lg r18,(-14 * GPR_BYTES)(r11)
	lg r19,(-13 * GPR_BYTES)(r11)
	lg r20,(-12 * GPR_BYTES)(r11)
	lg r21,(-11 * GPR_BYTES)(r11)
	lg r22,(-10 * GPR_BYTES)(r11)
	lg r23,( -9 * GPR_BYTES)(r11)
	lg r24,( -8 * GPR_BYTES)(r11)
	lg r25,( -7 * GPR_BYTES)(r11)
	lg r26,( -6 * GPR_BYTES)(r11)
	lg r27,( -5 * GPR_BYTES)(r11)
	lg r28,( -4 * GPR_BYTES)(r11)
	lg r29,( -3 * GPR_BYTES)(r11)
	lg r30,( -2 * GPR_BYTES)(r11)
	lg r31,( -1 * GPR_BYTES)(r11)
	blr

	.private_extern restGPRx
restGPRx:
	lg r13,(-19 * GPR_BYTES)(r11)
	lg r14,(-18 * GPR_BYTES)(r11)
	lg r15,(-17 * GPR_BYTES)(r11)
	lg r16,(-16 * GPR_BYTES)(r11)
	lg r17,(-15 * GPR_BYTES)(r11)
	lg r18,(-14 * GPR_BYTES)(r11)
	lg r19,(-13 * GPR_BYTES)(r11)
	lg r20,(-12 * GPR_BYTES)(r11)
	lg r21,(-11 * GPR_BYTES)(r11)
	lg r22,(-10 * GPR_BYTES)(r11)
	lg r23,( -9 * GPR_BYTES)(r11)
	lg r24,( -8 * GPR_BYTES)(r11)
	lg r25,( -7 * GPR_BYTES)(r11)
	lg r26,( -6 * GPR_BYTES)(r11)
	lg r27,( -5 * GPR_BYTES)(r11)
	lg r28,( -4 * GPR_BYTES)(r11)
	lg r29,( -3 * GPR_BYTES)(r11)
	/* Like the FP restore, we start from the offset for r30
	   thus a restore of only r31 is not going to work.  */
	lg r0,SAVED_LR_OFFSET(r1)
	lg r30,( -2 * GPR_BYTES)(r11)
	mtlr r0
	lg r31,( -1 * GPR_BYTES)(r11)
	blr
