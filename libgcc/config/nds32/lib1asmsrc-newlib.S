/* newlib libgcc routines of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

	.section	.mdebug.abi_nds32
	.previous

#ifdef L_divsi3

	.text
	.align	2
	.globl	__divsi3
	.type	__divsi3, @function
__divsi3:
	movi	$r5, 0		! res = 0
	xor	$r4, $r0, $r1	! neg
	bltz	$r0, .L1
	bltz	$r1, .L2
.L3:
	movi	$r2, 1		! bit = 1
	slt	$r3, $r1, $r0	! test if dividend is smaller than or equal to divisor
	beqz	$r3, .L5
	bltz	$r1, .L5

.L4:
	slli	$r2, $r2, 1
	beqz	$r2, .L6
	slli	$r1, $r1, 1
	slt	$r3, $r1, $r0
	beqz	$r3, .L5
	bgez	$r1, .L4

.L5:
	slt	$r3, $r0, $r1
	bnez	$r3, .L8
	sub	$r0, $r0, $r1
	or	$r5, $r5, $r2
.L8:
	srli	$r1, $r1, 1
	srli	$r2, $r2, 1
	bnez	$r2, .L5
.L6:
	bgez	$r4, .L7
	subri	$r5, $r5, 0	! negate if $r4 < 0
.L7:
	move	$r0, $r5
	ret
.L1:
	subri	$r0, $r0, 0	! change neg to pos
	bgez	$r1, .L3
.L2:
	subri	$r1, $r1, 0	! change neg to pos
	j	.L3
	.size	__divsi3, .-__divsi3

#endif /* L_divsi3 */


#ifdef L_modsi3

	.text
	.align	2
	.globl	__modsi3
	.type	__modsi3, @function
__modsi3:
	movi	$r5, 0		! res = 0
	move	$r4, $r0	! neg
	bltz	$r0, .L1
	bltz	$r1, .L2
.L3:
	movi	$r2, 1		! bit = 1
	slt	$r3, $r1, $r0	! test if dividend is smaller than or equal to divisor
	beqz	$r3, .L5
	bltz	$r1, .L5

.L4:
	slli	$r2, $r2, 1
	beqz	$r2, .L6
	slli	$r1, $r1, 1
	slt	$r3, $r1, $r0
	beqz	$r3, .L5
	bgez	$r1, .L4

.L5:
	slt	$r3, $r0, $r1
	bnez	$r3, .L8
	sub	$r0, $r0, $r1
	or	$r5, $r5, $r2
.L8:
	srli	$r1, $r1, 1
	srli	$r2, $r2, 1
	bnez	$r2, .L5
.L6:
	bgez	$r4, .L7
	subri	$r0, $r0, 0	! negate if $r4 < 0
.L7:
	ret
.L1:
	subri	$r0, $r0, 0	! change neg to pos
	bgez	$r1, .L3
.L2:
	subri	$r1, $r1, 0	! change neg to pos
	j	.L3
	.size	__modsi3, .-__modsi3

#endif /* L_modsi3  */


#ifdef L_udivsi3

	.text
	.align	2
	.globl	__udivsi3
	.type	__udivsi3, @function
__udivsi3:
	movi	$r5, 0		! res = 0
	movi	$r2, 1		! bit = 1
	slt	$r3, $r1, $r0	! test if dividend is smaller than or equal to divisor
	beqz	$r3, .L5
	bltz	$r1, .L5

.L4:
	slli	$r2, $r2, 1
	beqz	$r2, .L6
	slli	$r1, $r1, 1
	slt	$r3, $r1, $r0
	beqz	$r3, .L5
	bgez	$r1, .L4

.L5:
	slt	$r3, $r0, $r1
	bnez	$r3, .L8
	sub	$r0, $r0, $r1
	or	$r5, $r5, $r2
.L8:
	srli	$r1, $r1, 1
	srli	$r2, $r2, 1
	bnez	$r2, .L5
.L6:
	move	$r0, $r5
	ret
	.size	__udivsi3, .-__udivsi3

#endif /* L_udivsi3  */


#ifdef L_umodsi3

	.text
	.align	2
	.globl	__umodsi3
	.type	__umodsi3, @function
__umodsi3:
	movi	$r5, 0		! res = 0
	movi	$r2, 1		! bit = 1
	slt	$r3, $r1, $r0	! test if dividend is smaller than or equal to divisor
	beqz	$r3, .L5
	bltz	$r1, .L5

.L4:
	slli	$r2, $r2, 1
	beqz	$r2, .L6
	slli	$r1, $r1, 1
	slt	$r3, $r1, $r0
	beqz	$r3, .L5
	bgez	$r1, .L4

.L5:
	slt	$r3, $r0, $r1
	bnez	$r3, .L8
	sub	$r0, $r0, $r1
	or	$r5, $r5, $r2
.L8:
	srli	$r1, $r1, 1
	srli	$r2, $r2, 1
	bnez	$r2, .L5
.L6:
	ret
	.size	__umodsi3, .-__umodsi3

#endif /* L_umodsi3  */

/* ----------------------------------------------------------- */
