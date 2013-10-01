/* Copyright (C) 2006, 2008-2012 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "arc-ieee-754.h"

#if 0 /* DEBUG */
	FUNC(__truncdfsf2)
	.global __truncdfsf2
	.balign 4
__truncdfsf2:
	push_s blink
	push_s r0
	bl.d __truncdfsf2_c
	push_s r1
	mov_s r2,r0
	pop_s r1
	ld r0,[sp]
	bl.d __truncdfsf2_asm
	st r2,[sp]
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	and r12,r0,r1
	bic.f 0,0x7f800000,r12
	bne 0f
	bmsk.f 0,r0,22
	bmsk.ne.f r1,r1,22
	jne_s [blink] ; both NaN -> OK
0:	bl abort
	ENDFUNC(__truncdfsf2)
#define __truncdfsf2 __truncdfsf2_asm
#endif /* DEBUG */

	.global __truncdfsf2
	.balign 4
	FUNC(__truncdfsf2)
__truncdfsf2:
	lsr r2,DBL0H,20
	asl_s DBL0H,DBL0H,12
	sub r12,r2,0x380
	bclr.f r3,r12,11
	brhs r3,0xff,.Lill_exp
	beq_l .Ldenorm0
	asl_s r12,r12,23
	tst DBL0L, \
		0x2fffffff /* Check if msb guard bit wants rounding up.  */
	lsr_s DBL0L,DBL0L,28
	lsr_s DBL0H,DBL0H,8
	add.ne DBL0L,DBL0L,1
	add_s DBL0H,DBL0H,DBL0L
	lsr_s DBL0H,DBL0H
	btst_s r2,11
	add_s r0,DBL0H,r12
	j_s.d [blink]
	bxor.ne r0,r0,31
	.balign 4
.Lill_exp:
	bbit1 r2,10,.Linf_nan
	bmsk_s r12,r12,9
	rsub.f r12,r12,8+0x400-32 ; Go from 9 to 1 guard bit in MSW.  */
	bhs_s .Lzero
	lsr r3,DBL0L,21
	rrc DBL0H,DBL0H ; insert leading 1
	asl.f 0,DBL0L,8 ; check lower 24 guard bits
	add_s r3,DBL0H,r3
	add.pnz r3,r3,1 ; assemble fraction with compressed guard bits.
	lsr r0,r3,r12
	neg_s r12,r12
	btst_s r0,1
	asl.eq.f r3,r3,r12
	add.ne r0,r0,1
	btst_s r2,11
	lsr_s r0,r0
	j_s.d [blink]
	bxor.ne r0,r0,31
.Lzero:
	lsr_s r2,r2,11
	j_s.d [blink]
	asl r0,r2,31
.Ldenorm0:
	asl_s r12,r12,20
	tst DBL0L, \
		0x5fffffff /* Check if msb guard bit wants rounding up.  */
	lsr_s DBL0L,DBL0L,29
	lsr_s DBL0H,DBL0H,9
	add.ne DBL0L,DBL0L,1
	bset_s DBL0H,DBL0H,23
	add_s DBL0H,DBL0H,DBL0L
	lsr_s DBL0H,DBL0H
	j_s.d [blink]
	add_l r0,DBL0H,r12

/* We would generally say that NaNs must have a non-zero high fraction part,
   but to allow hardware double precision floating point to interoperate
   with single precision software floating point, we make an exception here.
   The cost is to replace a tst_s DBL0H with an or.f DBL0L,DBL0L,DBL0H .
   As we start out unaligned, and there is an odd number of other short insns,
   we have a choice of letting this cost us a misalign penalty or
   4 more bytes (if we align the code).  We choose the former here because
   infinity / NaN is not expected to be prevalent in time-critical code.  */
.Linf_nan:
	or.f DBL0L,DBL0L,DBL0H
	mov_s r0,1
	add.ne r2,r2,1
	tst r2,0x7ff
	asl.ne r0,r0,23
	btst_s r12,11
	neg r0,r0
	j_s.d [blink]
	bxor.eq r0,r0,31
	ENDFUNC(__truncdfsf2)
