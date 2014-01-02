/* Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

/*
   - calculate 15..18 bit inverse using a table of approximating polynoms.
     precision is higher for polynoms used to evaluate input with larger
     value.
   - do one newton-raphson iteration step to double the precision,
     then multiply this with the divisor
	-> more time to decide if dividend is subnormal
     - the worst error propagation is on the side of the value range
       with the least initial defect, thus giving us about 30 bits precision.
 */
#include "../arc-ieee-754.h"

#if 0 /* DEBUG */
	.global __divsf3
	FUNC(__divsf3)
	.balign 4
__divsf3:
	push_s blink
	push_s r1
	bl.d __divsf3_c
	push_s r0
	ld_s r1,[sp,4]
	st_s r0,[sp,4]
	bl.d __divsf3_asm
	pop_s r0
	pop_s r1
	pop_s blink
	cmp r0,r1
#if 1
	bne abort
	jeq_s [blink]
	b abort
#else
	bne abort
	j_s [blink]
#endif
	ENDFUNC(__divsf3)
#define __divsf3 __divsf3_asm
#endif /* DEBUG */

	FUNC(__divsf3)
	.balign 4
.Ldivtab:
	.long 0xfc0ffff0
	.long 0xf46ffefd
	.long 0xed1ffd2a
	.long 0xe627fa8e
	.long 0xdf7ff73b
	.long 0xd917f33b
	.long 0xd2f7eea3
	.long 0xcd1fe986
	.long 0xc77fe3e7
	.long 0xc21fdddb
	.long 0xbcefd760
	.long 0xb7f7d08c
	.long 0xb32fc960
	.long 0xae97c1ea
	.long 0xaa27ba26
	.long 0xa5e7b22e
	.long 0xa1cfa9fe
	.long 0x9ddfa1a0
	.long 0x9a0f990c
	.long 0x9667905d
	.long 0x92df878a
	.long 0x8f6f7e84
	.long 0x8c27757e
	.long 0x88f76c54
	.long 0x85df630c
	.long 0x82e759c5
	.long 0x8007506d
	.long 0x7d3f470a
	.long 0x7a8f3da2
	.long 0x77ef341e
	.long 0x756f2abe
	.long 0x72f7212d
	.long 0x709717ad
	.long 0x6e4f0e44
	.long 0x6c1704d6
	.long 0x69e6fb44
	.long 0x67cef1d7
	.long 0x65c6e872
	.long 0x63cedf18
	.long 0x61e6d5cd
	.long 0x6006cc6d
	.long 0x5e36c323
	.long 0x5c76b9f3
	.long 0x5abeb0b7
	.long 0x5916a79b
	.long 0x57769e77
	.long 0x55de954d
	.long 0x54568c4e
	.long 0x52d6834d
	.long 0x51667a7f
	.long 0x4ffe71b5
	.long 0x4e9e68f1
	.long 0x4d466035
	.long 0x4bf65784
	.long 0x4aae4ede
	.long 0x496e4646
	.long 0x48363dbd
	.long 0x47063547
	.long 0x45de2ce5
	.long 0x44be2498
	.long 0x43a61c64
	.long 0x4296144a
	.long 0x41860c0e
	.long 0x407e03ee
.L7f800000:
	.long 0x7f800000
	.balign 4
	.global __divsf3_support
__divsf3_support:
.Linf_NaN:
	bclr.f 0,r0,31 ; 0/0 -> NaN
	xor_s r0,r0,r1
	bmsk r1,r0,30
	bic_s r0,r0,r1
	sub.eq r0,r0,1
	j_s.d [blink]
	or r0,r0,r9
.Lret0:
	xor_s r0,r0,r1
	bmsk r1,r0,30
	j_s.d [blink]
	bic_s r0,r0,r1
/* N.B. the spacing between divtab and the sub3 to get its address must
   be a multiple of 8.  */
__divsf3:
	lsr r2,r1,17
	sub3 r3,pcl,37 ; (.-.Ldivtab) >> 3
	bmsk_s r2,r2,5
	ld.as r5,[r3,r2]
	asl r4,r1,9
	ld.as r9,[pcl,-13]; [pcl,(-((.-.L7f800000) >> 2))] ; 0x7f800000
	mulu64 r5,r4
	and.f r11,r1,r9
	asl r6,r1,8
	bset r6,r6,31
	beq.d .Ldenorm_fp1
	asl r5,r5,13
	breq.d r11,r9,.Linf_nan_fp1
	and.f r2,r0,r9
	sub r7,r5,mhi
	mulu64 r7,r6
	beq.d .Ldenorm_fp0
	asl r12,r0,8
	breq.d r2,r9,.Linf_nan_fp0
	mulu64 mhi,r7
.Lpast_denorm_fp1:
	bset r3,r12,31
.Lpast_denorm_fp0:
	cmp_s r3,r6
	lsr.cc r3,r3,1
	add_s r2,r2, /* wait for immediate */ \
		0x3f000000
	sub r7,r7,mhi ; u1.31 inverse, about 30 bit
	mulu64 r3,r7
	sbc r2,r2,r11
	xor.f 0,r0,r1
	and r0,r2,r9
	bclr r3,r9,23 ; 0x7f000000
	brhs.d r2,r3,.Linf_denorm
	bxor.mi r0,r0,31
.Lpast_denorm:
	add r3,mhi,0x22 ; round to nearest or higher
	tst r3,0x3c ; check if rounding was unsafe
	lsr r3,r3,6
	jne.d [blink] ; return if rounding was safe.
	add_s r0,r0,r3
        /* work out exact rounding if we fall through here.  */
        /* We know that the exact result cannot be represented in single
           precision.  Find the mid-point between the two nearest
           representable values, multiply with the divisor, and check if
           the result is larger than the dividend.  */
        add_s r3,r3,r3
        sub_s r3,r3,1
        mulu64 r3,r6
	asr.f 0,r0,1 ; for round-to-even in case this is a denorm
	rsub r2,r9,25
        asl_s r12,r12,r2
        sub.f 0,r12,mlo
        j_s.d [blink]
        sub.mi r0,r0,1
.Linf_nan_fp1:
	lsr_s r0,r0,31
	bmsk.f 0,r1,22
	asl_s r0,r0,31
	bne_s 0f ; inf/inf -> nan
	brne r2,r9,.Lsigned0 ; x/inf -> 0, but x/nan -> nan
0:	j_s.d [blink]
	mov r0,-1
.Lsigned0:
.Linf_nan_fp0:
	tst_s r1,r1
	j_s.d [blink]
	bxor.mi r0,r0,31
	.balign 4
	.global __divsf3
/* For denormal results, it is possible that an exact result needs
   rounding, and thus the round-to-even rule has to come into play.  */
.Linf_denorm:
	brlo r2,0xc0000000,.Linf
.Ldenorm:
	asr_s r2,r2,23
	bic r0,r0,r9
	neg r9,r2
	brlo.d r9,25,.Lpast_denorm
	lsr r3,mlo,r9
	/* Fall through: return +- 0 */
	j_s [blink]
.Linf:
	j_s.d [blink]
	or r0,r0,r9
	.balign 4
.Ldenorm_fp1:
	bclr r6,r6,31
	norm.f r12,r6 ; flag for x/0 -> Inf check
	add r6,r6,r6
	rsub r5,r12,16
	ror r5,r1,r5
	asl r6,r6,r12
	bmsk r5,r5,5
	ld.as r5,[r3,r5]
	add r4,r6,r6
	; load latency
	mulu64 r5,r4
	bic.ne.f 0, \
		0x60000000,r0 ; large number / denorm -> Inf
	asl r5,r5,13
	sub r7,r5,mhi
	beq.d .Linf_NaN
	mulu64 r7,r6
	asl_s r12,r12,23
	and.f r2,r0,r9
	add_s r2,r2,r12
	asl r12,r0,8
	bne.d .Lpast_denorm_fp1
.Ldenorm_fp0: mulu64 mhi,r7
	bclr r12,r12,31
	norm.f r3,r12 ; flag for 0/x -> 0 check
	bic.ne.f 0,0x60000000,r1 ; denorm/large number -> 0
	beq_s .Lret0
	asl_s r12,r12,r3
	asl_s r3,r3,23
	add_s r12,r12,r12
	add r11,r11,r3
	b.d .Lpast_denorm_fp0
	mov_s r3,r12
	ENDFUNC(__divsf3)
