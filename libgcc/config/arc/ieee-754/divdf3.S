/* Copyright (C) 2008-2013 Free Software Foundation, Inc.
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
   to calculate a := b/x as b*y, with y := 1/x:
   - x is in the range [1..2)
   - calculate 15..18 bit inverse y0 using a table of approximating polynoms.
     Precision is higher for polynoms used to evaluate input with larger
     value.
   - Do one newton-raphson iteration step to double the precision,
     then multiply this with the divisor
	-> more time to decide if dividend is subnormal
     - the worst error propagation is on the side of the value range
       with the least initial defect, thus giving us about 30 bits precision.
      The truncation error for the either is less than 1 + x/2 ulp.
      A 31 bit inverse can be simply calculated by using x with implicit 1
      and chaining the multiplies.  For a 32 bit inverse, we multiply y0^2
      with the bare fraction part of x, then add in y0^2 for the implicit
      1 of x.
    - If calculating a 31 bit inverse, the systematic error is less than
      -1 ulp; likewise, for 32 bit, it is less than -2 ulp.
    - If we calculate our seed with a 32 bit fraction, we can archive a
      tentative result strictly better than -2 / +2.5 (1) ulp/128, i.e. we
      only need to take the step to calculate the 2nd stage rest and
      rounding adjust 1/32th of the time.  However, if we use a 20 bit
      fraction for the seed, the negative error can exceed -2 ulp/128, (2)
      thus for a simple add / tst check, we need to do the 2nd stage
      rest calculation/ rounding adjust 1/16th of the time.
      (1): The inexactness of the 32 bit inverse contributes an error in the
      range of (-1 .. +(1+x/2) ) ulp/128.  Leaving out the low word of the
      rest contributes an error < +1/x ulp/128 .  In the interval [1,2),
      x/2 + 1/x <= 1.5 .
      (2): Unless proven otherwise.  I have not actually looked for an
      example where -2 ulp/128 is exceeded, and my calculations indicate
      that the excess, if existent, is less than -1/512 ulp.
 */
#include "arc-ieee-754.h"

/* N.B. fp-bit.c does double rounding on denormal numbers.  */
#if 0 /* DEBUG */
	.global __divdf3
	FUNC(__divdf3)
	.balign 4
__divdf3:
	push_s blink
	push_s r2
	push_s r3
	push_s r0
	bl.d __divdf3_c
	push_s r1
	ld_s r2,[sp,12]
	ld_s r3,[sp,8]
	st_s r0,[sp,12]
	st_s r1,[sp,8]
	pop_s r1
	bl.d __divdf3_asm
	pop_s r0
	pop_s r3
	pop_s r2
	pop_s blink
	cmp r0,r2
	cmp.eq r1,r3
	jeq_s [blink]
	and r12,DBL0H,DBL1H
	bic.f 0,0x7ff80000,r12 ; both NaN -> OK
	jeq_s [blink]
	bl abort
	ENDFUNC(__divdf3)
#define __divdf3 __divdf3_asm
#endif /* DEBUG */

	FUNC(__divdf3)
__divdf3_support: /* This label makes debugger output saner.  */
	.balign 4
.Ldenorm_dbl1:
	brge r6, \
		0x43500000,.Linf_NaN ; large number / denorm -> Inf
	bmsk.f r12,DBL1H,19
	mov.eq r12,DBL1L
	mov.eq DBL1L,0
	sub.eq r7,r7,32
	norm.f r11,r12 ; flag for x/0 -> Inf check
	beq_s .Linf_NaN
	mov.mi r11,0
	add.pl r11,r11,1
	add_s r12,r12,r12
	asl r8,r12,r11
	rsub r12,r11,31
	lsr r12,DBL1L,r12
	tst_s DBL1H,DBL1H
	or r8,r8,r12
	lsr r4,r8,26
	lsr DBL1H,r8,12
	ld.as r4,[r10,r4]
	bxor.mi DBL1H,DBL1H,31
	sub r11,r11,11
	asl DBL1L,DBL1L,r11
	sub r11,r11,1
	mpyhu r5,r4,r8
	sub r7,r7,r11
	asl r4,r4,12
	b.d .Lpast_denorm_dbl1
	asl r7,r7,20
	; wb stall

	.balign 4
.Ldenorm_dbl0:
	bmsk.f r12,DBL0H,19
	; wb stall
	mov.eq r12,DBL0L
	sub.eq r6,r6,32
	norm.f r11,r12 ; flag for 0/x -> 0 check
	brge r7, \
		0x43500000, .Lret0_NaN ; denorm/large number -> 0
	beq_s .Lret0_NaN
	mov.mi r11,0
	add.pl r11,r11,1
	asl r12,r12,r11
	sub r6,r6,r11
	add.f 0,r6,31
	lsr r10,DBL0L,r6
	mov.mi r10,0
	add r6,r6,11+32
	neg.f r11,r6
	asl DBL0L,DBL0L,r11
	mov.pl DBL0L,0
	sub r6,r6,32-1
	b.d .Lpast_denorm_dbl0
	asl r6,r6,20

.Linf_NaN:
	tst_s DBL0L,DBL0L ; 0/0 -> NaN
	xor_s DBL1H,DBL1H,DBL0H
	bclr.eq.f DBL0H,DBL0H,31
	bmsk DBL0H,DBL1H,30
	xor_s DBL0H,DBL0H,DBL1H
	sub.eq DBL0H,DBL0H,1
	mov_s DBL0L,0
	j_s.d [blink]
	or DBL0H,DBL0H,r9
	.balign 4
.Lret0_NaN:
	xor_s DBL1H,DBL1H,DBL0H
	cmp_s r12,r9
	mov_s DBL0L,0
	bmsk DBL0H,DBL1H,30
	xor_s DBL0H,DBL0H,DBL1H
	j_s.d [blink]
	sub.hi DBL0H,DBL0H,1
.Linf_nan_dbl1: ; Inf/Inf -> NaN x/Inf-> 0 x/NaN -> NaN
	not_s DBL0L,DBL1H
	cmp r6,r9
	sub_s.ne DBL0L,DBL0L,DBL0L
	tst_s DBL0H,DBL0H
	add_s DBL0H,DBL1H,DBL0L
	j_s.d [blink]
	bxor.mi DBL0H,DBL0H,31
.Linf_nan_dbl0:
	tst_s DBL1H,DBL1H
	j_s.d [blink]
	bxor.mi DBL0H,DBL0H,31
	.balign 4
	.global __divdf3
/* N.B. the spacing between divtab and the add3 to get its address must
   be a multiple of 8.  */
__divdf3:
	asl r8,DBL1H,12
	lsr r12,DBL1L,20
	lsr r4,r8,26
	add3 r10,pcl,59 ; (.Ldivtab-.) >> 3
	ld.as r4,[r10,r4]
	ld.as r9,[pcl,180]; [pcl,(-((.-.L7ff00000) >> 2))] ; 0x7ff00000
	or r8,r8,r12
	mpyhu r5,r4,r8
	and.f r7,DBL1H,r9
	asl r4,r4,12 ; having the asl here is a concession to the XMAC pipeline.
	beq.d .Ldenorm_dbl1
	and r6,DBL0H,r9
.Lpast_denorm_dbl1: ; wb stall
	sub r4,r4,r5
	mpyhu r5,r4,r4
	breq.d r6,0,.Ldenorm_dbl0
	lsr r8,r8,1
	asl r12,DBL0H,11
	lsr r10,DBL0L,21
.Lpast_denorm_dbl0: ; wb stall
	bset r8,r8,31
	mpyhu r11,r5,r8
	add_s r12,r12,r10
	bset r5,r12,31
	cmp r5,r8
	cmp.eq DBL0L,DBL1L
	; wb stall
	lsr.cc r5,r5,1
	sub r4,r4,r11 ; u1.31 inverse, about 30 bit
	mpyhu r11,r5,r4 ; result fraction highpart
	breq r7,r9,.Linf_nan_dbl1
	lsr r8,r8,2 ; u3.29
	add r5,r6, /* wait for immediate /  XMAC wb stall */ \
		0x3fe00000
	; wb stall (not for XMAC)
	breq r6,r9,.Linf_nan_dbl0
	mpyu r12,r11,r8 ; u-28.31
	asl_s DBL1L,DBL1L,9 ; u-29.23:9
	sbc r6,r5,r7
	; resource conflict (not for XMAC)
	mpyhu r5,r11,DBL1L ; u-28.23:9
	add.cs DBL0L,DBL0L,DBL0L
	asl_s DBL0L,DBL0L,6 ; u-26.25:7
	asl r10,r11,23
	sub_l DBL0L,DBL0L,r12
	; wb stall (before 'and' for XMAC)
	lsr r7,r11,9
	sub r5,DBL0L,r5 ; rest msw ; u-26.31:0
	mpyh r12,r5,r4 ; result fraction lowpart
	xor.f 0,DBL0H,DBL1H
	and DBL0H,r6,r9
	add_s DBL0H,DBL0H,r7 ; (XMAC wb stall)
	bxor.mi DBL0H,DBL0H,31
	brhs r6, /*  wb stall / wait for immediate */ \
		0x7fe00000,.Linf_denorm
	add.f r12,r12,0x11
	asr r9,r12,5
	sub.mi DBL0H,DBL0H,1
	add.f DBL0L,r9,r10
	tst r12,0x1c
	jne.d [blink]
	add.cs DBL0H,DBL0H,1
        /* work out exact rounding if we fall through here.  */
        /* We know that the exact result cannot be represented in double
           precision.  Find the mid-point between the two nearest
           representable values, multiply with the divisor, and check if
           the result is larger than the dividend.  Since we want to know
	   only the sign bit, it is sufficient to calculate only the
	   highpart of the lower 64 bits.  */
	sub.f DBL0L,DBL0L,1
	asl r12,r9,2 ; u-22.30:2
	mpyu r10,r11,DBL1L ; rest before considering r12 in r5 : -r10
	sub.cs DBL0H,DBL0H,1
	sub.f r12,r12,2
	; resource conflict (not for XMAC)
	mpyhu r7,r12,DBL1L ; u-51.32
	asl r5,r5,25 ; s-51.7:25
	lsr r10,r10,7 ; u-51.30:2
	; resource conflict (not for XMAC)
	; resource conflict (not for XMAC)
	mpyu r9,r12,r8 ; u-51.31:1
	sub r5,r5,r10
	add.mi r5,r5,DBL1L ; signed multiply adjust for r12*DBL1L
	bset r7,r7,0 ; make sure that the result is not zero, and that
	; wb stall (one earlier for XMAC)
	sub r5,r5,r7 ; a highpart zero appears negative
	sub.f r5,r5,r9 ; rest msw
	add.pl.f DBL0L,DBL0L,1
	j_s.d [blink]
	add.eq DBL0H,DBL0H,1

	.balign 4
.Linf_denorm:
	brlo r6,0xc0000000,.Linf
.Ldenorm:
	asr r6,r6,20
	neg r9,r6
	mov_s DBL0H,0
	brhs.d r9,54,.Lret0
	bxor.mi DBL0H,DBL0H,31
	add_l r12,r12,1
	and r12,r12,-4
	rsub r7,r6,5
	asr r10,r12,28
	bmsk r4,r12,27
	asrs DBL0L,r4,r7
	add DBL1H,r11,r10
	add.f r7,r6,32-5
	abss r10,r4
	asl r4,r4,r7
	mov.mi r4,r10
	add.f r10,r6,23
	rsub r7,r6,9
	lsr r7,DBL1H,r7
	asl r10,DBL1H,r10
	or.pnz DBL0H,DBL0H,r7
	or.mi r4,r4,r10
	mov.mi r10,r7
	add.f DBL0L,r10,DBL0L
	add.cs.f DBL0H,DBL0H,1 ; carry clear after this point
	bxor.f 0,r4,31
	add.pnz.f DBL0L,DBL0L,1
	add.cs.f DBL0H,DBL0H,1
	jne_l [blink]
	/* Calculation so far was not conclusive; calculate further rest.  */
	mpyu r11,r11,DBL1L ; rest before considering r12 in r5 : -r11
	asr.f r12,r12,3
	asl r5,r5,25 ; s-51.7:25
	; resource conflict (not for XMAC)
	mpyu DBL1H,r12,r8 ; u-51.31:1
	and r9,DBL0L,1 ; tie-breaker: round to even
	lsr r11,r11,7 ; u-51.30:2
	; resource conflict (not for XMAC)
	mpyhu r8,r12,DBL1L ; u-51.32
	sub.mi r11,r11,DBL1L ; signed multiply adjust for r12*DBL1L
	add_s DBL1H,DBL1H,r11
	; resource conflict (not for XMAC)
	; resource conflict (not for XMAC)
	mpyu r12,r12,DBL1L ; u-83.30:2
	sub DBL1H,DBL1H,r5 ; -rest msw
	add_s DBL1H,DBL1H,r8 ; -rest msw
	add.f 0,DBL1H,DBL1H ; can't ror.f by 32 :-(
	; wb stall (XMAC: Before add.f)
	tst_s DBL1H,DBL1H
	cmp.eq r12,r9
	add.cs.f DBL0L,DBL0L,1
	j_s.d [blink]
	add.cs DBL0H,DBL0H,1

.Lret0:
	/* return +- 0 */
	j_s.d [blink]
	mov_s DBL0L,0
.Linf:
	mov_s DBL0H,r9
	mov_s DBL0L,0
	j_s.d [blink]
	bxor.mi DBL0H,DBL0H,31

	.balign 4
.Ldivtab:
	.long 0xfc0fffe1
	.long 0xf46ffdfb
	.long 0xed1ffa54
	.long 0xe61ff515
	.long 0xdf7fee75
	.long 0xd91fe680
	.long 0xd2ffdd52
	.long 0xcd1fd30c
	.long 0xc77fc7cd
	.long 0xc21fbbb6
	.long 0xbcefaec0
	.long 0xb7efa100
	.long 0xb32f92bf
	.long 0xae8f83b7
	.long 0xaa2f7467
	.long 0xa5ef6479
	.long 0xa1cf53fa
	.long 0x9ddf433e
	.long 0x9a0f3216
	.long 0x965f2091
	.long 0x92df0f11
	.long 0x8f6efd05
	.long 0x8c1eeacc
	.long 0x88eed876
	.long 0x85dec615
	.long 0x82eeb3b9
	.long 0x800ea10b
	.long 0x7d3e8e0f
	.long 0x7a8e7b3f
	.long 0x77ee6836
	.long 0x756e5576
	.long 0x72fe4293
	.long 0x709e2f93
	.long 0x6e4e1c7f
	.long 0x6c0e095e
	.long 0x69edf6c5
	.long 0x67cde3a5
	.long 0x65cdd125
	.long 0x63cdbe25
	.long 0x61ddab3f
	.long 0x600d991f
	.long 0x5e3d868c
	.long 0x5c6d7384
	.long 0x5abd615f
	.long 0x590d4ecd
	.long 0x576d3c83
	.long 0x55dd2a89
	.long 0x545d18e9
	.long 0x52dd06e9
	.long 0x516cf54e
	.long 0x4ffce356
	.long 0x4e9cd1ce
	.long 0x4d3cbfec
	.long 0x4becae86
	.long 0x4aac9da4
	.long 0x496c8c73
	.long 0x483c7bd3
	.long 0x470c6ae8
	.long 0x45dc59af
	.long 0x44bc4915
	.long 0x43ac3924
	.long 0x428c27fb
	.long 0x418c187a
	.long 0x407c07bd
.L7ff00000:
	.long 0x7ff00000
	ENDFUNC(__divdf3)
