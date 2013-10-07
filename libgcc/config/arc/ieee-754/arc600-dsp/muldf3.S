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

#include "../arc-ieee-754.h"

#if 0 /* DEBUG */
	.global __muldf3
	.balign 4
__muldf3:
	push_s blink
	push_s r2
	push_s r3
	push_s r0
	bl.d __muldf3_c
	push_s r1
	ld_s r2,[sp,12]
	ld_s r3,[sp,8]
	st_s r0,[sp,12]
	st_s r1,[sp,8]
	pop_s r1
	bl.d __muldf3_asm
	pop_s r0
	pop_s r3
	pop_s r2
	pop_s blink
	cmp r0,r2
	cmp.eq r1,r3
	jeq_s [blink]
	b abort
#define __muldf3 __muldf3_asm
#endif /* DEBUG */

__muldf3_support: /* This label makes debugger output saner.  */
	.balign 4
	FUNC(__muldf3)
.Ldenorm_2:
	breq.d DBL1L,0,.Lret0_2 ; 0 input -> 0 output
	norm.f r12,DBL1L
	mov.mi r12,21
	add.pl r12,r12,22
	neg r11,r12
	asl_s r12,r12,20
	lsr.f DBL1H,DBL1L,r11
	ror DBL1L,DBL1L,r11
	sub_s DBL0H,DBL0H,r12
	mov.eq DBL1H,DBL1L
	sub_l DBL1L,DBL1L,DBL1H
	/* Fall through.  */
	.global __muldf3
	.balign 4
__muldf3:
	mululw 0,DBL0L,DBL1L
	machulw r4,DBL0L,DBL1L
	ld.as r9,[pcl,0x67] ; ((.L7ff00000-.+2)/4)]
	bmsk r6,DBL0H,19
	bset r6,r6,20
	mov r8,acc2
	mululw 0,r4,1
	and r11,DBL0H,r9
	breq.d r11,0,.Ldenorm_dbl0
	and r12,DBL1H,r9
	breq.d r12,0,.Ldenorm_dbl1
	maclw 0,r6,DBL1L
	machulw 0,r6,DBL1L
	breq.d r11,r9,.Linf_nan
	bmsk r10,DBL1H,19
	breq.d r12,r9,.Linf_nan
	bset r10,r10,20
	maclw 0,r10,DBL0L
	machulw r5,r10,DBL0L
	add_s r12,r12,r11 ; add exponents
	mov r4,acc2
	mululw 0,r5,1
	maclw 0,r6,r10
	machulw r7,r6,r10 ; fraction product in r7:acc2:r4:r8
	tst r8,r8
	bclr r8,r9,30 ; 0x3ff00000
	bset.ne r4,r4,0 ; put least significant word into sticky bit
	bclr r6,r9,20 ; 0x7fe00000
	lsr.f r10,r7,9
	rsub.eq r8,r8,r9 ; 0x40000000
	sub r12,r12,r8 ; subtract bias + implicit 1
	brhs.d r12,r6,.Linf_denorm
	rsub r10,r10,12
.Lshift_frac:
	neg r8,r10
	asl r6,r4,r10
	lsr DBL0L,r4,r8
	add.f 0,r6,r6
	btst.eq DBL0L,0
	cmp.eq r4,r4 ; round to nearest / round to even
	asl r4,acc2,r10
	lsr r5,acc2,r8
	adc.f DBL0L,DBL0L,r4
	xor.f 0,DBL0H,DBL1H
	asl r7,r7,r10
	add_s r12,r12,r5
	adc DBL0H,r12,r7
	j_s.d [blink]
	bset.mi DBL0H,DBL0H,31

/* N.B. This is optimized for ARC700.
  ARC600 has very different scheduling / instruction selection criteria.  */

/* If one number is denormal, subtract some from the exponent of the other
   one (if the other exponent is too small, return 0), and normalize the
   denormal.  Then re-run the computation.  */
.Lret0_2:
	lsr_s DBL0H,DBL0H,31
	asl_s DBL0H,DBL0H,31
	j_s.d [blink]
	mov_s DBL0L,0
	.balign 4
.Ldenorm_dbl0:
	mov_s r12,DBL0L
	mov_s DBL0L,DBL1L
	mov_s DBL1L,r12
	mov_s r12,DBL0H
	mov_s DBL0H,DBL1H
	mov_s DBL1H,r12
	and r11,DBL0H,r9
.Ldenorm_dbl1:
	brhs r11,r9,.Linf_nan
	brhs 0x3ca00001,r11,.Lret0
	sub_s DBL0H,DBL0H,DBL1H
	bmsk.f DBL1H,DBL1H,30
	add_s DBL0H,DBL0H,DBL1H
	beq.d .Ldenorm_2
	norm r12,DBL1H
	sub_s r12,r12,10
	asl r5,r12,20
	asl_s DBL1H,DBL1H,r12
	sub DBL0H,DBL0H,r5
	neg r5,r12
	lsr r6,DBL1L,r5
	asl_s DBL1L,DBL1L,r12
	b.d __muldf3
	add_s DBL1H,DBL1H,r6

.Lret0:	xor_s DBL0H,DBL0H,DBL1H
	bclr DBL1H,DBL0H,31
	xor_s DBL0H,DBL0H,DBL1H
	j_s.d [blink]
	mov_s DBL0L,0

	.balign 4
.Linf_nan:
	bclr r12,DBL1H,31
	xor_s DBL1H,DBL1H,DBL0H
	bclr_s DBL0H,DBL0H,31
	max r8,DBL0H,r12 ; either NaN -> NaN ; otherwise inf
	or.f 0,DBL0H,DBL0L
	mov_s DBL0L,0
	or.ne.f DBL1L,DBL1L,r12
	not_s DBL0H,DBL0L ; inf * 0 -> NaN
	mov.ne DBL0H,r8
	tst_s DBL1H,DBL1H
	j_s.d [blink]
	bset.mi DBL0H,DBL0H,31

/* We have checked for infinity / NaN input before, and transformed
   denormalized inputs into normalized inputs.  Thus, the worst case
   exponent overflows are:
       1 +     1 - 0x400 == 0xc02 : maximum underflow
   0x7fe + 0x7fe - 0x3ff == 0xbfd ; maximum overflow
   N.B. 0x7e and 0x7f are also values for overflow.

   If (r12 <= -54), we have an underflow to zero.  */
	.balign 4
.Linf_denorm:
	lsr r6,r12,28
	brlo.d r6,0xc,.Linf
	asr r6,r12,20
	add.f r10,r10,r6
	brgt.d r10,0,.Lshift_frac
	mov_s r12,0
	beq.d .Lround_frac
	add r10,r10,32
.Lshift32_frac:
	tst r4,r4
	mov r4,acc2
	bset.ne r4,r4,1
	mululw 0,r7,1
	brge.d r10,1,.Lshift_frac
	mov r7,0
	breq.d r10,0,.Lround_frac
	add r10,r10,32
	brgt r10,21,.Lshift32_frac
	b_s .Lret0

.Lround_frac:
	add.f 0,r4,r4
	btst.eq acc2,0
	mov_s DBL0L,acc2
	mov_s DBL0H,r7
	adc.eq.f DBL0L,DBL0L,0
	j_s.d [blink]
	adc.eq DBL0H,DBL0H,0

.Linf:	mov_s DBL0L,0
	xor.f DBL1H,DBL1H,DBL0H
	mov_s DBL0H,r9
	j_s.d [blink]
	bset.mi DBL0H,DBL0H,31
	ENDFUNC(__muldf3)

	.balign 4
.L7ff00000:
	.long 0x7ff00000
