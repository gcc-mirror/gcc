/* Copyright (C) 2008-2023 Free Software Foundation, Inc.
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
	.global __mulsf3
	FUNC(__mulsf3)
	.balign 4
__mulsf3:
	push_s blink
	push_s r1
	bl.d __mulsf3_c
	push_s r0
	ld_s r1,[sp,4]
	st_s r0,[sp,4]
	bl.d __mulsf3_asm
	pop_s r0
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
	ENDFUNC(__mulsf3)
#define __mulsf3 __mulsf3_asm
#endif /* DEBUG */

	.balign	4
	.global	__mulsf3
	FUNC(__mulsf3)
__mulsf3:
	ld.as	r9,[pcl,80]; [pcl,((.L7f800000-.+2)/4)]
	bmsk	r4,r1,22
	bset	r2,r0,23
	asl_s	r2,r2,8
	bset	r3,r4,23
	mulu64	r2,r3
	and	r11,r0,r9
	breq.d	r11,0,.Ldenorm_dbl0
	and	r12,r1,r9
	breq.d	r12,0,.Ldenorm_dbl1
	xor_s	r0,r0,r1
	breq.d	r11,r9,.Linf_nan_dbl0
	ld.as	r4,[pcl,70]; [pcl,((.L7fffffff-.+2)/4)]
	breq.d	r12,r9,.Linf_nan_dbl1
.Lpast_denorm:
	asl.f	0,mhi,8
	mov	r6,mhi
	mov	r7,mlo
	add.pl	r6,r6,r6
	bclr.pl	r6,r6,23
	add.pl.f r7,r7,r7
	add.cs	r6,r6,1
	lsr.f	0,r6,1
	add_s	r12,r12,r11
	adc.f	0,r7,r4
	add_s	r12,r12, \
		-0x3f800000
	adc.f	r8,r6,r12
	tst.pl	r8,r9
	bic	r0,r0,r4
	min	r3,r8,r9
	jpnz.d	[blink]
	add.pnz	r0,r0,r3
; infinity or denormal number
	add.ne.f r3,r3,r3
	asr_s	r3,r3,23+1
	bset	r6,r6,23
	bpnz.d	.Linfinity
	sub_s	r3,r3,1
	neg_s	r2,r3
	brhi.d	r2,24,.Lret_r0 ; right shift shift > 24 -> return +-0
	lsr	r2,r6,r2
	asl	r9,r6,r3
	lsr.f	0,r2,1
	tst	r7,r7
	add_s	r0,r0,r2
	bset.ne	r9,r9,0
	adc.f	0,r9,r4
	j_s.d	[blink]
	add.cs	r0,r0,1
.Linfinity:
	j_s.d	[blink]
	add_s	r0,r0,r9

.Lret_r0: j_s [blink]

	.balign	4
.Ldenorm_dbl0:
	bclr_s	r2,r2,31
	norm.f	r4,r2
	add_s	r2,r2,r2
	asl	r2,r2,r4
	mulu64	r2,r3
	breq.d	r12,r9,.Ldenorm_dbl0_inf_nan_dbl1
	asl	r4,r4,23
	sub.ne.f r12,r12,r4
	ld.as	r4,[pcl,29]; [pcl,((.L7fffffff-.+2)/4)]
	bhi.d	.Lpast_denorm
	xor_s	r0,r0,r1
	bmsk	r1,r0,30
	j_s.d	[blink]
	bic_s	r0,r0,r1

	.balign	4
.Ldenorm_dbl0_inf_nan_dbl1:
	bmsk.f	0,r0,30
	beq_s	.Lretnan
	xor_s	r0,r0,r1
.Linf_nan_dbl1:
	xor_s	r1,r1,r0
.Linf_nan_dbl0:
	bclr_s	r1,r1,31
	cmp_s	r1,r9
	jls.d	[blink]
	xor_s	r0,r0,r1
; r1 NaN -> result NaN
.Lretnan:
	j_s.d [blink]
	mov r0,-1

	.balign	4
.Ldenorm_dbl1:
	breq.d	r11,r9,.Linf_nan_dbl0_2
	norm.f	r3,r4
	sub_s	r3,r3,7
	asl	r4,r4,r3
	mulu64	r2,r4
	sub_s	r3,r3,1
	asl_s	r3,r3,23
	sub.ne.f r11,r11,r3
	ld.as	r4,[pcl,11]; [pcl,((.L7fffffff-.+2)/4)]
	bhi.d	.Lpast_denorm
	bmsk	r8,r0,30
	j_s.d	[blink]
	bic	r0,r0,r8

	.balign	4
.Linf_nan_dbl0_2:
	bclr_s	r1,r1,31
	xor_s	r0,r0,r1
	sub.eq	r1,r1,1 ; inf/nan * 0 -> nan
	bic.f	0,r9,r1
	j_s.d	[blink]
	or.eq	r0,r0,r1 ; r1 nan -> result nan

	.balign	4
.L7f800000:
	.long	0x7f800000
.L7fffffff:
	.long	0x7fffffff
	ENDFUNC(__mulsf3)
