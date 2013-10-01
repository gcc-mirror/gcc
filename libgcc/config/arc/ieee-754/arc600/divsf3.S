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
        jeq_s [blink]
        and r12,r0,r1
        bic.f 0,0x7f800000,r12 ; both NaN -> OK
        jeq_s [blink]
        bl abort
	ENDFUNC(__divsf3)
#define __divsf3 __divsf3_asm
#endif /* DEBUG */

	.balign 4
__divdf3_support: /* This label makes debugger output saner.  */
	FUNC(__divsf3)
.Ldenorm_fp0:
	norm.f r12,r2 ; flag for 0/x -> 0 check
	bic.ne.f 0,0x60000000,r1 ; denorm/large number -> 0
	beq_s .Lret0_NaN
	tst r1,r9
	add_s r2,r2,r2
	sub_s r12,r12,8
	asl_s r2,r2,r12
	asl_l r12,r12,23
	bne.d .Lpast_denorm_fp0
	add r5,r5,r12
/* r0 is subnormal, r1 is subnormal or 0.  */

	.balign 4
.Ldenorm_fp1:
	norm.f r12,r3 ; flag for  x/0 -> Inf check
	bic.ne.f 0,0x60000000,r0 ; large number/denorm -> Inf
	beq_s .Linf
	add_s r3,r3,r3
	sub_s r12,r12,8
	asl_s r3,r3,r12
	asl_s r12,r12,23
	b.d .Lpast_denorm_fp1
	add r4,r4,r12

.Lret0_NaN:
	bclr.f 0,r1,31 ; 0/0 -> NaN
	bic r0,r10,r9
	j_s.d [blink]
	sub.eq r0,r0,1

	.balign 4
.Linf_nan_fp0:
	bic.f 0,r9,r1 ; fp1 Inf -> result NaN
	bic r1,r5,r9  ; fp1 sign
	sub.eq r1,r1,1
	j_s.d [blink]
	xor_s r0,r0,r1
.Linf_nan_fp1:
	bic r0,r4,r9 ; fp0 sign
	bmsk.f 0,r1,22 ; x/inf -> 0, x/nan -> nan
	xor.eq r1,r1,r9
	j_s.d [blink]
	xor_s r0,r0,r1

	.global __divsf3
	.balign 4
        .long 0x7f800000 ; exponent mask
__divsf3:
	ld r9,[pcl,-4]
	bmsk r2,r0,22
	xor r4,r0,r2
	bmsk r3,r1,22
	xor r5,r1,r3
	and r11,r0,r9
	breq.d r11,0,.Ldenorm_fp0
	xor r10,r4,r5
	breq r11,r9,.Linf_nan_fp0
	bset_s r2,r2,23
	and r11,r1,r9
	breq r11,0,.Ldenorm_fp1
	breq r11,r9,.Linf_nan_fp1
.Lpast_denorm_fp0:
	bset_s r3,r3,23
.Lpast_denorm_fp1:
	cmp r2,r3
	asl_s r2,r2,6+1
	asl_s r3,r3,7
	add.lo r2,r2,r2
	bclr r8,r9,30 ; exponent bias
	bclr.lo r8,r8,23 ; reduce exp by one if fraction is shifted
	sub r4,r4,r5
	add r4,r4,r8
	xor.f 0,r10,r4
	bmi .Linf_denorm
	and.f r12,r4,r9
	beq .Ldenorm
	sub_s r2,r2,r3 ; discard implicit 1
	rsub r3,r3,1 ; prime r3 for two-insn divide-step use
.Ldiv_23bit:
	.rep 6
	add1.f r2,r3,r2
	sub.cc r2,r2,r3
	.endr
	breq r12,r9,.Linf
	bmsk r0,r2,6
	xor_s r2,r2,r0
.Ldiv_17bit:
	.rep 7
	add1.f r2,r3,r2
	sub.cc r2,r2,r3
	.endr
	asl_s r0,r0,7
	bmsk r1,r2,6
	xor_s r2,r2,r1
	or_s r0,r0,r1
.Ldiv_10bit:
	.rep 7
	add1.f r2,r3,r2
	sub.cc r2,r2,r3
	.endr
	asl_s r0,r0,7
	bmsk r1,r2,6
	xor_s r2,r2,r1
	or_s r0,r0,r1
.Ldiv_3bit:
	.rep 3
	add1.f r2,r3,r2
	sub.cc r2,r2,r3
	.endr
	asl_s r0,r0,3
.Ldiv_0bit:
	add1.f r1,r3,r2
	sub.cc r1,r1,r3
	bmsk_s r2,r2,2
	tst r1,-0x7e ; 0xffffff82, test for rest or odd
	bmsk_s r1,r1,0
	add_s r0,r0,r2 ; assemble fraction
	add_s r0,r0,r4 ; add in sign & exponent
	j_s.d [blink]
	add.ne r0,r0,r1 ; round to nearest / even

	.balign 4
.Linf:
	j_s.d [blink]
	or r0,r10,r9

.Lret_r4:
	j_s.d [blink]
	mov_s r0,r4
	.balign 4
.Linf_denorm:
	add.f r12,r4,r4
	asr_l r12,r12,24
	bpl .Linf
	max r12,r12,-24
.Ldenorm:
	rsub r3,r3,1
	add r1,pcl,68; .Ldenorm_tab-.
	ldw.as r12,[r1,r12]
	mov_s r0,0
	lsr_s r2,r2
	sub_s r1,r1,r12
	j_s.d [r1]
	bic r4,r10,r9
	.short .Ldenorm_tab-.Lret_r4
	.short .Ldenorm_tab-.Ldiv_0bit
	.short .Ldenorm_tab-.Ldiv_3bit-2*8
	.short .Ldenorm_tab-.Ldiv_3bit-1*8
	.short .Ldenorm_tab-.Ldiv_3bit
	.short .Ldenorm_tab-.Ldiv_10bit-6*8
	.short .Ldenorm_tab-.Ldiv_10bit-5*8
	.short .Ldenorm_tab-.Ldiv_10bit-3*8
	.short .Ldenorm_tab-.Ldiv_10bit-3*8
	.short .Ldenorm_tab-.Ldiv_10bit-2*8
	.short .Ldenorm_tab-.Ldiv_10bit-1*8
	.short .Ldenorm_tab-.Ldiv_10bit
	.short .Ldenorm_tab-.Ldiv_17bit-6*8
	.short .Ldenorm_tab-.Ldiv_17bit-5*8
	.short .Ldenorm_tab-.Ldiv_17bit-4*8
	.short .Ldenorm_tab-.Ldiv_17bit-3*8
	.short .Ldenorm_tab-.Ldiv_17bit-2*8
	.short .Ldenorm_tab-.Ldiv_17bit-1*8
	.short .Ldenorm_tab-.Ldiv_17bit
	.short .Ldenorm_tab-.Ldiv_23bit-5*8
	.short .Ldenorm_tab-.Ldiv_23bit-4*8
	.short .Ldenorm_tab-.Ldiv_23bit-3*8
	.short .Ldenorm_tab-.Ldiv_23bit-2*8
	.short .Ldenorm_tab-.Ldiv_23bit-1*8
.Ldenorm_tab:
	.short .Ldenorm_tab-.Ldiv_23bit
	ENDFUNC(__divsf3)
