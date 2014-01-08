/* Copyright (C) 2004-2014 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


/* libgcc routines for the STMicroelectronics ST40-300 CPU.
   Contributed by J"orn Rennecke joern.rennecke@st.com.  */

#include "lib1funcs.h"

#if !__SHMEDIA__
#ifdef L_div_table
#if defined (__SH3__) || defined (__SH3E__) || defined (__SH4__) || defined (__SH4_SINGLE__) || defined (__SH4_SINGLE_ONLY__) || defined (__SH4_NOFPU__)
/* This code used shld, thus is not suitable for SH1 / SH2.  */

/* Signed / unsigned division without use of FPU, optimized for SH4-300.
   Uses a lookup table for divisors in the range -128 .. +127, and
   div1 with case distinction for larger divisors in three more ranges.
   The code is lumped together with the table to allow the use of mova.  */
#ifdef __LITTLE_ENDIAN__
#define L_LSB 0
#define L_LSWMSB 1
#define L_MSWLSB 2
#else
#define L_LSB 3
#define L_LSWMSB 2
#define L_MSWLSB 1
#endif

	.global	GLOBAL(udivsi3_i4i)
	.global	GLOBAL(sdivsi3_i4i)
	FUNC(GLOBAL(udivsi3_i4i))
	FUNC(GLOBAL(sdivsi3_i4i))

	.balign 4
LOCAL(div_ge8m): ! 10 cycles up to here
	rotcr r1 ! signed shift must use original sign from r4
	div0s r5,r4
	mov #24,r7
	shld r7,r6
	shad r0,r1
	rotcl r6
	div1 r5,r1
	swap.w r5,r0 ! detect -0x80000000 : 0x800000
	rotcl r6
	swap.w r4,r7
	div1 r5,r1
	swap.b r7,r7
	rotcl r6
	or r7,r0
	div1 r5,r1
	swap.w r0,r7
	rotcl r6
	or r7,r0
	div1 r5,r1
	add #-0x80,r0
	rotcl r6
	extu.w r0,r0
	div1 r5,r1
	neg r0,r0
	rotcl r6
	swap.w r0,r0
	div1 r5,r1
	mov.l @r15+,r7
	and r6,r0
	rotcl r6
	div1 r5,r1
	shll2 r0
	rotcl r6
	exts.b r0,r0
	div1 r5,r1
	swap.w r0,r0
	exts.w r0,r1
	exts.b r6,r0
	mov.l @r15+,r6
	rotcl r0
	rts
	sub r1,r0
	! 31 cycles up to here

	.balign 4
LOCAL(udiv_ge64k): ! 3 cycles up to here
	mov r4,r0
	shlr8 r0
	div0u
	cmp/hi r0,r5
	bt LOCAL(udiv_r8)
	mov.l r5,@-r15
	shll8 r5
	! 7 cycles up to here
	.rept 8
	div1 r5,r0
	.endr
	extu.b r4,r1 ! 15 cycles up to here
	extu.b r0,r6
	xor r1,r0
	xor r6,r0
	swap.b r6,r6
	.rept 8
	div1 r5,r0
	.endr ! 25 cycles up to here
	extu.b r0,r0
	mov.l @r15+,r5
	or r6,r0
	mov.l @r15+,r6
	rts
	rotcl r0 ! 28 cycles up to here

	.balign 4
LOCAL(udiv_r8): ! 6 cycles up to here
	mov.l r4,@-r15
	shll16 r4
	shll8 r4
	!
	shll r4
	mov r0,r1
	div1 r5,r1
	mov r4,r0
	rotcl r0
	mov.l @r15+,r4
	div1 r5,r1
	! 12 cycles up to here
	.rept 6
	rotcl r0; div1 r5,r1
	.endr
	mov.l @r15+,r6 ! 24 cycles up to here
	rts
	rotcl r0

	.balign 4
LOCAL(div_ge32k): ! 6 cycles up to here
	mov.l r7,@-r15
	swap.w r5,r6
	exts.b r6,r7
	exts.w r6,r6
	cmp/eq r6,r7
	extu.b r1,r6
	bf/s LOCAL(div_ge8m)
	cmp/hi r1,r4 ! copy sign bit of r4 into T
	rotcr r1 ! signed shift must use original sign from r4
	div0s r5,r4
	shad r0,r1
	shll8 r5
	div1 r5,r1
	mov r5,r7 ! detect r4 == 0x80000000 && r5 == 0x8000(00)
	div1 r5,r1
	shlr8 r7
	div1 r5,r1
	swap.w r4,r0
	div1 r5,r1
	swap.b r0,r0
	div1 r5,r1
	or r0,r7
	div1 r5,r1
	add #-80,r7
	div1 r5,r1
	swap.w r7,r0
	div1 r5,r1
	or r0,r7
	extu.b r1,r0
	xor r6,r1
	xor r0,r1
	exts.b r0,r0
	div1 r5,r1
	extu.w r7,r7
	div1 r5,r1
	neg r7,r7 ! upper 16 bit of r7 == 0 if r4 == 0x80000000 && r5 == 0x8000
	div1 r5,r1
	and r0,r7
	div1 r5,r1
	swap.w r7,r7 ! 26 cycles up to here.
	div1 r5,r1
	shll8 r0
	div1 r5,r1
	exts.w r7,r7
	div1 r5,r1
	add r0,r0
	div1 r5,r1
	sub r7,r0
	extu.b r1,r1
	mov.l @r15+,r7
	rotcl r1
	mov.l @r15+,r6
	add r1,r0
	mov #-8,r1
	rts
	shad r1,r5 ! 34 cycles up to here

	.balign 4
GLOBAL(udivsi3_i4i):
	mov.l r6,@-r15
	extu.w r5,r6
	cmp/eq r5,r6
	mov #0x7f,r0
	bf LOCAL(udiv_ge64k)
	cmp/hi r0,r5
	bf LOCAL(udiv_le128)
	mov r4,r1
	shlr8 r1
	div0u
	shlr r1
	shll16 r6
	div1 r6,r1
	extu.b r4,r0 ! 7 cycles up to here
	.rept 8
	div1 r6,r1
	.endr     ! 15 cycles up to here
	xor r1,r0 ! xor dividend with result lsb
	.rept 6
	div1 r6,r1
	.endr
	mov.l r7,@-r15 ! 21 cycles up to here
	div1 r6,r1
	extu.b r0,r7
	div1 r6,r1
	shll8 r7
	extu.w r1,r0
	xor r7,r1 ! replace lsb of result with lsb of dividend
	div1 r6,r1
	mov #0,r7
	div1 r6,r1
	!
	div1 r6,r1
	bra LOCAL(div_end)
	div1 r6,r1 ! 28 cycles up to here

	/* This is link-compatible with a GLOBAL(sdivsi3) call,
	   but we effectively clobber only r1, macl and mach  */
        /* Because negative quotients are calculated as one's complements,
	   -0x80000000 divided by the smallest positive number of a number
	   range (0x80, 0x8000, 0x800000) causes saturation in the one's
           complement representation, and we have to suppress the
	   one's -> two's complement adjustment.  Since positive numbers
	   don't get such an adjustment, it's OK to also compute one's -> two's
	   complement adjustment suppression for a dividend of 0.  */
	.balign 4
GLOBAL(sdivsi3_i4i):
	mov.l r6,@-r15
	exts.b r5,r6
	cmp/eq r5,r6
	mov #-1,r1
	bt/s LOCAL(div_le128)
	cmp/pz r4
	addc r4,r1
	exts.w r5,r6
	cmp/eq r5,r6
	mov #-7,r0
	bf/s LOCAL(div_ge32k)
	cmp/hi r1,r4 ! copy sign bit of r4 into T
	rotcr r1
	shll16 r6  ! 7 cycles up to here
	shad r0,r1
	div0s r5,r4
	div1 r6,r1
	mov.l r7,@-r15
	div1 r6,r1
	mov r4,r0 ! re-compute adjusted dividend
	div1 r6,r1
	mov #-31,r7
	div1 r6,r1
	shad r7,r0
	div1 r6,r1
	add r4,r0 ! adjusted dividend
	div1 r6,r1
	mov.l r8,@-r15
	div1 r6,r1
	swap.w r4,r8 ! detect special case r4 = 0x80000000, r5 = 0x80
	div1 r6,r1
	swap.b r8,r8
	xor r1,r0 ! xor dividend with result lsb
	div1 r6,r1
	div1 r6,r1
	or r5,r8
	div1 r6,r1
	add #-0x80,r8 ! r8 is 0 iff there is a match
	div1 r6,r1
	swap.w r8,r7 ! or upper 16 bits...
	div1 r6,r1
	or r7,r8 !...into lower 16 bits
	div1 r6,r1
	extu.w r8,r8
	div1 r6,r1
	extu.b r0,r7
	div1 r6,r1
	shll8 r7
	exts.w r1,r0
	xor r7,r1 ! replace lsb of result with lsb of dividend
	div1 r6,r1
	neg r8,r8 ! upper 16 bits of r8 are now 0xffff iff we want end adjm.
	div1 r6,r1
	and r0,r8
	div1 r6,r1
	swap.w r8,r7
	div1 r6,r1
	mov.l @r15+,r8 ! 58 insns, 29 cycles up to here
LOCAL(div_end):
	div1 r6,r1
	shll8 r0
	div1 r6,r1
	exts.w r7,r7
	div1 r6,r1
	add r0,r0
	div1 r6,r1
	sub r7,r0
	extu.b r1,r1
	mov.l @r15+,r7
	rotcl r1
	mov.l @r15+,r6
	rts
	add r1,r0

	.balign 4
LOCAL(udiv_le128): ! 4 cycles up to here (or 7 for mispredict)
	mova LOCAL(div_table_inv),r0
	shll2 r6
	mov.l @(r0,r6),r1
	mova LOCAL(div_table_clz),r0
	lds r4,mach
	!
	!
	!
	tst r1,r1
	!
	bt 0f
	dmulu.l r1,r4
0:	mov.b @(r0,r5),r1
	clrt
	!
	!
	sts mach,r0
	addc r4,r0
	rotcr r0
	mov.l @r15+,r6
	rts
	shld r1,r0

	.balign 4
LOCAL(div_le128): ! 3 cycles up to here (or 6 for mispredict)
	mova LOCAL(div_table_inv),r0
	shll2 r6
	mov.l @(r0,r6),r1
	mova LOCAL(div_table_clz),r0
	neg r4,r6
	bf 0f
	mov r4,r6
0:	lds r6,mach
	tst r1,r1
	bt 0f
	dmulu.l r1,r6
0:	div0s r4,r5
	mov.b @(r0,r5),r1
	bt/s LOCAL(le128_neg)
	clrt
	!
	sts mach,r0
	addc r6,r0
	rotcr r0
	mov.l @r15+,r6
	rts
	shld r1,r0

/* Could trap divide by zero for the cost of one cycle more mispredict penalty:
...
	dmulu.l r1,r6
0:	div0s r4,r5
	bt/s LOCAL(le128_neg)
	tst r5,r5
	bt LOCAL(div_by_zero)
	mov.b @(r0,r5),r1
	sts mach,r0
	addc r6,r0
...
LOCAL(div_by_zero):
	trapa #
	.balign 4
LOCAL(le128_neg):
	bt LOCAL(div_by_zero)
	mov.b @(r0,r5),r1
	sts mach,r0
	addc r6,r0
...  */

	.balign 4
LOCAL(le128_neg):
	sts mach,r0
	addc r6,r0
	rotcr r0
	mov.l @r15+,r6
	shad r1,r0
	rts
	neg r0,r0
	ENDFUNC(GLOBAL(udivsi3_i4i))
	ENDFUNC(GLOBAL(sdivsi3_i4i))

/* This table has been generated by divtab-sh4.c.  */
	.balign 4
	.byte	-7
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-2
	.byte	-2
	.byte	-2
	.byte	-2
	.byte	-1
	.byte	-1
	.byte	0
LOCAL(div_table_clz):
	.byte	0
	.byte	0
	.byte	-1
	.byte	-1
	.byte	-2
	.byte	-2
	.byte	-2
	.byte	-2
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
/* 1/-128 .. 1/127, normalized.  There is an implicit leading 1 in bit 32,
   or in bit 33 for powers of two.  */
	.balign 4
	.long   0x0
	.long	0x2040811
	.long	0x4104105
	.long	0x624DD30
	.long	0x8421085
	.long	0xA6810A7
	.long	0xC9714FC
	.long	0xECF56BF
	.long	0x11111112
	.long	0x135C8114
	.long	0x15B1E5F8
	.long	0x18118119
	.long	0x1A7B9612
	.long	0x1CF06ADB
	.long	0x1F7047DD
	.long	0x21FB7813
	.long	0x24924925
	.long	0x27350B89
	.long	0x29E4129F
	.long	0x2C9FB4D9
	.long	0x2F684BDB
	.long	0x323E34A3
	.long	0x3521CFB3
	.long	0x38138139
	.long	0x3B13B13C
	.long	0x3E22CBCF
	.long	0x41414142
	.long	0x446F8657
	.long	0x47AE147B
	.long	0x4AFD6A06
	.long	0x4E5E0A73
	.long	0x51D07EAF
	.long	0x55555556
	.long	0x58ED2309
	.long	0x5C9882BA
	.long	0x60581606
	.long	0x642C8591
	.long	0x68168169
	.long	0x6C16C16D
	.long	0x702E05C1
	.long	0x745D1746
	.long	0x78A4C818
	.long	0x7D05F418
	.long	0x81818182
	.long	0x86186187
	.long	0x8ACB90F7
	.long	0x8F9C18FA
	.long	0x948B0FCE
	.long	0x9999999A
	.long	0x9EC8E952
	.long	0xA41A41A5
	.long	0xA98EF607
	.long	0xAF286BCB
	.long	0xB4E81B4F
	.long	0xBACF914D
	.long	0xC0E07039
	.long	0xC71C71C8
	.long	0xCD856891
	.long	0xD41D41D5
	.long	0xDAE6076C
	.long	0xE1E1E1E2
	.long	0xE9131AC0
	.long	0xF07C1F08
	.long	0xF81F81F9
	.long	0x0
	.long	0x4104105
	.long	0x8421085
	.long	0xC9714FC
	.long	0x11111112
	.long	0x15B1E5F8
	.long	0x1A7B9612
	.long	0x1F7047DD
	.long	0x24924925
	.long	0x29E4129F
	.long	0x2F684BDB
	.long	0x3521CFB3
	.long	0x3B13B13C
	.long	0x41414142
	.long	0x47AE147B
	.long	0x4E5E0A73
	.long	0x55555556
	.long	0x5C9882BA
	.long	0x642C8591
	.long	0x6C16C16D
	.long	0x745D1746
	.long	0x7D05F418
	.long	0x86186187
	.long	0x8F9C18FA
	.long	0x9999999A
	.long	0xA41A41A5
	.long	0xAF286BCB
	.long	0xBACF914D
	.long	0xC71C71C8
	.long	0xD41D41D5
	.long	0xE1E1E1E2
	.long	0xF07C1F08
	.long	0x0
	.long	0x8421085
	.long	0x11111112
	.long	0x1A7B9612
	.long	0x24924925
	.long	0x2F684BDB
	.long	0x3B13B13C
	.long	0x47AE147B
	.long	0x55555556
	.long	0x642C8591
	.long	0x745D1746
	.long	0x86186187
	.long	0x9999999A
	.long	0xAF286BCB
	.long	0xC71C71C8
	.long	0xE1E1E1E2
	.long	0x0
	.long	0x11111112
	.long	0x24924925
	.long	0x3B13B13C
	.long	0x55555556
	.long	0x745D1746
	.long	0x9999999A
	.long	0xC71C71C8
	.long	0x0
	.long	0x24924925
	.long	0x55555556
	.long	0x9999999A
	.long	0x0
	.long	0x55555556
	.long	0x0
	.long	0x0
LOCAL(div_table_inv):
	.long	0x0
	.long	0x0
	.long	0x0
	.long	0x55555556
	.long	0x0
	.long	0x9999999A
	.long	0x55555556
	.long	0x24924925
	.long	0x0
	.long	0xC71C71C8
	.long	0x9999999A
	.long	0x745D1746
	.long	0x55555556
	.long	0x3B13B13C
	.long	0x24924925
	.long	0x11111112
	.long	0x0
	.long	0xE1E1E1E2
	.long	0xC71C71C8
	.long	0xAF286BCB
	.long	0x9999999A
	.long	0x86186187
	.long	0x745D1746
	.long	0x642C8591
	.long	0x55555556
	.long	0x47AE147B
	.long	0x3B13B13C
	.long	0x2F684BDB
	.long	0x24924925
	.long	0x1A7B9612
	.long	0x11111112
	.long	0x8421085
	.long	0x0
	.long	0xF07C1F08
	.long	0xE1E1E1E2
	.long	0xD41D41D5
	.long	0xC71C71C8
	.long	0xBACF914D
	.long	0xAF286BCB
	.long	0xA41A41A5
	.long	0x9999999A
	.long	0x8F9C18FA
	.long	0x86186187
	.long	0x7D05F418
	.long	0x745D1746
	.long	0x6C16C16D
	.long	0x642C8591
	.long	0x5C9882BA
	.long	0x55555556
	.long	0x4E5E0A73
	.long	0x47AE147B
	.long	0x41414142
	.long	0x3B13B13C
	.long	0x3521CFB3
	.long	0x2F684BDB
	.long	0x29E4129F
	.long	0x24924925
	.long	0x1F7047DD
	.long	0x1A7B9612
	.long	0x15B1E5F8
	.long	0x11111112
	.long	0xC9714FC
	.long	0x8421085
	.long	0x4104105
	.long	0x0
	.long	0xF81F81F9
	.long	0xF07C1F08
	.long	0xE9131AC0
	.long	0xE1E1E1E2
	.long	0xDAE6076C
	.long	0xD41D41D5
	.long	0xCD856891
	.long	0xC71C71C8
	.long	0xC0E07039
	.long	0xBACF914D
	.long	0xB4E81B4F
	.long	0xAF286BCB
	.long	0xA98EF607
	.long	0xA41A41A5
	.long	0x9EC8E952
	.long	0x9999999A
	.long	0x948B0FCE
	.long	0x8F9C18FA
	.long	0x8ACB90F7
	.long	0x86186187
	.long	0x81818182
	.long	0x7D05F418
	.long	0x78A4C818
	.long	0x745D1746
	.long	0x702E05C1
	.long	0x6C16C16D
	.long	0x68168169
	.long	0x642C8591
	.long	0x60581606
	.long	0x5C9882BA
	.long	0x58ED2309
	.long	0x55555556
	.long	0x51D07EAF
	.long	0x4E5E0A73
	.long	0x4AFD6A06
	.long	0x47AE147B
	.long	0x446F8657
	.long	0x41414142
	.long	0x3E22CBCF
	.long	0x3B13B13C
	.long	0x38138139
	.long	0x3521CFB3
	.long	0x323E34A3
	.long	0x2F684BDB
	.long	0x2C9FB4D9
	.long	0x29E4129F
	.long	0x27350B89
	.long	0x24924925
	.long	0x21FB7813
	.long	0x1F7047DD
	.long	0x1CF06ADB
	.long	0x1A7B9612
	.long	0x18118119
	.long	0x15B1E5F8
	.long	0x135C8114
	.long	0x11111112
	.long	0xECF56BF
	.long	0xC9714FC
	.long	0xA6810A7
	.long	0x8421085
	.long	0x624DD30
	.long	0x4104105
	.long	0x2040811
	/* maximum error: 0.987342 scaled: 0.921875*/

#endif /* SH3 / SH4 */

#endif /* L_div_table */
#endif /* !__SHMEDIA__ */
