/* mculib libgcc routines of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2016 Free Software Foundation, Inc.
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


/* ------------------------------------------- */
/* FPBIT floating point operations for libgcc  */
/* ------------------------------------------- */

#ifdef L_addsub_sf

	.text
	.align	2
	.global	__subsf3
	.type	__subsf3, @function
__subsf3:
	push    $lp
	pushm   $r6, $r9

	move    $r2, #0x80000000
	xor     $r1, $r1, $r2

	j       .Lsfpadd

	.global	__addsf3
	.type	__addsf3, @function
__addsf3:
	push    $lp
	pushm   $r6, $r9
.Lsfpadd:
	srli    $r5, $r0, #23
	andi    $r5, $r5, #0xff
	srli    $r7, $r1, #23
	andi    $r7, $r7, #0xff
	move    $r3, #0x80000000
	slli    $r4, $r0, #8
	or      $r4, $r4, $r3
	slli    $r6, $r1, #8
	or      $r6, $r6, $r3

	addi    $r9, $r5, #-1
	slti    $r15, $r9, #0xfe
	beqzs8  .LEspecA

.LElab1:
	addi    $r9, $r7, #-1
	slti    $r15, $r9, #0xfe
	beqzs8  .LEspecB

.LElab2:
	sub     $r8, $r5, $r7
	sltsi   $r15, $r8, #0
	bnezs8  .Li1
	sltsi   $r15, $r8, #0x20
	bnezs8  .Li2
	move    $r6, #2
	j       .Le1
.Li2:
	move    $r2, $r6
	srl     $r6, $r6, $r8
	sll     $r9, $r6, $r8
	beq     $r9, $r2, .Le1
	ori     $r6, $r6, #2
	j       .Le1
.Li1:
	move    $r5, $r7
	subri   $r8, $r8, #0
	sltsi   $r15, $r8, #0x20
	bnezs8  .Li4
	move    $r4, #2
	j       .Le1
.Li4:
	move    $r2, $r4
	srl     $r4, $r4, $r8
	sll     $r9, $r4, $r8
	beq     $r9, $r2, .Le1
	ori     $r4, $r4, #2

.Le1:
	and     $r8, $r0, $r3
	xor     $r9, $r8, $r1
	sltsi   $r15, $r9, #0
	bnezs8  .LEsub1

	#ADD($r4, $r6)
	add     $r4, $r4, $r6
	slt     $r15, $r4, $r6
	beqzs8  .LEres
	andi    $r9, $r4, #1
	beqz    $r9, .Li7
	ori     $r4, $r4, #2
.Li7:
	srli    $r4, $r4, #1
	addi    $r5, $r5, #1
	subri   $r15, $r5, #0xff
	bnezs8  .LEres
	move    $r4, #0
	j       .LEres

.LEsub1:
	#SUB($r4, $r6)
	move    $r15, $r4
	sub     $r4, $r4, $r6
	slt     $r15, $r15, $r4
	beqzs8  .Li9
	subri   $r4, $r4, #0
	xor     $r8, $r8, $r3
	j       .Le9
.Li9:
	beqz    $r4, .LEzer
.Le9:
#ifdef __NDS32_PERF_EXT__
	clz	$r2, $r4
#else
	pushm	$r0, $r1
	pushm	$r3, $r5
	move	$r0, $r4
	bal	__clzsi2
	move	$r2, $r0
	popm	$r3, $r5
	popm	$r0, $r1
#endif
	sub     $r5, $r5, $r2
	sll     $r4, $r4, $r2

.LEres:
	blez    $r5, .LEund

.LElab12:
	#ADD($r4, $0x80)
	move    $r15, #0x80
	add     $r4, $r4, $r15
	slt     $r15, $r4, $r15

	#ADDC($r5, $0x0)
	add     $r5, $r5, $r15
	srli    $r9, $r4, #8
	andi    $r9, $r9, #1
	sub     $r4, $r4, $r9
	slli    $r4, $r4, #1
	srli    $r4, $r4, #9
	slli    $r9, $r5, #23
	or      $r4, $r4, $r9
	or      $r0, $r4, $r8

.LE999:
	popm    $r6, $r9
	pop     $lp
	ret5    $lp

.LEund:
	subri   $r2, $r5, #1
	slti    $r15, $r2, #0x20
	beqzs8  .LEzer
	move    $r9, #0x80000000
	or      $r4, $r4, $r9
	subri   $r9, $r2, #0x20
	sll     $r5, $r4, $r9
	srl     $r4, $r4, $r2
	beqz    $r5, .Li10
	ori     $r4, $r4, #1
.Li10:
	move    $r5, #0
	addi    $r9, $r4, #0x80
	sltsi   $r15, $r9, #0
	beqzs8  .LElab12
	move    $r5, #1
	j       .LElab12

.LEspecA:
	bnez    $r5, .Li12
	add     $r4, $r4, $r4
	beqz    $r4, .Li13
#ifdef __NDS32_PERF_EXT__
	clz	$r8, $r4
#else
	pushm	$r0, $r5
	move	$r0, $r4
	bal	__clzsi2
	move	$r8, $r0
	popm	$r0, $r5
#endif
	sub     $r5, $r5, $r8
	sll     $r4, $r4, $r8
	j       .LElab1
.Li13:
	subri   $r15, $r7, #0xff
	beqzs8  .LEspecB
	move    $r9, #0x80000000
	bne     $r1, $r9, .LEretB
.Li12:
	add     $r9, $r4, $r4
	bnez    $r9, .LEnan
	subri   $r15, $r7, #0xff
	bnezs8  .LEretA
	xor     $r9, $r0, $r1
	sltsi   $r15, $r9, #0
	bnezs8  .LEnan
	j       .LEretB

.LEspecB:
	bnez    $r7, .Li15
	add     $r6, $r6, $r6
	beqz    $r6, .LEretA
#ifdef __NDS32_PERF_EXT__
	clz	$r8, $r6
#else
	pushm	$r0, $r5
	move	$r0, $r6
	bal	__clzsi2
	move	$r8, $r0
	popm	$r0, $r5
#endif
	sub     $r7, $r7, $r8
	sll     $r6, $r6, $r8
	j       .LElab2
.Li15:
	add     $r9, $r6, $r6
	bnez    $r9, .LEnan

.LEretB:
	move    $r0, $r1
	j       .LE999

.LEretA:
	j       .LE999

.LEzer:
	move    $r0, #0
	j       .LE999

.LEnan:
	move    $r0, #0xffc00000
	j       .LE999
	.size	__subsf3, .-__subsf3
	.size	__addsf3, .-__addsf3
#endif /* L_addsub_sf */



#ifdef L_sf_to_si

	.text
	.align	2
	.global	__fixsfsi
	.type	__fixsfsi, @function
__fixsfsi:
	push    $lp

	slli    $r1, $r0, #8
	move    $r3, #0x80000000
	or      $r1, $r1, $r3
	srli    $r3, $r0, #23
	andi    $r3, $r3, #0xff
	subri   $r2, $r3, #0x9e
	blez    $r2, .LJspec
	sltsi   $r15, $r2, #0x20
	bnezs8  .Li42
	move    $r0, #0
	j       .LJ999
.Li42:
	srl     $r1, $r1, $r2
	sltsi   $r15, $r0, #0
	beqzs8  .Li43
	subri   $r1, $r1, #0
.Li43:
	move    $r0, $r1

.LJ999:
	pop     $lp
	ret5    $lp

.LJspec:
	move    $r3, #0x7f800000
	slt     $r15, $r3, $r0
	beqzs8  .Li44
	move    $r0, #0x80000000
	j       .LJ999
.Li44:
	move    $r0, #0x7fffffff
	j       .LJ999
	.size	__fixsfsi, .-__fixsfsi
#endif /* L_sf_to_si */



#ifdef L_divsi3

	.text
	.align	2
	.globl	__divsi3
	.type	__divsi3, @function
__divsi3:
	! ---------------------------------------------------------------------
	! neg = 0;
	! if (a < 0)
	! {   a = -a;
	!     neg = !neg;
	! }
	! ---------------------------------------------------------------------
	sltsi	$r5, $r0, 0			! $r5  <- neg = (a < 0) ? 1 : 0
	subri	$r4, $r0, 0			! $r4  <- a = -a
	cmovn	$r0, $r4, $r5			! $r0  <- a = neg ? -a : a
.L2:
	! ---------------------------------------------------------------------
	! if (b < 0)
	! ---------------------------------------------------------------------
	bgez	$r1, .L3			! if b >= 0, skip
	! ---------------------------------------------------------------------
	! {   b=-b;
	!     neg=!neg;
	! }
	! ---------------------------------------------------------------------
	subri	$r1, $r1, 0			! $r1  <- b = -b
	subri	$r5, $r5, 1			! $r5  <- neg = !neg
.L3:
	! ---------------------------------------------------------------------
	!!res = udivmodsi4 (a, b, 1);
	! res = 0;
	! if (den != 0)
	! ---------------------------------------------------------------------
	movi	$r2, 0				! $r2  <- res = 0
	beqz	$r1, .L1			! if den == 0, skip
	! ---------------------------------------------------------------------
	! bit = 1;
	! ---------------------------------------------------------------------
	movi	$r4, 1				! $r4  <- bit = 1
#ifndef __OPTIMIZE_SIZE__
.L6:
#endif
	! ---------------------------------------------------------------------
	! while (den < num && bit && !(den & (1L << 31)))
	! ---------------------------------------------------------------------
	slt	$ta, $r1, $r0			! $ta  <- den < num ?
	beqz	$ta, .L5			! if no, skip
	! ---------------------------------------------------------------------
	! {   den << = 1;
	!     bit << = 1;
	! }
	! ---------------------------------------------------------------------
#if defined (__OPTIMIZE_SIZE__) && !defined (__NDS32_ISA_V3M__)
	clz	$r3, $r1			! $r3  <- leading zero count for den
	clz	$ta, $r0			! $ta  <- leading zero count for num
	sub	$r3, $r3, $ta			! $r3  <- number of bits to shift
	sll	$r1, $r1, $r3			! $r1  <- den
	sll	$r4, $r4, $r3			! $r2  <- bit
#else
	slli	$r1, $r1, 1			! $r1  <- den << = 1
	slli	$r4, $r4, 1			! $r4  <- bit << = 1
	b	.L6				! continue loop
#endif
.L5:
	! ---------------------------------------------------------------------
	! while (bit)
	! {   if (num >= den)
	! ---------------------------------------------------------------------
	slt	$ta, $r0, $r1			! $ta  <- num < den ?
	bnez	$ta, .L9			! if yes, skip
	! ---------------------------------------------------------------------
	!     {   num -= den;
	!         res |= bit;
	!     }
	! ---------------------------------------------------------------------
	sub	$r0, $r0, $r1			! $r0  <- num -= den
	or	$r2, $r2, $r4			! $r2  <- res |= bit
.L9:
	! ---------------------------------------------------------------------
	!     bit >> = 1;
	!     den >> = 1;
	! }
	!!if (modwanted)
	!!    return num;
	!!return res;
	! ---------------------------------------------------------------------
	srli	$r4, $r4, 1			! $r4  <- bit >> = 1
	srli	$r1, $r1, 1			! $r1  <- den >> = 1
	bnez	$r4, .L5			! if bit != 0, continue loop
.L1:
	! ---------------------------------------------------------------------
	! if (neg)
	!     res = -res;
	! return res;
	! ---------------------------------------------------------------------
	subri	$r0, $r2, 0			! $r0  <- -res
	cmovz	$r0, $r2, $r5			! $r0  <- neg ? -res : res
	! ---------------------------------------------------------------------
	ret
	.size	__divsi3, .-__divsi3
#endif /* L_divsi3 */



#ifdef L_divdi3

	!--------------------------------------
	#ifdef __big_endian__
		#define  V1H  $r0
		#define  V1L  $r1
		#define  V2H  $r2
		#define  V2L  $r3
	#else
		#define  V1H  $r1
		#define  V1L  $r0
		#define  V2H  $r3
		#define  V2L  $r2
	#endif
	!--------------------------------------
	.text
	.align	2
	.globl	__divdi3
	.type	__divdi3, @function
__divdi3:
	! prologue
#ifdef __NDS32_ISA_V3M__
	push25	$r10, 0
#else
	smw.adm	$r6, [$sp], $r10, 2
#endif
	! end of prologue
	move	$r8, V1L
	move	$r9, V1H
	move	$r6, V2L
	move	$r7, V2H
	movi	$r10, 0
	bgez	V1H, .L80
	bal	__negdi2
	move	$r8, V1L
	move	$r9, V1H
	movi	$r10, -1
.L80:
	bgez	$r7, .L81
	move	V1L, $r6
	move	V1H, $r7
	bal	__negdi2
	move	$r6, V1L
	move	$r7, V1H
	nor	$r10, $r10, $r10
.L81:
	move	V2L, $r6
	move	V2H, $r7
	move	V1L, $r8
	move	V1H, $r9
	movi	$r4, 0
	bal	__udivmoddi4
	beqz	$r10, .L82
	bal	__negdi2
.L82:
	! epilogue
#ifdef __NDS32_ISA_V3M__
	pop25	$r10, 0
#else
	lmw.bim	$r6, [$sp], $r10, 2
	ret
#endif
	.size	__divdi3, .-__divdi3
#endif /* L_divdi3 */



#ifdef L_modsi3

	.text
	.align	2
	.globl	__modsi3
	.type	__modsi3, @function
__modsi3:
	! ---------------------------------------------------------------------
	! neg=0;
	! if (a<0)
	! {   a=-a;
	!     neg=1;
	! }
	! ---------------------------------------------------------------------
	sltsi	$r5, $r0, 0			! $r5  <- neg < 0 ? 1 : 0
	subri	$r4, $r0, 0			! $r4  <- -a
	cmovn	$r0, $r4, $r5			! $r0  <- |a|
	! ---------------------------------------------------------------------
	! if (b < 0)
#ifndef __NDS32_PERF_EXT__
	! ---------------------------------------------------------------------
	bgez	$r1, .L3			! if b >= 0, skip
	! ---------------------------------------------------------------------
	!     b = -b;
	! ---------------------------------------------------------------------
	subri	$r1, $r1, 0			! $r1  <- |b|
.L3:
	! ---------------------------------------------------------------------
	!!res = udivmodsi4 (a, b, 1);
	! if (den != 0)
	! ---------------------------------------------------------------------
#else /* __NDS32_PERF_EXT__ */
	!     b = -b;
	!!res = udivmodsi4 (a, b, 1);
	! if (den != 0)
	! ---------------------------------------------------------------------
	abs	$r1, $r1			! $r1  <- |b|
#endif /* __NDS32_PERF_EXT__ */
	beqz	$r1, .L1			! if den == 0, skip
	! ---------------------------------------------------------------------
	! {   bit = 1;
	!     res = 0;
	! ---------------------------------------------------------------------
	movi	$r4, 1				! $r4  <- bit = 1
#ifndef __OPTIMIZE_SIZE__
.L6:
#endif
	! ---------------------------------------------------------------------
	!     while (den < num&&bit && !(den & (1L << 31)))
	! ---------------------------------------------------------------------
	slt	$ta, $r1, $r0			! $ta  <- den < num ?
	beqz	$ta, .L5			! if no, skip
	! ---------------------------------------------------------------------
	!     {   den << = 1;
	!         bit << = 1;
	!     }
	! ---------------------------------------------------------------------
#if defined (__OPTIMIZE_SIZE__) && ! defined (__NDS32_ISA_V3M__)
	clz	$r3, $r1			! $r3  <- leading zero count for den
	clz	$ta, $r0			! $ta  <- leading zero count for num
	sub	$r3, $r3, $ta			! $r3  <- number of bits to shift
	sll	$r1, $r1, $r3			! $r1  <- den
	sll	$r4, $r4, $r3			! $r2  <- bit
#else
	slli	$r1, $r1, 1			! $r1  <- den << = 1
	slli	$r4, $r4, 1			! $r4  <- bit << = 1
	b	.L6				! continue loop
#endif
.L5:
	! ---------------------------------------------------------------------
	!     while (bit)
	!     {   if (num >= den)
	!         {   num -= den;
	!             res |= bit;
	!         }
	!         bit >> = 1;
	!         den >> = 1;
	!     }
	! }
	!!if (modwanted)
	!!    return num;
	!!return res;
	! ---------------------------------------------------------------------
	sub	$r2, $r0, $r1			! $r2  <- num - den
	slt	$ta, $r0, $r1			! $ta  <- num < den ?
	srli	$r4, $r4, 1			! $r4  <- bit >> = 1
	cmovz	$r0, $r2, $ta			! $r0  <- num = (num < den) ? num : num - den
	srli	$r1, $r1, 1			! $r1  <- den >> = 1
	bnez	$r4, .L5			! if bit != 0, continue loop
.L1:
	! ---------------------------------------------------------------------
	! if (neg)
	!     res = -res;
	! return res;
	! ---------------------------------------------------------------------
	subri	$r3, $r0, 0			! $r3  <- -res
	cmovn	$r0, $r3, $r5			! $r0  <- neg ? -res : res
	! ---------------------------------------------------------------------
	ret
	.size	__modsi3, .-__modsi3
#endif /* L_modsi3 */



#ifdef L_moddi3

	!--------------------------------------
	#ifdef __big_endian__
		#define  V1H  $r0
		#define  V1L  $r1
		#define  V2H  $r2
		#define  V2L  $r3
	#else
		#define  V1H  $r1
		#define  V1L  $r0
		#define  V2H  $r3
		#define  V2L  $r2
	#endif
	!--------------------------------------
	.text
	.align	2
	.globl	__moddi3
	.type	__moddi3, @function
__moddi3:
	! =====================================================================
	! stack allocation:
	! sp+32 +-----------------------+
	!       | $lp                   |
	! sp+28 +-----------------------+
	!       | $r6 - $r10            |
	! sp+8  +-----------------------+
	!       |                       |
	! sp+4  +-----------------------+
	!       |                       |
	! sp    +-----------------------+
	! =====================================================================
	! prologue
#ifdef __NDS32_ISA_V3M__
	push25	$r10, 8
#else
	smw.adm	$r6, [$sp], $r10, 2
	addi	$sp, $sp, -8
#endif
	! end of prologue
	!------------------------------------------
	! 	__moddi3 (DWtype u, DWtype v)
	!		{
	!			word_type c = 0;
	!			DWunion uu = {.ll = u};
	!			DWunion vv = {.ll = v};
	!			DWtype w;
	!		if (uu.s.high < 0)
	!  		  c = ~c,
	!		  uu.ll = -uu.ll;
	!---------------------------------------------
	move	$r8, V1L
	move	$r9, V1H
	move	$r6, V2L
	move	$r7, V2H
	movi	$r10, 0        ! r10 = c = 0
	bgez	V1H, .L80      ! if u > 0 , go L80
	bal	__negdi2
	move	$r8, V1L
	move	$r9, V1H
	movi	$r10, -1       ! r10 = c = ~c
	!------------------------------------------------
	!	 	if (vv.s.high < 0)
	!		  vv.ll = -vv.ll;
	!----------------------------------------------
.L80:
	bgez	$r7, .L81     !  if v > 0 , go L81
	move	V1L, $r6
	move	V1H, $r7
	bal	__negdi2
	move	$r6, V1L
	move	$r7, V1H
	!------------------------------------------
	!		(void) __udivmoddi4 (uu.ll, vv.ll, &w);
	!		if (c)
	!		  w = -w;
	!		return w;
	!-----------------------------------------
.L81:
	move	V2L, $r6
	move	V2H, $r7
	move	V1L, $r8
	move	V1H, $r9
	addi	$r4, $sp, 0
	bal	__udivmoddi4
	lwi	$r0, [$sp+(0)]    ! le: sp + 0 is low, be: sp + 0 is high
	lwi	$r1, [$sp+(4)]    ! le: sp + 4 is low, be: sp + 4 is high
	beqz	$r10, .L82
	bal	__negdi2
.L82:
	! epilogue
#ifdef __NDS32_ISA_V3M__
	pop25	$r10, 8
#else
	addi	$sp, $sp, 8
	lmw.bim	$r6, [$sp], $r10, 2
	ret
#endif
	.size	__moddi3, .-__moddi3
#endif /* L_moddi3 */



#ifdef L_mulsi3

	.text
	.align	2
	.globl	__mulsi3
	.type	__mulsi3, @function
__mulsi3:
	! ---------------------------------------------------------------------
	! r = 0;
	! while (a)
	! $r0:       r
	! $r1:       b
	! $r2:       a
	! ---------------------------------------------------------------------
	beqz	$r0, .L7			! if a == 0, done
	move	$r2, $r0			! $r2  <- a
	movi	$r0, 0				! $r0  <- r <- 0
.L8:
	! ---------------------------------------------------------------------
	! {   if (a & 1)
	!         r += b;
	!     a >> = 1;
	!     b << = 1;
	! }
	! $r0:       r
	! $r1:       b
	! $r2:       a
	! $r3:       scratch
	! $r4:       scratch
	! ---------------------------------------------------------------------
	andi	$r3, $r2, 1			! $r3  <- a & 1
	add	$r4, $r0, $r1			! $r4  <- r += b
	cmovn	$r0, $r4, $r3			! $r0  <- r
	srli	$r2, $r2, 1			! $r2  <- a >> = 1
	slli	$r1, $r1, 1			! $r1  <- b << = 1
	bnez	$r2, .L8			! if a != 0, continue loop
.L7:
	! ---------------------------------------------------------------------
	! $r0:       return code
	! ---------------------------------------------------------------------
	ret
	.size	__mulsi3, .-__mulsi3
#endif /* L_mulsi3 */



#ifdef L_udivsi3

	.text
	.align	2
	.globl	__udivsi3
	.type	__udivsi3, @function
__udivsi3:
	! ---------------------------------------------------------------------
	!!res=udivmodsi4(a,b,0);
	! res=0;
	! if (den!=0)
	! ---------------------------------------------------------------------
	movi	$r2, 0				! $r2  <- res=0
	beqz	$r1, .L1			! if den==0, skip
	! ---------------------------------------------------------------------
	! {   bit=1;
	! ---------------------------------------------------------------------
	movi	$r4, 1				! $r4  <- bit=1
#ifndef __OPTIMIZE_SIZE__
.L6:
#endif
	! ---------------------------------------------------------------------
	!     while (den<num
	! ---------------------------------------------------------------------
	slt	$ta, $r1, $r0			! $ta  <- den<num?
	beqz	$ta, .L5			! if no, skip
	! ---------------------------------------------------------------------
	!          &&bit&&!(den&(1L<<31)))
	! ---------------------------------------------------------------------
	bltz	$r1, .L5			! if den<0, skip
	! ---------------------------------------------------------------------
	!     {   den<<=1;
	!         bit<<=1;
	!     }
	! ---------------------------------------------------------------------
#if defined (__OPTIMIZE_SIZE__) && ! defined (__NDS32_ISA_V3M__)
	clz	$r3, $r1			! $r3  <- leading zero count for den
	clz	$ta, $r0			! $ta  <- leading zero count for num
	sub	$r3, $r3, $ta			! $r3  <- number of bits to shift
	sll	$r1, $r1, $r3			! $r1  <- den
	sll	$r2, $r2, $r3			! $r2  <- bit
#else
	slli	$r1, $r1, 1			! $r1  <- den<<=1
	slli	$r4, $r4, 1			! $r4  <- bit<<=1
	b	.L6				! continue loop
#endif
.L5:
	! ---------------------------------------------------------------------
	!     while (bit)
	!     {   if (num>=den)
	! ---------------------------------------------------------------------
	slt	$ta, $r0, $r1			! $ta  <- num<den?
	bnez	$ta, .L9			! if yes, skip
	! ---------------------------------------------------------------------
	!         {   num-=den;
	!             res|=bit;
	!         }
	! ---------------------------------------------------------------------
	sub	$r0, $r0, $r1			! $r0  <- num-=den
	or	$r2, $r2, $r4			! $r2  <- res|=bit
.L9:
	! ---------------------------------------------------------------------
	!         bit>>=1;
	!         den>>=1;
	!     }
	! }
	!!if (modwanted)
	!!    return num;
	!!return res;
	! ---------------------------------------------------------------------
	srli	$r4, $r4, 1			! $r4  <- bit>>=1
	srli	$r1, $r1, 1			! $r1  <- den>>=1
	bnez	$r4, .L5			! if bit!=0, continue loop
.L1:
	! ---------------------------------------------------------------------
	! return res;
	! ---------------------------------------------------------------------
	move	$r0, $r2			! $r0  <- return value
	! ---------------------------------------------------------------------
	! ---------------------------------------------------------------------
	ret
	.size	__udivsi3, .-__udivsi3
#endif /* L_udivsi3 */



#ifdef L_udivdi3

	!--------------------------------------
	#ifdef __big_endian__
		#define  V1H  $r0
		#define  V1L  $r1
		#define  V2H  $r2
		#define  V2L  $r3
	#else
		#define  V1H  $r1
		#define  V1L  $r0
		#define  V2H  $r3
		#define  V2L  $r2
	#endif
	!--------------------------------------

	.text
	.align	2
	.globl	__udivdi3
	.type	__udivdi3, @function
__udivdi3:
	! prologue
#ifdef __NDS32_ISA_V3M__
	push25	$r8, 0
#else
	smw.adm	$r6, [$sp], $r8, 2
#endif
	! end of prologue
	movi	$r4, 0
	bal	__udivmoddi4
	! epilogue
#ifdef __NDS32_ISA_V3M__
	pop25	$r8, 0
#else
	lmw.bim	$r6, [$sp], $r8, 2
	ret
#endif
	.size	__udivdi3, .-__udivdi3
#endif /* L_udivdi3 */



#ifdef L_udivmoddi4

	.text
	.align	2
	.globl	fudiv_qrnnd
	.type	fudiv_qrnnd, @function
	#ifdef __big_endian__
		#define P1H     $r0
		#define P1L     $r1
		#define P2H     $r2
		#define P2L     $r3
		#define W6H     $r4
		#define W6L     $r5
		#define OFFSET_L 4
		#define OFFSET_H 0
	#else
		#define P1H     $r1
		#define P1L     $r0
		#define P2H     $r3
		#define P2L     $r2
		#define W6H     $r5
		#define W6L     $r4
		#define OFFSET_L 0
		#define OFFSET_H 4
	#endif
fudiv_qrnnd:
	!------------------------------------------------------
	! function:  fudiv_qrnnd(quotient, remainder, high_numerator, low_numerator, denominator)
	!            divides a UDWtype, composed by the UWtype integers,HIGH_NUMERATOR (from $r4)
	!            and LOW_NUMERATOR(from $r5) by DENOMINATOR(from $r6), and places the quotient
	!            in $r7 and the remainder in $r8.
	!------------------------------------------------------
	!  in reg:$r4(n1), $r5(n0), $r6(d0)
	!  __d1 = ((USItype) (d) >> ((4 * 8) / 2));
	!  __d0 = ((USItype) (d) & (((USItype) 1 << ((4 * 8) / 2)) - 1));
	!  __r1 = (n1) % __d1;
	!  __q1 = (n1) / __d1;
	!  __m = (USItype) __q1 * __d0;
	!  __r1 = __r1 * ((USItype) 1 << ((4 * 8) / 2)) | ((USItype) (n0) >> ((4 * 8) / 2));
	!   if (__r1 < __m)
	!    {
	!------------------------------------------------------
	smw.adm $r0, [$sp], $r4, 2				! store $lp, when use BASELINE_V1,and must store $r0-$r3
	srli	$r7, $r6, 16					! $r7 = d1 =__ll_highpart (d)
	movi	$ta, 65535
	and	  $r8, $r6, $ta       				! $r8 = d0 = __ll_lowpart (d)

	divr	$r9, $r10, $r4, $r7				! $r9 = q1, $r10 = r1
	and	  $r4, $r5, $ta       				! $r4 = __ll_lowpart (n0)
	slli	$r10, $r10, 16      				! $r10 = r1 << 16
	srli	$ta, $r5, 16        				! $ta = __ll_highpart (n0)

	or	$r10, $r10, $ta					! $r10 <- $r0|$r3=__r1
	mul	$r5, $r9, $r8					! $r5 = m =  __q1*__d0
	slt	$ta, $r10, $r5					! $ta <- __r1<__m
	beqz	$ta, .L2					!if yes,skip
	!------------------------------------------------------
	!    __q1--, __r1 += (d);
	!    if (__r1 >= (d))
	!     {
	!------------------------------------------------------

	add	$r10, $r10, $r6					!$r10 <- __r1+d=__r1
	addi	$r9, $r9, -1					!$r9 <- __q1--=__q1
	slt	$ta, $r10, $r6					!$ta <- __r1<d
	bnez	$ta, .L2					!if yes,skip
	!------------------------------------------------------
	!       if (__r1 < __m)
	!        {
	!------------------------------------------------------

	slt	$ta, $r10, $r5					!$ta <- __r1<__m
	beqz	$ta, .L2					!if yes,skip
	!------------------------------------------------------
	!           __q1--, __r1 += (d);
	!        }
	!     }
	!  }
	!------------------------------------------------------

	addi	$r9, $r9, -1					!$r9 <- __q1--=__q1
	add	$r10, $r10, $r6					!$r2 <- __r1+d=__r1
.L2:
	!------------------------------------------------------
	!  __r1 -= __m;
	!  __r0 = __r1 % __d1;
	!  __q0 = __r1 / __d1;
	!  __m = (USItype) __q0 * __d0;
	!  __r0 = __r0 * ((USItype) 1 << ((4 * 8) / 2)) \
	!        | ((USItype) (n0) & (((USItype) 1 << ((4 * 8) / 2)) - 1));
	!  if (__r0 < __m)
	!   {
	!------------------------------------------------------
	sub  $r10, $r10, $r5					!$r10 <- __r1-__m=__r1
	divr	$r7, $r10, $r10, $r7				!$r7 <- r1/__d1=__q0,$r10 <- r1%__d1=__r0
	slli	$r10, $r10, 16					!$r10 <- __r0<<16
	mul	$r5, $r8, $r7					!$r5 <- __q0*__d0=__m
	or	$r10, $r4, $r10					!$r3 <- $r0|__ll_lowpart (n0) =__r0
	slt	$ta, $r10, $r5					!$ta <- __r0<__m
	beqz	$ta, .L5					!if yes,skip
	!------------------------------------------------------
	!      __q0--, __r0 += (d);
	!      if (__r0 >= (d))
	!       {
	!------------------------------------------------------

	add	$r10, $r10, $r6					!$r10 <- __r0+d=__r0
	addi	$r7, $r7, -1					!$r7 <- __q0--=__q0
	slt	$ta, $r10, $r6					!$ta <- __r0<d
	bnez	$ta, .L5					!if yes,skip
	!------------------------------------------------------
	!         if (__r0 < __m)
	!          {
	!------------------------------------------------------

	slt	$ta, $r10, $r5					!$ta <- __r0<__m
	beqz	$ta, .L5					!if yes,skip
	!------------------------------------------------------
	!             __q0--, __r0 += (d);
	!          }
	!       }
	!   }
	!------------------------------------------------------

	add	  $r10, $r10, $r6				!$r3 <- __r0+d=__r0
	addi	$r7, $r7, -1					!$r2 <- __q0--=__q0
.L5:
	!------------------------------------------------------
	!   __r0 -= __m;
	!   *q = (USItype) __q1 * ((USItype) 1 << ((4 * 8) / 2)) | __q0;
	!   *r = __r0;
	!}
	!------------------------------------------------------

	sub		$r8, $r10, $r5				!$r8 = r = r0 = __r0-__m
	slli	$r9, $r9, 16					!$r9 <- __q1<<16
	or	$r7, $r9, $r7					!$r7 = q = $r9|__q0
	lmw.bim $r0, [$sp], $r4, 2
	ret
	.size	fudiv_qrnnd, .-fudiv_qrnnd

	.align	2
	.globl	__udivmoddi4
	.type	__udivmoddi4, @function
__udivmoddi4:
	! =====================================================================
	! stack allocation:
	! sp+40 +------------------+
	!       | q1               |
	! sp+36 +------------------+
	!       | q0               |
	! sp+32 +------------------+
	!       | bm               |
	! sp+28 +------------------+
	!       | $lp              |
	! sp+24 +------------------+
	!       | $fp              |
	! sp+20 +------------------+
	!       | $r6 - $r10       |
	! sp    +------------------+
	! =====================================================================

	addi	$sp, $sp, -40
	smw.bi	$r6, [$sp], $r10, 10
	!------------------------------------------------------
	!  d0 = dd.s.low;
	!  d1 = dd.s.high;
	!  n0 = nn.s.low;
	!  n1 = nn.s.high;
	!  if (d1 == 0)
	!   {
	!------------------------------------------------------

	move	$fp, $r4					!$fp <- rp
	bnez	P2H, .L9					!if yes,skip
	!------------------------------------------------------
	!     if (d0 > n1)
	!      {
	!------------------------------------------------------

	slt	$ta, P1H, P2L					!$ta <- n1<d0
	beqz	$ta, .L10					!if yes,skip
#ifndef __NDS32_PERF_EXT__
	smw.adm $r0, [$sp], $r5, 0
	move    $r0, P2L
	bal __clzsi2
	move	$r7, $r0
	lmw.bim $r0, [$sp], $r5, 0
#else
	clz  $r7, P2L
#endif
	swi     $r7,  [$sp+(28)]
	beqz	$r7, .L18					!if yes,skip
	!------------------------------------------------------
	!         d0 = d0 << bm;
	!         n1 = (n1 << bm) | (n0 >> ((4 * 8) - bm));
	!         n0 = n0 << bm;
	!      }
	!------------------------------------------------------

	subri	$r5, $r7, 32					!$r5 <- 32-bm
	srl	$r5, P1L, $r5					!$r5 <- n0>>$r5
	sll	$r6, P1H, $r7					!$r6 <- n1<<bm
	or	P1H, $r6, $r5					!P2h <- $r5|$r6=n1
	sll	P1L, P1L, $r7					!P1H <- n0<<bm=n0
	sll	P2L, P2L, $r7					!P2L <- d0<<bm=d0
.L18:
	!------------------------------------------------------
	!    fudiv_qrnnd (&q0, &n0, n1, n0, d0);
	!    q1 = 0;
	!  } #if (d0 > n1)
	!------------------------------------------------------

	move 	$r4,P1H						! give fudiv_qrnnd args
	move 	$r5,P1L						!
	move 	$r6,P2L						!
	bal	fudiv_qrnnd					!calcaulte q0 n0
	movi	$r6, 0						!P1L <- 0
	swi     $r7,[$sp+32]                                    !q0
	swi     $r6,[$sp+36]                                    !q1
	move    P1L,$r8						!n0
	b	.L19
.L10:
	!------------------------------------------------------
	!  else #if (d0 > n1)
	!   {
	!     if(d0 == 0)
	!------------------------------------------------------

	bnez	P2L, .L20					!if yes,skip
	!------------------------------------------------------
	!      d0 = 1 / d0;
	!------------------------------------------------------

	movi	$r4, 1						!P1L <- 1
	divr	P2L, $r4, $r4, P2L				!$r9=1/d0,P1L=1%d0
.L20:

#ifndef __NDS32_PERF_EXT__
	smw.adm $r0, [$sp], $r5, 0
	move    $r0, P2L
	bal __clzsi2
	move    $r7, $r0
	lmw.bim $r0, [$sp], $r5, 0
#else
	clz  $r7, P2L
#endif
	swi     $r7,[$sp+(28)]      ! store bm
	beqz	$r7, .L28					! if yes,skip
	!------------------------------------------------------
	!         b = (4 * 8) - bm;
	!         d0 = d0 << bm;
	!         n2 = n1 >> b;
	!         n1 = (n1 << bm) | (n0 >> b);
	!         n0 = n0 << bm;
	!         fudiv_qrnnd (&q1, &n1, n2, n1, d0);
	!    }
	!------------------------------------------------------

	subri	$r10, $r7, 32					!$r10 <- 32-bm=b
	srl	$r4, P1L, $r10					!$r4 <- n0>>b
	sll	$r5, P1H, $r7					!$r5 <- n1<<bm
	or	$r5, $r5, $r4					!$r5 <- $r5|$r4=n1  !for fun
	sll	P2L, P2L, $r7					!P2L <- d0<<bm=d0   !for fun
	sll	P1L, P1L, $r7					!P1L <- n0<<bm=n0
	srl	$r4, P1H, $r10					!$r4 <- n1>>b=n2    !for fun

	move    $r6,P2L                     			!for fun
	bal	fudiv_qrnnd					!caculate q1, n1

	swi  $r7,[$sp+(36)]          ! q1 store
	move P1H,$r8                 ! n1 store

	move $r4,$r8	             ! prepare for next fudiv_qrnnd()
	move $r5,P1L
	move $r6,P2L
	b	.L29
.L28:
	!------------------------------------------------------
	!    else // bm != 0
	!     {
	!        n1 -= d0;
	!        q1 = 1;
	!
	!------------------------------------------------------

	sub	P1H, P1H, P2L					!P1L <- n1-d0=n1
	movi	$ta, 1						!
	swi	$ta, [$sp+(36)]	                                !1 -> [$sp+(36)]

	move $r4,P1H						! give fudiv_qrnnd args
	move $r5,P1L
	move $r6,P2L
.L29:
	!------------------------------------------------------
	!    fudiv_qrnnd (&q0, &n0, n1, n0, d0);
	!------------------------------------------------------

	bal	fudiv_qrnnd					!calcuate  q0, n0
	swi     $r7,[$sp+(32)]  !q0 store
	move    P1L,$r8		!n0
.L19:
	!------------------------------------------------------
	!    if (rp != 0)
	!     {
	!------------------------------------------------------

	beqz	$fp, .L31					!if yes,skip
	!------------------------------------------------------
	!         rr.s.low = n0 >> bm;
	!         rr.s.high = 0;
	!         *rp = rr.ll;
	!     }
	!------------------------------------------------------

	movi    $r5, 0							!$r5 <- 0
	lwi     $r7,[$sp+(28)]    					!load bm
	srl	$r4, P1L, $r7     	     				!$r4 <- n0>>bm
        swi	$r4, [$fp+OFFSET_L]	  !r0				!$r4 -> [$sp+(48)]
	swi	$r5, [$fp+OFFSET_H]	  !r1				!0 -> [$sp+(52)]
	b .L31
.L9:
	!------------------------------------------------------
	! else # d1 == 0
	!  {
	!     if(d1 > n1)
	!      {
	!------------------------------------------------------

	slt	$ta, P1H, P2H					!$ta <- n1<d1
	beqz	$ta, .L32					!if yes,skip
	!------------------------------------------------------
	!         q0 = 0;
	!	  q1 = 0;
	!         if (rp != 0)
	!          {
	!------------------------------------------------------

	movi	$r5, 0						!$r5 <- 0
	swi	$r5, [$sp+(32)]	   !q0				!0 -> [$sp+(40)]=q1
	swi	$r5, [$sp+(36)]    !q1				!0 -> [$sp+(32)]=q0
	beqz	$fp, .L31					!if yes,skip
	!------------------------------------------------------
	!             rr.s.low = n0;
	!	      rr.s.high = n1;
	!             *rp = rr.ll;
	!          }
	!------------------------------------------------------

	swi	P1L, [$fp+OFFSET_L]					!P1L -> [rp]
	swi	P1H, [$fp+OFFSET_H]					!P1H -> [rp+4]
	b	.L31
.L32:
#ifndef __NDS32_PERF_EXT__
	smw.adm $r0, [$sp], $r5, 0
	move    $r0, P2H
	bal __clzsi2
	move    $r7, $r0
	lmw.bim $r0, [$sp], $r5, 0
#else
	clz  $r7,P2H
#endif
        swi     $r7,[$sp+(28)] 	                                !$r7=bm  store
	beqz	$r7, .L42					!if yes,skip
	!------------------------------------------------------
	!        USItype m1, m0;
	!        b = (4 * 8) - bm;
	!        d1 = (d0 >> b) | (d1 << bm);
	!        d0 = d0 << bm;
	!        n2 = n1 >> b;
	!        n1 = (n0 >> b) | (n1 << bm);
	!        n0 = n0 << bm;
	!        fudiv_qrnnd (&q0, &n1, n2, n1, d1);
	!------------------------------------------------------

	subri	$r10, $r7, 32					!$r10 <- 32-bm=b
	srl	$r5, P2L, $r10					!$r5 <- d0>>b
	sll	$r6, P2H, $r7					!$r6 <- d1<<bm
	or      $r6, $r5, $r6                                   !$r6 <- $r5|$r6=d1  !! func
	move	P2H, $r6 					!P2H <- d1
	srl     $r4, P1H, $r10                                  !$r4 <- n1>>b=n2    !!! func
	srl	$r8, P1L, $r10					!$r8 <- n0>>b       !!$r8
	sll     $r9, P1H, $r7                                   !$r9 <- n1<<bm
	or	$r5, $r8, $r9					!$r5 <- $r8|$r9=n1  !func
	sll     P2L, P2L, $r7                                   !P2L <- d0<<bm=d0
	sll	P1L, P1L, $r7					!P1L <- n0<<bm=n0

	bal	fudiv_qrnnd					! cal  q0,n1
	swi     $r7,[$sp+(32)]
	move    P1H,$r8            ! fudiv_qrnnd (&q0, &n1, n2, n1, d1);
        move    $r6, $r7           ! from func

	!----------------------------------------------------
	!       #umul_ppmm (m1, m0, q0, d0);
	!        do
	!         {     USItype __x0, __x1, __x2, __x3;
	!               USItype __ul, __vl, __uh, __vh;
	!               __ul = ((USItype) (q0) & (((USItype) 1 << ((4 * 8) / 2)) - 1));
	!               __uh = ((USItype) (q0) >> ((4 * 8) / 2));
	!               __vl = ((USItype) (d0) & (((USItype) 1 << ((4 * 8) / 2)) - 1));
	!               __vh = ((USItype) (d0) >> ((4 * 8) / 2));
	!               __x0 = (USItype) __ul * __vl;
	!               __x1 = (USItype) __ul * __vh;
	!               __x2 = (USItype) __uh * __vl;
	!               __x3 = (USItype) __uh * __vh;
	!               __x1 += ((USItype) (__x0) >> ((4 * 8) / 2));
	!               __x1 += __x2;
	!               if (__x1 < __x2)
	!                  __x3 += ((USItype) 1 << ((4 * 8) / 2));
	!               (m1) = __x3 + ((USItype) (__x1) >> ((4 * 8) / 2));
	!               (m0) = (USItype)(q0*d0);
	!        }
	!        if (m1 > n1)
	!---------------------------------------------------
#ifdef __NDS32_ISA_V3M__
        !mulr64  $r4, P2L, $r6
	smw.adm $r0, [$sp], $r3, 0
	move	P1L, P2L
	move	P2L, $r6
	movi	P1H, 0
	movi	P2H, 0
	bal	__muldi3
	movd44	$r4, $r0
	lmw.bim $r0, [$sp], $r3, 0
        move    $r8, W6H
        move    $r5, W6L
#else
        mulr64  $r4, P2L, $r6
        move    $r8, W6H
        move    $r5, W6L
#endif
	slt	$ta, P1H, $r8					!$ta <- n1<m1
	bnez	$ta, .L46					!if yes,skip
	!------------------------------------------------------
	!   if(m1 == n1)
	!------------------------------------------------------

	bne	$r8, P1H, .L45					!if yes,skip
	!------------------------------------------------------
	!   if(m0 > n0)
	!------------------------------------------------------

	slt	$ta, P1L, $r5					!$ta <- n0<m0
	beqz	$ta, .L45					!if yes,skip
.L46:
	!------------------------------------------------------
	!    {
	!       q0--;
	!       # sub_ddmmss (m1, m0, m1, m0, d1, d0);
	!       do
	!        {   USItype __x;
	!            __x = (m0) - (d0);
	!            (m1) = (m1) - (d1) - (__x > (m0));
	!            (m0) = __x;
	!        }
	!    }
	!------------------------------------------------------

	sub	$r4, $r5, P2L					!$r4 <- m0-d0=__x
	addi	$r6, $r6, -1					!$r6 <- q0--=q0
	sub	$r8, $r8, P2H					!$r8 <- m1-d1
	swi	$r6, [$sp+(32)]	      ! q0			!$r6->[$sp+(32)]
	slt	$ta, $r5, $r4					!$ta <- m0<__x
	sub	$r8, $r8, $ta					!$r8 <- P1H-P1L=m1
	move	$r5, $r4					!$r5 <- __x=m0
.L45:
	!------------------------------------------------------
	!    q1 = 0;
	!    if (rp != 0)
	!     {
	!------------------------------------------------------

	movi	$r4, 0						!$r4 <- 0
	swi	$r4, [$sp+(36)]					!0 -> [$sp+(40)]=q1
	beqz	$fp, .L31					!if yes,skip
	!------------------------------------------------------
	!      # sub_ddmmss (n1, n0, n1, n0, m1, m0);
	!      do
	!       {   USItype __x;
	!           __x = (n0) - (m0);
	!           (n1) = (n1) - (m1) - (__x > (n0));
	!           (n0) = __x;
	!       }
	!       rr.s.low = (n1 << b) | (n0 >> bm);
	!       rr.s.high = n1 >> bm;
	!       *rp = rr.ll;
	!------------------------------------------------------

	sub	$r4, P1H, $r8					!$r4 <- n1-m1
	sub	$r6, P1L, $r5					!$r6 <- n0-m0=__x=n0
	slt	$ta, P1L, $r6					!$ta <- n0<__x
	sub	P1H, $r4, $ta					!P1H <- $r4-$ta=n1
	move    P1L, $r6

	lwi     $r7,[$sp+(28)]         ! load bm
	subri   $r10,$r7,32
	sll	$r4, P1H, $r10					!$r4 <- n1<<b
	srl	$r5, P1L, $r7					!$r5 <- __x>>bm
	or	$r6, $r5, $r4					!$r6 <- $r5|$r4=rr.s.low
	srl	$r8, P1H, $r7					!$r8 <- n1>>bm =rr.s.high
	swi	$r6, [$fp+OFFSET_L]				!
	swi	$r8, [$fp+OFFSET_H]				!
	b	.L31
.L42:
	!------------------------------------------------------
	!  else
	!   {
	!     if(n1 > d1)
	!------------------------------------------------------

	slt	$ta, P2H, P1H					!$ta <- P2H<P1H
	bnez	$ta, .L52					!if yes,skip
	!------------------------------------------------------
	!     if (n0 >= d0)
	!------------------------------------------------------

	slt	$ta, P1L, P2L					!$ta <- P1L<P2L
	bnez	$ta, .L51					!if yes,skip
	!------------------------------------------------------
	!        q0 = 1;
	!        do
	!         {   USItype __x;
	!             __x = (n0) - (d0);
	!             (n1) = (n1) - (d1) - (__x > (n0));
	!             (n0) = __x;
	!         }
	!------------------------------------------------------
.L52:
	sub	$r4, P1H, P2H					!$r4 <- P1H-P2H
	sub	$r6, P1L, P2L					!$r6 <- no-d0=__x=n0
	slt	$ta, P1L, $r6					!$ta <- no<__x
	sub	P1H, $r4, $ta					!P1H <- $r4-$ta=n1
	move    P1L, $r6					!n0
	movi	$r5, 1						!
	swi	$r5, [$sp+(32)]					!1 -> [$sp+(32)]=q0
	b	.L54
.L51:
	!------------------------------------------------------
	!       q0 = 0;
	!------------------------------------------------------

	movi    $r5,0
	swi	$r5, [$sp+(32)]					!$r5=0 -> [$sp+(32)]
.L54:
	!------------------------------------------------------
	!       q1 = 0;
	!       if (rp != 0)
	!        {
	!------------------------------------------------------

	movi	$r5, 0						!
	swi	$r5, [$sp+(36)]					!0 -> [$sp+(36)]
	beqz	$fp, .L31
	!------------------------------------------------------
	!          rr.s.low = n0;
	!          rr.s.high = n1;
	!          *rp = rr.ll;
	!        }
	!------------------------------------------------------

	swi	P1L, [$fp+OFFSET_L]				!remainder
	swi	P1H, [$fp+OFFSET_H]				!
.L31:
	!------------------------------------------------------
	! const DWunion ww = {{.low = q0, .high = q1}};
	! return ww.ll;
	!}
	!------------------------------------------------------

	lwi	P1L, [$sp+(32)]					!quotient
	lwi	P1H, [$sp+(36)]
	lmw.bim	$r6, [$sp], $r10, 10
	addi	$sp, $sp, 12
	ret
	.size	__udivmoddi4, .-__udivmoddi4
#endif /* L_udivmoddi4 */



#ifdef L_umodsi3

	! =====================================================================
	.text
	.align	2
	.globl	__umodsi3
	.type	__umodsi3, @function
__umodsi3:
	! ---------------------------------------------------------------------
	!!res=udivmodsi4(a,b,1);
	! if (den==0)
	!     return num;
	! ---------------------------------------------------------------------
	beqz	$r1, .L1			! if den==0, skip
	! ---------------------------------------------------------------------
	! bit=1;
	! res=0;
	! ---------------------------------------------------------------------
	movi	$r4, 1				! $r4  <- bit=1
#ifndef __OPTIMIZE_SIZE__
.L6:
#endif
	! ---------------------------------------------------------------------
	! while (den<num
	! ---------------------------------------------------------------------
	slt	$ta, $r1, $r0			! $ta  <- den<num?
	beqz	$ta, .L5			! if no, skip
	! ---------------------------------------------------------------------
	!      &&bit&&!(den&(1L<<31)))
	! ---------------------------------------------------------------------
	bltz	$r1, .L5			! if den<0, skip
	! ---------------------------------------------------------------------
	! {   den<<=1;
	!     bit<<=1;
	! }
	! ---------------------------------------------------------------------
#if defined (__OPTIMIZE_SIZE__) && ! defined (__NDS32_ISA_V3M__)
	clz	$r3, $r1			! $r3  <- leading zero count for den
	clz	$ta, $r0			! $ta  <- leading zero count for num
	sub	$r3, $r3, $ta			! $r3  <- number of bits to shift
	sll	$r1, $r1, $r3			! $r1  <- den
	sll	$r4, $r4, $r3			! $r2  <- bit
#else
	slli	$r1, $r1, 1			! $r1  <- den<<=1
	slli	$r4, $r4, 1			! $r4  <- bit<<=1
	b	.L6				! continue loop
#endif
.L5:
	! ---------------------------------------------------------------------
	! while (bit)
	! {   if (num>=den)
	!     {   num-=den;
	!         res|=bit;
	!     }
	!     bit>>=1;
	!     den>>=1;
	! }
	!!if (modwanted)
	!!    return num;
	!!return res;
	! ---------------------------------------------------------------------
	sub	$r2, $r0, $r1			! $r2  <- num-den
	slt	$ta, $r0, $r1			! $ta  <- num<den?
	srli	$r4, $r4, 1			! $r4  <- bit>>=1
	cmovz	$r0, $r2, $ta			! $r0  <- num=(num<den)?num:num-den
	srli	$r1, $r1, 1			! $r1  <- den>>=1
	bnez	$r4, .L5			! if bit!=0, continue loop
.L1:
	! ---------------------------------------------------------------------
	! return res;
	! ---------------------------------------------------------------------
	ret
	.size	__umodsi3, .-__umodsi3
#endif /* L_umodsi3 */



#ifdef L_umoddi3

	!--------------------------------------
	#ifdef __big_endian__
		#define  V1H  $r0
		#define  V1L  $r1
		#define  V2H  $r2
		#define  V2L  $r3
	#else
		#define  V1H  $r1
		#define  V1L  $r0
		#define  V2H  $r3
		#define  V2L  $r2
	#endif
	!--------------------------------------
	.text
	.align	2
	.globl	__umoddi3
	.type	__umoddi3, @function
__umoddi3:
	! prologue
	addi	$sp, $sp, -12
	swi $lp, [$sp+(0)]
	! end of prologue
	addi	$r4, $sp, 4
	bal	__udivmoddi4
	lwi	$r0, [$sp+(4)]    ! __udivmoddi4 return low when LE mode or return high when BE mode
	lwi	$r1, [$sp+(8)]    !
.L82:
	! epilogue
	lwi $lp, [$sp+(0)]
	addi	$sp, $sp, 12
	ret
	.size	__umoddi3, .-__umoddi3
#endif /* L_umoddi3 */



#ifdef L_muldi3

#ifdef __big_endian__
	#define P1H	$r0
	#define P1L	$r1
	#define P2H	$r2
	#define P2L	$r3

	#define V2H $r4
	#define V2L $r5
#else
	#define P1H	$r1
	#define P1L	$r0
	#define P2H	$r3
	#define P2L	$r2

	#define V2H $r5
	#define V2L $r4
#endif

	! ====================================================================
	.text
	.align	2
	.globl	__muldi3
	.type	__muldi3, @function
__muldi3:
	! parameter passing for libgcc functions normally involves 2 doubles
	!---------------------------------------
#ifdef __NDS32_ISA_V3M__
	! There is no mulr64 instruction in Andes ISA V3M.
	! So we must provide a sequence of calculations to complete the job.
	smw.adm   $r6, [$sp], $r9, 0x0
	zeh33	  $r4, P1L
	srli      $r7, P1L, 16
	zeh33     $r5, P2L
	mul       $r6, $r5, $r4
	mul33     $r5, $r7
	srli      $r8, P2L, 16
	mov55     $r9, $r5
	maddr32   $r9, $r8, $r4
	srli      $r4, $r6, 16
	add       $r4, $r9, $r4
	slt45     $r4, $r5
	slli      $r5, $r15, 16
	maddr32   $r5, $r8, $r7
	mul       P2L, P1H, P2L
	srli      $r7, $r4, 16
	maddr32   P2L, P2H, P1L
	add333    P1H, $r5, $r7
	slli      $r4, $r4, 16
	zeh33     $r6, $r6
	add333    P1L, $r4, $r6
	add333    P1H, P2L, P1H
	lmw.bim   $r6, [$sp], $r9, 0x0
	ret
#else /* not  __NDS32_ISA_V3M__ */
	mul	    $ta, P1L, P2H
	mulr64	$r4, P1L, P2L
	maddr32	$ta, P1H, P2L
	move	  P1L, V2L
	add	    P1H, $ta, V2H
	ret
#endif /* not __NDS32_ISA_V3M__ */
	.size	__muldi3, .-__muldi3
#endif /* L_muldi3 */



#ifdef L_addsub_df

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
	#define P3L     $r4
	#define P3H     $r5
	#define O1L     $r7
	#define O1H	$r8
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
	#define P3H     $r4
	#define P3L     $r5
	#define O1H     $r7
	#define O1L	$r8
#endif
	.text
	.align	2
	.global  __subdf3
	.type    __subdf3, @function
__subdf3:
	push    $lp
	pushm   $r6, $r10

	move    $r4, #0x80000000
	xor     P2H, P2H, $r4

	j       .Lsdpadd

	.global  __adddf3
	.type    __adddf3, @function
__adddf3:
	push    $lp
	pushm   $r6, $r10
.Lsdpadd:
	slli    $r6, P1H, #1
	srli    $r6, $r6, #21
	slli    P3H, P1H, #11
	srli    $r10, P1L, #21
	or      P3H, P3H, $r10
	slli    P3L, P1L, #11
	move    O1L, #0x80000000
	or      P3H, P3H, O1L
	slli    $r9, P2H, #1
	srli    $r9, $r9, #21
	slli    O1H, P2H, #11
	srli    $r10, P2L, #21
	or      O1H, O1H, $r10
	or      O1H, O1H, O1L
	slli    O1L, P2L, #11

	addi    $r10, $r6, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LEspecA

.LElab1:
	addi    $r10, $r9, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LEspecB

.LElab2:
	#NORMd($r4, P2L, P1L)
	bnez    P3H, .LL1
	bnez    P3L, .LL2
	move    $r6, #0
	j       .LL3
.LL2:
	move    P3H, P3L
	move    P3L, #0
	move    P2L, #32
	sub     $r6, $r6, P2L
.LL1:
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r2, $r5
#else
	pushm	$r0, $r1
	pushm	$r3, $r5
	move	$r0, $r5
	bal	__clzsi2
	move	$r2, $r0
	popm	$r3, $r5
	popm	$r0, $r1
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r3, $r4
#else
	pushm	$r0, $r2
	pushm	$r4, $r5
	move	$r0, $r4
	bal	__clzsi2
	move	$r3, $r0
	popm	$r4, $r5
	popm	$r0, $r2
#endif
#endif /* __big_endian__ */
	beqz    P2L, .LL3
	sub     $r6, $r6, P2L
	subri   P1L, P2L, #32
	srl     P1L, P3L, P1L
	sll     P3L, P3L, P2L
	sll     P3H, P3H, P2L
	or      P3H, P3H, P1L
.LL3:
	#NORMd End

	#NORMd($r7, P2L, P1L)
	bnez    O1H, .LL4
	bnez    O1L, .LL5
	move    $r9, #0
	j       .LL6
.LL5:
	move    O1H, O1L
	move    O1L, #0
	move    P2L, #32
	sub     $r9, $r9, P2L
.LL4:
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r2, O1H
#else
	pushm	$r0, $r1
	pushm	$r3, $r5
	move	$r0, O1H
	bal	__clzsi2
	move	$r2, $r0
	popm	$r3, $r5
	popm	$r0, $r1
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r3, O1H
#else
	pushm	$r0, $r2
	pushm	$r4, $r5
	move	$r0, O1H
	bal	__clzsi2
	move	$r3, $r0
	popm	$r4, $r5
	popm	$r0, $r2
#endif
#endif /* __big_endian__ */
	beqz    P2L, .LL6
	sub     $r9, $r9, P2L
	subri   P1L, P2L, #32
	srl     P1L, O1L, P1L
	sll     O1L, O1L, P2L
	sll     O1H, O1H, P2L
	or      O1H, O1H, P1L
.LL6:
	#NORMd End

	move    $r10, #0x80000000
	and     P1H, P1H, $r10

	beq     $r6, $r9, .LEadd3
	slts    $r15, $r9, $r6
	beqzs8  .Li1
	sub     $r9, $r6, $r9
	move    P2L, #0
.LL7:
	move    $r10, #0x20
	slt     $r15, $r9, $r10
	bnezs8  .LL8
	or      P2L, P2L, O1L
	move    O1L, O1H
	move    O1H, #0
	addi    $r9, $r9, #0xffffffe0
	bnez    O1L, .LL7
.LL8:
	beqz    $r9, .LEadd3
	move    P1L, O1H
	move    $r10, O1L
	srl     O1L, O1L, $r9
	srl     O1H, O1H, $r9
	subri   $r9, $r9, #0x20
	sll     P1L, P1L, $r9
	or      O1L, O1L, P1L
	sll     $r10, $r10, $r9
	or      P2L, P2L, $r10
	beqz    P2L, .LEadd3
	ori     O1L, O1L, #1
	j       .LEadd3
.Li1:
	move    $r15, $r6
	move    $r6, $r9
	sub     $r9, $r9, $r15
	move    P2L, #0
.LL10:
	move    $r10, #0x20
	slt     $r15, $r9, $r10
	bnezs8  .LL11
	or      P2L, P2L, P3L
	move    P3L, P3H
	move    P3H, #0
	addi    $r9, $r9, #0xffffffe0
	bnez    P3L, .LL10
.LL11:
	beqz    $r9, .LEadd3
	move    P1L, P3H
	move    $r10, P3L
	srl     P3L, P3L, $r9
	srl     P3H, P3H, $r9
	subri   $r9, $r9, #0x20
	sll     P1L, P1L, $r9
	or      P3L, P3L, P1L
	sll     $r10, $r10, $r9
	or      P2L, P2L, $r10
	beqz    P2L, .LEadd3
	ori     P3L, P3L, #1

.LEadd3:
	xor     $r10, P1H, P2H
	sltsi   $r15, $r10, #0
	bnezs8  .LEsub1

	#ADD(P3L, O1L)
	add     P3L, P3L, O1L
	slt     $r15, P3L, O1L

	#ADDCC(P3H, O1H)
	beqzs8  .LL13
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
	beqzs8  .LL14
	addi    P3H, P3H, #0x1
	j       .LL15
.LL14:
	move    $r15, #1
	add     P3H, P3H, $r15
	slt     $r15, P3H, $r15
	j       .LL15
.LL13:
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
.LL15:

	beqzs8  .LEres
	andi    $r10, P3L, #1
	beqz    $r10, .Li3
	ori     P3L, P3L, #2
.Li3:
	srli    P3L, P3L, #1
	slli    $r10, P3H, #31
	or      P3L, P3L, $r10
	srli    P3H, P3H, #1
	move    $r10, #0x80000000
	or      P3H, P3H, $r10
	addi    $r6, $r6, #1
	subri   $r15, $r6, #0x7ff
	bnezs8  .LEres
	move    $r10, #0x7ff00000
	or      P1H, P1H, $r10
	move    P1L, #0
	j       .LEretA

.LEsub1:
	#SUB(P3L, O1L)
	move    $r15, P3L
	sub     P3L, P3L, O1L
	slt     $r15, $r15, P3L

	#SUBCC(P3H, O1H)
	beqzs8  .LL16
	move    $r15, P3H
	sub     P3H, P3H, O1H
	slt     $r15, $r15, P3H
	beqzs8  .LL17
	subi333 P3H, P3H, #1
	j       .LL18
.LL17:
	move    $r15, P3H
	subi333 P3H, P3H, #1
	slt     $r15, $r15, P3H
	j       .LL18
.LL16:
	move    $r15, P3H
	sub     P3H, P3H, O1H
	slt     $r15, $r15, P3H
.LL18:

	beqzs8  .Li5
	move    $r10, #0x80000000
	xor     P1H, P1H, $r10

	subri   P3H, P3H, #0
	beqz    P3L, .LL19
	subri   P3L, P3L, #0
	subi45  P3H, #1
.LL19:

.Li5:
	#NORMd($r4, $r9, P1L)
	bnez    P3H, .LL20
	bnez    P3L, .LL21
	move    $r6, #0
	j       .LL22
.LL21:
	move    P3H, P3L
	move    P3L, #0
	move    $r9, #32
	sub     $r6, $r6, $r9
.LL20:
#ifdef __NDS32_PERF_EXT__
	clz	$r9, P3H
#else
	pushm	$r0, $r5
	move	$r0, P3H
	bal	__clzsi2
	move	$r9, $r0
	popm	$r0, $r5
#endif
	beqz    $r9, .LL22
	sub     $r6, $r6, $r9
	subri   P1L, $r9, #32
	srl     P1L, P3L, P1L
	sll     P3L, P3L, $r9
	sll     P3H, P3H, $r9
	or      P3H, P3H, P1L
.LL22:
	#NORMd End

	or      $r10, P3H, P3L
	bnez    $r10, .LEres
	move    P1H, #0

.LEres:
	blez    $r6, .LEund

.LElab8:
	#ADD(P3L, $0x400)
	move    $r15, #0x400
	add     P3L, P3L, $r15
	slt     $r15, P3L, $r15

	#ADDCC(P3H, $0x0)
	beqzs8  .LL25
	add     P3H, P3H, $r15
	slt     $r15, P3H, $r15
.LL25:

	#ADDC($r6, $0x0)
	add     $r6, $r6, $r15
	srli    $r10, P3L, #11
	andi    $r10, $r10, #1
	sub     P3L, P3L, $r10
	srli    P1L, P3L, #11
	slli    $r10, P3H, #21
	or      P1L, P1L, $r10
	slli    $r10, P3H, #1
	srli    $r10, $r10, #12
	or      P1H, P1H, $r10
	slli    $r10, $r6, #20
	or      P1H, P1H, $r10

.LEretA:
.LE999:
	popm    $r6, $r10
	pop     $lp
	ret5    $lp

.LEspecA:
	#ADD(P3L, P3L)
	move    $r15, P3L
	add     P3L, P3L, P3L
	slt     $r15, P3L, $r15

	#ADDC(P3H, P3H)
	add     P3H, P3H, P3H
	add     P3H, P3H, $r15
	bnez    $r6, .Li7
	or      $r10, P3H, P3L
	beqz    $r10, .Li8
	j       .LElab1
.Li8:
	subri   $r15, $r9, #0x7ff
	beqzs8  .LEspecB
	add     P3L, P2H, P2H
	or      $r10, P3L, P2L
	bnez    $r10, .LEretB
	sltsi   $r15, P2H, #0
	bnezs8  .LEretA

.LEretB:
	move    P1L, P2L
	move    P1H, P2H
	j       .LE999
.Li7:
	or      $r10, P3H, P3L
	bnez    $r10, .LEnan
	subri   $r15, $r9, #0x7ff
	bnezs8  .LEretA
	xor     $r10, P1H, P2H
	sltsi   $r15, $r10, #0
	bnezs8  .LEnan
	j       .LEretB

.LEspecB:
	#ADD(O1L, O1L)
	move    $r15, O1L
	add     O1L, O1L, O1L
	slt     $r15, O1L, $r15

	#ADDC(O1H, O1H)
	add     O1H, O1H, O1H
	add     O1H, O1H, $r15
	bnez    $r9, .Li11
	or      $r10, O1H, O1L
	beqz    $r10, .LEretA
	j       .LElab2
.Li11:
	or      $r10, O1H, O1L
	beqz    $r10, .LEretB

.LEnan:
	move    P1H, #0xfff80000
	move    P1L, #0
	j       .LEretA

.LEund:
	subri   $r9, $r6, #1
	move    P2L, #0
.LL26:
	move    $r10, #0x20
	slt     $r15, $r9, $r10
	bnezs8  .LL27
	or      P2L, P2L, P3L
	move    P3L, P3H
	move    P3H, #0
	addi    $r9, $r9, #0xffffffe0
	bnez    P3L, .LL26
.LL27:
	beqz    $r9, .LL28
	move    P1L, P3H
	move    $r10, P3L
	srl     P3L, P3L, $r9
	srl     P3H, P3H, $r9
	subri   $r9, $r9, #0x20
	sll     P1L, P1L, $r9
	or      P3L, P3L, P1L
	sll     $r10, $r10, $r9
	or      P2L, P2L, $r10
	beqz    P2L, .LL28
	ori     P3L, P3L, #1
.LL28:
	move    $r6, #0
	j       .LElab8
	.size   __subdf3, .-__subdf3
	.size   __adddf3, .-__adddf3
#endif /* L_addsub_df */



#ifdef L_mul_sf

#if !defined (__big_endian__)
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
#endif
	.text
	.align	2
	.global	__mulsf3
	.type	__mulsf3, @function
__mulsf3:
	push    $lp
	pushm   $r6, $r10

	srli    $r3, $r0, #23
	andi    $r3, $r3, #0xff
	srli    $r5, $r1, #23
	andi    $r5, $r5, #0xff
	move    $r6, #0x80000000
	slli    $r2, $r0, #8
	or      $r2, $r2, $r6
	slli    $r4, $r1, #8
	or      $r4, $r4, $r6
	xor     $r8, $r0, $r1
	and     $r6, $r6, $r8

	addi    $r8, $r3, #-1
	slti    $r15, $r8, #0xfe
	beqzs8  .LFspecA

.LFlab1:
	addi    $r8, $r5, #-1
	slti    $r15, $r8, #0xfe
	beqzs8  .LFspecB

.LFlab2:
	move    $r10, $r3
/* This is a 64-bit multiple. ($r2, $r7) is (high, low). */
#ifndef __NDS32_ISA_V3M__
	mulr64	$r2, $r2, $r4
#else
	pushm	$r0, $r1
	pushm	$r4, $r5
	move	P1L, $r2
	movi	P1H, #0
	move	P2L, $r4
	movi	P2H, #0
	bal	__muldi3
	movd44	$r2, $r0
	popm	$r4, $r5
	popm	$r0, $r1
#endif
#ifndef __big_endian__
	move    $r7, $r2
	move    $r2, $r3
#else
	move	$r7, $r3
#endif
	move    $r3, $r10

	beqz    $r7, .Li17
	ori     $r2, $r2, #1

.Li17:
	sltsi   $r15, $r2, #0
	bnezs8  .Li18
	slli    $r2, $r2, #1
	addi    $r3, $r3, #-1
.Li18:
	addi    $r8, $r5, #0xffffff82
	add     $r3, $r3, $r8
	addi    $r8, $r3, #-1
	slti    $r15, $r8, #0xfe
	beqzs8  .LFoveund

.LFlab8:
	#ADD($r2, $0x80)
	move    $r15, #0x80
	add     $r2, $r2, $r15
	slt     $r15, $r2, $r15

	#ADDC($r3, $0x0)
	add     $r3, $r3, $r15
	srli    $r8, $r2, #8
	andi    $r8, $r8, #1
	sub     $r2, $r2, $r8
	slli    $r2, $r2, #1
	srli    $r2, $r2, #9
	slli    $r8, $r3, #23
	or      $r2, $r2, $r8
	or      $r0, $r2, $r6

.LF999:
	popm    $r6, $r10
	pop     $lp
	ret5    $lp

.LFspecA:
	bnez    $r3, .Li19
	add     $r2, $r2, $r2
	beqz    $r2, .Li20
#ifdef __NDS32_PERF_EXT__
	clz	$r7, $r2
#else
	pushm	$r0, $r5
	move	$r0, $r2
	bal	__clzsi2
	move	$r7, $r0
	popm	$r0, $r5
#endif
	sub     $r3, $r3, $r7
	sll     $r2, $r2, $r7
	j       .LFlab1
.Li20:
	subri   $r15, $r5, #0xff
	beqzs8  .LFnan
	j       .LFzer
.Li19:
	add     $r8, $r2, $r2
	bnez    $r8, .LFnan
	bnez    $r5, .Li21
	add     $r8, $r4, $r4
	beqz    $r8, .LFnan
.Li21:
	subri   $r15, $r5, #0xff
	bnezs8  .LFinf

.LFspecB:
	bnez    $r5, .Li22
	add     $r4, $r4, $r4
	beqz    $r4, .LFzer
#ifdef __NDS32_PERF_EXT__
	clz	$r7, $r4
#else
	pushm	$r0, $r5
	move	$r0, $r4
	bal	__clzsi2
	move	$r7, $r0
	popm	$r0, $r5
#endif
	sub     $r5, $r5, $r7
	sll     $r4, $r4, $r7
	j       .LFlab2

.LFzer:
	move    $r0, $r6
	j       .LF999
.Li22:
	add     $r8, $r4, $r4
	bnez    $r8, .LFnan

.LFinf:
	move    $r8, #0x7f800000
	or      $r0, $r6, $r8
	j       .LF999

.LFnan:
	move    $r0, #0xffc00000
	j       .LF999

.LFoveund:
	bgtz    $r3, .LFinf
	subri   $r7, $r3, #1
	slti    $r15, $r7, #0x20
	beqzs8  .LFzer
	subri   $r8, $r7, #0x20
	sll     $r3, $r2, $r8
	srl     $r2, $r2, $r7
	beqz    $r3, .Li25
	ori     $r2, $r2, #2
.Li25:
	move    $r3, #0
	addi    $r8, $r2, #0x80
	sltsi   $r15, $r8, #0
	beqzs8  .LFlab8
	move    $r3, #1
	j       .LFlab8
	.size	__mulsf3, .-__mulsf3
#endif /* L_mul_sf */



#ifdef L_mul_df

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
	#define P3L     $r4
	#define P3H     $r5
	#define O1L     $r7
	#define O1H	$r8
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
	#define P3H     $r4
	#define P3L     $r5
	#define O1H     $r7
	#define O1L	$r8
#endif
	.text
	.align	2
	.global	__muldf3
	.type	__muldf3, @function
__muldf3:
	push    $lp
	pushm   $r6, $r10

	slli    $r6, P1H, #1
	srli    $r6, $r6, #21
	slli    P3H, P1H, #11
	srli    $r10, P1L, #21
	or      P3H, P3H, $r10
	slli    P3L, P1L, #11
	move    O1L, #0x80000000
	or      P3H, P3H, O1L
	slli    $r9, P2H, #1
	srli    $r9, $r9, #21
	slli    O1H, P2H, #11
	srli    $r10, P2L, #21
	or      O1H, O1H, $r10
	or      O1H, O1H, O1L
	xor     P1H, P1H, P2H
	and     P1H, P1H, O1L
	slli    O1L, P2L, #11

	addi    $r10, $r6, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LFspecA

.LFlab1:
	addi    $r10, $r9, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LFspecB

.LFlab2:
	addi    $r10, $r9, #0xfffffc02
	add     $r6, $r6, $r10

	move    $r10, $r8
/* This is a 64-bit multiple. */
#ifndef __big_endian__
/* For little endian: ($r9, $r3) is (high, low). */
#ifndef __NDS32_ISA_V3M__
	mulr64	$r8, $r5, $r8
#else
	pushm	$r0, $r5
	move	$r0, $r5
	movi	$r1, #0
	move	$r2, $r8
	movi	$r3, #0
	bal	__muldi3
	movd44	$r8, $r0
	popm	$r0, $r5
#endif
	move    $r3, $r8
#else /* __big_endian__ */
/* For big endain: ($r9, $r2) is (high, low). */
#ifndef __NDS32_ISA_V3M__
	mulr64	$r8, $r4, $r7
#else
	pushm	$r0, $r5
	move	$r1, $r4
	movi	$r0, #0
	move	$r3, $r7
	movi	$r2, #0
	bal	__muldi3
	movd44	$r8, $r0
	popm	$r0, $r5
#endif
	move    $r2, $r9
	move    $r9, $r8
#endif /* __big_endian__ */
	move    $r8, $r10

	move    $r10, P1H
/* This is a 64-bit multiple. */
#ifndef __big_endian__
/* For little endian: ($r0, $r2) is (high, low). */
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r4, $r8
#else
	pushm	$r2, $r5
	move	$r0, $r4
	movi	$r1, #0
	move	$r2, $r8
	movi	$r3, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move    $r2, $r0
	move    $r0, $r1
#else /* __big_endian__ */
/* For big endain: ($r1, $r3) is (high, low). */
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r5, $r7
#else
	pushm	$r2, $r5
	move	$r1, $r5
	movi	$r0, #0
	move	$r3, $r7
	movi	$r2, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move    $r3, $r1
	move    $r1, $r0
#endif /* __big_endian__ */
	move    P1H, $r10

	#ADD(P2H, P1L)
	add     P2H, P2H, P1L
	slt     $r15, P2H, P1L

	#ADDC($r9, $0x0)
	add     $r9, $r9, $r15

	move    $r10, P1H
/* This is a 64-bit multiple. */
#ifndef __big_endian__
/* For little endian: ($r0, $r8) is (high, low). */
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r5, $r7
#else
	pushm	$r2, $r5
	move	$r0, $r5
	movi	$r1, #0
	move	$r2, $r7
	movi	$r3, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move    $r8, $r0
	move    $r0, $r1
#else /* __big_endian__ */
/* For big endian: ($r1, $r7) is (high, low). */
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r4, $r8
#else
	pushm	$r2, $r5
	move	$r1, $r4
	movi	$r0, #0
	move	$r3, $r8
	movi	$r2, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move	$r7, $r1
	move	$r1, $r0
#endif /* __big_endian__ */
	move    P1H, $r10

	#ADD(P2L, O1H)
	add     P2L, P2L, O1H
	slt     $r15, P2L, O1H


	#ADDCC(P2H, P1L)
	beqzs8  .LL29
	add     P2H, P2H, P1L
	slt     $r15, P2H, P1L
	beqzs8  .LL30
	addi    P2H, P2H, #0x1
	j       .LL31
.LL30:
	move    $r15, #1
	add     P2H, P2H, $r15
	slt     $r15, P2H, $r15
	j       .LL31
.LL29:
	add     P2H, P2H, P1L
	slt     $r15, P2H, P1L
.LL31:

	#ADDC($r9, $0x0)
	add     $r9, $r9, $r15

/* This is a 64-bit multiple. */
#ifndef __big_endian__
/* For little endian: ($r8, $r0) is (high, low). */
	move    $r10, $r9
#ifndef __NDS32_ISA_V3M__
	mulr64	$r8, $r4, $r7
#else
	pushm	$r0, $r5
	move	$r0, $r4
	movi	$r1, #0
	move	$r2, $r7
	movi	$r3, #0
	bal	__muldi3
	movd44	$r8, $r0
	popm	$r0, $r5
#endif
	move    $r0, $r8
	move    $r8, $r9
	move    $r9, $r10
#else /* __big_endian__ */
/* For big endian: ($r7, $r1) is (high, low). */
	move	$r10, $r6
#ifndef __NDS32_ISA_V3M__
	mulr64	$r6, $r5, $r8
#else
	pushm	$r0, $r5
	move	$r1, $r5
	movi	$r0, #0
	move	$r3, $r8
	movi	$r2, #0
	bal	__muldi3
	movd44	$r6, $r0
	popm	$r0, $r5
#endif
	move	$r1, $r7
	move	$r7, $r6
	move	$r6, $r10
#endif /* __big_endian__ */

	#ADD(P2L, O1H)
	add     P2L, P2L, O1H
	slt     $r15, P2L, O1H


	#ADDCC(P2H, $0x0)
	beqzs8  .LL34
	add     P2H, P2H, $r15
	slt     $r15, P2H, $r15
.LL34:

	#ADDC($r9, $0x0)
	add     $r9, $r9, $r15
	or      $r10, P1L, P2L
	beqz    $r10, .Li13
	ori     P2H, P2H, #1
.Li13:
	move    P3H, $r9
	move    P3L, P2H
	sltsi   $r15, P3H, #0
	bnezs8  .Li14

	move    $r15, P3L
	add     P3L, P3L, P3L
	slt     $r15, P3L, $r15
	add     P3H, P3H, P3H
	add     P3H, P3H, $r15
	addi    $r6, $r6, #-1
.Li14:
	addi    $r10, $r6, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LFoveund

	#ADD(P3L, $0x400)
	move    $r15, #0x400
	add     P3L, P3L, $r15
	slt     $r15, P3L, $r15


	#ADDCC(P3H, $0x0)
	beqzs8  .LL37
	add     P3H, P3H, $r15
	slt     $r15, P3H, $r15
.LL37:

	#ADDC($r6, $0x0)
	add     $r6, $r6, $r15

.LFlab8:
	srli    $r10, P3L, #11
	andi    $r10, $r10, #1
	sub     P3L, P3L, $r10
	srli    P1L, P3L, #11
	slli    $r10, P3H, #21
	or      P1L, P1L, $r10
	slli    $r10, P3H, #1
	srli    $r10, $r10, #12
	or      P1H, P1H, $r10
	slli    $r10, $r6, #20
	or      P1H, P1H, $r10

.LFret:
.LF999:
	popm    $r6, $r10
	pop     $lp
	ret5    $lp

.LFspecA:
	#ADD(P3L, P3L)
	move    $r15, P3L
	add     P3L, P3L, P3L
	slt     $r15, P3L, $r15

	#ADDC(P3H, P3H)
	add     P3H, P3H, P3H
	add     P3H, P3H, $r15
	bnez    $r6, .Li15
	or      $r10, P3H, P3L
	beqz    $r10, .Li16


	#NORMd($r4, P1L, P2H)
	bnez    P3H, .LL38
	bnez    P3L, .LL39
	move    $r6, #0
	j       .LL40
.LL39:
	move    P3H, P3L
	move    P3L, #0
	move    P1L, #32
	sub     $r6, $r6, P1L
.LL38:
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r0, P3H
#else
	pushm	$r1, P3H
	move	$r0, P3H
	bal	__clzsi2
	popm	$r1, $r5
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r1, $r4
#else
	push	$r0
	pushm	$r2, $r5
	move	$r0, $r4
	bal	__clzsi2
	move	$r1, $r0
	popm	$r2, $r5
	pop	$r0
#endif
#endif /* __big_endian__ */
	beqz    P1L, .LL40
	sub     $r6, $r6, P1L
	subri   P2H, P1L, #32
	srl     P2H, P3L, P2H
	sll     P3L, P3L, P1L
	sll     P3H, P3H, P1L
	or      P3H, P3H, P2H
.LL40:
	#NORMd End

	j       .LFlab1
.Li16:
	subri   $r15, $r9, #0x7ff
	beqzs8  .LFnan
	j       .LFret
.Li15:
	or      $r10, P3H, P3L
	bnez    $r10, .LFnan
	bnez    $r9, .Li17
	slli    $r10, O1H, #1
	or      $r10, $r10, O1L
	beqz    $r10, .LFnan
.Li17:
	subri   $r15, $r9, #0x7ff
	bnezs8  .LFinf

.LFspecB:
	#ADD(O1L, O1L)
	move    $r15, O1L
	add     O1L, O1L, O1L
	slt     $r15, O1L, $r15

	#ADDC(O1H, O1H)
	add     O1H, O1H, O1H
	add     O1H, O1H, $r15
	bnez    $r9, .Li18
	or      $r10, O1H, O1L
	beqz    $r10, .Li19


	#NORMd($r7, P2L, P1L)
	bnez    O1H, .LL41
	bnez    O1L, .LL42
	move    $r9, #0
	j       .LL43
.LL42:
	move    O1H, O1L
	move    O1L, #0
	move    P2L, #32
	sub     $r9, $r9, P2L
.LL41:
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r2, $r8
#else
	pushm	$r0, $r1
	pushm	$r3, $r5
	move	$r0, $r8
	bal	__clzsi2
	move	$r2, $r0
	popm	$r3, $r5
	popm	$r0, $r1
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r3, $r7
#else
	pushm	$r0, $r2
	pushm	$r4, $r5
	move	$r0, $r7
	bal	__clzsi2
	move	$r3, $r0
	popm	$r4, $r5
	popm	$r0, $r2
#endif
#endif /* __big_endian__ */
	beqz    P2L, .LL43
	sub     $r9, $r9, P2L
	subri   P1L, P2L, #32
	srl     P1L, O1L, P1L
	sll     O1L, O1L, P2L
	sll     O1H, O1H, P2L
	or      O1H, O1H, P1L
.LL43:
	#NORMd End

	j       .LFlab2
.Li19:
	move    P1L, #0
	j       .LFret
.Li18:
	or      $r10, O1H, O1L
	bnez    $r10, .LFnan

.LFinf:
	move    $r10, #0x7ff00000
	or      P1H, P1H, $r10
	move    P1L, #0
	j       .LFret

.LFnan:
	move    P1H, #0xfff80000
	move    P1L, #0
	j       .LFret

.LFoveund:
	bgtz    $r6, .LFinf
	subri   P1L, $r6, #1
	move    P2L, #0
.LL44:
	move    $r10, #0x20
	slt     $r15, P1L, $r10
	bnezs8  .LL45
	or      P2L, P2L, P3L
	move    P3L, P3H
	move    P3H, #0
	addi    P1L, P1L, #0xffffffe0
	bnez    P3L, .LL44
.LL45:
	beqz    P1L, .LL46
	move    P2H, P3H
	move    $r10, P3L
	srl     P3L, P3L, P1L
	srl     P3H, P3H, P1L
	subri   P1L, P1L, #0x20
	sll     P2H, P2H, P1L
	or      P3L, P3L, P2H
	sll     $r10, $r10, P1L
	or      P2L, P2L, $r10
	beqz    P2L, .LL46
	ori     P3L, P3L, #1
.LL46:
	#ADD(P3L, $0x400)
	move    $r15, #0x400
	add     P3L, P3L, $r15
	slt     $r15, P3L, $r15

	#ADDC(P3H, $0x0)
	add     P3H, P3H, $r15
	srli    $r6, P3H, #31
	j       .LFlab8
	.size __muldf3, .-__muldf3
#endif /* L_mul_df */



#ifdef L_div_sf

	.text
	.align	2
	.global	__divsf3
	.type	__divsf3, @function
__divsf3:
	push    $lp
	pushm   $r6, $r10

	move    $r7, #0x80000000
	srli    $r4, $r0, #23
	andi    $r4, $r4, #0xff
	srli    $r6, $r1, #23
	andi    $r6, $r6, #0xff
	slli    $r3, $r0, #8
	or      $r3, $r3, $r7
	slli    $r5, $r1, #8
	or      $r5, $r5, $r7
	xor     $r10, $r0, $r1
	and     $r7, $r7, $r10

	addi    $r10, $r4, #-1
	slti    $r15, $r10, #0xfe
	beqzs8  .LGspecA

.LGlab1:
	addi    $r10, $r6, #-1
	slti    $r15, $r10, #0xfe
	beqzs8  .LGspecB

.LGlab2:
	slt     $r15, $r3, $r5
	bnezs8  .Li27
	srli    $r3, $r3, #1
	addi    $r4, $r4, #1
.Li27:
	srli    $r8, $r5, #14
	divr    $r0, $r2, $r3, $r8
	andi    $r9, $r5, #0x3fff
	mul     $r1, $r9, $r0
	slli    $r2, $r2, #14

	#SUB($r2, $r1)
	move    $r15, $r2
	sub     $r2, $r2, $r1
	slt     $r15, $r15, $r2
	beqzs8  .Li28
	addi    $r0, $r0, #-1

	#ADD($r2, $r5)
	add     $r2, $r2, $r5
	slt     $r15, $r2, $r5
.Li28:
	divr    $r3, $r2, $r2, $r8
	mul     $r1, $r9, $r3
	slli    $r2, $r2, #14

	#SUB($r2, $r1)
	move    $r15, $r2
	sub     $r2, $r2, $r1
	slt     $r15, $r15, $r2
	beqzs8  .Li29
	addi    $r3, $r3, #-1

	#ADD($r2, $r5)
	add     $r2, $r2, $r5
	slt     $r15, $r2, $r5
.Li29:
	slli    $r10, $r0, #14
	add     $r3, $r3, $r10
	slli    $r3, $r3, #4
	beqz    $r2, .Li30
	ori     $r3, $r3, #1
.Li30:
	subri   $r10, $r6, #0x7e
	add     $r4, $r4, $r10
	addi    $r10, $r4, #-1
	slti    $r15, $r10, #0xfe
	beqzs8  .LGoveund

.LGlab8:
	#ADD($r3, $0x80)
	move    $r15, #0x80
	add     $r3, $r3, $r15
	slt     $r15, $r3, $r15

	#ADDC($r4, $0x0)
	add     $r4, $r4, $r15
	srli    $r10, $r3, #8
	andi    $r10, $r10, #1
	sub     $r3, $r3, $r10
	slli    $r3, $r3, #1
	srli    $r3, $r3, #9
	slli    $r10, $r4, #23
	or      $r3, $r3, $r10
	or      $r0, $r3, $r7

.LG999:
	popm    $r6, $r10
	pop     $lp
	ret5    $lp

.LGspecA:
	bnez    $r4, .Li31
	add     $r3, $r3, $r3
	beqz    $r3, .Li31
#ifdef __NDS32_PERF_EXT__
	clz	$r8, $r3
#else
	pushm	$r0, $r5
	move	$r0, $r3
	bal	__clzsi2
	move	$r8, $r0
	popm	$r0, $r5
#endif
	sub     $r4, $r4, $r8
	sll     $r3, $r3, $r8
	j       .LGlab1
.Li31:
	bne     $r6, $r4, .Li33
	add     $r10, $r5, $r5
	beqz    $r10, .LGnan
.Li33:
	subri   $r15, $r6, #0xff
	beqzs8  .LGspecB
	beqz    $r4, .LGzer
	add     $r10, $r3, $r3
	bnez    $r10, .LGnan
	j       .LGinf

.LGspecB:
	bnez    $r6, .Li34
	add     $r5, $r5, $r5
	beqz    $r5, .LGinf
#ifdef __NDS32_PERF_EXT__
	clz	$r8, $r5
#else
	pushm	$r0, $r5
	move	$r0, $r5
	bal	__clzsi2
	move	$r8, $r0
	popm	$r0, $r5
#endif
	sub     $r6, $r6, $r8
	sll     $r5, $r5, $r8
	j       .LGlab2
.Li34:
	add     $r10, $r5, $r5
	bnez    $r10, .LGnan

.LGzer:
	move    $r0, $r7
	j       .LG999

.LGoveund:
	bgtz    $r4, .LGinf
	subri   $r8, $r4, #1
	slti    $r15, $r8, #0x20
	beqzs8  .LGzer
	subri   $r10, $r8, #0x20
	sll     $r4, $r3, $r10
	srl     $r3, $r3, $r8
	beqz    $r4, .Li37
	ori     $r3, $r3, #2
.Li37:
	move    $r4, #0
	addi    $r10, $r3, #0x80
	sltsi   $r15, $r10, #0
	beqzs8  .LGlab8
	move    $r4, #1
	j       .LGlab8

.LGinf:
	move    $r10, #0x7f800000
	or      $r0, $r7, $r10
	j       .LG999

.LGnan:
	move    $r0, #0xffc00000
	j       .LG999
	.size	__divsf3, .-__divsf3
#endif /* L_div_sf */



#ifdef L_div_df

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
	#define P3L     $r4
	#define P3H     $r5
	#define O1L     $r7
	#define O1H	$r8
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
	#define P3H     $r4
	#define P3L     $r5
	#define O1H     $r7
	#define O1L	$r8
#endif
	.text
	.align	2
	.global	__divdf3
	.type	__divdf3, @function
__divdf3:
	push    $lp
	pushm   $r6, $r10

	slli    $r6, P1H, #1
	srli    $r6, $r6, #21
	slli    P3H, P1H, #11
	srli    $r10, P1L, #21
	or      P3H, P3H, $r10
	slli    P3L, P1L, #11
	move    O1L, #0x80000000
	or      P3H, P3H, O1L
	slli    $r9, P2H, #1
	srli    $r9, $r9, #21
	slli    O1H, P2H, #11
	srli    $r10, P2L, #21
	or      O1H, O1H, $r10
	or      O1H, O1H, O1L
	xor     P1H, P1H, P2H
	and     P1H, P1H, O1L
	slli    O1L, P2L, #11

	addi    $r10, $r6, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LGspecA

.LGlab1:
	addi    $r10, $r9, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LGspecB

.LGlab2:
	sub     $r6, $r6, $r9
	addi    $r6, $r6, #0x3ff
	srli    P3L, P3L, #1
	slli    $r10, P3H, #31
	or      P3L, P3L, $r10
	srli    P3H, P3H, #1
	srli    $r9, O1H, #16
	divr    P2H, P3H, P3H, $r9
	move    $r10, #0xffff
	and     P2L, O1H, $r10
	mul     P1L, P2L, P2H
	slli    P3H, P3H, #16
	srli    $r10, P3L, #16
	or      P3H, P3H, $r10

	#SUB(P3H, P1L)
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
	beqzs8  .Li20

.Lb21:
	addi    P2H, P2H, #-1
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
	beqzs8  .Lb21
.Li20:
	divr    $r9, P3H, P3H, $r9
	mul     P1L, P2L, $r9
	slli    P3H, P3H, #16
	move    $r15, #0xffff
	and     $r10, P3L, $r15
	or      P3H, P3H, $r10

	#SUB(P3H, P1L)
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
	beqzs8  .Li22

.Lb23:
	addi    $r9, $r9, #-1
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
	beqzs8  .Lb23
.Li22:
	slli    P2H, P2H, #16
	add     P2H, P2H, $r9

/* This is a 64-bit multiple. */
#ifndef __big_endian__
/* For little endian: ($r0, $r9) is (high, low). */
	move    $r10, $r1
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r3, $r7
#else
	pushm	$r2, $r5
	move	$r0, $r3
	movi	$r1, #0
	move	$r2, $r7
	movi	$r3, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move    $r9, $r0
	move    $r0, $r1
	move    $r1, $r10
#else /* __big_endian__ */
/* For big endian: ($r1, $r9) is (high, low). */
	move    $r10, $r0
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r2, $r8
#else
	pushm	$r2, $r5
	move	$r1, $r2
	movi	$r0, #0
	move	$r3, $r8
	movi	$r2, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move    $r9, $r1
	move    $r1, $r0
	move    $r0, $r10
#endif /* __big_endian__ */

	move    P3L, #0

	#SUB(P3L, $r9)
	move    $r15, P3L
	sub     P3L, P3L, $r9
	slt     $r15, $r15, P3L


	#SUBCC(P3H, P1L)
	beqzs8  .LL47
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
	beqzs8  .LL48
	subi333 P3H, P3H, #1
	j       .LL49
.LL48:
	move    $r15, P3H
	subi333 P3H, P3H, #1
	slt     $r15, $r15, P3H
	j       .LL49
.LL47:
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
.LL49:

	beqzs8  .Li24

.LGlab3:
	addi    P2H, P2H, #-1

	#ADD(P3L, O1L)
	add     P3L, P3L, O1L
	slt     $r15, P3L, O1L


	#ADDCC(P3H, O1H)
	beqzs8  .LL50
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
	beqzs8  .LL51
	addi    P3H, P3H, #0x1
	j       .LL52
.LL51:
	move    $r15, #1
	add     P3H, P3H, $r15
	slt     $r15, P3H, $r15
	j       .LL52
.LL50:
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
.LL52:

	beqzs8  .LGlab3
.Li24:
	bne     P3H, O1H, .Li25
	move    P1L, O1L
	move    P3H, P3L
	move    $r9, #0
	move    P2L, $r9
	j       .Le25
.Li25:
	srli    P2L, O1H, #16
	divr    $r9, P3H, P3H, P2L
	move    $r10, #0xffff
	and     $r10, O1H, $r10
	mul     P1L, $r10, $r9
	slli    P3H, P3H, #16
	srli    $r15, P3L, #16
	or      P3H, P3H, $r15

	#SUB(P3H, P1L)
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
	beqzs8  .Li26

.Lb27:
	addi    $r9, $r9, #-1
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
	beqzs8  .Lb27
.Li26:
	divr    P2L, P3H, P3H, P2L
	mul     P1L, $r10, P2L
	slli    P3H, P3H, #16
	move    $r10, #0xffff
	and     $r10, P3L, $r10
	or      P3H, P3H, $r10

	#SUB(P3H, P1L)
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
	beqzs8  .Li28

.Lb29:
	addi    P2L, P2L, #-1
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
	beqzs8  .Lb29
.Li28:
	slli    $r9, $r9, #16
	add     $r9, $r9, P2L

/* This is a 64-bit multiple. */
#ifndef __big_endian__
/* For little endian: ($r0, $r2) is (high, low). */
	move    $r10, $r1
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r9, $r7
#else
	pushm	$r2, $r5
	move	$r0, $r9
	movi	$r1, #0
	move	$r2, $r7
	movi	$r3, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move    $r2, $r0
	move    $r0, $r1
	move    $r1, $r10
#else /* __big_endian__ */
/* For big endian: ($r1, $r3) is (high, low). */
	move	$r10, $r0
#ifndef __NDS32_ISA_V3M__
	mulr64	$r0, $r9, $r8
#else
	pushm	$r2, $r5
	move	$r0, $r9
	movi	$r1, #0
	move	$r2, $r7
	movi	$r3, #0
	bal	__muldi3
	popm	$r2, $r5
#endif
	move	$r3, $r1
	move	$r1, $r0
	move	$r0, $r10
#endif /* __big_endian__ */

.Le25:
	move    P3L, #0

	#SUB(P3L, P2L)
	move    $r15, P3L
	sub     P3L, P3L, P2L
	slt     $r15, $r15, P3L


	#SUBCC(P3H, P1L)
	beqzs8  .LL53
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
	beqzs8  .LL54
	subi333 P3H, P3H, #1
	j       .LL55
.LL54:
	move    $r15, P3H
	subi333 P3H, P3H, #1
	slt     $r15, $r15, P3H
	j       .LL55
.LL53:
	move    $r15, P3H
	sub     P3H, P3H, P1L
	slt     $r15, $r15, P3H
.LL55:

	beqzs8  .Li30

.LGlab4:
	addi    $r9, $r9, #-1

	#ADD(P3L, O1L)
	add     P3L, P3L, O1L
	slt     $r15, P3L, O1L


	#ADDCC(P3H, O1H)
	beqzs8  .LL56
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
	beqzs8  .LL57
	addi    P3H, P3H, #0x1
	j       .LL58
.LL57:
	move    $r15, #1
	add     P3H, P3H, $r15
	slt     $r15, P3H, $r15
	j       .LL58
.LL56:
	add     P3H, P3H, O1H
	slt     $r15, P3H, O1H
.LL58:

	beqzs8  .LGlab4
.Li30:
	sltsi   $r15, P2H, #0
	bnezs8  .Li31

	#ADD($r9, $r9)
	move    $r15, $r9
	add     $r9, $r9, $r9
	slt     $r15, $r9, $r15

	#ADDC(P2H, P2H)
	add     P2H, P2H, P2H
	add     P2H, P2H, $r15
	addi    $r6, $r6, #-1
.Li31:
	or      $r10, P3H, P3L
	beqz    $r10, .Li32
	ori     $r9, $r9, #1
.Li32:
	move    P3H, P2H
	move    P3L, $r9
	addi    $r10, $r6, #-1
	slti    $r15, $r10, #0x7fe
	beqzs8  .LGoveund

	#ADD(P3L, $0x400)
	move    $r15, #0x400
	add     P3L, P3L, $r15
	slt     $r15, P3L, $r15


	#ADDCC(P3H, $0x0)
	beqzs8  .LL61
	add     P3H, P3H, $r15
	slt     $r15, P3H, $r15
.LL61:

	#ADDC($r6, $0x0)
	add     $r6, $r6, $r15

.LGlab8:
	srli    $r10, P3L, #11
	andi    $r10, $r10, #1
	sub     P3L, P3L, $r10
	srli    P1L, P3L, #11
	slli    $r10, P3H, #21
	or      P1L, P1L, $r10
	slli    $r10, P3H, #1
	srli    $r10, $r10, #12
	or      P1H, P1H, $r10
	slli    $r10, $r6, #20
	or      P1H, P1H, $r10

.LGret:
.LG999:
	popm    $r6, $r10
	pop     $lp
	ret5    $lp

.LGoveund:
	bgtz    $r6, .LGinf
	subri   P2H, $r6, #1
	move    P1L, #0
.LL62:
	move    $r10, #0x20
	slt     $r15, P2H, $r10
	bnezs8  .LL63
	or      P1L, P1L, P3L
	move    P3L, P3H
	move    P3H, #0
	addi    P2H, P2H, #0xffffffe0
	bnez    P3L, .LL62
.LL63:
	beqz    P2H, .LL64
	move    P2L, P3H
	move    $r10, P3L
	srl     P3L, P3L, P2H
	srl     P3H, P3H, P2H
	subri   P2H, P2H, #0x20
	sll     P2L, P2L, P2H
	or      P3L, P3L, P2L
	sll     $r10, $r10, P2H
	or      P1L, P1L, $r10
	beqz    P1L, .LL64
	ori     P3L, P3L, #1
.LL64:
	#ADD(P3L, $0x400)
	move    $r15, #0x400
	add     P3L, P3L, $r15
	slt     $r15, P3L, $r15

	#ADDC(P3H, $0x0)
	add     P3H, P3H, $r15
	srli    $r6, P3H, #31
	j       .LGlab8

.LGspecA:
	#ADD(P3L, P3L)
	move    $r15, P3L
	add     P3L, P3L, P3L
	slt     $r15, P3L, $r15

	#ADDC(P3H, P3H)
	add     P3H, P3H, P3H
	add     P3H, P3H, $r15
	bnez    $r6, .Li33
	or      $r10, P3H, P3L
	beqz    $r10, .Li33


	#NORMd($r4, P2H, P2L)
	bnez    P3H, .LL65
	bnez    P3L, .LL66
	move    $r6, #0
	j       .LL67
.LL66:
	move    P3H, P3L
	move    P3L, #0
	move    P2H, #32
	sub     $r6, $r6, P2H
.LL65:
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r3, $r5
#else
	pushm	$r0, $r2
	pushm	$r4, $r5
	move	$r0, $r5
	bal	__clzsi2
	move	$r3, $r0
	popm	$r4, $r5
	popm	$r0, $r2
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r2, $r4
#else
	pushm	$r0, $r1
	pushm	$r3, $r5
	move	$r0, $r4
	bal	__clzsi2
	move	$r2, $r0
	popm	$r3, $r5
	popm	$r0, $r1
#endif
#endif /* __big_endian_ */
	beqz    P2H, .LL67
	sub     $r6, $r6, P2H
	subri   P2L, P2H, #32
	srl     P2L, P3L, P2L
	sll     P3L, P3L, P2H
	sll     P3H, P3H, P2H
	or      P3H, P3H, P2L
.LL67:
	#NORMd End

	j       .LGlab1
.Li33:
	bne     $r6, $r9, .Li35
	slli    $r10, O1H, #1
	or      $r10, $r10, O1L
	beqz    $r10, .LGnan
.Li35:
	subri   $r15, $r9, #0x7ff
	beqzs8  .LGspecB
	beqz    $r6, .LGret
	or      $r10, P3H, P3L
	bnez    $r10, .LGnan

.LGinf:
	move    $r10, #0x7ff00000
	or      P1H, P1H, $r10
	move    P1L, #0
	j       .LGret

.LGspecB:
	#ADD(O1L, O1L)
	move    $r15, O1L
	add     O1L, O1L, O1L
	slt     $r15, O1L, $r15

	#ADDC(O1H, O1H)
	add     O1H, O1H, O1H
	add     O1H, O1H, $r15
	bnez    $r9, .Li36
	or      $r10, O1H, O1L
	beqz    $r10, .LGinf


	#NORMd($r7, P2H, P2L)
	bnez    O1H, .LL68
	bnez    O1L, .LL69
	move    $r9, #0
	j       .LL70
.LL69:
	move    O1H, O1L
	move    O1L, #0
	move    P2H, #32
	sub     $r9, $r9, P2H
.LL68:
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r3, $r8
#else
	pushm	$r0, $r2
	pushm	$r4, $r5
	move	$r0, $r8
	bal	__clzsi2
	move	$r3, $r0
	popm	$r4, $r5
	popm	$r0, $r2
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r2, $r7
#else
	pushm	$r0, $r1
	pushm	$r3, $r5
	move	$r0, $r7
	bal	__clzsi2
	move	$r2, $r0
	popm	$r3, $r5
	popm	$r0, $r1
#endif
#endif /* __big_endian__ */
	beqz    P2H, .LL70
	sub     $r9, $r9, P2H
	subri   P2L, P2H, #32
	srl     P2L, O1L, P2L
	sll     O1L, O1L, P2H
	sll     O1H, O1H, P2H
	or      O1H, O1H, P2L
.LL70:
	#NORMd End

	j       .LGlab2
.Li36:
	or      $r10, O1H, O1L
	beqz    $r10, .Li38

.LGnan:
	move    P1H, #0xfff80000
.Li38:
	move    P1L, #0
	j       .LGret
	.size __divdf3, .-__divdf3
#endif /* L_div_df */



#ifdef L_negate_sf

	.text
	.align	2
	.global	__negsf2
	.type	__negsf2, @function
__negsf2:
	push    $lp

	move    $r1, #0x80000000
	xor     $r0, $r0, $r1

.LN999:
	pop     $lp
	ret5    $lp
	.size __negsf2, .-__negsf2
#endif /* L_negate_sf */



#ifdef L_negate_df

#ifndef __big_endian__
	#define P1H     $r1
#else
	#define P1H     $r0
#endif
	.text
	.align	2
	.global	__negdf2
	.type	__negdf2, @function
__negdf2:
	push    $lp

	move    $r2, #0x80000000
	xor     P1H, P1H, $r2

.LP999:
	pop     $lp
	ret5    $lp
	.size __negdf2, .-__negdf2
#endif /* L_negate_df */



#ifdef L_sf_to_df

#ifndef __big_endian__
	#define O1L     $r1
	#define O1H     $r2
#else
	#define O1H     $r1
	#define O1L     $r2
#endif
	.text
	.align	2
	.global	__extendsfdf2
	.type	__extendsfdf2, @function
__extendsfdf2:
	push    $lp

	srli    $r3, $r0, #23
	andi    $r3, $r3, #0xff
	move    $r5, #0x80000000
	and     O1H, $r0, $r5
	addi    $r5, $r3, #-1
	slti    $r15, $r5, #0xfe
	beqzs8  .LJspec

.LJlab1:
	addi    $r3, $r3, #0x380
	slli    $r5, $r0, #9
	srli    $r5, $r5, #12
	or      O1H, O1H, $r5
	slli    O1L, $r0, #29

.LJret:
	slli    $r5, $r3, #20
	or      O1H, O1H, $r5
	move    $r0, $r1
	move    $r1, $r2

.LJ999:
	pop     $lp
	ret5    $lp

.LJspec:
	move    O1L, #0
	add     $r0, $r0, $r0
	beqz    $r0, .LJret
	bnez    $r3, .Li42

.Lb43:
	addi    $r3, $r3, #-1
	add     $r0, $r0, $r0
	move    $r5, #0x800000
	slt     $r15, $r0, $r5
	bnezs8  .Lb43
	j       .LJlab1
.Li42:
	move    $r3, #0x7ff
	move    $r5, #0xff000000
	slt     $r15, $r5, $r0
	beqzs8  .LJret
	move    O1H, #0xfff80000
	j       .LJret
	.size __extendsfdf2, .-__extendsfdf2
#endif /* L_sf_to_df */



#ifdef L_df_to_sf

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
#endif
	.text
	.align	2
	.global	__truncdfsf2
	.type	__truncdfsf2, @function
__truncdfsf2:
	push    $lp
	pushm   $r6, $r8

	slli    P2H, P1H, #11
	srli    $r7, P1L, #21
	or      P2H, P2H, $r7
	slli    P2L, P1L, #11
	move    $r7, #0x80000000
	or      P2H, P2H, $r7
	and     $r5, P1H, $r7
	slli    $r4, P1H, #1
	srli    $r4, $r4, #21
	addi    $r4, $r4, #0xfffffc80
	addi    $r7, $r4, #-1
	slti    $r15, $r7, #0xfe
	beqzs8  .LKspec

.LKlab1:
	beqz    P2L, .Li45
	ori     P2H, P2H, #1
.Li45:
	#ADD(P2H, $0x80)
	move    $r15, #0x80
	add     P2H, P2H, $r15
	slt     $r15, P2H, $r15

	#ADDC($r4, $0x0)
	add     $r4, $r4, $r15
	srli    $r7, P2H, #8
	andi    $r7, $r7, #1
	sub     P2H, P2H, $r7
	slli    P2H, P2H, #1
	srli    P2H, P2H, #9
	slli    $r7, $r4, #23
	or      P2H, P2H, $r7
	or      $r0, P2H, $r5

.LK999:
	popm    $r6, $r8
	pop     $lp
	ret5    $lp

.LKspec:
	subri   $r15, $r4, #0x47f
	bnezs8  .Li46
	slli    $r7, P2H, #1
	or      $r7, $r7, P2L
	beqz    $r7, .Li46
	move    $r0, #0xffc00000
	j       .LK999
.Li46:
	sltsi   $r15, $r4, #0xff
	bnezs8  .Li48
	move    $r7, #0x7f800000
	or      $r0, $r5, $r7
	j       .LK999
.Li48:
	subri   $r6, $r4, #1
	move    $r7, #0x20
	slt     $r15, $r6, $r7
	bnezs8  .Li49
	move    $r0, $r5
	j       .LK999
.Li49:
	subri   $r8, $r6, #0x20
	sll     $r7, P2H, $r8
	or      P2L, P2L, $r7
	srl     P2H, P2H, $r6
	move    $r4, #0
	move    $r7, #0x80000000
	or      P2H, P2H, $r7
	j       .LKlab1
	.size __truncdfsf2, .-__truncdfsf2
#endif /* L_df_to_sf */



#ifdef L_df_to_si

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
#else
	#define P1H     $r0
	#define P1L     $r1
#endif
	.global	__fixdfsi
	.type	__fixdfsi, @function
__fixdfsi:
	push    $lp
	pushm   $r6, $r6

	slli    $r3, P1H, #11
	srli    $r6, P1L, #21
	or      $r3, $r3, $r6
	move    $r6, #0x80000000
	or      $r3, $r3, $r6
	slli    $r6, P1H, #1
	srli    $r6, $r6, #21
	subri   $r2, $r6, #0x41e
	blez    $r2, .LLnaninf
	move    $r6, #0x20
	slt     $r15, $r2, $r6
	bnezs8  .LL72
	move    $r3, #0
.LL72:
	srl     $r3, $r3, $r2
	sltsi   $r15, P1H, #0
	beqzs8  .Li50
	subri   $r3, $r3, #0
.Li50:
	move    $r0, $r3

.LL999:
	popm    $r6, $r6
	pop     $lp
	ret5    $lp

.LLnaninf:
	beqz    P1L, .Li51
	ori     P1H, P1H, #1
.Li51:
	move    $r6, #0x7ff00000
	slt     $r15, $r6, P1H
	beqzs8  .Li52
	move    $r0, #0x80000000
	j       .LL999
.Li52:
	move    $r0, #0x7fffffff
	j       .LL999
	.size __fixdfsi, .-__fixdfsi
#endif /* L_df_to_si */



#ifdef L_fixsfdi

#ifndef __big_endian__
	#define O1L     $r1
	#define O1H     $r2
#else
	#define O1H     $r1
	#define O1L     $r2
#endif
	.text
	.align	2
	.global	__fixsfdi
	.type	__fixsfdi, @function
__fixsfdi:
	push    $lp

	srli    $r3, $r0, #23
	andi    $r3, $r3, #0xff
	slli    O1H, $r0, #8
	move    $r5, #0x80000000
	or      O1H, O1H, $r5
	move    O1L, #0
	sltsi   $r15, $r3, #0xbe
	beqzs8  .LCinfnan
	subri   $r3, $r3, #0xbe
.LL8:
	move    $r5, #0x20
	slt     $r15, $r3, $r5
	bnezs8  .LL9
	move    O1L, O1H
	move    O1H, #0
	addi    $r3, $r3, #0xffffffe0
	bnez    O1L, .LL8
.LL9:
	beqz    $r3, .LL10
	move    $r4, O1H
	srl     O1L, O1L, $r3
	srl     O1H, O1H, $r3
	subri   $r3, $r3, #0x20
	sll     $r4, $r4, $r3
	or      O1L, O1L, $r4
.LL10:
	sltsi   $r15, $r0, #0
	beqzs8  .LCret

	subri   O1H, O1H, #0
	beqz    O1L, .LL11
	subri   O1L, O1L, #0
	subi45  O1H, #1
.LL11:

.LCret:
	move    $r0, $r1
	move    $r1, $r2

.LC999:
	pop     $lp
	ret5    $lp

.LCinfnan:
	sltsi   $r15, $r0, #0
	bnezs8  .LCret3
	subri   $r15, $r3, #0xff
	bnezs8  .Li7
	slli    $r5, O1H, #1
	beqz    $r5, .Li7

.LCret3:
	move    O1H, #0x80000000
	j       .LCret
.Li7:
	move    O1H, #0x7fffffff
	move    O1L, #-1
	j       .LCret
	.size	__fixsfdi, .-__fixsfdi
#endif /* L_fixsfdi */



#ifdef L_fixdfdi

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define O1L     $r3
	#define O1H     $r4
#else
	#define P1H     $r0
	#define P1L     $r1
	#define O1H     $r3
	#define O1L     $r4
#endif
	.text
	.align	2
	.global	__fixdfdi
	.type	__fixdfdi, @function
__fixdfdi:
	push    $lp
	pushm   $r6, $r6

	slli    $r5, P1H, #1
	srli    $r5, $r5, #21
	slli    O1H, P1H, #11
	srli    $r6, P1L, #21
	or      O1H, O1H, $r6
	slli    O1L, P1L, #11
	move    $r6, #0x80000000
	or      O1H, O1H, $r6
	slti    $r15, $r5, #0x43e
	beqzs8  .LCnaninf
	subri   $r2, $r5, #0x43e
.LL14:
	move    $r6, #0x20
	slt     $r15, $r2, $r6
	bnezs8  .LL15
	move    O1L, O1H
	move    O1H, #0
	addi    $r2, $r2, #0xffffffe0
	bnez    O1L, .LL14
.LL15:
	beqz    $r2, .LL16
	move    P1L, O1H
	srl     O1L, O1L, $r2
	srl     O1H, O1H, $r2
	subri   $r2, $r2, #0x20
	sll     P1L, P1L, $r2
	or      O1L, O1L, P1L
.LL16:
	sltsi   $r15, P1H, #0
	beqzs8  .LCret

	subri   O1H, O1H, #0
	beqz    O1L, .LL17
	subri   O1L, O1L, #0
	subi45  O1H, #1
.LL17:

.LCret:
	move    P1L, O1L
	move    P1H, O1H

.LC999:
	popm    $r6, $r6
	pop     $lp
	ret5    $lp

.LCnaninf:
	sltsi   $r15, P1H, #0
	bnezs8  .LCret3
	subri   $r15, $r5, #0x7ff
	bnezs8  .Li5
	slli    $r6, O1H, #1
	or      $r6, $r6, O1L
	beqz    $r6, .Li5

.LCret3:
	move    O1H, #0x80000000
	move    O1L, #0
	j       .LCret
.Li5:
	move    O1H, #0x7fffffff
	move    O1L, #-1
	j       .LCret
	.size	__fixdfdi, .-__fixdfdi
#endif /* L_fixdfdi */



#ifdef L_fixunssfsi

	.global	__fixunssfsi
	.type	__fixunssfsi, @function
__fixunssfsi:
	push    $lp

	slli    $r1, $r0, #8
	move    $r3, #0x80000000
	or      $r1, $r1, $r3
	srli    $r3, $r0, #23
	andi    $r3, $r3, #0xff
	subri   $r2, $r3, #0x9e
	sltsi   $r15, $r2, #0
	bnezs8  .LLspec
	sltsi   $r15, $r2, #0x20
	bnezs8  .Li45
	move    $r0, #0
	j       .LL999
.Li45:
	srl     $r1, $r1, $r2
	sltsi   $r15, $r0, #0
	beqzs8  .Li46
	subri   $r1, $r1, #0
.Li46:
	move    $r0, $r1

.LL999:
	pop     $lp
	ret5    $lp

.LLspec:
	move    $r3, #0x7f800000
	slt     $r15, $r3, $r0
	beqzs8  .Li47
	move    $r0, #0x80000000
	j       .LL999
.Li47:
	move    $r0, #-1
	j       .LL999
	.size	__fixunssfsi, .-__fixunssfsi
#endif /* L_fixunssfsi */



#ifdef L_fixunsdfsi

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
#else
	#define P1H     $r0
	#define P1L     $r1
#endif
	.text
	.align	2
	.global	__fixunsdfsi
	.type	__fixunsdfsi, @function
__fixunsdfsi:
	push    $lp
	pushm   $r6, $r6

	slli    $r3, P1H, #11
	srli    $r6, P1L, #21
	or      $r3, $r3, $r6
	move    $r6, #0x80000000
	or      $r3, $r3, $r6
	slli    $r6, P1H, #1
	srli    $r6, $r6, #21
	subri   $r2, $r6, #0x41e
	sltsi   $r15, $r2, #0
	bnezs8  .LNnaninf
	move    $r6, #0x20
	slt     $r15, $r2, $r6
	bnezs8  .LL73
	move    $r3, #0
.LL73:
	srl     $r3, $r3, $r2
	sltsi   $r15, P1H, #0
	beqzs8  .Li53
	subri   $r3, $r3, #0
.Li53:
	move    $r0, $r3

.LN999:
	popm    $r6, $r6
	pop     $lp
	ret5    $lp

.LNnaninf:
	beqz    P1L, .Li54
	ori     P1H, P1H, #1
.Li54:
	move    $r6, #0x7ff00000
	slt     $r15, $r6, P1H
	beqzs8  .Li55
	move    $r0, #0x80000000
	j       .LN999
.Li55:
	move    $r0, #-1
	j       .LN999
	.size __fixunsdfsi, .-__fixunsdfsi
#endif /* L_fixunsdfsi */



#ifdef L_fixunssfdi

#ifndef __big_endian__
	#define O1L     $r1
	#define O1H     $r2
#else
	#define O1H     $r1
	#define O1L     $r2
#endif
	.text
	.align	2
	.global	__fixunssfdi
	.type	__fixunssfdi, @function
__fixunssfdi:
	push    $lp

	srli    $r3, $r0, #23
	andi    $r3, $r3, #0xff
	slli    O1H, $r0, #8
	move    $r5, #0x80000000
	or      O1H, O1H, $r5
	move    O1L, #0
	sltsi   $r15, $r3, #0xbe
	beqzs8  .LDinfnan
	subri   $r3, $r3, #0xbe
.LL12:
	move    $r5, #0x20
	slt     $r15, $r3, $r5
	bnezs8  .LL13
	move    O1L, O1H
	move    O1H, #0
	addi    $r3, $r3, #0xffffffe0
	bnez    O1L, .LL12
.LL13:
	beqz    $r3, .LL14
	move    $r4, O1H
	srl     O1L, O1L, $r3
	srl     O1H, O1H, $r3
	subri   $r3, $r3, #0x20
	sll     $r4, $r4, $r3
	or      O1L, O1L, $r4
.LL14:
	sltsi   $r15, $r0, #0
	beqzs8  .LDret

	subri   O1H, O1H, #0
	beqz    O1L, .LL15
	subri   O1L, O1L, #0
	subi45  O1H, #1
.LL15:

.LDret:
	move    $r0, $r1
	move    $r1, $r2

.LD999:
	pop     $lp
	ret5    $lp

.LDinfnan:
	move    O1H, #0x80000000
	move    O1L, #0
	j       .LDret
	.size	__fixunssfdi, .-__fixunssfdi
#endif /* L_fixunssfdi */



#ifdef L_fixunsdfdi

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define O1L     $r3
	#define O1H     $r4
#else
	#define P1H     $r0
	#define P1L     $r1
	#define O1H     $r3
	#define O1L     $r4
#endif
	.text
	.align	2
	.global	__fixunsdfdi
	.type	__fixunsdfdi, @function
__fixunsdfdi:
	push    $lp
	pushm   $r6, $r6

	slli    $r5, P1H, #1
	srli    $r5, $r5, #21
	slli    O1H, P1H, #11
	srli    $r6, P1L, #21
	or      O1H, O1H, $r6
	slli    O1L, P1L, #11
	move    $r6, #0x80000000
	or      O1H, O1H, $r6
	slti    $r15, $r5, #0x43e
	beqzs8  .LDnaninf
	subri   $r2, $r5, #0x43e
.LL18:
	move    $r6, #0x20
	slt     $r15, $r2, $r6
	bnezs8  .LL19
	move    O1L, O1H
	move    O1H, #0
	addi    $r2, $r2, #0xffffffe0
	bnez    O1L, .LL18
.LL19:
	beqz    $r2, .LL20
	move    P1L, O1H
	srl     O1L, O1L, $r2
	srl     O1H, O1H, $r2
	subri   $r2, $r2, #0x20
	sll     P1L, P1L, $r2
	or      O1L, O1L, P1L
.LL20:
	sltsi   $r15, P1H, #0
	beqzs8  .LDret

	subri   O1H, O1H, #0
	beqz    O1L, .LL21
	subri   O1L, O1L, #0
	subi45  O1H, #1
.LL21:

.LDret:
	move    P1L, O1L
	move    P1H, O1H

.LD999:
	popm    $r6, $r6
	pop     $lp
	ret5    $lp

.LDnaninf:
	move    O1H, #0x80000000
	move    O1L, #0
	j       .LDret
	.size	__fixunsdfdi, .-__fixunsdfdi
#endif /* L_fixunsdfdi */



#ifdef L_si_to_sf

	.text
	.align	2
	.global	__floatsisf
	.type	__floatsisf, @function
__floatsisf:
	push    $lp

	move    $r4, #0x80000000
	and     $r2, $r0, $r4
	beqz    $r0, .Li39
	sltsi   $r15, $r0, #0
	beqzs8  .Li40
	subri   $r0, $r0, #0
.Li40:
	move    $r1, #0x9e
#ifdef __NDS32_PERF_EXT__
	clz	$r3, $r0
#else
	pushm	$r0, $r2
	pushm	$r4, $r5
	bal	__clzsi2
	move	$r3, $r0
	popm	$r4, $r5
	popm	$r0, $r2
#endif
	sub     $r1, $r1, $r3
	sll     $r0, $r0, $r3

	#ADD($r0, $0x80)
	move    $r15, #0x80
	add     $r0, $r0, $r15
	slt     $r15, $r0, $r15

	#ADDC($r1, $0x0)
	add     $r1, $r1, $r15
	srai    $r4, $r0, #8
	andi    $r4, $r4, #1
	sub     $r0, $r0, $r4
	slli    $r0, $r0, #1
	srli    $r0, $r0, #9
	slli    $r4, $r1, #23
	or      $r0, $r0, $r4
.Li39:
	or      $r0, $r0, $r2

.LH999:
	pop     $lp
	ret5    $lp
	.size	__floatsisf, .-__floatsisf
#endif /* L_si_to_sf */



#ifdef L_si_to_df

#ifndef __big_endian__
	#define O1L     $r1
	#define O1H     $r2
	#define O2L     $r4
	#define O2H	$r5
#else
	#define O1H     $r1
	#define O1L     $r2
	#define O2H     $r4
	#define O2L	$r5
#endif
	.text
	.align	2
	.global	__floatsidf
	.type	__floatsidf, @function
__floatsidf:
	push    $lp
	pushm   $r6, $r6

	move    O1L, #0
	move    O2H, O1L
	move    $r3, O1L
	move    O1H, $r0
	beqz    O1H, .Li39
	sltsi   $r15, O1H, #0
	beqzs8  .Li40
	move    O2H, #0x80000000

	subri   O1H, O1H, #0
	beqz    O1L, .LL71
	subri   O1L, O1L, #0
	subi45  O1H, #1
.LL71:
.Li40:
	move    $r3, #0x41e
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r4, $r2
#else
	pushm	$r0, $r3
	push	$r5
	move	$r0, $r2
	bal	__clzsi2
	move	$r4, $r0
	pop	$r5
	popm	$r0, $r3
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r5, $r1
#else
	pushm	$r0, $r4
	move	$r0, $r1
	bal	__clzsi2
	move	$r5, $r0
	popm	$r0, $r4
#endif
#endif /* __big_endian__ */
	sub     $r3, $r3, O2L
	sll     O1H, O1H, O2L
.Li39:
	srli    O2L, O1L, #11
	slli    $r6, O1H, #21
	or      O2L, O2L, $r6
	slli    $r6, O1H, #1
	srli    $r6, $r6, #12
	or      O2H, O2H, $r6
	slli    $r6, $r3, #20
	or      O2H, O2H, $r6
	move    $r0, $r4
	move    $r1, $r5

.LH999:
	popm    $r6, $r6
	pop     $lp
	ret5    $lp
	.size __floatsidf, .-__floatsidf
#endif /* L_si_to_df */



#ifdef L_floatdisf

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
#endif
	.text
	.align	2
	.global	__floatdisf
	.type	__floatdisf, @function
__floatdisf:
	push    $lp
	pushm   $r6, $r7

	move    $r7, #0x80000000
	and     $r5, P1H, $r7
	move    P2H, P1H
	move    P2L, P1L
	or      $r7, P1H, P1L
	beqz    $r7, .Li1
	sltsi   $r15, P1H, #0
	beqzs8  .Li2

	subri   P2H, P2H, #0
	beqz    P2L, .LL1
	subri   P2L, P2L, #0
	subi45  P2H, #1
.LL1:
.Li2:
	move    $r4, #0xbe


	#NORMd($r2, $r6, P1L)
	bnez    P2H, .LL2
	bnez    P2L, .LL3
	move    $r4, #0
	j       .LL4
.LL3:
	move    P2H, P2L
	move    P2L, #0
	move    $r6, #32
	sub     $r4, $r4, $r6
.LL2:
#ifdef __NDS32_PERF_EXT__
	clz	$r6, P2H
#else
	pushm	$r0, $r5
	move	$r0, P2H
	bal	__clzsi2
	move	$r6, $r0
	popm	$r0, $r5
#endif
	beqz    $r6, .LL4
	sub     $r4, $r4, $r6
	subri   P1L, $r6, #32
	srl     P1L, P2L, P1L
	sll     P2L, P2L, $r6
	sll     P2H, P2H, $r6
	or      P2H, P2H, P1L
.LL4:
	#NORMd End

	beqz    P2L, .Li3
	ori     P2H, P2H, #1
.Li3:
	#ADD(P2H, $0x80)
	move    $r15, #0x80
	add     P2H, P2H, $r15
	slt     $r15, P2H, $r15

	#ADDC($r4, $0x0)
	add     $r4, $r4, $r15
	srli    $r7, P2H, #8
	andi    $r7, $r7, #1
	sub     P2H, P2H, $r7
	slli    P2H, P2H, #1
	srli    P2H, P2H, #9
	slli    $r7, $r4, #23
	or      P2H, P2H, $r7
.Li1:
	or      $r0, P2H, $r5

.LA999:
	popm    $r6, $r7
	pop     $lp
	ret5    $lp
	.size	__floatdisf, .-__floatdisf
#endif /* L_floatdisf */



#ifdef L_floatdidf

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
	#define O1L     $r5
	#define O1H     $r6
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
	#define O1H     $r5
	#define O1L     $r6
#endif
	.text
	.align	2
	.global	__floatdidf
	.type	__floatdidf, @function
__floatdidf:
	push    $lp
	pushm   $r6, $r8

	move    $r4, #0
	move    $r7, $r4
	move    P2H, P1H
	move    P2L, P1L
	or      $r8, P1H, P1L
	beqz    $r8, .Li1
	move    $r4, #0x43e
	sltsi   $r15, P1H, #0
	beqzs8  .Li2
	move    $r7, #0x80000000

	subri   P2H, P2H, #0
	beqz    P2L, .LL1
	subri   P2L, P2L, #0
	subi45  P2H, #1
.LL1:

.Li2:
	#NORMd($r2, O1H, O1L)
	bnez    P2H, .LL2
	bnez    P2L, .LL3
	move    $r4, #0
	j       .LL4
.LL3:
	move    P2H, P2L
	move    P2L, #0
	move    O1H, #32
	sub     $r4, $r4, O1H
.LL2:
#ifdef __NDS32_PERF_EXT__
	clz	O1H, P2H
#else /* not __NDS32_PERF_EXT__ */
/*
  Replace clz with function call.
	clz     O1H, P2H
  EL:	clz     $r6, $r3
  EB:	clz	$r5, $r2
*/
#ifndef __big_endian__
	pushm	$r0, $r5
	move	$r0, $r3
	bal	__clzsi2
	move	$r6, $r0
	popm	$r0, $r5
#else
	pushm	$r0, $r4
	move	$r0, $r2
	bal	__clzsi2
	move	$r5, $r0
	popm	$r0, $r4
#endif
#endif /* not __NDS32_PERF_EXT__ */
	beqz    O1H, .LL4
	sub     $r4, $r4, O1H
	subri   O1L, O1H, #32
	srl     O1L, P2L, O1L
	sll     P2L, P2L, O1H
	sll     P2H, P2H, O1H
	or      P2H, P2H, O1L
.LL4:
	#NORMd End

	#ADD(P2L, $0x400)
	move    $r15, #0x400
	add     P2L, P2L, $r15
	slt     $r15, P2L, $r15


	#ADDCC(P2H, $0x0)
	beqzs8  .LL7
	add     P2H, P2H, $r15
	slt     $r15, P2H, $r15
.LL7:

	#ADDC($r4, $0x0)
	add     $r4, $r4, $r15
	srli    $r8, P2L, #11
	andi    $r8, $r8, #1
	sub     P2L, P2L, $r8
.Li1:
	srli    O1L, P2L, #11
	slli    $r8, P2H, #21
	or      O1L, O1L, $r8
	slli    O1H, P2H, #1
	srli    O1H, O1H, #12
	slli    $r8, $r4, #20
	or      O1H, O1H, $r8
	or      O1H, O1H, $r7
	move    P1L, O1L
	move    P1H, O1H

.LA999:
	popm    $r6, $r8
	pop     $lp
	ret5    $lp
	.size	__floatdidf, .-__floatdidf
#endif /* L_floatdidf */



#ifdef L_floatunsisf

	.text
	.align	2
	.global	__floatunsisf
	.type	__floatunsisf, @function
__floatunsisf:
	push    $lp

	beqz    $r0, .Li41
	move    $r2, #0x9e
#ifdef __NDS32_PERF_EXT__
	clz	$r1, $r0
#else
	push	$r0
	pushm	$r2, $r5
	bal	__clzsi2
	move	$r1, $r0
	popm	$r2, $r5
	pop	$r0
#endif

	sub     $r2, $r2, $r1
	sll     $r0, $r0, $r1

	#ADD($r0, $0x80)
	move    $r15, #0x80
	add     $r0, $r0, $r15
	slt     $r15, $r0, $r15

	#ADDC($r2, $0x0)
	add     $r2, $r2, $r15
	srli    $r3, $r0, #8
	andi    $r3, $r3, #1
	sub     $r0, $r0, $r3
	slli    $r0, $r0, #1
	srli    $r0, $r0, #9
	slli    $r3, $r2, #23
	or      $r0, $r0, $r3

.Li41:
.LI999:
	pop     $lp
	ret5    $lp
	.size	__floatunsisf, .-__floatunsisf
#endif /* L_floatunsisf */



#ifdef L_floatunsidf

#ifndef __big_endian__
	#define O1L     $r1
	#define O1H     $r2
	#define O2L     $r4
	#define O2H	$r5
#else
	#define O1H     $r1
	#define O1L     $r2
	#define O2H     $r4
	#define O2L	$r5
#endif
	.text
	.align	2
	.global	__floatunsidf
	.type	__floatunsidf, @function
__floatunsidf:
	push    $lp
	pushm   $r6, $r6

	move    O1L, #0
	move    $r3, O1L
	move    O1H, $r0
	beqz    O1H, .Li41
	move    $r3, #0x41e
#ifndef __big_endian__
#ifdef __NDS32_PERF_EXT__
	clz	$r5, $r2
#else
	pushm	$r0, $r4
	move	$r0, $r2
	bal	__clzsi2
	move	$r5, $r0
	popm	$r0, $r4
#endif
#else /* __big_endian__ */
#ifdef __NDS32_PERF_EXT__
	clz	$r4, $r1
#else
	pushm	$r0, $r3
	push	$r5
	move	$r0, $r1
	bal	__clzsi2
	move	$r4, $r0
	pop	$r5
	popm	$r0, $r3
#endif
#endif /* __big_endian__ */
	sub     $r3, $r3, O2H
	sll     O1H, O1H, O2H
.Li41:
	srli    O2L, O1L, #11
	slli    $r6, O1H, #21
	or      O2L, O2L, $r6
	slli    O2H, O1H, #1
	srli    O2H, O2H, #12
	slli    $r6, $r3, #20
	or      O2H, O2H, $r6
	move    $r0, $r4
	move    $r1, $r5

.LI999:
	popm    $r6, $r6
	pop     $lp
	ret5    $lp
	.size __floatunsidf, .-__floatunsidf
#endif /* L_floatunsidf */



#ifdef L_floatundisf

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
#endif
	.text
	.align	2
	.global	__floatundisf
	.type	__floatundisf, @function
__floatundisf:
	push    $lp
	pushm   $r6, $r6

	move    P2H, P1H
	move    P2L, P1L
	or      $r6, P1H, P1L
	beqz    $r6, .Li4
	move    $r4, #0xbe


	#NORMd($r2, $r5, P1L)
	bnez    P2H, .LL5
	bnez    P2L, .LL6
	move    $r4, #0
	j       .LL7
.LL6:
	move    P2H, P2L
	move    P2L, #0
	move    $r5, #32
	sub     $r4, $r4, $r5
.LL5:
#ifdef __NDS32_PERF_EXT__
	clz	$r5, P2H
#else
	pushm	$r0, $r4
	move	$r0, P2H
	bal	__clzsi2
	move	$r5, $r0
	popm	$r0, $r4
#endif
	beqz    $r5, .LL7
	sub     $r4, $r4, $r5
	subri   P1L, $r5, #32
	srl     P1L, P2L, P1L
	sll     P2L, P2L, $r5
	sll     P2H, P2H, $r5
	or      P2H, P2H, P1L
.LL7:
	#NORMd End

	beqz    P2L, .Li5
	ori     P2H, P2H, #1
.Li5:
	#ADD(P2H, $0x80)
	move    $r15, #0x80
	add     P2H, P2H, $r15
	slt     $r15, P2H, $r15

	#ADDC($r4, $0x0)
	add     $r4, $r4, $r15
	srli    $r6, P2H, #8
	andi    $r6, $r6, #1
	sub     P2H, P2H, $r6
	slli    P2H, P2H, #1
	srli    P2H, P2H, #9
	slli    $r6, $r4, #23
	or      P2H, P2H, $r6
.Li4:
	move    $r0, P2H

.LB999:
	popm    $r6, $r6
	pop     $lp
	ret5    $lp
	.size	__floatundisf, .-__floatundisf
#endif /* L_floatundisf */



#ifdef L_floatundidf

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
	#define O1L     $r5
	#define O1H     $r6
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
	#define O1H     $r5
	#define O1L     $r6
#endif
	.text
	.align	2
	.global	__floatundidf
	.type	__floatundidf, @function
__floatundidf:
	push    $lp
	pushm   $r6, $r7

	move    $r4, #0
	move    P2H, P1H
	move    P2L, P1L
	or      $r7, P1H, P1L
	beqz    $r7, .Li3
	move    $r4, #0x43e


	#NORMd($r2, O1H, O1L)
	bnez    P2H, .LL8
	bnez    P2L, .LL9
	move    $r4, #0
	j       .LL10
.LL9:
	move    P2H, P2L
	move    P2L, #0
	move    O1H, #32
	sub     $r4, $r4, O1H
.LL8:
#ifdef __NDS32_PERF_EXT__
	clz	O1H, P2H
#else /* not __NDS32_PERF_EXT__ */
/*
  Replace clz with function call.
	clz     O1H, P2H
  EL:	clz     $r6, $r3
  EB:	clz	$r5, $r2
*/
#ifndef __big_endian__
	pushm	$r0, $r5
	move	$r0, $r3
	bal	__clzsi2
	move	$r6, $r0
	popm	$r0, $r5
#else
	pushm	$r0, $r4
	move	$r0, $r2
	bal	__clzsi2
	move	$r5, $r0
	popm	$r0, $r4
#endif
#endif /* not __NDS32_PERF_EXT__ */
	beqz    O1H, .LL10
	sub     $r4, $r4, O1H
	subri   O1L, O1H, #32
	srl     O1L, P2L, O1L
	sll     P2L, P2L, O1H
	sll     P2H, P2H, O1H
	or      P2H, P2H, O1L
.LL10:
	#NORMd End

	#ADD(P2L, $0x400)
	move    $r15, #0x400
	add     P2L, P2L, $r15
	slt     $r15, P2L, $r15


	#ADDCC(P2H, $0x0)
	beqzs8  .LL13
	add     P2H, P2H, $r15
	slt     $r15, P2H, $r15
.LL13:

	#ADDC($r4, $0x0)
	add     $r4, $r4, $r15
	srli    $r7, P2L, #11
	andi    $r7, $r7, #1
	sub     P2L, P2L, $r7
.Li3:
	srli    O1L, P2L, #11
	slli    $r7, P2H, #21
	or      O1L, O1L, $r7
	slli    O1H, P2H, #1
	srli    O1H, O1H, #12
	slli    $r7, $r4, #20
	or      O1H, O1H, $r7
	move    P1L, O1L
	move    P1H, O1H

.LB999:
	popm    $r6, $r7
	pop     $lp
	ret5    $lp
	.size	__floatundidf, .-__floatundidf
#endif /* L_floatundidf */



#ifdef L_compare_sf

	.text
	.align	2
	.global	__cmpsf2
	.type	__cmpsf2, @function
__cmpsf2:
	.global	__eqsf2
	.type	__eqsf2, @function
__eqsf2:
	.global	__ltsf2
	.type	__ltsf2, @function
__ltsf2:
	.global	__lesf2
	.type	__lesf2, @function
__lesf2:
	.global	__nesf2
	.type	__nesf2, @function
__nesf2:
	move    $r4, #1
	j	.LA

	.global	__gesf2
	.type	__gesf2, @function
__gesf2:
	.global	__gtsf2
	.type	__gtsf2, @function
__gtsf2:
	move	$r4, #-1
.LA:
	push    $lp

	slli    $r2, $r0, #1
	slli    $r3, $r1, #1
	or      $r5, $r2, $r3
	beqz    $r5, .LMequ
	move    $r5, #0xff000000
	slt     $r15, $r5, $r2
	bnezs8  .LMnan
	slt     $r15, $r5, $r3
	bnezs8  .LMnan
	srli    $r2, $r2, #1
	sltsi   $r15, $r0, #0
	beqzs8  .Li48
	subri   $r2, $r2, #0
.Li48:
	srli    $r3, $r3, #1
	sltsi   $r15, $r1, #0
	beqzs8  .Li49
	subri   $r3, $r3, #0
.Li49:
	slts    $r15, $r2, $r3
	beqzs8  .Li50
	move    $r0, #-1
	j       .LM999
.Li50:
	slts    $r15, $r3, $r2
	beqzs8  .LMequ
	move    $r0, #1
	j       .LM999

.LMequ:
	move    $r0, #0

.LM999:
	pop     $lp
	ret5    $lp

.LMnan:
	move    $r0, $r4
	j       .LM999
	.size   __cmpsf2, .-__cmpsf2
	.size   __eqsf2, .-__eqsf2
	.size   __ltsf2, .-__ltsf2
	.size   __lesf2, .-__lesf2
	.size   __nesf2, .-__nesf2
	.size   __gesf2, .-__gesf2
	.size   __gtsf2, .-__gtsf2
#endif /* L_compare_sf */



#ifdef L_compare_df

#ifdef __big_endian__
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
#else
	#define P1H     $r1
	#define P1L     $r0
	#define P2H     $r3
	#define P2L     $r2
#endif
	.align	2
	.globl	__gtdf2
	.globl	__gedf2
	.globl	__ltdf2
	.globl	__ledf2
	.globl	__eqdf2
	.globl	__nedf2
	.globl	__cmpdf2
	.type	__gtdf2, @function
	.type	__gedf2, @function
	.type	__ltdf2, @function
	.type	__ledf2, @function
	.type	__eqdf2, @function
	.type	__nedf2, @function
	.type	__cmpdf2, @function
__gtdf2:
__gedf2:
	movi	$r4, -1
	b	.L1

__ltdf2:
__ledf2:
__cmpdf2:
__nedf2:
__eqdf2:
	movi	$r4, 1
.L1:
#if defined (__NDS32_ISA_V3M__)
	push25	$r10, 0
#else
	smw.adm	$r6, [$sp], $r9, 0
#endif

	sethi	$r5, 0x7ff00
	and	$r6, P1H, $r5	! r6=aExp
	and	$r7, P2H, $r5	! r7=bExp
	slli	$r8, P1H, 12	! r8=aSig0
	slli	$r9, P2H, 12	! r9=bSig0
	beq	$r6, $r5, .L11	! aExp==0x7ff
	beq	$r7, $r5, .L12	! bExp==0x7ff
.L2:
	slli	$ta, P1H, 1	! ta=ahigh<<1
	or	$ta, P1L, $ta	!
	xor	$r5, P1H, P2H	! r5=ahigh^bhigh
	beqz	$ta, .L3	! if(ahigh<<1)==0,go .L3
	!-------------------------------
	! (ahigh<<1)!=0 || (bhigh<<1)!=0
	!-------------------------------
.L4:
	beqz	$r5, .L5	! ahigh==bhigh, go .L5
	!--------------------
	! a != b
	!--------------------
.L6:
	bltz	$r5, .L7	! if(aSign!=bSign), go .L7
	!--------------------
	! aSign==bSign
	!--------------------
	slt	$ta, $r6, $r7	! ta=(aExp<bExp)
	bne	$r6, $r7, .L8	! if(aExp!=bExp),go .L8
	slt	$ta, $r8, $r9	! ta=(aSig0<bSig0)
	bne	$r8, $r9, .L8	! if(aSig0!=bSig0),go .L8
	slt	$ta, P1L, P2L	! ta=(aSig1<bSig1)
.L8:
	beqz	$ta, .L10	! if(|a|>|b|), go .L10
	nor	$r0, P2H, P2H	! if(|a|<|b|),return (~yh)
.L14:
#if defined (__NDS32_ISA_V3M__)
	pop25	$r10, 0
#else
	lmw.bim	$r6, [$sp], $r9, 0
	ret
#endif
.L10:
	ori	$r0, P2H, 1	! return (yh|1)
	b	.L14
	!--------------------
	! (ahigh<<1)=0
	!--------------------
.L3:
	slli	$ta, P2H, 1	! ta=bhigh<<1
	or	$ta, P2L, $ta	!
	bnez	$ta, .L4	! ta=(bhigh<<1)!=0,go .L4
.L5:
	xor	$ta, P1L, P2L	! ta=alow^blow
	bnez	$ta, .L6	! alow!=blow,go .L6
	movi	$r0, 0		! a==b, return 0
	b	.L14
	!--------------------
	! aExp=0x7ff;
	!--------------------
.L11:
	or	P1L, P1L, $r8	! x1=(aSig0|aSig1)
	bnez	P1L, .L13	! if(a=nan), go.L13
	xor	$ta, $r7, $r5	! ta=(bExp^0x7ff)
	bnez	$ta, .L2	! if(bExp!=0x7ff), go .L2
	!--------------------
	! bExp=0x7ff;
	!--------------------
.L12:
	or	$ta, P2L, $r9	! ta=(bSig0|bSig1)
	beqz	$ta, .L2	! if(b!=nan), go .L2
.L13:
	move	$r0, $r4
	b	.L14
	!--------------------
	! aSign!=bSign
	!--------------------
.L7:
	ori	$r0, P1H, 1	! if(aSign!=bSign), return (ahigh|1)
	b	.L14

	.size	__gtdf2, .-__gtdf2
	.size	__gedf2, .-__gedf2
	.size	__ltdf2, .-__ltdf2
	.size	__ledf2, .-__ledf2
	.size	__eqdf2, .-__eqdf2
	.size	__nedf2, .-__nedf2
	.size	__cmpdf2, .-__cmpdf2
#endif /* L_compare_df */



#ifdef L_unord_sf

	.text
	.align	2
	.global	__unordsf2
	.type	__unordsf2, @function
__unordsf2:
	push    $lp

	slli    $r2, $r0, #1
	move    $r3, #0xff000000
	slt     $r15, $r3, $r2
	beqzs8  .Li52
	move    $r0, #1
	j       .LP999
.Li52:
	slli    $r2, $r1, #1
	move    $r3, #0xff000000
	slt     $r15, $r3, $r2
	beqzs8  .Li53
	move    $r0, #1
	j       .LP999
.Li53:
	move    $r0, #0

.LP999:
	pop     $lp
	ret5    $lp
	.size	__unordsf2, .-__unordsf2
#endif /* L_unord_sf */



#ifdef L_unord_df

#ifndef __big_endian__
	#define P1L     $r0
	#define P1H     $r1
	#define P2L     $r2
	#define P2H     $r3
#else
	#define P1H     $r0
	#define P1L     $r1
	#define P2H     $r2
	#define P2L     $r3
#endif
	.text
	.align	2
	.global	__unorddf2
	.type	__unorddf2, @function
__unorddf2:
	push    $lp

	slli    $r4, P1H, #1
	beqz    P1L, .Li66
	addi    $r4, $r4, #1
.Li66:
	move    $r5, #0xffe00000
	slt     $r15, $r5, $r4
	beqzs8  .Li67
	move    $r0, #1
	j       .LR999
.Li67:
	slli    $r4, P2H, #1
	beqz    P2L, .Li68
	addi    $r4, $r4, #1
.Li68:
	move    $r5, #0xffe00000
	slt     $r15, $r5, $r4
	beqzs8  .Li69
	move    $r0, #1
	j       .LR999
.Li69:
	move    $r0, #0

.LR999:
	pop     $lp
	ret5    $lp
	.size __unorddf2, .-__unorddf2
#endif /* L_unord_df */
/* ------------------------------------------- */
/* DPBIT floating point operations for libgcc  */
/* ------------------------------------------- */
