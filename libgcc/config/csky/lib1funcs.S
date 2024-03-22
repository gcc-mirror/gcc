/* libgcc routines for C-SKY.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
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


/* Use the right prefix for global labels.  */
#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b
#define SYM(x) CONCAT1 (__, x)

#ifndef __CSKYBE__
#define xl r0
#define xh r1
#define yl r2
#define yh r3
#else
#define xh r0
#define xl r1
#define yh r2
#define yl r3
#endif


#ifdef __ELF__
#define TYPE(x) .type SYM (x),@function
#define SIZE(x) .size SYM (x), . - SYM (x)
#else
#define TYPE(x)
#define SIZE(x)
#endif

.macro FUNC_START name
	.text
	.align	2
	.globl SYM (\name)
	TYPE (\name)
SYM (\name):
.endm

.macro FUNC_END name
	SIZE (\name)
.endm


/* Emulate FF1 ("fast find 1") instruction on ck801.
   Result goes in rx, clobbering ry.  */
#if defined(__CK801__)
.macro FF1_M rx, ry
	movi	\rx, 32
10:
	cmphsi	\ry, 1
	bf	11f
	subi	\rx, \rx, 1
	lsri	\ry, \ry, 1
	br	10b
11:
.endm
#else
.macro FF1_M rx, ry
	ff1	\rx, \ry
.endm
#endif

/* Likewise emulate lslc instruction ("logical left shift to C") on CK801.  */
#if defined(__CK801__)
.macro LSLC_M rx
	cmpne	\rx, \rx
	addc	\rx, \rx
.endm
#else
.macro LSLC_M rx
	lslc	\rx
.endm
#endif

/* Emulate the abs instruction.  */
#if  defined(__CK802__)
.macro ABS_M rx
	btsti	\rx, 31
	bf	10f
	not	\rx
	addi	\rx, 1
10:
.endm
#elif defined(__CK801__)
.macro ABS_M rx
	cmplti	\rx, 1
	bf	10f
	not	\rx
	addi	\rx, 1
10:
.endm
#else
.macro ABS_M rx
	abs	\rx
.endm
#endif

/* Emulate the ld.hs ("load signed halfword and extend") instruction
   on ck801 and ck802.  */
#if defined(__CK801__)
.macro LDBS_M rx, ry
	ld.b \rx, (\ry, 0x0)
	sextb \rx, \rx
.endm
#else
.macro LDBS_M rx, ry
	ld.bs \rx, (\ry, 0x0)
.endm
#endif

#if defined(__CK801__)
.macro LDHS_M rx, ry
	ld.h \rx, (\ry, 0x0)
	sexth \rx, \rx
.endm
#else
.macro LDHS_M rx, ry
	ld.hs \rx, (\ry, 0x0)
.endm
#endif


/* Signed and unsigned div/mod/rem functions.  */

#ifdef	L_udivsi3
FUNC_START udiv32
FUNC_START udivsi3
	cmpnei	a1, 0	// look for 0 divisor
	bt	9f
	trap	3	// divide by 0
9:
	// control iterations, skip across high order 0 bits in dividend
	cmpnei	a0, 0
	bt	8f
	jmp	lr	// 0 dividend quick return
8:
	push	l0
	movi	a2, 1	// a2 is quotient (1 for a sentinel)
	mov	a3, a0
	FF1_M	l0, a3	// figure distance to skip
	lsl	a2, l0	// move the sentinel along (with 0's behind)
	lsl	a0, l0	// and the low 32 bits of numerator

	// FIXME:  Is this correct?
	mov	a3, a1	// looking at divisor
	FF1_M	l0, a3	// I can move 32-l0 more bits to left.
	addi	l0, 1	// ok, one short of that...
	mov	a3, a0
	lsr	a3, l0	// bits that came from low order...
	not	l0	// l0 == "32-n" == LEFT distance
	addi	l0, 33	// this is (32-n)
	lsl	a2,l0	// fixes the high 32 (quotient)
	lsl	a0,l0
	cmpnei	a2,0
	bf	4f	// the sentinel went away...

	// run the remaining bits
1:
	LSLC_M	a0	// 1 bit left shift of a3-a0
	addc	a3, a3
	cmphs	a3, a1	// upper 32 of dividend >= divisor?
	bf	2f
	subu	a3, a1	// if yes, subtract divisor
2:
	addc	a2, a2	// shift by 1 and count subtracts
	bf	1b	// if sentinel falls out of quotient, stop

4:
	mov	a0, a2	// return quotient
	mov	a1, a3	// and piggyback the remainder
	pop	l0
FUNC_END udiv32
FUNC_END udivsi3
#endif

#ifdef	L_umodsi3
FUNC_START urem32
FUNC_START umodsi3
	cmpnei	a1, 0	// look for 0 divisor
	bt	9f
	trap	3	// divide by 0
9:
	// control iterations, skip across high order 0 bits in dividend
	cmpnei	a0, 0
	bt	8f
	jmp	lr	// 0 dividend quick return
8:
	mov	a2, a0
	FF1_M	a3, a2	// figure distance to skip
	movi	a2, 1	// a2 is quotient (1 for a sentinel)
	lsl	a2, a3	// move the sentinel along (with 0's behind)
	lsl	a0, a3	// and the low 32 bits of numerator
	movi	a3, 0

1:
	LSLC_M	a0	// 1 bit left shift of a3-a0
	addc	a3, a3
	cmphs	a3, a1	// upper 32 of dividend >= divisor?
	bf	2f
	subu	a3, a1	// if yes, subtract divisor
2:
	addc	a2, a2	// shift by 1 and count subtracts
	bf	1b	// if sentinel falls out of quotient, stop

4:
	mov	a0, a3	// and piggyback the remainder
	jmp	lr
FUNC_END urem32
FUNC_END umodsi3
#endif


#ifdef	L_divsi3
FUNC_START div32
FUNC_START divsi3
	cmpnei	a1, 0	// look for 0 divisor
	bt	9f
	trap	3	// divide by 0
9:
	// control iterations, skip across high order 0 bits in dividend
	cmpnei	a0, 0
	bt	8f
	jmp	lr	// 0 dividend quick return
8:
	push	l0, l1
	mov	l1, a0
	xor	l1, a1	// calc sign of quotient
	ABS_M	a0
	ABS_M	a1
	movi	a2, 1	// a2 is quotient (1 for a sentinel)
	mov	a3, a0
	FF1_M	l0, a3	// figure distance to skip
	lsl	a2, l0	// move the sentinel along (with 0's behind)
	lsl	a0, l0	// and the low 32 bits of numerator

	// FIXME: is this correct?
	mov	a3, a1	// looking at divisor
	FF1_M	l0, a3	// I can move 32-l0 more bits to left.
	addi	l0, 1	// ok, one short of that...
	mov	a3, a0
	lsr	a3, l0	// bits that came from low order...
	not	l0	// l0 == "32-n" == LEFT distance
	addi	l0, 33	// this is (32-n)
	lsl	a2,l0	// fixes the high 32 (quotient)
	lsl	a0,l0
	cmpnei	a2,0
	bf	4f	// the sentinel went away...

	// run the remaining bits
1:
	LSLC_M	a0	// 1 bit left shift of a3-a0
	addc	a3, a3
	cmphs	a3, a1	// upper 32 of dividend >= divisor?
	bf	2f
	subu	a3, a1	// if yes, subtract divisor
2:
	addc	a2, a2	// shift by 1 and count subtracts
	bf	1b	// if sentinel falls out of quotient, stop

4:
	mov	a0, a2	// return quotient
	mov	a1, a3	// and piggyback the remainder
	LSLC_M	l1	// after adjusting for sign
	bf	3f
	not	a0
	addi	a0, 1
	not	a1
	addi	a1, 1
3:
	pop	l0, l1
FUNC_END div32
FUNC_END divsi3
#endif

#ifdef	L_modsi3
FUNC_START rem32
FUNC_START modsi3
	push	l0
	cmpnei	a1, 0	// look for 0 divisor
	bt	9f
	trap	3	// divide by 0
9:
	// control iterations, skip across high order 0 bits in dividend
	cmpnei	a0, 0
	bt	8f
	pop	l0	// 0 dividend quick return
8:
	mov	l0, a0
	ABS_M	a0
	ABS_M	a1
	mov	a2, a0
	FF1_M	a3, a2	// figure distance to skip
	movi	a2, 1	// a2 is quotient (1 for a sentinel)
	lsl	a2, a3	// move the sentinel along (with 0's behind)
	lsl	a0, a3	// and the low 32 bits of numerator
	movi	a3, 0

	// run the remaining bits
1:
	LSLC_M	a0	// 1 bit left shift of a3-a0
	addc	a3, a3
	cmphs	a3, a1	// upper 32 of dividend >= divisor?
	bf	2f
	subu	a3, a1	// if yes, subtract divisor
2:
	addc	a2, a2	// shift by 1 and count subtracts
	bf	1b	// if sentinel falls out of quotient, stop

4:
	mov	a0, a3	// and piggyback the remainder
	LSLC_M	l0	// after adjusting for sign
	bf	3f
	not	a0
	addi	a0, 1
3:
	pop	l0
FUNC_END rem32
FUNC_END modsi3
#endif

/* Unordered comparisons for single and double float.  */

#ifdef L_unordsf2
FUNC_START unordsf2
#if defined(__CK801__)
	 subi	  sp, 4
	 st.w	  r4, (sp, 0x0)
	 lsli	  r2, r0, 1
	 lsli	  r3, r1, 1
	 asri	  r4, r2, 24
	 not	  r4
	 cmpnei	  r4, 0
	 bt	  1f
	 lsli	  r4, r0, 9
	 cmpnei	  r4, 0
	 bt	  3f
1:
	 asri	  r4, r3, 24
	 not	  r4
	 cmpnei	  r4, 0
	 bt	  2f
	 lsli	  r4, r1, 9
	 cmpnei	  r4, 0
	 bt	  3f
2:
	 ld.w	  r4, (sp, 0x0)
	 addi	  sp, 4
	 movi	  r0, 0
	 rts
3:
	 ld.w	  r4, (sp, 0x0)
	 addi	  sp, 4
	 movi	  r0, 1
	 rts
#elif defined(__CK802__)
	 lsli	  r2, r0, 1
	 lsli	  r3, r1, 1
	 asri	  r2, r2, 24
	 not	  r13, r2
	 cmpnei	  r13, 0
	 bt	  1f
	 lsli	  r13, r0, 9
	 cmpnei	  r13, 0
	 bt	  3f
1:
	 asri	  r3, r3, 24
	 not	  r13, r3
	 cmpnei	  r13, 0
	 bt	  2f
	 lsli	  r13, r1, 9
	 cmpnei	  r13, 0
	 bt	  3f
2:
	 movi	  r0, 0
	 rts
3:
	 movi	  r0, 1
	 rts
#else
	 lsli	  r2, r0, 1
	 lsli	  r3, r1, 1
	 asri	  r2, r2, 24
	 not	  r13, r2
	 bnez	  r13, 1f
	 lsli	  r13, r0, 9
	 bnez	  r13, 3f
1:
	 asri	  r3, r3, 24
	 not	  r13, r3
	 bnez	  r13, 2f
	 lsli	  r13, r1, 9
	 bnez	  r13, 3f
2:
	 movi	  r0, 0
	 rts
3:
	 movi	  r0, 1
	 rts
#endif
FUNC_END unordsf2
#endif

#ifdef L_unorddf2
FUNC_START unorddf2
#if defined(__CK801__)
	 subi	  sp, 8
	 st.w	  r4, (sp, 0x0)
	 st.w	  r5, (sp, 0x4)
	 lsli	  r4, xh, 1
	 asri	  r4, r4, 21
	 not	  r4
	 cmpnei	  r4, 0
	 bt	  1f
	 mov	  r4, xl
	 lsli	  r5, xh, 12
	 or	  r4, r5
	 cmpnei	  r4, 0
	 bt	  3f
1:
	 lsli	  r4, yh, 1
	 asri	  r4, r4, 21
	 not	  r4
	 cmpnei	  r4, 0
	 bt	  2f
	 mov	  r4,yl
	 lsli	  r5, yh, 12
	 or	  r4, r5
	 cmpnei	  r4, 0
	 bt	  3f
2:
	 ld.w	  r4, (sp, 0x0)
	 ld.w	  r5, (sp, 0x4)
	 addi	  sp, 8
	 movi	  r0, 0
	 rts
3:
	 ld.w	  r4, (sp, 0x0)
	 ld.w	  r5, (sp, 0x4)
	 addi	  sp, 8
	 movi	  r0, 1
	 rts
#elif defined(__CK802__)
	 lsli	  r13, xh, 1
	 asri	  r13, r13, 21
	 not	  r13
	 cmpnei	  r13, 0
	 bt	  1f
	 lsli	  xh, xh, 12
	 or	  r13, xl, xh
	 cmpnei	  r13, 0
	 bt	  3f
1:
	 lsli	  r13, yh, 1
	 asri	  r13, r13, 21
	 not	  r13
	 cmpnei	  r13, 0
	 bt	  2f
	 lsli	  yh, yh, 12
	 or	  r13, yl, yh
	 cmpnei	  r13, 0
	 bt	  3f
2:
	 movi	  r0, 0
	 rts
3:
	 movi	  r0, 1
	 rts
#else
	 lsli	  r13, xh, 1
	 asri	  r13, r13, 21
	 not	  r13
	 bnez	  r13, 1f
	 lsli	  xh, xh, 12
	 or	  r13, xl, xh
	 bnez	  r13, 3f
1:
	 lsli	  r13, yh, 1
	 asri	  r13, r13, 21
	 not	  r13
	 bnez	  r13, 2f
	 lsli	  yh, yh, 12
	 or	  r13, yl, yh
	 bnez	  r13, 3f
2:
	 movi	  r0, 0
	 rts
3:
	 movi	  r0, 1
	 rts
#endif
FUNC_END unorddf2
#endif

/* When optimizing for size on ck801 and ck802, GCC emits calls to the
   following helper functions when expanding casesi, instead of emitting
   the table lookup and jump inline.  Note that in these functions the
   jump is handled by tweaking the value of lr before rts.  */
#ifdef L_csky_case_sqi
FUNC_START _gnu_csky_case_sqi
	subi	sp, 4
	st.w	a1, (sp, 0x0)
	mov	a1, lr
	add	a1, a1, a0
	LDBS_M	a1, a1
	lsli	a1, a1, 1
	add	lr, lr, a1
	ld.w	a1, (sp, 0x0)
	addi	sp, 4
	rts
FUNC_END _gnu_csky_case_sqi
#endif

#ifdef L_csky_case_uqi
FUNC_START _gnu_csky_case_uqi
	subi	sp, 4
	st.w	a1, (sp, 0x0)
	mov	a1, lr
	add	a1, a1, a0
	ld.b	a1, (a1, 0x0)
	lsli	a1, a1, 1
	add	lr, lr, a1
	ld.w	a1, (sp, 0x0)
	addi	sp, 4
	rts
FUNC_END _gnu_csky_case_uqi
#endif

#ifdef L_csky_case_shi
FUNC_START _gnu_csky_case_shi
	subi	sp, 8
	st.w	a0, (sp, 0x4)
	st.w	a1, (sp, 0x0)
	mov	a1, lr
	lsli	a0, a0, 1
	add	a1, a1, a0
	LDHS_M	a1, a1
	lsli	a1, a1, 1
	add	lr, lr, a1
	ld.w	a0, (sp, 0x4)
	ld.w	a1, (sp, 0x0)
	addi	sp, 8
	rts
FUNC_END _gnu_csky_case_shi
#endif

#ifdef L_csky_case_uhi
FUNC_START _gnu_csky_case_uhi
	subi	sp, 8
	st.w	a0, (sp, 0x4)
	st.w	a1, (sp, 0x0)
	mov	a1, lr
	lsli	a0, a0, 1
	add	a1, a1, a0
	ld.h	a1, (a1, 0x0)
	lsli	a1, a1, 1
	add	lr, lr, a1
	ld.w	a0, (sp, 0x4)
	ld.w	a1, (sp, 0x0)
	addi	sp, 8
	rts
FUNC_END _gnu_csky_case_uhi
#endif

#ifdef L_csky_case_si
FUNC_START _gnu_csky_case_si
	subi	sp, 8
	st.w	a0, (sp, 0x4)
	st.w	a1, (sp, 0x0)
	mov	a1, lr
	addi	a1, a1, 2	// Align to word.
	bclri	a1, a1, 1
	mov	lr, a1
	lsli	a0, a0, 2
	add	a1, a1, a0
	ld.w	a0, (a1, 0x0)
	add	lr, lr, a0
	ld.w	a0, (sp, 0x4)
	ld.w	a1, (sp, 0x0)
	addi	sp, 8
	rts
FUNC_END _gnu_csky_case_si
#endif

/* GCC expects that {__eq,__ne,__gt,__ge,__le,__lt}{df2,sf2}
   will behave as __cmpdf2. So, we stub the implementations to
   jump on to __cmpdf2 and __cmpsf2.

   All of these short-circuit the return path so that __cmp{sd}f2
   will go directly back to the caller.  */

.macro	COMPARE_DF_JUMP name
	.import SYM (cmpdf2)
FUNC_START \name
	jmpi SYM (cmpdf2)
FUNC_END \name
.endm

#ifdef	L_eqdf2
COMPARE_DF_JUMP eqdf2
#endif /* L_eqdf2 */

#ifdef	L_nedf2
COMPARE_DF_JUMP nedf2
#endif /* L_nedf2 */

#ifdef	L_gtdf2
COMPARE_DF_JUMP gtdf2
#endif /* L_gtdf2 */

#ifdef	L_gedf2
COMPARE_DF_JUMP gedf2
#endif /* L_gedf2 */

#ifdef	L_ltdf2
COMPARE_DF_JUMP ltdf2
#endif /* L_ltdf2 */

#ifdef	L_ledf2
COMPARE_DF_JUMP ledf2
#endif /* L_ledf2 */

/* Single-precision floating point stubs.  */

.macro	COMPARE_SF_JUMP name
	.import SYM (cmpsf2)
FUNC_START \name
	jmpi SYM (cmpsf2)
FUNC_END \name
.endm

#ifdef	L_eqsf2
COMPARE_SF_JUMP eqsf2
#endif /* L_eqsf2 */

#ifdef	L_nesf2
COMPARE_SF_JUMP nesf2
#endif /* L_nesf2 */

#ifdef	L_gtsf2
COMPARE_SF_JUMP gtsf2
#endif /* L_gtsf2 */

#ifdef	L_gesf2
COMPARE_SF_JUMP __gesf2
#endif /* L_gesf2 */

#ifdef	L_ltsf2
COMPARE_SF_JUMP __ltsf2
#endif /* L_ltsf2 */

#ifdef	L_lesf2
COMPARE_SF_JUMP lesf2
#endif /* L_lesf2 */
