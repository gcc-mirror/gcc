/* Copyright (C) 2010-2020 Free Software Foundation, Inc.
   Contributed by Bernd Schmidt <bernds@codesourcery.com>.

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

	;; ABI considerations for the divide functions
	;; The following registers are call-used:
	;; __c6xabi_divi A0,A1,A2,A4,A6,B0,B1,B2,B4,B5
	;; __c6xabi_divu A0,A1,A2,A4,A6,B0,B1,B2,B4
	;; __c6xabi_remi A1,A2,A4,A5,A6,B0,B1,B2,B4
	;; __c6xabi_remu A1,A4,A5,A7,B0,B1,B2,B4
	;;
	;; In our implementation, divu and remu are leaf functions,
	;; while both divi and remi call into divu.
	;; A0 is not clobbered by any of the functions.
	;; divu does not clobber B2 either, which is taken advantage of
	;; in remi.
	;; divi uses B5 to hold the original return address during
	;; the call to divu.
	;; remi uses B2 and A5 to hold the input values during the
	;; call to divu.  It stores B3 in on the stack.

#ifdef L_divsi3
.text
.align 2
.global __c6xabi_divi
.hidden __c6xabi_divi
.type __c6xabi_divi, STT_FUNC

__c6xabi_divi:
	call .s2	__c6xabi_divu
||	mv .d2		B3, B5
||	cmpgt .l1	0, A4, A1
||	cmpgt .l2	0, B4, B1

	[A1] neg .l1	A4, A4
||	[B1] neg .l2	B4, B4
||	xor .s1x	A1, B1, A1

#ifdef _TMS320C6400
	[A1] addkpc .s2	1f, B3, 4
#else
	[A1] mvkl .s2	1f, B3
	[A1] mvkh .s2	1f, B3
	nop		2
#endif
1:
	neg .l1		A4, A4
||	mv .l2		B3,B5
||	ret .s2		B5
	nop		5
#endif

#if defined L_modsi3 || defined L_divmodsi4
.align 2
#ifdef L_modsi3
#define MOD_OUTPUT_REG A4
.global __c6xabi_remi
.hidden __c6xabi_remi
.type __c6xabi_remi, STT_FUNC
#else
#define MOD_OUTPUT_REG A5
.global __c6xabi_divremi
.hidden __c6xabi_divremi
.type __c6xabi_divremi, STT_FUNC
__c6xabi_divremi:
#endif

__c6xabi_remi:
	stw .d2t2	B3, *B15--[2]
||	cmpgt .l1	0, A4, A1
||	cmpgt .l2	0, B4, B2
||	mv .s1		A4, A5
||	call .s2	__c6xabi_divu

	[A1] neg .l1	A4, A4
||	[B2] neg .l2	B4, B4
||	xor .s2x	B2, A1, B0
||	mv .d2		B4, B2

#ifdef _TMS320C6400
	[B0] addkpc .s2	1f, B3, 1
	[!B0] addkpc .s2 2f, B3, 1
	nop		2
#else
	[B0] mvkl .s2	1f,B3
	[!B0] mvkl .s2	2f,B3

	[B0] mvkh .s2	1f,B3
	[!B0] mvkh .s2	2f,B3
#endif
1:
	neg .l1		A4, A4
2:
	ldw .d2t2	*++B15[2], B3

#ifdef _TMS320C6400_PLUS
	mpy32 .m1x	A4, B2, A6
	nop		3
	ret .s2		B3
	sub .l1		A5, A6, MOD_OUTPUT_REG
	nop		4
#else
	mpyu .m1x	A4, B2, A1
	nop		1
	mpylhu .m1x	A4, B2, A6
||	mpylhu .m2x	B2, A4, B2
	nop		1
	add .l1x	A6, B2, A6
||	ret .s2		B3
	shl .s1		A6, 16, A6
	add .d1		A6, A1, A6
	sub .l1		A5, A6, MOD_OUTPUT_REG
	nop		2
#endif

#endif

#if defined L_udivsi3 || defined L_udivmodsi4
.align 2
#ifdef L_udivsi3
.global __c6xabi_divu
.hidden __c6xabi_divu
.type __c6xabi_divu, STT_FUNC
__c6xabi_divu:
#else
.global __c6xabi_divremu
.hidden __c6xabi_divremu
.type __c6xabi_divremu, STT_FUNC
__c6xabi_divremu:
#endif
	;; We use a series of up to 31 subc instructions.  First, we find
	;; out how many leading zero bits there are in the divisor.  This
	;; gives us both a shift count for aligning (shifting) the divisor
	;; to the, and the number of times we have to execute subc.

	;; At the end, we have both the remainder and most of the quotient
	;; in A4.  The top bit of the quotient is computed first and is
	;; placed in A2.

	;; Return immediately if the dividend is zero.  Setting B4 to 1
	;; is a trick to allow us to leave the following insns in the jump
	;; delay slot without affecting the result.
	mv	.s2x	A4, B1

#ifndef _TMS320C6400
[!b1]	mvk	.s2	1, B4
#endif
[b1]	lmbd	.l2	1, B4, B1
||[!b1] b	.s2	B3	; RETURN A
#ifdef _TMS320C6400
||[!b1] mvk	.d2	1, B4
#endif
#ifdef L_udivmodsi4
||[!b1] zero	.s1	A5
#endif
	mv	.l1x	B1, A6
||	shl	.s2	B4, B1, B4

	;; The loop performs a maximum of 28 steps, so we do the
	;; first 3 here.
	cmpltu	.l1x	A4, B4, A2
[!A2]	sub	.l1x	A4, B4, A4
||	shru	.s2	B4, 1, B4
||	xor	.s1	1, A2, A2

	shl	.s1	A2, 31, A2
|| [b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1

	;; RETURN A may happen here (note: must happen before the next branch)
0:
	cmpgt	.l2	B1, 7, B0
|| [b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
|| [b0] b	.s1	0b
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
	;; loop backwards branch happens here

	ret	.s2	B3
||	mvk	.s1	32, A1
	sub	.l1	A1, A6, A6
#ifdef L_udivmodsi4
||	extu	.s1	A4, A6, A5
#endif
	shl	.s1	A4, A6, A4
	shru	.s1	A4, 1, A4
||	sub	.l1	A6, 1, A6
	or	.l1	A2, A4, A4
	shru	.s1	A4, A6, A4
	nop

#endif

#ifdef L_umodsi3
.align 2
.global __c6xabi_remu
.hidden __c6xabi_remu
.type __c6xabi_remu, STT_FUNC
__c6xabi_remu:
	;; The ABI seems designed to prevent these functions calling each other,
	;; so we duplicate most of the divsi3 code here.
	mv	.s2x	A4, B1
#ifndef _TMS320C6400
[!b1]	mvk	.s2	1, B4
#endif
	lmbd	.l2	1, B4, B1
||[!b1] b	.s2	B3	; RETURN A
#ifdef _TMS320C6400
||[!b1] mvk	.d2	1, B4
#endif

	mv	.l1x	B1, A7
||	shl	.s2	B4, B1, B4

	cmpltu	.l1x	A4, B4, A1
[!a1]	sub	.l1x	A4, B4, A4
	shru	.s2	B4, 1, B4

0:
	cmpgt	.l2	B1, 7, B0
|| [b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
	;; RETURN A may happen here (note: must happen before the next branch)
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
|| [b0] b	.s1	0b
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
	;; loop backwards branch happens here

	ret	.s2	B3
[b1]	subc	.l1x	A4,B4,A4
|| [b1]	add	.s2	-1, B1, B1
[b1]	subc	.l1x	A4,B4,A4

	extu	.s1	A4, A7, A4
	nop	2
#endif

#if defined L_strasgi_64plus && defined _TMS320C6400_PLUS

.align 2
.global __c6xabi_strasgi_64plus
.hidden __c6xabi_strasgi_64plus
.type __c6xabi_strasgi_64plus, STT_FUNC
__c6xabi_strasgi_64plus:
	shru	.s2x	a6, 2, b31
||	mv	.s1	a4, a30
||	mv	.d2	b4, b30

	add	.s2	-4, b31, b31

	sploopd		1
||	mvc	.s2	b31, ilc
	ldw	.d2t2	*b30++, b31
	nop	4
	mv	.s1x	b31,a31
	spkernel	6, 0
||	stw	.d1t1	a31, *a30++

	ret	.s2	b3
	nop 5
#endif

#ifdef L_strasgi
.global __c6xabi_strasgi
.type __c6xabi_strasgi, STT_FUNC
__c6xabi_strasgi:
	;; This is essentially memcpy, with alignment known to be at least
	;; 4, and the size a multiple of 4 greater than or equal to 28.
	ldw	.d2t1	*B4++, A0
||	mvk	.s2	16, B1
	ldw	.d2t1	*B4++, A1
||	mvk	.s2	20, B2
||	sub	.d1	A6, 24, A6
	ldw	.d2t1	*B4++, A5
	ldw	.d2t1	*B4++, A7
||	mv	.l2x	A6, B7
	ldw	.d2t1	*B4++, A8
	ldw	.d2t1	*B4++, A9
||	mv	.s2x	A0, B5
||	cmpltu	.l2	B2, B7, B0

0:
	stw	.d1t2	B5, *A4++
||[b0]	ldw	.d2t1	*B4++, A0
||	mv	.s2x	A1, B5
||	mv	.l2	B7, B6

[b0]	sub	.d2	B6, 24, B7
||[b0]	b	.s2	0b
||	cmpltu	.l2	B1, B6, B0

[b0]	ldw	.d2t1	*B4++, A1
||	stw	.d1t2	B5, *A4++
||	mv	.s2x	A5, B5
||	cmpltu	.l2	12, B6, B0

[b0]	ldw	.d2t1	*B4++, A5
||	stw	.d1t2	B5, *A4++
||	mv	.s2x	A7, B5
||	cmpltu	.l2	8, B6, B0

[b0]	ldw	.d2t1	*B4++, A7
||	stw	.d1t2	B5, *A4++
||	mv	.s2x	A8, B5
||	cmpltu	.l2	4, B6, B0

[b0]	ldw	.d2t1	*B4++, A8
||	stw	.d1t2	B5, *A4++
||	mv	.s2x	A9, B5
||	cmpltu	.l2	0, B6, B0

[b0]	ldw	.d2t1	*B4++, A9
||	stw	.d1t2	B5, *A4++
||	mv	.s2x	A0, B5
||	cmpltu	.l2	B2, B7, B0

	;; loop back branch happens here

	cmpltu	.l2	B1, B6, B0
||	ret	.s2	b3

[b0]	stw	.d1t1	A1, *A4++
||	cmpltu	.l2	12, B6, B0
[b0]	stw	.d1t1	A5, *A4++
||	cmpltu	.l2	8, B6, B0
[b0]	stw	.d1t1	A7, *A4++
||	cmpltu	.l2	4, B6, B0
[b0]	stw	.d1t1	A8, *A4++
||	cmpltu	.l2	0, B6, B0
[b0]	stw	.d1t1	A9, *A4++

	;; return happens here

#endif

#ifdef _TMS320C6400_PLUS
#ifdef L_push_rts
.align 2
.global __c6xabi_push_rts
.hidden __c6xabi_push_rts
.type __c6xabi_push_rts, STT_FUNC
__c6xabi_push_rts:
	stw .d2t2	B14, *B15--[2]
	stdw .d2t1	A15:A14, *B15--
||	b .s2x		A3
	stdw .d2t2	B13:B12, *B15--
	stdw .d2t1	A13:A12, *B15--
	stdw .d2t2	B11:B10, *B15--
	stdw .d2t1	A11:A10, *B15--
	stdw .d2t2	B3:B2, *B15--
#endif

#ifdef L_pop_rts
.align 2
.global __c6xabi_pop_rts
.hidden __c6xabi_pop_rts
.type __c6xabi_pop_rts, STT_FUNC
__c6xabi_pop_rts:
	lddw .d2t2	*++B15, B3:B2
	lddw .d2t1	*++B15, A11:A10
	lddw .d2t2	*++B15, B11:B10
	lddw .d2t1	*++B15, A13:A12
	lddw .d2t2	*++B15, B13:B12
	lddw .d2t1	*++B15, A15:A14
||	b .s2		B3
	ldw .d2t2	*++B15[2], B14
	nop		4
#endif

#ifdef L_call_stub
.align 2
.global __c6xabi_call_stub
.type __c6xabi_call_stub, STT_FUNC
__c6xabi_call_stub:
	stw .d2t1	A2, *B15--[2]
	stdw .d2t1	A7:A6, *B15--
||	call .s2	B31
	stdw .d2t1	A1:A0, *B15--
	stdw .d2t2	B7:B6, *B15--
	stdw .d2t2	B5:B4, *B15--
	stdw .d2t2	B1:B0, *B15--
	stdw .d2t2	B3:B2, *B15--
||	addkpc .s2	1f, B3, 0
1:
	lddw .d2t2	*++B15, B3:B2
	lddw .d2t2	*++B15, B1:B0
	lddw .d2t2	*++B15, B5:B4
	lddw .d2t2	*++B15, B7:B6
	lddw .d2t1	*++B15, A1:A0
	lddw .d2t1	*++B15, A7:A6
||	b .s2		B3
	ldw .d2t1	*++B15[2], A2
	nop		4
#endif

#endif

