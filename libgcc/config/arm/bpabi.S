/* Miscellaneous BPABI functions.

   Copyright (C) 2003-2020 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

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

	.cfi_sections .debug_frame

#ifdef __ARM_EABI__
/* Some attributes that are common to all routines in this file.  */
	/* Tag_ABI_align_needed: This code does not require 8-byte
	   alignment from the caller.  */
	/* .eabi_attribute 24, 0  -- default setting.  */
	/* Tag_ABI_align_preserved: This code preserves 8-byte
	   alignment in any callee.  */
	.eabi_attribute 25, 1
#endif /* __ARM_EABI__ */

#ifdef L_aeabi_lcmp

ARM_FUNC_START aeabi_lcmp
	cmp	xxh, yyh
	do_it	lt
	movlt	r0, #-1
	do_it	gt
	movgt	r0, #1
	do_it	ne
	RETc(ne)
	subs	r0, xxl, yyl
	do_it	lo
	movlo	r0, #-1
	do_it	hi
	movhi	r0, #1
	RET
	FUNC_END aeabi_lcmp

#endif /* L_aeabi_lcmp */
	
#ifdef L_aeabi_ulcmp

ARM_FUNC_START aeabi_ulcmp
	cmp	xxh, yyh
	do_it	lo
	movlo	r0, #-1
	do_it	hi
	movhi	r0, #1
	do_it	ne
	RETc(ne)
	cmp	xxl, yyl
	do_it	lo
	movlo	r0, #-1
	do_it	hi
	movhi	r0, #1
	do_it	eq
	moveq	r0, #0
	RET
	FUNC_END aeabi_ulcmp

#endif /* L_aeabi_ulcmp */

.macro test_div_by_zero signed
/* Tail-call to divide-by-zero handlers which may be overridden by the user,
   so unwinding works properly.  */
#if defined(__thumb2__)
	cbnz	yyh, 2f
	cbnz	yyl, 2f
	cmp	xxh, #0
	.ifc \signed, unsigned
	do_it	eq
	cmpeq	xxl, #0
	do_it	ne, t
	movne	xxh, #0xffffffff
	movne	xxl, #0xffffffff
	.else
	do_it	lt, tt
	movlt	xxl, #0
	movlt	xxh, #0x80000000
	blt	1f
	do_it	eq
	cmpeq	xxl, #0
	do_it	ne, t
	movne	xxh, #0x7fffffff
	movne	xxl, #0xffffffff
	.endif
1:	
	b	SYM (__aeabi_ldiv0) __PLT__
2:
#else
	/* Note: Thumb-1 code calls via an ARM shim on processors which
	   support ARM mode.  */
	cmp	yyh, #0
	cmpeq	yyl, #0
	bne	2f
	cmp	xxh, #0
	.ifc \signed, unsigned
	cmpeq	xxl, #0
	movne	xxh, #0xffffffff
	movne	xxl, #0xffffffff
	.else
	movlt	xxh, #0x80000000
	movlt	xxl, #0
	blt	1f
	cmpeq	xxl, #0
	movne	xxh, #0x7fffffff
	movne	xxl, #0xffffffff
	.endif
1:
	b	SYM (__aeabi_ldiv0) __PLT__
2:
#endif
.endm

/* we can use STRD/LDRD on v5TE and later, and any Thumb-2 architecture. */
#if (defined(__ARM_EABI__)                                            \
     && (defined(__thumb2__)                                          \
         || (__ARM_ARCH >= 5 && defined(__TARGET_FEATURE_DSP))))
#define CAN_USE_LDRD 1
#else
#define CAN_USE_LDRD 0
#endif

/* set up stack from for call to __udivmoddi4. At the end of the macro the
   stack is arranged as follows:
		sp+12	/ space for remainder
		sp+8	\ (written by __udivmoddi4)
		sp+4	lr
		sp+0	sp+8 [rp (remainder pointer) argument for __udivmoddi4]

 */
.macro push_for_divide fname
#if defined(__thumb2__) && CAN_USE_LDRD
	sub	ip, sp, #8
	strd	ip, lr, [sp, #-16]!
#else
	sub	sp, sp, #8
	do_push	{sp, lr}
#endif
	.cfi_adjust_cfa_offset 16
	.cfi_offset 14, -12
.endm

/* restore stack */
.macro pop_for_divide
	ldr	lr, [sp, #4]
#if CAN_USE_LDRD
	ldrd	r2, r3, [sp, #8]
	add	sp, sp, #16
#else
	add	sp, sp, #8
	do_pop	{r2, r3}
#endif
	.cfi_restore 14
	.cfi_adjust_cfa_offset 0
.endm

#ifdef L_aeabi_ldivmod

/* Perform 64 bit signed division.
   Inputs:
	r0:r1	numerator
	r2:r3	denominator
   Outputs:
	r0:r1	quotient
	r2:r3	remainder
 */
ARM_FUNC_START aeabi_ldivmod
	.cfi_startproc
	test_div_by_zero	signed

	push_for_divide	__aeabi_ldivmod
	cmp	xxh, #0
	blt	1f
	cmp	yyh, #0
	blt	2f
	/* arguments in (r0:r1), (r2:r3) and *sp */
	bl	SYM(__udivmoddi4) __PLT__
	.cfi_remember_state
	pop_for_divide
	RET

1: /* xxh:xxl is negative */
	.cfi_restore_state
	negs	xxl, xxl
	sbc	xxh, xxh, xxh, lsl #1	/* Thumb-2 has no RSC, so use X - 2X */
	cmp	yyh, #0
	blt	3f
	/* arguments in (r0:r1), (r2:r3) and *sp */
	bl	SYM(__udivmoddi4) __PLT__
	.cfi_remember_state
	pop_for_divide
	negs	xxl, xxl
	sbc	xxh, xxh, xxh, lsl #1	/* Thumb-2 has no RSC, so use X - 2X */
	negs	yyl, yyl
	sbc	yyh, yyh, yyh, lsl #1	/* Thumb-2 has no RSC, so use X - 2X */
	RET

2: /* only yyh:yyl is negative */
	.cfi_restore_state
	negs	yyl, yyl
	sbc	yyh, yyh, yyh, lsl #1	/* Thumb-2 has no RSC, so use X - 2X */
	/* arguments in (r0:r1), (r2:r3) and *sp */
	bl	SYM(__udivmoddi4) __PLT__
	.cfi_remember_state
	pop_for_divide
	negs	xxl, xxl
	sbc	xxh, xxh, xxh, lsl #1	/* Thumb-2 has no RSC, so use X - 2X */
	RET

3: /* both xxh:xxl and yyh:yyl are negative */
	.cfi_restore_state
	negs	yyl, yyl
	sbc	yyh, yyh, yyh, lsl #1	/* Thumb-2 has no RSC, so use X - 2X */
	/* arguments in (r0:r1), (r2:r3) and *sp */
	bl	SYM(__udivmoddi4) __PLT__
	pop_for_divide
	negs	yyl, yyl
	sbc	yyh, yyh, yyh, lsl #1	/* Thumb-2 has no RSC, so use X - 2X */
	RET

	.cfi_endproc
	
#endif /* L_aeabi_ldivmod */

#ifdef L_aeabi_uldivmod

/* Perform 64 bit signed division.
   Inputs:
	r0:r1	numerator
	r2:r3	denominator
   Outputs:
	r0:r1	quotient
	r2:r3	remainder
 */
ARM_FUNC_START aeabi_uldivmod
	.cfi_startproc
	test_div_by_zero	unsigned

	push_for_divide	__aeabi_uldivmod
	/* arguments in (r0:r1), (r2:r3) and *sp */
	bl	SYM(__udivmoddi4) __PLT__
	pop_for_divide
	RET
	.cfi_endproc

#endif /* L_aeabi_divmod */
	
