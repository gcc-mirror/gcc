/* Miscellaneous BPABI functions.

   Copyright (C) 2003-2013 Free Software Foundation, Inc.
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
	cbnz	yyh, 1f
	cbnz	yyl, 1f
	cmp	xxh, #0
	do_it	eq
	cmpeq	xxl, #0
	.ifc \signed, unsigned
	beq	2f
	mov	xxh, #0xffffffff
	mov	xxl, xxh
2:
	.else
	do_it	lt, t
	movlt	xxl, #0
	movlt	xxh, #0x80000000
	do_it	gt, t
	movgt	xxh, #0x7fffffff
	movgt	xxl, #0xffffffff
	.endif
	b	SYM (__aeabi_ldiv0) __PLT__
1:
#else
	/* Note: Thumb-1 code calls via an ARM shim on processors which
	   support ARM mode.  */
	cmp	yyh, #0
	cmpeq	yyl, #0
	bne	2f
	cmp	xxh, #0
	cmpeq	xxl, #0
	.ifc \signed, unsigned
	movne	xxh, #0xffffffff
	movne	xxl, #0xffffffff
	.else
	movlt	xxh, #0x80000000
	movlt	xxl, #0
	movgt	xxh, #0x7fffffff
	movgt	xxl, #0xffffffff
	.endif
	b	SYM (__aeabi_ldiv0) __PLT__
2:
#endif
.endm

#ifdef L_aeabi_ldivmod

ARM_FUNC_START aeabi_ldivmod
	test_div_by_zero signed

	sub sp, sp, #8
#if defined(__thumb2__)
	mov ip, sp
	push {ip, lr}
#else
	do_push {sp, lr}
#endif
	bl SYM(__gnu_ldivmod_helper) __PLT__
	ldr lr, [sp, #4]
	add sp, sp, #8
	do_pop {r2, r3}
	RET
	
#endif /* L_aeabi_ldivmod */

#ifdef L_aeabi_uldivmod

ARM_FUNC_START aeabi_uldivmod
	test_div_by_zero unsigned

	sub sp, sp, #8
#if defined(__thumb2__)
	mov ip, sp
	push {ip, lr}
#else
	do_push {sp, lr}
#endif
	bl SYM(__gnu_uldivmod_helper) __PLT__
	ldr lr, [sp, #4]
	add sp, sp, #8
	do_pop {r2, r3}
	RET
	
#endif /* L_aeabi_divmod */
	
