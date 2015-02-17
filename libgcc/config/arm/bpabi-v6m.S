/* Miscellaneous BPABI functions.  ARMv6M implementation

   Copyright (C) 2006-2013 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

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

FUNC_START aeabi_lcmp
	cmp	xxh, yyh
	beq	1f
	bgt	2f
	mov	r0, #1
	neg	r0, r0
	RET
2:
	mov	r0, #1
	RET
1:
	sub	r0, xxl, yyl
	beq	1f
	bhi	2f
	mov	r0, #1
	neg	r0, r0
	RET
2:
	mov	r0, #1
1:
	RET
	FUNC_END aeabi_lcmp

#endif /* L_aeabi_lcmp */
	
#ifdef L_aeabi_ulcmp

FUNC_START aeabi_ulcmp
	cmp	xxh, yyh
	bne	1f
	sub	r0, xxl, yyl
	beq	2f
1:
	bcs	1f
	mov	r0, #1
	neg	r0, r0
	RET
1:
	mov	r0, #1
2:
	RET
	FUNC_END aeabi_ulcmp

#endif /* L_aeabi_ulcmp */

.macro test_div_by_zero signed
	cmp	yyh, #0
	bne	7f
	cmp	yyl, #0
	bne	7f
	cmp	xxh, #0
	.ifc	\signed, unsigned
	bne	2f
	cmp	xxl, #0
2:
	beq	3f
	mov	xxh, #0
	mvn	xxh, xxh		@ 0xffffffff
	mov	xxl, xxh
3:
	.else
	blt	6f
	bgt	4f
	cmp	xxl, #0
	beq	5f
4:	mov	xxl, #0
	mvn	xxl, xxl		@ 0xffffffff
	lsr	xxh, xxl, #1		@ 0x7fffffff
	b	5f
6:	mov	xxh, #0x80
	lsl	xxh, xxh, #24		@ 0x80000000
	mov	xxl, #0
5:
	.endif
	@ tailcalls are tricky on v6-m.
	push	{r0, r1, r2}
	ldr	r0, 1f
	adr	r1, 1f
	add	r0, r1
	str	r0, [sp, #8]
	@ We know we are not on armv4t, so pop pc is safe.
	pop	{r0, r1, pc}
	.align	2
1:
	.word	__aeabi_ldiv0 - 1b
7:
.endm

#ifdef L_aeabi_ldivmod

FUNC_START aeabi_ldivmod
	test_div_by_zero signed

	push {r0, r1}
	mov r0, sp
	push {r0, lr}
	ldr r0, [sp, #8]
	bl SYM(__gnu_ldivmod_helper)
	ldr r3, [sp, #4]
	mov lr, r3
	add sp, sp, #8
	pop {r2, r3}
	RET
	FUNC_END aeabi_ldivmod

#endif /* L_aeabi_ldivmod */

#ifdef L_aeabi_uldivmod

FUNC_START aeabi_uldivmod
	test_div_by_zero unsigned

	push {r0, r1}
	mov r0, sp
	push {r0, lr}
	ldr r0, [sp, #8]
	bl SYM(__gnu_uldivmod_helper)
	ldr r3, [sp, #4]
	mov lr, r3
	add sp, sp, #8
	pop {r2, r3}
	RET
	FUNC_END aeabi_uldivmod
	
#endif /* L_aeabi_uldivmod */

#ifdef L_arm_addsubsf3

FUNC_START aeabi_frsub

      push	{r4, lr}
      mov	r4, #1
      lsl	r4, #31
      eor	r0, r0, r4
      bl	__aeabi_fadd
      pop	{r4, pc}

      FUNC_END aeabi_frsub

#endif /* L_arm_addsubsf3 */

#ifdef L_arm_cmpsf2

FUNC_START aeabi_cfrcmple

	mov	ip, r0
	mov	r0, r1
	mov	r1, ip
	b	6f

FUNC_START aeabi_cfcmpeq
FUNC_ALIAS aeabi_cfcmple aeabi_cfcmpeq

	@ The status-returning routines are required to preserve all
	@ registers except ip, lr, and cpsr.
6:	push	{r0, r1, r2, r3, r4, lr}
	bl	__lesf2
	@ Set the Z flag correctly, and the C flag unconditionally.
	cmp	r0, #0
	@ Clear the C flag if the return value was -1, indicating
	@ that the first operand was smaller than the second.
	bmi 1f
	mov	r1, #0
	cmn	r0, r1
1:
	pop	{r0, r1, r2, r3, r4, pc}

	FUNC_END aeabi_cfcmple
	FUNC_END aeabi_cfcmpeq
	FUNC_END aeabi_cfrcmple

FUNC_START	aeabi_fcmpeq

	push	{r4, lr}
	bl	__eqsf2
	neg	r0, r0
	add	r0, r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_fcmpeq

.macro COMPARISON cond, helper, mode=sf2
FUNC_START	aeabi_fcmp\cond

	push	{r4, lr}
	bl	__\helper\mode
	cmp	r0, #0
	b\cond	1f
	mov	r0, #0
	pop	{r4, pc}
1:
	mov	r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_fcmp\cond
.endm

COMPARISON lt, le
COMPARISON le, le
COMPARISON gt, ge
COMPARISON ge, ge

#endif /* L_arm_cmpsf2 */

#ifdef L_arm_addsubdf3

FUNC_START aeabi_drsub

      push	{r4, lr}
      mov	r4, #1
      lsl	r4, #31
      eor	xxh, xxh, r4
      bl	__aeabi_dadd
      pop	{r4, pc}

      FUNC_END aeabi_drsub

#endif /* L_arm_addsubdf3 */

#ifdef L_arm_cmpdf2

FUNC_START aeabi_cdrcmple

	mov	ip, r0
	mov	r0, r2
	mov	r2, ip
	mov	ip, r1
	mov	r1, r3
	mov	r3, ip
	b	6f

FUNC_START aeabi_cdcmpeq
FUNC_ALIAS aeabi_cdcmple aeabi_cdcmpeq

	@ The status-returning routines are required to preserve all
	@ registers except ip, lr, and cpsr.
6:	push	{r0, r1, r2, r3, r4, lr}
	bl	__ledf2
	@ Set the Z flag correctly, and the C flag unconditionally.
	cmp	r0, #0
	@ Clear the C flag if the return value was -1, indicating
	@ that the first operand was smaller than the second.
	bmi 1f
	mov	r1, #0
	cmn	r0, r1
1:
	pop	{r0, r1, r2, r3, r4, pc}

	FUNC_END aeabi_cdcmple
	FUNC_END aeabi_cdcmpeq
	FUNC_END aeabi_cdrcmple

FUNC_START	aeabi_dcmpeq

	push	{r4, lr}
	bl	__eqdf2
	neg	r0, r0
	add	r0, r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_dcmpeq

.macro COMPARISON cond, helper, mode=df2
FUNC_START	aeabi_dcmp\cond

	push	{r4, lr}
	bl	__\helper\mode
	cmp	r0, #0
	b\cond	1f
	mov	r0, #0
	pop	{r4, pc}
1:
	mov	r0, #1
	pop	{r4, pc}

	FUNC_END aeabi_dcmp\cond
.endm

COMPARISON lt, le
COMPARISON le, le
COMPARISON gt, ge
COMPARISON ge, ge

#endif /* L_arm_cmpdf2 */
