/* Support functions for the unwinder.
   Copyright (C) 2003-2020 Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* An executable stack is *not* required for these functions.  */
#if defined(__ELF__) && defined(__linux__)
.section .note.GNU-stack,"",%progbits
.previous
#endif

#ifdef __ARM_EABI__
/* Some attributes that are common to all routines in this file.  */
	/* Tag_ABI_align_needed: This code does not require 8-byte
	   alignment from the caller.  */
	/* .eabi_attribute 24, 0  -- default setting.  */
	/* Tag_ABI_align_preserved: This code preserves 8-byte
	   alignment in any callee.  */
	.eabi_attribute 25, 1
#endif /* __ARM_EABI__ */

#ifndef __symbian__

#include "lib1funcs.S"

.macro UNPREFIX name
	.global SYM (\name)
	EQUIV SYM (\name), SYM (__\name)
.endm

#if (__ARM_ARCH == 4)
/* Some coprocessors require armv5t.  We know this code will never be run on
   other cpus.  Tell gas to allow armv5t, but only mark the objects as armv4.
 */
.arch armv5t
#ifdef __ARM_ARCH_4T__
.object_arch armv4t
#else
.object_arch armv4
#endif
#endif

#if !__ARM_ARCH_ISA_ARM && __ARM_ARCH_ISA_THUMB == 1

/* r0 points to a 16-word block.  Upload these values to the actual core
   state.  */
FUNC_START restore_core_regs
	movs	r1, r0
	adds	r1, r1, #52
	ldmia	r1!, {r3, r4, r5}
	subs	r3, r3, #4
	mov	ip, r3
	str	r5, [r3]
	mov	lr, r4
	/* Restore r8-r11.  */
	movs	r1, r0
	adds	r1, r1, #32
	ldmia	r1!, {r2, r3, r4, r5}
	mov	r8, r2
	mov	r9, r3
	mov	sl, r4
	mov	fp, r5
	movs	r1, r0
	adds	r1, r1, #8
	ldmia	r1!, {r2, r3, r4, r5, r6, r7}
	ldr	r1, [r0, #4]
	ldr	r0, [r0]
	mov	sp, ip
	pop	{pc}
	FUNC_END restore_core_regs
	UNPREFIX restore_core_regs

/* ARMV6M does not have coprocessors, so these should never be used.  */
FUNC_START gnu_Unwind_Restore_VFP
	RET

/* Store VFR regsters d0-d15 to the address in r0.  */
FUNC_START gnu_Unwind_Save_VFP
	RET

/* Load VFP registers d0-d15 from the address in r0.
   Use this to load from FSTMD format.  */
FUNC_START gnu_Unwind_Restore_VFP_D
	RET

/* Store VFP registers d0-d15 to the address in r0.
   Use this to store in FLDMD format.  */
FUNC_START gnu_Unwind_Save_VFP_D
	RET

/* Load VFP registers d16-d31 from the address in r0.
   Use this to load from FSTMD (=VSTM) format.  Needs VFPv3.  */
FUNC_START gnu_Unwind_Restore_VFP_D_16_to_31
	RET

/* Store VFP registers d16-d31 to the address in r0.
   Use this to store in FLDMD (=VLDM) format.  Needs VFPv3.  */
FUNC_START gnu_Unwind_Save_VFP_D_16_to_31
	RET

FUNC_START gnu_Unwind_Restore_WMMXD
	RET

FUNC_START gnu_Unwind_Save_WMMXD
	RET

FUNC_START gnu_Unwind_Restore_WMMXC
	RET

FUNC_START gnu_Unwind_Save_WMMXC
	RET

.macro  UNWIND_WRAPPER name nargs
	FUNC_START \name
	/* Create a phase2_vrs structure.  */
	/* Save r0 in the PC slot so we can use it as a scratch register.  */
	push	{r0}
	add	r0, sp, #4
	push	{r0, lr} /* Push original SP and LR.  */
	/* Make space for r8-r12.  */
	sub	sp, sp, #20
	/* Save low registers.  */
	push	{r0, r1, r2, r3, r4, r5, r6, r7}
	/* Save high registers.  */
	add	r0, sp, #32
	mov	r1, r8
	mov	r2, r9
	mov	r3, sl
	mov	r4, fp
	mov	r5, ip
	stmia	r0!, {r1, r2, r3, r4, r5}
	/* Restore original low register values.  */
	add	r0, sp, #4
	ldmia	r0!, {r1, r2, r3, r4, r5}
	/* Restore orginial r0.  */
	ldr	r0, [sp, #60]
	str	r0, [sp]
	/* Demand-save flags, plus an extra word for alignment.  */
	movs	r3, #0
	push	{r2, r3}
	/* Point r1 at the block.  Pass r[0..nargs) unchanged.  */
	add	r\nargs, sp, #4

	bl	SYM (__gnu\name)

	ldr	r3, [sp, #64]
	add	sp, sp, #72
	bx	r3

	FUNC_END \name
	UNPREFIX \name
.endm

#else /* __ARM_ARCH_ISA_ARM || __ARM_ARCH_ISA_THUMB != 1 */

/* r0 points to a 16-word block.  Upload these values to the actual core
   state.  */
ARM_FUNC_START restore_core_regs
	/* We must use sp as the base register when restoring sp.  Push the
	   last 3 registers onto the top of the current stack to achieve
	   this.  */
	add r1, r0, #52
	ldmia r1, {r3, r4, r5}  /* {sp, lr, pc}.  */
#if defined(__thumb2__)
	/* Thumb-2 doesn't allow sp in a load-multiple instruction, so push
	   the target address onto the target stack.  This is safe as
	   we're always returning to somewhere further up the call stack.  */
	mov ip, r3
	mov lr, r4
	str r5, [ip, #-4]!
#elif defined(__INTERWORKING__)
	/* Restore pc into ip.  */
	mov r2, r5
	stmfd sp!, {r2, r3, r4}
#else
	stmfd sp!, {r3, r4, r5}
#endif
	/* Don't bother restoring ip.  */
	ldmia r0, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp}
#if defined(__thumb2__)
	/* Pop the return address off the target stack.  */
	mov sp, ip
	pop {pc}
#elif defined(__INTERWORKING__)
	/* Pop the three registers we pushed earlier.  */
	ldmfd sp, {ip, sp, lr}
	bx ip
#else
	ldmfd sp, {sp, lr, pc}
#endif
	FUNC_END restore_core_regs
	UNPREFIX restore_core_regs

/* Load VFP registers d0-d15 from the address in r0.
   Use this to load from FSTMX format.  */
ARM_FUNC_START gnu_Unwind_Restore_VFP
	/* Use the generic coprocessor form so that gas doesn't complain
	   on soft-float targets.  */
	ldc   p11,cr0,[r0],{0x21} /* fldmiax r0, {d0-d15} */
	RET

/* Store VFP registers d0-d15 to the address in r0.
   Use this to store in FSTMX format.  */
ARM_FUNC_START gnu_Unwind_Save_VFP
	/* Use the generic coprocessor form so that gas doesn't complain
	   on soft-float targets.  */
	stc   p11,cr0,[r0],{0x21} /* fstmiax r0, {d0-d15} */
	RET

/* Load VFP registers d0-d15 from the address in r0.
   Use this to load from FSTMD format.  */
ARM_FUNC_START gnu_Unwind_Restore_VFP_D
	ldc   p11,cr0,[r0],{0x20} /* fldmiad r0, {d0-d15} */
	RET

/* Store VFP registers d0-d15 to the address in r0.
   Use this to store in FLDMD format.  */
ARM_FUNC_START gnu_Unwind_Save_VFP_D
	stc   p11,cr0,[r0],{0x20} /* fstmiad r0, {d0-d15} */
	RET

/* Load VFP registers d16-d31 from the address in r0.
   Use this to load from FSTMD (=VSTM) format.  Needs VFPv3.  */
ARM_FUNC_START gnu_Unwind_Restore_VFP_D_16_to_31
	ldcl  p11,cr0,[r0],{0x20} /* vldm r0, {d16-d31} */
	RET

/* Store VFP registers d16-d31 to the address in r0.
   Use this to store in FLDMD (=VLDM) format.  Needs VFPv3.  */
ARM_FUNC_START gnu_Unwind_Save_VFP_D_16_to_31
	stcl  p11,cr0,[r0],{0x20} /* vstm r0, {d16-d31} */
	RET

ARM_FUNC_START gnu_Unwind_Restore_WMMXD
	/* Use the generic coprocessor form so that gas doesn't complain
	   on non-iWMMXt targets.  */
	ldcl  p1, cr0, [r0], #8 /* wldrd wr0, [r0], #8 */
	ldcl  p1, cr1, [r0], #8 /* wldrd wr1, [r0], #8 */
	ldcl  p1, cr2, [r0], #8 /* wldrd wr2, [r0], #8 */
	ldcl  p1, cr3, [r0], #8 /* wldrd wr3, [r0], #8 */
	ldcl  p1, cr4, [r0], #8 /* wldrd wr4, [r0], #8 */
	ldcl  p1, cr5, [r0], #8 /* wldrd wr5, [r0], #8 */
	ldcl  p1, cr6, [r0], #8 /* wldrd wr6, [r0], #8 */
	ldcl  p1, cr7, [r0], #8 /* wldrd wr7, [r0], #8 */
	ldcl  p1, cr8, [r0], #8 /* wldrd wr8, [r0], #8 */
	ldcl  p1, cr9, [r0], #8 /* wldrd wr9, [r0], #8 */
	ldcl  p1, cr10, [r0], #8 /* wldrd wr10, [r0], #8 */
	ldcl  p1, cr11, [r0], #8 /* wldrd wr11, [r0], #8 */
	ldcl  p1, cr12, [r0], #8 /* wldrd wr12, [r0], #8 */
	ldcl  p1, cr13, [r0], #8 /* wldrd wr13, [r0], #8 */
	ldcl  p1, cr14, [r0], #8 /* wldrd wr14, [r0], #8 */
	ldcl  p1, cr15, [r0], #8 /* wldrd wr15, [r0], #8 */
	RET

ARM_FUNC_START gnu_Unwind_Save_WMMXD
	/* Use the generic coprocessor form so that gas doesn't complain
	   on non-iWMMXt targets.  */
	stcl  p1, cr0, [r0], #8 /* wstrd wr0, [r0], #8 */
	stcl  p1, cr1, [r0], #8 /* wstrd wr1, [r0], #8 */
	stcl  p1, cr2, [r0], #8 /* wstrd wr2, [r0], #8 */
	stcl  p1, cr3, [r0], #8 /* wstrd wr3, [r0], #8 */
	stcl  p1, cr4, [r0], #8 /* wstrd wr4, [r0], #8 */
	stcl  p1, cr5, [r0], #8 /* wstrd wr5, [r0], #8 */
	stcl  p1, cr6, [r0], #8 /* wstrd wr6, [r0], #8 */
	stcl  p1, cr7, [r0], #8 /* wstrd wr7, [r0], #8 */
	stcl  p1, cr8, [r0], #8 /* wstrd wr8, [r0], #8 */
	stcl  p1, cr9, [r0], #8 /* wstrd wr9, [r0], #8 */
	stcl  p1, cr10, [r0], #8 /* wstrd wr10, [r0], #8 */
	stcl  p1, cr11, [r0], #8 /* wstrd wr11, [r0], #8 */
	stcl  p1, cr12, [r0], #8 /* wstrd wr12, [r0], #8 */
	stcl  p1, cr13, [r0], #8 /* wstrd wr13, [r0], #8 */
	stcl  p1, cr14, [r0], #8 /* wstrd wr14, [r0], #8 */
	stcl  p1, cr15, [r0], #8 /* wstrd wr15, [r0], #8 */
	RET

ARM_FUNC_START gnu_Unwind_Restore_WMMXC
	/* Use the generic coprocessor form so that gas doesn't complain
	   on non-iWMMXt targets.  */
	ldc2  p1, cr8, [r0], #4 /* wldrw wcgr0, [r0], #4 */
	ldc2  p1, cr9, [r0], #4 /* wldrw wcgr1, [r0], #4 */
	ldc2  p1, cr10, [r0], #4 /* wldrw wcgr2, [r0], #4 */
	ldc2  p1, cr11, [r0], #4 /* wldrw wcgr3, [r0], #4 */
	RET

ARM_FUNC_START gnu_Unwind_Save_WMMXC
	/* Use the generic coprocessor form so that gas doesn't complain
	   on non-iWMMXt targets.  */
	stc2  p1, cr8, [r0], #4 /* wstrw wcgr0, [r0], #4 */
	stc2  p1, cr9, [r0], #4 /* wstrw wcgr1, [r0], #4 */
	stc2  p1, cr10, [r0], #4 /* wstrw wcgr2, [r0], #4 */
	stc2  p1, cr11, [r0], #4 /* wstrw wcgr3, [r0], #4 */
	RET

/* Wrappers to save core registers, then call the real routine.   */

.macro  UNWIND_WRAPPER name nargs
	ARM_FUNC_START \name
	/* Create a phase2_vrs structure.  */
	/* Split reg push in two to ensure the correct value for sp.  */
#if defined(__thumb2__)
	mov ip, sp
	push {lr} /* PC is ignored.  */
	push {ip, lr} /* Push original SP and LR.  */
#else
	stmfd sp!, {sp, lr, pc}
#endif
	stmfd sp!, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
	
	/* Demand-save flags, plus an extra word for alignment.  */
	mov r3, #0
	stmfd sp!, {r2, r3}

	/* Point r1 at the block.  Pass r[0..nargs) unchanged.  */
	add r\nargs, sp, #4
#if defined(__thumb__) && !defined(__thumb2__)
	/* Switch back to thumb mode to avoid interworking hassle.  */
	adr ip, .L1_\name
	orr ip, ip, #1
	bx ip
	.thumb
.L1_\name:
	bl SYM (__gnu\name) __PLT__
	ldr r3, [sp, #64]
	add sp, #72
	bx r3
#else
	bl SYM (__gnu\name) __PLT__
	ldr lr, [sp, #64]
	add sp, sp, #72
	RET
#endif
	FUNC_END \name
	UNPREFIX \name
.endm

#endif /* __ARM_ARCH_ISA_ARM || __ARM_ARCH_ISA_THUMB != 1 */

UNWIND_WRAPPER _Unwind_RaiseException 1
UNWIND_WRAPPER _Unwind_Resume 1
UNWIND_WRAPPER _Unwind_Resume_or_Rethrow 1
UNWIND_WRAPPER _Unwind_ForcedUnwind 3
UNWIND_WRAPPER _Unwind_Backtrace 2

#endif  /* ndef __symbian__ */
