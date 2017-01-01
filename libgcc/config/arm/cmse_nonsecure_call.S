/* CMSE wrapper function used to save, clear and restore callee saved registers
   for cmse_nonsecure_call's.

   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

.syntax unified
.thumb
.global __gnu_cmse_nonsecure_call
__gnu_cmse_nonsecure_call:
#if defined(__ARM_ARCH_8M_MAIN__)
push	    {r5-r11,lr}
mov	    r7, r4
mov	    r8, r4
mov	    r9, r4
mov	    r10, r4
mov	    r11, r4
mov	    ip, r4

/* Save and clear callee-saved registers only if we are dealing with hard float
   ABI.  The unused caller-saved registers have already been cleared by GCC
   generated code.  */
#ifdef __ARM_PCS_VFP
vpush.f64   {d8-d15}
mov	    r5, #0
vmov	    d8, r5, r5
#if __ARM_FP & 0x04
vmov	    s18, s19, r5, r5
vmov	    s20, s21, r5, r5
vmov	    s22, s23, r5, r5
vmov	    s24, s25, r5, r5
vmov	    s26, s27, r5, r5
vmov	    s28, s29, r5, r5
vmov	    s30, s31, r5, r5
#elif __ARM_FP & 0x08
vmov.f64    d9, d8
vmov.f64    d10, d8
vmov.f64    d11, d8
vmov.f64    d12, d8
vmov.f64    d13, d8
vmov.f64    d14, d8
vmov.f64    d15, d8
#else
#error "Half precision implementation not supported."
#endif
/* Clear the cumulative exception-status bits (0-4,7) and the
   condition code bits (28-31) of the FPSCR.  */
vmrs	    r5, fpscr
movw	    r6, #65376
movt	    r6, #4095
ands	    r5, r6
vmsr	    fpscr, r5

/* We are not dealing with hard float ABI, so we can safely use the vlstm and
   vlldm instructions without needing to preserve the registers used for
   argument passing.  */
#else
sub	    sp, sp, #0x88 /* Reserve stack space to save all floating point
			     registers, including FPSCR.  */
vlstm	    sp		  /* Lazy store and clearance of d0-d16 and FPSCR.  */
#endif /* __ARM_PCS_VFP */

/* Make sure to clear the 'GE' bits of the APSR register if 32-bit SIMD
   instructions are available.  */
#if defined(__ARM_FEATURE_SIMD32)
msr	    APSR_nzcvqg, r4
#else
msr	    APSR_nzcvq, r4
#endif

mov	    r5, r4
mov	    r6, r4
blxns	    r4

#ifdef __ARM_PCS_VFP
vpop.f64    {d8-d15}
#else
vlldm	    sp		  /* Lazy restore of d0-d16 and FPSCR.  */
add	    sp, sp, #0x88 /* Free space used to save floating point registers.  */
#endif /* __ARM_PCS_VFP */

pop	    {r5-r11, pc}

#elif defined (__ARM_ARCH_8M_BASE__)
push	    {r5-r7, lr}
mov	    r5, r8
mov	    r6, r9
mov	    r7, r10
push	    {r5-r7}
mov	    r5, r11
push	    {r5}
mov	    r5, r4
mov	    r6, r4
mov	    r7, r4
mov	    r8, r4
mov	    r9, r4
mov	    r10, r4
mov	    r11, r4
mov	    ip, r4
msr	    APSR_nzcvq, r4
blxns	    r4
pop	    {r5}
mov	    r11, r5
pop	    {r5-r7}
mov	    r10, r7
mov	    r9, r6
mov	    r8, r5
pop	    {r5-r7, pc}

#else
#error "This should only be used for armv8-m base- and mainline."
#endif
