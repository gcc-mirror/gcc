/*  -*- Mode: Asm -*-  */
/* Copyright (C) 1998-2025 Free Software Foundation, Inc.
   Contributed by Denis Chertykov <chertykov@gmail.com>

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

#if defined (__AVR_TINY__)
#define __zero_reg__ r17
#define __tmp_reg__ r16
#else
#define __zero_reg__ r1
#define __tmp_reg__ r0
#endif
#define __SREG__ 0x3f
#if defined (__AVR_HAVE_SPH__)
#define __SP_H__ 0x3e
#endif
#define __SP_L__ 0x3d
#define __RAMPZ__ 0x3B
#define __EIND__  0x3C

/* Most of the functions here are called directly from avr.md
   patterns, instead of using the standard libcall mechanisms.
   This can make better code because GCC knows exactly which
   of the call-used registers (not all of them) are clobbered.  */

/* FIXME:  At present, there is no SORT directive in the linker
           script so that we must not assume that different modules
           in the same input section like .libgcc.text.mul will be
           located close together.  Therefore, we cannot use
           RCALL/RJMP to call a function like __udivmodhi4 from
           __divmodhi4 and have to use lengthy XCALL/XJMP even
           though they are in the same input section and all same
           input sections together are small enough to reach every
           location with a RCALL/RJMP instruction.  */

#if defined (__AVR_HAVE_EIJMP_EICALL__) && !defined (__AVR_HAVE_ELPMX__)
#error device not supported
#endif

	.macro	mov_l  r_dest, r_src
#if defined (__AVR_HAVE_MOVW__)
	movw	\r_dest, \r_src
#else
	mov	\r_dest, \r_src
#endif
	.endm

	.macro	mov_h  r_dest, r_src
#if defined (__AVR_HAVE_MOVW__)
	; empty
#else
	mov	\r_dest, \r_src
#endif
	.endm

.macro	wmov  r_dest, r_src
#if defined (__AVR_HAVE_MOVW__)
    movw \r_dest,   \r_src
#else
    mov \r_dest,    \r_src
    mov \r_dest+1,  \r_src+1
#endif
.endm

.macro	mov4  r_dest, r_src
    wmov \r_dest,   \r_src
    wmov \r_dest+2, \r_src+2
.endm

#if defined (__AVR_HAVE_JMP_CALL__)
#define XCALL call
#define XJMP  jmp
#else
#define XCALL rcall
#define XJMP  rjmp
#endif

#if defined (__AVR_HAVE_EIJMP_EICALL__)
#define XICALL eicall
#define XIJMP  eijmp
#else
#define XICALL icall
#define XIJMP  ijmp
#endif

;;; [R]JMP to label \labl when \reg is positive (\reg.7 = 0).
;;; Otherwise, fallthrough.
.macro  .branch_plus    reg, labl
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Some cores have a problem skipping 2-word instructions
    tst     \reg
    brmi    .L..\@
#else
    sbrs    \reg, 7
#endif /* skip erratum */
    XJMP    \labl
.L..\@:
.endm ; .branch_plus

;; Prologue stuff

.macro do_prologue_saves n_pushed n_frame=0
    ldi r26, lo8(\n_frame)
    ldi r27, hi8(\n_frame)
    ldi r30, lo8(gs(.L_prologue_saves.\@))
    ldi r31, hi8(gs(.L_prologue_saves.\@))
    XJMP __prologue_saves__ + ((18 - (\n_pushed)) * 2)
.L_prologue_saves.\@:
.endm

;; Epilogue stuff

.macro do_epilogue_restores n_pushed n_frame=0
    in      r28, __SP_L__
#ifdef __AVR_HAVE_SPH__
    in      r29, __SP_H__
.if \n_frame > 63
    subi    r28, lo8(-\n_frame)
    sbci    r29, hi8(-\n_frame)
.elseif \n_frame > 0
    adiw    r28, \n_frame
.endif
#else
    clr     r29
.if \n_frame > 0
    subi    r28, lo8(-\n_frame)
.endif
#endif /* HAVE SPH */
    ldi     r30, \n_pushed
    XJMP __epilogue_restores__ + ((18 - (\n_pushed)) * 2)
.endm

;; Support function entry and exit for convenience

.macro wsubi r_arg1, i_arg2
#if defined (__AVR_TINY__)
    subi \r_arg1,   lo8(\i_arg2)
    sbci \r_arg1+1, hi8(\i_arg2)
#else
    sbiw \r_arg1, \i_arg2
#endif
.endm

.macro waddi r_arg1, i_arg2
#if defined (__AVR_TINY__)
    subi \r_arg1,   lo8(-\i_arg2)
    sbci \r_arg1+1, hi8(-\i_arg2)
#else
    adiw \r_arg1, \i_arg2
#endif
.endm

.macro DEFUN name
.global \name
.func \name
\name:
.endm

.macro ENDF name
.size \name, .-\name
.endfunc
.endm

.macro FALIAS name
.global \name
.func \name
\name:
.size \name, .-\name
.endfunc
.endm

;; Skip next instruction, typically a jump target
#define skip cpse 16,16

;; Negate a 2-byte value held in consecutive registers
.macro NEG2  reg
    com     \reg+1
    neg     \reg
    sbci    \reg+1, -1
.endm

;; Negate a 4-byte value held in consecutive registers
;; Sets the V flag for signed overflow tests if REG >= 16
.macro NEG4  reg
    com     \reg+3
    com     \reg+2
    com     \reg+1
.if \reg >= 16
    neg     \reg
    sbci    \reg+1, -1
    sbci    \reg+2, -1
    sbci    \reg+3, -1
.else
    com     \reg
    adc     \reg,   __zero_reg__
    adc     \reg+1, __zero_reg__
    adc     \reg+2, __zero_reg__
    adc     \reg+3, __zero_reg__
.endif
.endm

#define exp_lo(N)  hlo8 ((N) << 23)
#define exp_hi(N)  hhi8 ((N) << 23)


.section .text.libgcc.mul, "ax", @progbits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/* Note: mulqi3, mulhi3 are open-coded on the enhanced core.  */
#if !defined (__AVR_HAVE_MUL__)
/*******************************************************
    Multiplication  8 x 8  without MUL
*******************************************************/
#if defined (L_mulqi3)

#define	r_arg2	r22		/* multiplicand */
#define	r_arg1 	r24		/* multiplier */
#define r_res	__tmp_reg__	/* result */

DEFUN __mulqi3
	clr	r_res		; clear result
__mulqi3_loop:
	sbrc	r_arg1,0
	add	r_res,r_arg2
	add	r_arg2,r_arg2	; shift multiplicand
	breq	__mulqi3_exit	; while multiplicand != 0
	lsr	r_arg1		;
	brne	__mulqi3_loop	; exit if multiplier = 0
__mulqi3_exit:	
	mov	r_arg1,r_res	; result to return register
	ret
ENDF __mulqi3

#undef r_arg2
#undef r_arg1
#undef r_res
	
#endif 	/* defined (L_mulqi3) */


/*******************************************************
    Widening Multiplication  16 = 8 x 8  without MUL
    Multiplication  16 x 16  without MUL
*******************************************************/

#define A0  22
#define A1  23
#define B0  24
#define BB0 20
#define B1  25
;; Output overlaps input, thus expand result in CC0/1
#define C0  24
#define C1  25
#define CC0  __tmp_reg__
#define CC1  21

#if defined (L_umulqihi3)
;;; R25:R24 = (unsigned int) R22 * (unsigned int) R24
;;; (C1:C0) = (unsigned int) A0  * (unsigned int) B0
;;; Clobbers: __tmp_reg__, R21..R23
DEFUN __umulqihi3
    clr     A1
    clr     B1
    XJMP    __mulhi3
ENDF __umulqihi3
#endif /* L_umulqihi3 */

#if defined (L_mulqihi3)
;;; R25:R24 = (signed int) R22 * (signed int) R24
;;; (C1:C0) = (signed int) A0  * (signed int) B0
;;; Clobbers: __tmp_reg__, R20..R23
DEFUN __mulqihi3
    ;; Sign-extend B0
    clr     B1
    sbrc    B0, 7
    com     B1
    ;; The multiplication runs twice as fast if A1 is zero, thus:
    ;; Zero-extend A0
    clr     A1
#ifdef __AVR_HAVE_JMP_CALL__
    ;; Store  B0 * sign of A
    clr     BB0
    sbrc    A0, 7
    mov     BB0, B0
    call    __mulhi3
#else /* have no CALL */
    ;; Skip sign-extension of A if A >= 0
    ;; Same size as with the first alternative but avoids errata skip
    ;; and is faster if A >= 0
    sbrs    A0, 7
    rjmp    __mulhi3
    ;; If  A < 0  store B
    mov     BB0, B0
    rcall   __mulhi3
#endif /* HAVE_JMP_CALL */
    ;; 1-extend A after the multiplication
    sub     C1, BB0
    ret
ENDF __mulqihi3
#endif /* L_mulqihi3 */

#if defined (L_mulhi3)
;;; R25:R24 = R23:R22 * R25:R24
;;; (C1:C0) = (A1:A0) * (B1:B0)
;;; Clobbers: __tmp_reg__, R21..R23
DEFUN __mulhi3

    ;; Clear result
    clr     CC0
    clr     CC1
    rjmp 3f
1:
    ;; Bit n of A is 1  -->  C += B << n
    add     CC0, B0
    adc     CC1, B1
2:
    lsl     B0
    rol     B1
3:
    ;; If B == 0 we are ready
    wsubi   B0, 0
    breq 9f

    ;; Carry = n-th bit of A
    lsr     A1
    ror     A0
    ;; If bit n of A is set, then go add  B * 2^n  to  C
    brcs 1b

    ;; Carry = 0  -->  The ROR above acts like  CP A0, 0
    ;; Thus, it is sufficient to CPC the high part to test A against 0
    cpc     A1, __zero_reg__
    ;; Only proceed if A != 0
    brne    2b
9:
    ;; Move Result into place
    mov     C0, CC0
    mov     C1, CC1
    ret
ENDF  __mulhi3
#endif /* L_mulhi3 */

#undef A0
#undef A1
#undef B0
#undef BB0
#undef B1
#undef C0
#undef C1
#undef CC0
#undef CC1


#define A0 22
#define A1 A0+1
#define A2 A0+2
#define A3 A0+3

#define B0 18
#define B1 B0+1
#define B2 B0+2
#define B3 B0+3

#define CC0 26
#define CC1 CC0+1
#define CC2 30
#define CC3 CC2+1

#define C0 22
#define C1 C0+1
#define C2 C0+2
#define C3 C0+3

/*******************************************************
    Widening Multiplication  32 = 16 x 16  without MUL
*******************************************************/

#if defined (L_umulhisi3)
DEFUN __umulhisi3
    wmov    B0, 24
    ;; Zero-extend B
    clr     B2
    clr     B3
    ;; Zero-extend A
    wmov    A2, B2
    XJMP    __mulsi3
ENDF __umulhisi3
#endif /* L_umulhisi3 */

#if defined (L_mulhisi3)
DEFUN __mulhisi3
    wmov    B0, 24
    ;; Sign-extend B
    lsl     r25
    sbc     B2, B2
    mov     B3, B2
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Sign-extend A
    clr     A2
    sbrc    A1, 7
    com     A2
    mov     A3, A2
    XJMP __mulsi3
#else /*  no __AVR_ERRATA_SKIP_JMP_CALL__ */
    ;; Zero-extend A and __mulsi3 will run at least twice as fast
    ;; compared to a sign-extended A.
    clr     A2
    clr     A3
    sbrs    A1, 7
    XJMP __mulsi3
    ;; If  A < 0  then perform the  B * 0xffff.... before the
    ;; very multiplication by initializing the high part of the
    ;; result CC with -B.
    wmov    CC2, A2
    sub     CC2, B0
    sbc     CC3, B1
    XJMP __mulsi3_helper
#endif /*  __AVR_ERRATA_SKIP_JMP_CALL__ */
ENDF __mulhisi3
#endif /* L_mulhisi3 */


/*******************************************************
    Multiplication  32 x 32  without MUL
*******************************************************/

#if defined (L_mulsi3)
DEFUN __mulsi3
#if defined (__AVR_TINY__)
    in     r26, __SP_L__ ; safe to use X, as it is CC0/CC1
    in     r27, __SP_H__
    subi   r26, lo8(-3)   ; Add 3 to point past return address
    sbci   r27, hi8(-3)
    push   B0    ; save callee saved regs
    push   B1
    ld     B0, X+   ; load from caller stack
    ld     B1, X+
    ld     B2, X+
    ld     B3, X
#endif
    ;; Clear result
    clr     CC2
    clr     CC3
    ;; FALLTHRU
ENDF  __mulsi3

DEFUN __mulsi3_helper
    clr     CC0
    clr     CC1
    rjmp 3f

1:  ;; If bit n of A is set, then add  B * 2^n  to the result in CC
    ;; CC += B
    add  CC0,B0  $  adc  CC1,B1  $  adc  CC2,B2  $  adc  CC3,B3

2:  ;; B <<= 1
    lsl  B0      $  rol  B1      $  rol  B2      $  rol  B3

3:  ;; A >>= 1:  Carry = n-th bit of A
    lsr  A3      $  ror  A2      $  ror  A1      $  ror  A0

    brcs 1b
    ;; Only continue if  A != 0
    sbci    A1, 0
    brne 2b
    wsubi   A2, 0
    brne 2b

    ;; All bits of A are consumed:  Copy result to return register C
    wmov    C0, CC0
    wmov    C2, CC2
#if defined (__AVR_TINY__)
    pop     B1      ; restore callee saved regs
    pop     B0 
#endif  /* defined (__AVR_TINY__) */

    ret
ENDF __mulsi3_helper
#endif /* L_mulsi3 */

#undef A0
#undef A1
#undef A2
#undef A3
#undef B0
#undef B1
#undef B2
#undef B3
#undef C0
#undef C1
#undef C2
#undef C3
#undef CC0
#undef CC1
#undef CC2
#undef CC3

#endif /* !defined (__AVR_HAVE_MUL__) */
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#if defined (__AVR_HAVE_MUL__)
#define A0 26
#define B0 18
#define C0 22

#define A1 A0+1

#define B1 B0+1
#define B2 B0+2
#define B3 B0+3

#define C1 C0+1
#define C2 C0+2
#define C3 C0+3

/*******************************************************
    Widening Multiplication  32 = 16 x 16  with MUL
*******************************************************/

#if defined (L_mulhisi3)
;;; R25:R22 = (signed long) R27:R26 * (signed long) R19:R18
;;; C3:C0   = (signed long) A1:A0   * (signed long) B1:B0
;;; Clobbers: __tmp_reg__
DEFUN __mulhisi3
    XCALL   __umulhisi3
    ;; Sign-extend B
    tst     B1
    brpl    1f
    sub     C2, A0
    sbc     C3, A1
1:  ;; Sign-extend A
    XJMP __usmulhisi3_tail
ENDF __mulhisi3
#endif /* L_mulhisi3 */

#if defined (L_usmulhisi3)
;;; R25:R22 = (signed long) R27:R26 * (unsigned long) R19:R18
;;; C3:C0   = (signed long) A1:A0   * (unsigned long) B1:B0
;;; Clobbers: __tmp_reg__
DEFUN __usmulhisi3
    XCALL   __umulhisi3
    ;; FALLTHRU
ENDF __usmulhisi3

DEFUN __usmulhisi3_tail
    ;; Sign-extend A
    sbrs    A1, 7
    ret
    sub     C2, B0
    sbc     C3, B1
    ret
ENDF __usmulhisi3_tail
#endif /* L_usmulhisi3 */

#if defined (L_umulhisi3)
;;; R25:R22 = (unsigned long) R27:R26 * (unsigned long) R19:R18
;;; C3:C0   = (unsigned long) A1:A0   * (unsigned long) B1:B0
;;; Clobbers: __tmp_reg__
DEFUN __umulhisi3
    mul     A0, B0
    movw    C0, r0
    mul     A1, B1
    movw    C2, r0
    mul     A0, B1
#ifdef __AVR_HAVE_JMP_CALL__
    ;; This function is used by many other routines, often multiple times.
    ;; Therefore, if the flash size is not too limited, avoid the RCALL
    ;; and inverst 6 Bytes to speed things up.
    add     C1, r0
    adc     C2, r1
    clr     __zero_reg__
    adc     C3, __zero_reg__
#else
    rcall   1f
#endif
    mul     A1, B0
1:  add     C1, r0
    adc     C2, r1
    clr     __zero_reg__
    adc     C3, __zero_reg__
    ret
ENDF __umulhisi3
#endif /* L_umulhisi3 */

/*******************************************************
    Widening Multiplication  32 = 16 x 32  with MUL
*******************************************************/

#if defined (L_mulshisi3)
;;; R25:R22 = (signed long) R27:R26 * R21:R18
;;; (C3:C0) = (signed long) A1:A0   * B3:B0
;;; Clobbers: __tmp_reg__
DEFUN __mulshisi3
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Some cores have problem skipping 2-word instruction
    tst     A1
    brmi    __mulohisi3
#else
    sbrs    A1, 7
#endif /* __AVR_HAVE_JMP_CALL__ */
    XJMP    __muluhisi3
    ;; FALLTHRU
ENDF __mulshisi3

;;; R25:R22 = (one-extended long) R27:R26 * R21:R18
;;; (C3:C0) = (one-extended long) A1:A0   * B3:B0
;;; Clobbers: __tmp_reg__
DEFUN __mulohisi3
    XCALL   __muluhisi3
    ;; One-extend R27:R26 (A1:A0)
    sub     C2, B0
    sbc     C3, B1
    ret
ENDF __mulohisi3
#endif /* L_mulshisi3 */

#if defined (L_muluhisi3)
;;; R25:R22 = (unsigned long) R27:R26 * R21:R18
;;; (C3:C0) = (unsigned long) A1:A0   * B3:B0
;;; Clobbers: __tmp_reg__
DEFUN __muluhisi3
    XCALL   __umulhisi3
    mul     A0, B3
    add     C3, r0
    mul     A1, B2
    add     C3, r0
    mul     A0, B2
    add     C2, r0
    adc     C3, r1
    clr     __zero_reg__
    ret
ENDF __muluhisi3
#endif /* L_muluhisi3 */

/*******************************************************
    Multiplication  32 x 32  with MUL
*******************************************************/

#if defined (L_mulsi3)
;;; R25:R22 = R25:R22 * R21:R18
;;; (C3:C0) = C3:C0   * B3:B0
;;; Clobbers: R26, R27, __tmp_reg__
DEFUN __mulsi3
    movw    A0, C0
    push    C2
    push    C3
    XCALL   __muluhisi3
    pop     A1
    pop     A0
    ;; A1:A0 now contains the high word of A
    mul     A0, B0
    add     C2, r0
    adc     C3, r1
    mul     A0, B1
    add     C3, r0
    mul     A1, B0
    add     C3, r0
    clr     __zero_reg__
    ret
ENDF __mulsi3
#endif /* L_mulsi3 */

#undef A0
#undef A1

#undef B0
#undef B1
#undef B2
#undef B3

#undef C0
#undef C1
#undef C2
#undef C3

#endif /* __AVR_HAVE_MUL__ */

/*******************************************************
       Multiplication 24 x 24 with MUL
*******************************************************/

#if defined (L_mulpsi3)

;; A[0..2]: In: Multiplicand; Out: Product
#define A0  22
#define A1  A0+1
#define A2  A0+2

;; B[0..2]: In: Multiplier
#define B0  18
#define B1  B0+1
#define B2  B0+2

#if defined (__AVR_HAVE_MUL__)

;; C[0..2]: Expand Result
#define C0  22
#define C1  C0+1
#define C2  C0+2

;; R24:R22 *= R20:R18
;; Clobbers: r21, r25, r26, r27, __tmp_reg__

#define AA0 26
#define AA2 21

DEFUN __mulpsi3
    wmov    AA0, A0
    mov     AA2, A2
    XCALL   __umulhisi3
    mul     AA2, B0     $  add  C2, r0
    mul     AA0, B2     $  add  C2, r0
    clr     __zero_reg__
    ret
ENDF __mulpsi3

#undef AA2
#undef AA0

#undef C2
#undef C1
#undef C0

#else /* !HAVE_MUL */
;; C[0..2]: Expand Result
#if defined (__AVR_TINY__)
#define C0  16
#else
#define C0  0
#endif /* defined (__AVR_TINY__) */
#define C1  C0+1
#define C2  21

;; R24:R22 *= R20:R18
;; Clobbers: __tmp_reg__, R18, R19, R20, R21

DEFUN __mulpsi3
#if defined (__AVR_TINY__)
    in r26,__SP_L__ 
    in r27,__SP_H__
    subi r26, lo8(-3)   ; Add 3 to point past return address
    sbci r27, hi8(-3)
    push B0    ; save callee saved regs
    push B1
    ld B0,X+   ; load from caller stack 
    ld B1,X+
    ld B2,X+
#endif /* defined (__AVR_TINY__) */

    ;; C[] = 0
    clr     __tmp_reg__
    clr     C2

0:  ;; Shift N-th Bit of B[] into Carry.  N = 24 - Loop
    LSR  B2     $  ror  B1     $  ror  B0

    ;; If the N-th Bit of B[] was set...
    brcc    1f

    ;; ...then add A[] * 2^N to the Result C[]
    ADD  C0,A0  $  adc  C1,A1  $  adc  C2,A2

1:  ;; Multiply A[] by 2
    LSL  A0     $  rol  A1     $  rol  A2

    ;; Loop until B[] is 0
    subi B0,0   $  sbci B1,0   $  sbci B2,0
    brne    0b

    ;; Copy C[] to the return Register A[]
    wmov    A0, C0
    mov     A2, C2

    clr     __zero_reg__
#if defined (__AVR_TINY__)
    pop B1
    pop B0
#endif /* (__AVR_TINY__) */
    ret
ENDF __mulpsi3

#undef C2
#undef C1
#undef C0

#endif /* HAVE_MUL */

#undef B2
#undef B1
#undef B0

#undef A2
#undef A1
#undef A0

#endif /* L_mulpsi3 */

#if defined (L_mulsqipsi3) && defined (__AVR_HAVE_MUL__)

;; A[0..2]: In: Multiplicand
#define A0  22
#define A1  A0+1
#define A2  A0+2

;; BB: In: Multiplier
#define BB  25

;; C[0..2]: Result
#define C0  18
#define C1  C0+1
#define C2  C0+2

;; C[] = A[] * sign_extend (BB)
DEFUN __mulsqipsi3
    mul     A0, BB
    movw    C0, r0
    mul     A2, BB
    mov     C2, r0
    mul     A1, BB
    add     C1, r0
    adc     C2, r1
    clr     __zero_reg__
    sbrs    BB, 7
    ret
    ;; One-extend BB
    sub     C1, A0
    sbc     C2, A1
    ret
ENDF __mulsqipsi3

#undef C2
#undef C1
#undef C0

#undef BB

#undef A2
#undef A1
#undef A0

#endif /* L_mulsqipsi3  &&  HAVE_MUL */

/*******************************************************
       Multiplication 64 x 64
*******************************************************/

;; A[] = A[] * B[]

;; A[0..7]: In: Multiplicand
;; Out: Product
#define A0  18
#define A1  A0+1
#define A2  A0+2
#define A3  A0+3
#define A4  A0+4
#define A5  A0+5
#define A6  A0+6
#define A7  A0+7

;; B[0..7]: In: Multiplier
#define B0  10
#define B1  B0+1
#define B2  B0+2
#define B3  B0+3
#define B4  B0+4
#define B5  B0+5
#define B6  B0+6
#define B7  B0+7

#ifndef __AVR_TINY__
#if defined (__AVR_HAVE_MUL__)
;; Define C[] for convenience
;; Notice that parts of C[] overlap A[] respective B[]
#define C0  16
#define C1  C0+1
#define C2  20
#define C3  C2+1
#define C4  28
#define C5  C4+1
#define C6  C4+2
#define C7  C4+3

#if defined (L_muldi3)

;; A[]     *= B[]
;; R25:R18 *= R17:R10
;; Ordinary ABI-Function

DEFUN __muldi3
    push    r29
    push    r28
    push    r17
    push    r16

    ;; Counting in Words, we have to perform a 4 * 4 Multiplication

    ;; 3 * 0  +  0 * 3
    mul  A7,B0  $             $  mov C7,r0
    mul  A0,B7  $             $  add C7,r0
    mul  A6,B1  $             $  add C7,r0
    mul  A6,B0  $  mov C6,r0  $  add C7,r1
    mul  B6,A1  $             $  add C7,r0
    mul  B6,A0  $  add C6,r0  $  adc C7,r1

    ;; 1 * 2
    mul  A2,B4  $  add C6,r0  $  adc C7,r1
    mul  A3,B4  $             $  add C7,r0
    mul  A2,B5  $             $  add C7,r0

    push    A5
    push    A4
    push    B1
    push    B0
    push    A3
    push    A2

    ;; 0 * 0
    wmov    26, B0
    XCALL   __umulhisi3
    wmov    C0, 22
    wmov    C2, 24

    ;; 0 * 2
    wmov    26, B4
    XCALL   __umulhisi3  $  wmov C4,22            $ add C6,24 $ adc C7,25

    wmov    26, B2
    ;; 0 * 1
    XCALL   __muldi3_6

    pop     A0
    pop     A1
    ;; 1 * 1
    wmov    26, B2
    XCALL   __umulhisi3  $  add C4,22 $ adc C5,23 $ adc C6,24 $ adc C7,25

    pop     r26
    pop     r27
    ;; 1 * 0
    XCALL   __muldi3_6

    pop     A0
    pop     A1
    ;; 2 * 0
    XCALL   __umulhisi3  $  add C4,22 $ adc C5,23 $ adc C6,24 $ adc C7,25

    ;; 2 * 1
    wmov    26, B2
    XCALL   __umulhisi3  $            $           $ add C6,22 $ adc C7,23

    ;; A[] = C[]
    wmov    A0, C0
    ;; A2 = C2 already
    wmov    A4, C4
    wmov    A6, C6

    pop     r16
    pop     r17
    pop     r28
    pop     r29
    ret
ENDF __muldi3
#endif /* L_muldi3 */

#if defined (L_muldi3_6)
;; A helper for some 64-bit multiplications with MUL available
DEFUN __muldi3_6
__muldi3_6:
    XCALL   __umulhisi3
    add     C2, 22
    adc     C3, 23
    adc     C4, 24
    adc     C5, 25
    brcc    0f
    adiw    C6, 1
0:  ret
ENDF __muldi3_6
#endif /* L_muldi3_6 */

#undef C7
#undef C6
#undef C5
#undef C4
#undef C3
#undef C2
#undef C1
#undef C0

#else /* !HAVE_MUL */

#if defined (L_muldi3)

#define C0  26
#define C1  C0+1
#define C2  C0+2
#define C3  C0+3
#define C4  C0+4
#define C5  C0+5
#define C6  0
#define C7  C6+1

#define Loop 9

;; A[]     *= B[]
;; R25:R18 *= R17:R10
;; Ordinary ABI-Function

DEFUN __muldi3
    push    r29
    push    r28
    push    Loop

    ldi     C0, 64
    mov     Loop, C0

    ;; C[] = 0
    clr     __tmp_reg__
    wmov    C0, 0
    wmov    C2, 0
    wmov    C4, 0

0:  ;; Rotate B[] right by 1 and set Carry to the N-th Bit of B[]
    ;; where N = 64 - Loop.
    ;; Notice that B[] = B[] >>> 64 so after this Routine has finished,
    ;; B[] will have its initial Value again.
    LSR  B7     $  ror  B6     $  ror  B5     $  ror  B4
    ror  B3     $  ror  B2     $  ror  B1     $  ror  B0

    ;; If the N-th Bit of B[] was set then...
    brcc    1f
    ;; ...finish Rotation...
    ori     B7, 1 << 7

    ;; ...and add A[] * 2^N to the Result C[]
    ADD  C0,A0  $  adc  C1,A1  $  adc  C2,A2  $  adc  C3,A3
    adc  C4,A4  $  adc  C5,A5  $  adc  C6,A6  $  adc  C7,A7

1:  ;; Multiply A[] by 2
    LSL  A0     $  rol  A1     $  rol  A2     $  rol  A3
    rol  A4     $  rol  A5     $  rol  A6     $  rol  A7

    dec     Loop
    brne    0b

    ;; We expanded the Result in C[]
    ;; Copy Result to the Return Register A[]
    wmov    A0, C0
    wmov    A2, C2
    wmov    A4, C4
    wmov    A6, C6

    clr     __zero_reg__
    pop     Loop
    pop     r28
    pop     r29
    ret
ENDF __muldi3

#undef Loop

#undef C7
#undef C6
#undef C5
#undef C4
#undef C3
#undef C2
#undef C1
#undef C0

#endif /* L_muldi3 */
#endif /* HAVE_MUL */
#endif /* if not __AVR_TINY__ */

#undef B7
#undef B6
#undef B5
#undef B4
#undef B3
#undef B2
#undef B1
#undef B0

#undef A7
#undef A6
#undef A5
#undef A4
#undef A3
#undef A2
#undef A1
#undef A0

/*******************************************************
   Widening Multiplication 64 = 32 x 32  with  MUL
*******************************************************/

#if defined (__AVR_HAVE_MUL__)
#define A0 r22
#define A1 r23 
#define A2 r24
#define A3 r25
 
#define B0 r18
#define B1 r19
#define B2 r20
#define B3 r21
 
#define C0  18
#define C1  C0+1
#define C2  20
#define C3  C2+1
#define C4  28
#define C5  C4+1
#define C6  C4+2
#define C7  C4+3

#if defined (L_umulsidi3)

;; Unsigned widening 64 = 32 * 32 Multiplication with MUL

;; R18[8] = R22[4] * R18[4]
;;
;; Ordinary ABI Function, but additionally sets
;; X = R20[2] = B2[2]
;; Z = R22[2] = A0[2]
DEFUN __umulsidi3
    clt
    ;; FALLTHRU
ENDF  __umulsidi3
    ;; T = sign (A)
DEFUN __umulsidi3_helper
    push    29  $  push    28 ; Y
    wmov    30, A2
    ;; Counting in Words, we have to perform 4 Multiplications
    ;; 0 * 0
    wmov    26, A0
    XCALL __umulhisi3
    push    23  $  push    22 ; C0
    wmov    28, B0
    wmov    18, B2
    wmov    C2, 24
    push    27  $  push    26 ; A0
    push    19  $  push    18 ; B2
    ;;
    ;;  18  20  22  24  26  28  30  |  B2, B3, A0, A1, C0, C1, Y
    ;;  B2  C2  --  --  --  B0  A2
    ;; 1 * 1
    wmov    26, 30      ; A2
    XCALL __umulhisi3
    ;; Sign-extend A.  T holds the sign of A
    brtc    0f
    ;; Subtract B from the high part of the result
    sub     22, 28
    sbc     23, 29
    sbc     24, 18
    sbc     25, 19
0:  wmov    18, 28      ;; B0
    wmov    C4, 22
    wmov    C6, 24
    ;;
    ;;  18  20  22  24  26  28  30  |  B2, B3, A0, A1, C0, C1, Y
    ;;  B0  C2  --  --  A2  C4  C6
    ;;
    ;; 1 * 0
    XCALL __muldi3_6
    ;; 0 * 1
    pop     26  $   pop 27  ;; B2
    pop     18  $   pop 19  ;; A0
    XCALL __muldi3_6

    ;; Move result C into place and save A0 in Z
    wmov    22, C4
    wmov    24, C6
    wmov    30, 18 ; A0
    pop     C0  $   pop C1

    ;; Epilogue
    pop     28  $   pop 29  ;; Y
    ret
ENDF __umulsidi3_helper
#endif /* L_umulsidi3 */


#if defined (L_mulsidi3)

;; Signed widening 64 = 32 * 32 Multiplication
;;
;; R18[8] = R22[4] * R18[4]
;; Ordinary ABI Function
DEFUN __mulsidi3
    bst     A3, 7
    sbrs    B3, 7           ; Enhanced core has no skip bug
    XJMP __umulsidi3_helper

    ;; B needs sign-extension
    push    A3
    push    A2
    XCALL __umulsidi3_helper
    ;; A0 survived in Z
    sub     r22, r30
    sbc     r23, r31
    pop     r26
    pop     r27
    sbc     r24, r26
    sbc     r25, r27
    ret
ENDF __mulsidi3
#endif /* L_mulsidi3 */

#undef A0
#undef A1
#undef A2
#undef A3
#undef B0
#undef B1
#undef B2
#undef B3
#undef C0
#undef C1
#undef C2
#undef C3
#undef C4
#undef C5
#undef C6
#undef C7
#endif /* HAVE_MUL */

/**********************************************************
    Widening Multiplication 64 = 32 x 32  without  MUL
**********************************************************/
#ifndef __AVR_TINY__ /* if not __AVR_TINY__ */
#if defined (L_mulsidi3) && !defined (__AVR_HAVE_MUL__)
#define A0 18
#define A1 A0+1
#define A2 A0+2
#define A3 A0+3
#define A4 A0+4
#define A5 A0+5
#define A6 A0+6
#define A7 A0+7

#define B0 10
#define B1 B0+1
#define B2 B0+2
#define B3 B0+3
#define B4 B0+4
#define B5 B0+5
#define B6 B0+6
#define B7 B0+7

#define AA0 22
#define AA1 AA0+1
#define AA2 AA0+2
#define AA3 AA0+3

#define BB0 18
#define BB1 BB0+1
#define BB2 BB0+2
#define BB3 BB0+3

#define Mask r30

;; Signed / Unsigned widening 64 = 32 * 32 Multiplication without MUL
;;
;; R18[8] = R22[4] * R18[4]
;; Ordinary ABI Function
DEFUN __mulsidi3
    set
    skip
    ;; FALLTHRU
ENDF  __mulsidi3

DEFUN __umulsidi3
    clt     ; skipped
    ;; Save 10 Registers: R10..R17, R28, R29
    do_prologue_saves 10
    ldi     Mask, 0xff
    bld     Mask, 7
    ;; Move B into place...
    wmov    B0, BB0
    wmov    B2, BB2
    ;; ...and extend it
    and     BB3, Mask
    lsl     BB3
    sbc     B4, B4
    mov     B5, B4
    wmov    B6, B4
    ;; Move A into place...
    wmov    A0, AA0
    wmov    A2, AA2
    ;; ...and extend it
    and     AA3, Mask
    lsl     AA3
    sbc     A4, A4
    mov     A5, A4
    wmov    A6, A4
    XCALL   __muldi3
    do_epilogue_restores 10
ENDF __umulsidi3

#undef A0
#undef A1
#undef A2
#undef A3
#undef A4
#undef A5
#undef A6
#undef A7
#undef B0
#undef B1
#undef B2
#undef B3
#undef B4
#undef B5
#undef B6
#undef B7
#undef AA0
#undef AA1
#undef AA2
#undef AA3
#undef BB0
#undef BB1
#undef BB2
#undef BB3
#undef Mask
#endif /* L_mulsidi3 && !HAVE_MUL */
#endif /* if not __AVR_TINY__ */
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

.section .text.libgcc.div, "ax", @progbits

/*******************************************************
       Division 8 / 8 => (result + remainder)
*******************************************************/
#define	r_rem	r25	/* remainder */
#define	r_arg1	r24	/* dividend, quotient */
#define	r_arg2	r22	/* divisor */
#define	r_cnt	r23	/* loop count */

#if defined (L_udivmodqi4)
DEFUN __udivmodqi4
	clr	r_rem		; clear remainder
	ldi	r_cnt,8		; init loop counter
	lsl	r_arg1		; shift dividend
__udivmodqi4_loop:
	rol	r_rem		; shift dividend into remainder
	cp	r_rem,r_arg2	; compare remainder & divisor
	brcs	__udivmodqi4_ep	; remainder <= divisor
	sub	r_rem,r_arg2	; restore remainder
__udivmodqi4_ep:
	rol	r_arg1		; shift dividend (with CARRY)
	dec	r_cnt		; decrement loop counter
	brne	__udivmodqi4_loop
	com	r_arg1		; complement result
				; because C flag was complemented in loop
	ret
ENDF __udivmodqi4
#endif /* defined (L_udivmodqi4) */

#if defined (L_divmodqi4)
DEFUN __divmodqi4
        bst     r_arg1,7	; store sign of dividend
        mov     __tmp_reg__,r_arg1
        eor     __tmp_reg__,r_arg2; r0.7 is sign of result
        sbrc	r_arg1,7
	neg     r_arg1		; dividend negative : negate
        sbrc	r_arg2,7
	neg     r_arg2		; divisor negative : negate
	XCALL	__udivmodqi4	; do the unsigned div/mod
	brtc	__divmodqi4_1
	neg	r_rem		; correct remainder sign
__divmodqi4_1:
	sbrc	__tmp_reg__,7
	neg	r_arg1		; correct result sign
__divmodqi4_exit:
	ret
ENDF __divmodqi4
#endif /* defined (L_divmodqi4) */

#undef r_rem
#undef r_arg1
#undef r_arg2
#undef r_cnt
	
		
/*******************************************************
       Division 16 / 16 => (result + remainder)
*******************************************************/
#define	r_remL	r26	/* remainder Low */
#define	r_remH	r27	/* remainder High */

/* return: remainder */
#define	r_arg1L	r24	/* dividend Low */
#define	r_arg1H	r25	/* dividend High */

/* return: quotient */
#define	r_arg2L	r22	/* divisor Low */
#define	r_arg2H	r23	/* divisor High */
	
#define	r_cnt	r21	/* loop count */

#if defined (L_udivmodhi4)
DEFUN __udivmodhi4
	sub	r_remL,r_remL
	sub	r_remH,r_remH	; clear remainder and carry
	ldi	r_cnt,17	; init loop counter
	rjmp	__udivmodhi4_ep	; jump to entry point
__udivmodhi4_loop:
        rol	r_remL		; shift dividend into remainder
	rol	r_remH
        cp	r_remL,r_arg2L	; compare remainder & divisor
	cpc	r_remH,r_arg2H
        brcs	__udivmodhi4_ep	; remainder < divisor
        sub	r_remL,r_arg2L	; restore remainder
        sbc	r_remH,r_arg2H
__udivmodhi4_ep:
        rol	r_arg1L		; shift dividend (with CARRY)
        rol	r_arg1H
        dec	r_cnt		; decrement loop counter
        brne	__udivmodhi4_loop
	com	r_arg1L
	com	r_arg1H
; div/mod results to return registers, as for the div() function
	mov_l	r_arg2L, r_arg1L	; quotient
	mov_h	r_arg2H, r_arg1H
	mov_l	r_arg1L, r_remL		; remainder
	mov_h	r_arg1H, r_remH
	ret
ENDF __udivmodhi4
#endif /* defined (L_udivmodhi4) */

#if defined (L_divmodhi4)
DEFUN __divmodhi4
    .global _div
_div:
    bst     r_arg1H,7           ; store sign of dividend
    mov     __tmp_reg__,r_arg2H
    brtc    0f
    com     __tmp_reg__         ; r0.7 is sign of result
    rcall   __divmodhi4_neg1    ; dividend negative: negate
0:
    sbrc    r_arg2H,7
    rcall   __divmodhi4_neg2    ; divisor negative: negate
    XCALL   __udivmodhi4        ; do the unsigned div/mod
    sbrc    __tmp_reg__,7
    rcall   __divmodhi4_neg2    ; correct remainder sign
    brtc    __divmodhi4_exit
__divmodhi4_neg1:
    ;; correct dividend/remainder sign
    com     r_arg1H
    neg     r_arg1L
    sbci    r_arg1H,0xff
    ret
__divmodhi4_neg2:
    ;; correct divisor/result sign
    com     r_arg2H
    neg     r_arg2L
    sbci    r_arg2H,0xff
__divmodhi4_exit:
    ret
ENDF __divmodhi4
#endif /* defined (L_divmodhi4) */

#undef r_remH
#undef r_remL

#undef r_arg1H
#undef r_arg1L

#undef r_arg2H
#undef r_arg2L
             	
#undef r_cnt   	

/*******************************************************
       Division 24 / 24 => (result + remainder)
*******************************************************/

;; A[0..2]: In: Dividend; Out: Quotient
#define A0  22
#define A1  A0+1
#define A2  A0+2

;; B[0..2]: In: Divisor;   Out: Remainder
#define B0  18
#define B1  B0+1
#define B2  B0+2

;; C[0..2]: Expand remainder
#define C0  __zero_reg__
#define C1  26
#define C2  25

;; Loop counter
#define r_cnt   21

#if defined (L_udivmodpsi4)
;; R24:R22 = R24:R24  udiv  R20:R18
;; R20:R18 = R24:R22  umod  R20:R18
;; Clobbers: R21, R25, R26

DEFUN __udivmodpsi4
    ; init loop counter
    ldi     r_cnt, 24+1
    ; Clear remainder and carry.  C0 is already 0
    clr     C1
    sub     C2, C2
    ; jump to entry point
    rjmp    __udivmodpsi4_start
__udivmodpsi4_loop:
    ; shift dividend into remainder
    rol     C0
    rol     C1
    rol     C2
    ; compare remainder & divisor
    cp      C0, B0
    cpc     C1, B1
    cpc     C2, B2
    brcs    __udivmodpsi4_start ; remainder <= divisor
    sub     C0, B0              ; restore remainder
    sbc     C1, B1
    sbc     C2, B2
__udivmodpsi4_start:
    ; shift dividend (with CARRY)
    rol     A0
    rol     A1
    rol     A2
    ; decrement loop counter
    dec     r_cnt
    brne    __udivmodpsi4_loop
    com     A0
    com     A1
    com     A2
    ; div/mod results to return registers
    ; remainder
    mov     B0, C0
    mov     B1, C1
    mov     B2, C2
    clr     __zero_reg__ ; C0
    ret
ENDF __udivmodpsi4
#endif /* defined (L_udivmodpsi4) */

#if defined (L_divmodpsi4)
;; R24:R22 = R24:R22  div  R20:R18
;; R20:R18 = R24:R22  mod  R20:R18
;; Clobbers: T, __tmp_reg__, R21, R25, R26

DEFUN __divmodpsi4
    ; R0.7 will contain the sign of the result:
    ; R0.7 = A.sign ^ B.sign
    mov __tmp_reg__, B2
    ; T-flag = sign of dividend
    bst     A2, 7
    brtc    0f
    com     __tmp_reg__
    ; Adjust dividend's sign
    rcall   __divmodpsi4_negA
0:
    ; Adjust divisor's sign
    sbrc    B2, 7
    rcall   __divmodpsi4_negB

    ; Do the unsigned div/mod
    XCALL   __udivmodpsi4

    ; Adjust quotient's sign
    sbrc    __tmp_reg__, 7
    rcall   __divmodpsi4_negA

    ; Adjust remainder's sign
    brtc    __divmodpsi4_end

__divmodpsi4_negB:
    ; Correct divisor/remainder sign
    com     B2
    com     B1
    neg     B0
    sbci    B1, -1
    sbci    B2, -1
    ret

    ; Correct dividend/quotient sign
__divmodpsi4_negA:
    com     A2
    com     A1
    neg     A0
    sbci    A1, -1
    sbci    A2, -1
__divmodpsi4_end:
    ret

ENDF __divmodpsi4
#endif /* defined (L_divmodpsi4) */

#undef A0
#undef A1
#undef A2

#undef B0
#undef B1
#undef B2

#undef C0
#undef C1
#undef C2

#undef r_cnt

/*******************************************************
       Division 32 / 32 => (result + remainder)
*******************************************************/
#define	r_remHH	r31	/* remainder High */
#define	r_remHL	r30
#define	r_remH	r27
#define	r_remL	r26	/* remainder Low */

/* return: remainder */
#define	r_arg1HH r25	/* dividend High */
#define	r_arg1HL r24
#define	r_arg1H  r23
#define	r_arg1L  r22	/* dividend Low */

/* return: quotient */
#define	r_arg2HH r21	/* divisor High */
#define	r_arg2HL r20
#define	r_arg2H  r19
#define	r_arg2L  r18	/* divisor Low */
	
#define	r_cnt __zero_reg__  /* loop count (0 after the loop!) */

#if defined (L_udivmodsi4)
DEFUN __udivmodsi4
	ldi	r_remL, 33	; init loop counter
	mov	r_cnt, r_remL
	sub	r_remL,r_remL
	sub	r_remH,r_remH	; clear remainder and carry
	mov_l	r_remHL, r_remL
	mov_h	r_remHH, r_remH
	rjmp	__udivmodsi4_ep	; jump to entry point
__udivmodsi4_loop:
        rol	r_remL		; shift dividend into remainder
	rol	r_remH
	rol	r_remHL
	rol	r_remHH
        cp	r_remL,r_arg2L	; compare remainder & divisor
	cpc	r_remH,r_arg2H
	cpc	r_remHL,r_arg2HL
	cpc	r_remHH,r_arg2HH
	brcs	__udivmodsi4_ep	; remainder <= divisor
        sub	r_remL,r_arg2L	; restore remainder
        sbc	r_remH,r_arg2H
        sbc	r_remHL,r_arg2HL
        sbc	r_remHH,r_arg2HH
__udivmodsi4_ep:
        rol	r_arg1L		; shift dividend (with CARRY)
        rol	r_arg1H
        rol	r_arg1HL
        rol	r_arg1HH
        dec	r_cnt		; decrement loop counter
        brne	__udivmodsi4_loop
				; __zero_reg__ now restored (r_cnt == 0)
	com	r_arg1L
	com	r_arg1H
	com	r_arg1HL
	com	r_arg1HH
; div/mod results to return registers, as for the ldiv() function
	mov_l	r_arg2L,  r_arg1L	; quotient
	mov_h	r_arg2H,  r_arg1H
	mov_l	r_arg2HL, r_arg1HL
	mov_h	r_arg2HH, r_arg1HH
	mov_l	r_arg1L,  r_remL	; remainder
	mov_h	r_arg1H,  r_remH
	mov_l	r_arg1HL, r_remHL
	mov_h	r_arg1HH, r_remHH
	ret
ENDF __udivmodsi4
#endif /* defined (L_udivmodsi4) */

#if defined (L_divmodsi4)
DEFUN __divmodsi4
    mov     __tmp_reg__,r_arg2HH
    bst     r_arg1HH,7          ; store sign of dividend
    brtc    0f
    com     __tmp_reg__         ; r0.7 is sign of result
    XCALL   __negsi2            ; dividend negative: negate
0:
    sbrc    r_arg2HH,7
    rcall   __divmodsi4_neg2    ; divisor negative: negate
    XCALL   __udivmodsi4        ; do the unsigned div/mod
    sbrc    __tmp_reg__, 7      ; correct quotient sign
    rcall   __divmodsi4_neg2
    brtc    __divmodsi4_exit    ; correct remainder sign
    XJMP    __negsi2
__divmodsi4_neg2:
    ;; correct divisor/quotient sign
    com     r_arg2HH
    com     r_arg2HL
    com     r_arg2H
    neg     r_arg2L
    sbci    r_arg2H,0xff
    sbci    r_arg2HL,0xff
    sbci    r_arg2HH,0xff
__divmodsi4_exit:
    ret
ENDF __divmodsi4
#endif /* defined (L_divmodsi4) */

#if defined (L_negsi2)
;; (set (reg:SI 22)
;;      (neg:SI (reg:SI 22)))
;; Sets the V flag for signed overflow tests
DEFUN __negsi2
    NEG4    22
    ret
ENDF __negsi2
#endif /* L_negsi2 */

#undef r_remHH
#undef r_remHL
#undef r_remH
#undef r_remL
#undef r_arg1HH
#undef r_arg1HL
#undef r_arg1H
#undef r_arg1L
#undef r_arg2HH
#undef r_arg2HL
#undef r_arg2H
#undef r_arg2L
#undef r_cnt

/* *di routines use registers below R19 and won't work with tiny arch
   right now. */

#if !defined (__AVR_TINY__)
/*******************************************************
       Division 64 / 64
       Modulo   64 % 64
*******************************************************/

;; Use Speed-optimized Version on "big" Devices, i.e. Devices with
;; at least 16k of Program Memory.  For smaller Devices, depend
;; on MOVW and SP Size.  There is a Connexion between SP Size and
;; Flash Size so that SP Size can be used to test for Flash Size.

#if defined (__AVR_HAVE_JMP_CALL__)
#   define SPEED_DIV 8
#elif defined (__AVR_HAVE_MOVW__) && defined (__AVR_HAVE_SPH__)
#   define SPEED_DIV 16
#else
#   define SPEED_DIV 0
#endif

;; A[0..7]: In: Dividend;
;; Out: Quotient  (T = 0)
;; Out: Remainder (T = 1)
#define A0  18
#define A1  A0+1
#define A2  A0+2
#define A3  A0+3
#define A4  A0+4
#define A5  A0+5
#define A6  A0+6
#define A7  A0+7

;; B[0..7]: In: Divisor;   Out: Clobber
#define B0  10
#define B1  B0+1
#define B2  B0+2
#define B3  B0+3
#define B4  B0+4
#define B5  B0+5
#define B6  B0+6
#define B7  B0+7

;; C[0..7]: Expand remainder;  Out: Remainder (unused)
#define C0  8
#define C1  C0+1
#define C2  30
#define C3  C2+1
#define C4  28
#define C5  C4+1
#define C6  26
#define C7  C6+1

;; Holds Signs during Division Routine
#define SS      __tmp_reg__

;; Bit-Counter in Division Routine
#define R_cnt   __zero_reg__

;; Scratch Register for Negation
#define NN      r31

#if defined (L_udivdi3)

;; R25:R18 = R24:R18  umod  R17:R10
;; Ordinary ABI-Function

DEFUN __umoddi3
    set
    rjmp __udivdi3_umoddi3
ENDF __umoddi3

;; R25:R18 = R24:R18  udiv  R17:R10
;; Ordinary ABI-Function

DEFUN __udivdi3
    clt
ENDF __udivdi3

DEFUN __udivdi3_umoddi3
    push    C0
    push    C1
    push    C4
    push    C5
    XCALL   __udivmod64
    pop     C5
    pop     C4
    pop     C1
    pop     C0
    ret
ENDF __udivdi3_umoddi3
#endif /* L_udivdi3 */

#if defined (L_udivmod64)

;; Worker Routine for 64-Bit unsigned Quotient and Remainder Computation
;; No Registers saved/restored; the Callers will take Care.
;; Preserves B[] and T-flag
;; T = 0: Compute Quotient  in A[]
;; T = 1: Compute Remainder in A[] and shift SS one Bit left

DEFUN __udivmod64

    ;; Clear Remainder (C6, C7 will follow)
    clr     C0
    clr     C1
    wmov    C2, C0
    wmov    C4, C0
    ldi     C7, 64

#if SPEED_DIV == 0 || SPEED_DIV == 16
    ;; Initialize Loop-Counter
    mov     R_cnt, C7
    wmov    C6, C0
#endif /* SPEED_DIV */

#if SPEED_DIV == 8

    push    A7
    clr     C6

1:  ;; Compare shifted Devidend against Divisor
    ;; If -- even after Shifting -- it is smaller...
    CP  A7,B0  $  cpc C0,B1  $  cpc C1,B2  $  cpc C2,B3
    cpc C3,B4  $  cpc C4,B5  $  cpc C5,B6  $  cpc C6,B7
    brcc    2f

    ;; ...then we can subtract it.  Thus, it is legal to shift left
               $  mov C6,C5  $  mov C5,C4  $  mov C4,C3
    mov C3,C2  $  mov C2,C1  $  mov C1,C0  $  mov C0,A7
    mov A7,A6  $  mov A6,A5  $  mov A5,A4  $  mov A4,A3
    mov A3,A2  $  mov A2,A1  $  mov A1,A0  $  clr A0

    ;; 8 Bits are done
    subi    C7, 8
    brne    1b

    ;; Shifted 64 Bits:  A7 has traveled to C7
    pop     C7
    ;; Divisor is greater than Dividend. We have:
    ;; A[] % B[] = A[]
    ;; A[] / B[] = 0
    ;; Thus, we can return immediately
    rjmp    5f

2:  ;; Initialze Bit-Counter with Number of Bits still to be performed
    mov     R_cnt, C7

    ;; Push of A7 is not needed because C7 is still 0
    pop     C7
    clr     C7

#elif  SPEED_DIV == 16

    ;; Compare shifted Dividend against Divisor
    cp      A7, B3
    cpc     C0, B4
    cpc     C1, B5
    cpc     C2, B6
    cpc     C3, B7
    brcc    2f

    ;; Divisor is greater than shifted Dividen: We can shift the Dividend
    ;; and it is still smaller than the Divisor --> Shift one 32-Bit Chunk
    wmov  C2,A6  $  wmov C0,A4
    wmov  A6,A2  $  wmov A4,A0
    wmov  A2,C6  $  wmov A0,C4

    ;; Set Bit Counter to 32
    lsr     R_cnt
2:
#elif SPEED_DIV
#error SPEED_DIV = ?
#endif /* SPEED_DIV */

;; The very Division + Remainder Routine

3:  ;; Left-shift Dividend...
    lsl A0     $  rol A1     $  rol A2     $  rol A3
    rol A4     $  rol A5     $  rol A6     $  rol A7

    ;; ...into Remainder
    rol C0     $  rol C1     $  rol C2     $  rol C3
    rol C4     $  rol C5     $  rol C6     $  rol C7

    ;; Compare Remainder and Divisor
    CP  C0,B0  $  cpc C1,B1  $  cpc C2,B2  $  cpc C3,B3
    cpc C4,B4  $  cpc C5,B5  $  cpc C6,B6  $  cpc C7,B7

    brcs 4f

    ;; Divisor fits into Remainder:  Subtract it from Remainder...
    SUB C0,B0  $  sbc C1,B1  $  sbc C2,B2  $  sbc C3,B3
    sbc C4,B4  $  sbc C5,B5  $  sbc C6,B6  $  sbc C7,B7

    ;; ...and set according Bit in the upcoming Quotient
    ;; The Bit will travel to its final Position
    ori A0, 1

4:  ;; This Bit is done
    dec     R_cnt
    brne    3b
    ;; __zero_reg__ is 0 again

    ;; T = 0: We are fine with the Quotient in A[]
    ;; T = 1: Copy Remainder to A[]
5:  brtc    6f
    wmov    A0, C0
    wmov    A2, C2
    wmov    A4, C4
    wmov    A6, C6
    ;; Move the Sign of the Result to SS.7
    lsl     SS

6:  ret

ENDF __udivmod64
#endif /* L_udivmod64 */


#if defined (L_divdi3)

;; R25:R18 = R24:R18  mod  R17:R10
;; Ordinary ABI-Function

DEFUN __moddi3
    set
    rjmp    __divdi3_moddi3
ENDF __moddi3

;; R25:R18 = R24:R18  div  R17:R10
;; Ordinary ABI-Function

DEFUN __divdi3
    clt
ENDF __divdi3

DEFUN  __divdi3_moddi3
#if SPEED_DIV
    mov     r31, A7
    or      r31, B7
    brmi    0f
    ;; Both Signs are 0:  the following Complexitiy is not needed
    XJMP    __udivdi3_umoddi3
#endif /* SPEED_DIV */

0:  ;; The Prologue
    ;; Save 12 Registers:  Y, 17...8
    ;; No Frame needed
    do_prologue_saves 12

    ;; SS.7 will contain the Sign of the Quotient  (A.sign * B.sign)
    ;; SS.6 will contain the Sign of the Remainder (A.sign)
    mov     SS, A7
    asr     SS
    ;; Adjust Dividend's Sign as needed
#if SPEED_DIV
    ;; Compiling for Speed we know that at least one Sign must be < 0
    ;; Thus, if A[] >= 0 then we know B[] < 0
    brpl    22f
#else
    brpl    21f
#endif /* SPEED_DIV */

    XCALL   __negdi2

    ;; Adjust Divisor's Sign and SS.7 as needed
21: tst     B7
    brpl    3f
22: ldi     NN, 1 << 7
    eor     SS, NN

    ldi NN, -1
    com B4     $  com B5     $  com B6     $  com B7
               $  com B1     $  com B2     $  com B3
    NEG B0
               $  sbc B1,NN  $  sbc B2,NN  $  sbc B3,NN
    sbc B4,NN  $  sbc B5,NN  $  sbc B6,NN  $  sbc B7,NN

3:  ;; Do the unsigned 64-Bit Division/Modulo (depending on T-flag)
    XCALL   __udivmod64

    ;; Adjust Result's Sign
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    tst     SS
    brpl    4f
#else
    sbrc    SS, 7
#endif /* __AVR_HAVE_JMP_CALL__ */
    XCALL   __negdi2

4:  ;; Epilogue: Restore 12 Registers and return
    do_epilogue_restores 12

ENDF __divdi3_moddi3

#endif /* L_divdi3 */

#undef R_cnt
#undef SS
#undef NN

.section .text.libgcc, "ax", @progbits

#define TT __tmp_reg__

#if defined (L_adddi3)
;; (set (reg:DI 18)
;;      (plus:DI (reg:DI 18)
;;               (reg:DI 10)))
;; Sets the V flag for signed overflow tests
;; Sets the C flag for unsigned overflow tests
DEFUN __adddi3
    ADD A0,B0  $  adc A1,B1  $  adc A2,B2  $  adc A3,B3
    adc A4,B4  $  adc A5,B5  $  adc A6,B6  $  adc A7,B7
    ret
ENDF __adddi3
#endif /* L_adddi3 */

#if defined (L_adddi3_s8)
;; (set (reg:DI 18)
;;      (plus:DI (reg:DI 18)
;;               (sign_extend:SI (reg:QI 26))))
;; Sets the V flag for signed overflow tests
;; Sets the C flag for unsigned overflow tests provided 0 <= R26 < 128
DEFUN __adddi3_s8
    clr     TT
    sbrc    r26, 7
    com     TT
    ADD A0,r26 $  adc A1,TT  $  adc A2,TT  $  adc A3,TT
    adc A4,TT  $  adc A5,TT  $  adc A6,TT  $  adc A7,TT
    ret
ENDF __adddi3_s8
#endif /* L_adddi3_s8 */

#if defined (L_subdi3)
;; (set (reg:DI 18)
;;      (minus:DI (reg:DI 18)
;;                (reg:DI 10)))
;; Sets the V flag for signed overflow tests
;; Sets the C flag for unsigned overflow tests
DEFUN __subdi3
    SUB A0,B0  $  sbc A1,B1  $  sbc A2,B2  $  sbc A3,B3
    sbc A4,B4  $  sbc A5,B5  $  sbc A6,B6  $  sbc A7,B7
    ret
ENDF __subdi3
#endif /* L_subdi3 */

#if defined (L_cmpdi2)
;; (set (cc0)
;;      (compare (reg:DI 18)
;;               (reg:DI 10)))
DEFUN __cmpdi2
    CP  A0,B0  $  cpc A1,B1  $  cpc A2,B2  $  cpc A3,B3
    cpc A4,B4  $  cpc A5,B5  $  cpc A6,B6  $  cpc A7,B7
    ret
ENDF __cmpdi2
#endif /* L_cmpdi2 */

#if defined (L_cmpdi2_s8)
;; (set (cc0)
;;      (compare (reg:DI 18)
;;               (sign_extend:SI (reg:QI 26))))
DEFUN __cmpdi2_s8
    clr     TT
    sbrc    r26, 7
    com     TT
    CP  A0,r26 $  cpc A1,TT  $  cpc A2,TT  $  cpc A3,TT
    cpc A4,TT  $  cpc A5,TT  $  cpc A6,TT  $  cpc A7,TT
    ret
ENDF __cmpdi2_s8
#endif /* L_cmpdi2_s8 */

#if defined (L_negdi2)
;; (set (reg:DI 18)
;;      (neg:DI (reg:DI 18)))
;; Sets the V flag for signed overflow tests
DEFUN __negdi2

    com  A4    $  com  A5    $  com  A6    $  com  A7
               $  com  A1    $  com  A2    $  com  A3
    NEG  A0
               $  sbci A1,-1 $  sbci A2,-1 $  sbci A3,-1
    sbci A4,-1 $  sbci A5,-1 $  sbci A6,-1 $  sbci A7,-1
    ret

ENDF __negdi2
#endif /* L_negdi2 */

#undef TT

#undef C7
#undef C6
#undef C5
#undef C4
#undef C3
#undef C2
#undef C1
#undef C0

#undef B7
#undef B6
#undef B5
#undef B4
#undef B3
#undef B2
#undef B1
#undef B0

#undef A7
#undef A6
#undef A5
#undef A4
#undef A3
#undef A2
#undef A1
#undef A0

#endif /* !defined (__AVR_TINY__) */


.section .text.libgcc.prologue, "ax", @progbits

/**********************************
 * This is a prologue subroutine
 **********************************/
#if !defined (__AVR_TINY__)
#if defined (L_prologue)

;; This function does not clobber T-flag; 64-bit division relies on it
DEFUN __prologue_saves__
	push r2
	push r3
	push r4
	push r5
	push r6
	push r7
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
	push r16
	push r17
	push r28
	push r29
#if !defined (__AVR_HAVE_SPH__)
	in	r28,__SP_L__
	sub	r28,r26
	out	__SP_L__,r28
	clr	r29
#elif defined (__AVR_XMEGA__)
	in	r28,__SP_L__
	in	r29,__SP_H__
	sub	r28,r26
	sbc	r29,r27
	out	__SP_L__,r28
	out	__SP_H__,r29
#else
	in	r28,__SP_L__
	in	r29,__SP_H__
	sub	r28,r26
	sbc	r29,r27
	in	__tmp_reg__,__SREG__
	cli
	out	__SP_H__,r29
	out	__SREG__,__tmp_reg__
	out	__SP_L__,r28
#endif /* #SP = 8/16 */

	XIJMP

ENDF __prologue_saves__
#endif /* defined (L_prologue) */

/*
 * This is an epilogue subroutine
 */
#if defined (L_epilogue)

DEFUN __epilogue_restores__
	ldd	r2,Y+18
	ldd	r3,Y+17
	ldd	r4,Y+16
	ldd	r5,Y+15
	ldd	r6,Y+14
	ldd	r7,Y+13
	ldd	r8,Y+12
	ldd	r9,Y+11
	ldd	r10,Y+10
	ldd	r11,Y+9
	ldd	r12,Y+8
	ldd	r13,Y+7
	ldd	r14,Y+6
	ldd	r15,Y+5
	ldd	r16,Y+4
	ldd	r17,Y+3
	ldd	r26,Y+2
#if !defined (__AVR_HAVE_SPH__)
	ldd	r29,Y+1
	add	r28,r30
	out	__SP_L__,r28
	mov	r28, r26
#elif defined (__AVR_XMEGA__)
	ldd  r27,Y+1
	add  r28,r30
	adc  r29,__zero_reg__
	out  __SP_L__,r28
	out  __SP_H__,r29
	wmov 28, 26
#else
	ldd	r27,Y+1
	add	r28,r30
	adc	r29,__zero_reg__
	in	__tmp_reg__,__SREG__
	cli
	out	__SP_H__,r29
	out	__SREG__,__tmp_reg__
	out	__SP_L__,r28
	mov_l	r28, r26
	mov_h	r29, r27
#endif /* #SP = 8/16 */
	ret
ENDF __epilogue_restores__
#endif /* defined (L_epilogue) */
#endif /* !defined (__AVR_TINY__) */

#ifdef L_exit
	.section .fini9,"ax",@progbits
DEFUN _exit
	.weak	exit
exit:
ENDF _exit

	/* Code from .fini8 ... .fini1 sections inserted by ld script.  */

	.section .fini0,"ax",@progbits
	cli
__stop_program:
	rjmp	__stop_program
#endif /* defined (L_exit) */

#ifdef L_cleanup
	.weak	_cleanup
	.func	_cleanup
_cleanup:
	ret
.endfunc
#endif /* defined (L_cleanup) */


.section .text.libgcc, "ax", @progbits

#ifdef L_tablejump2
DEFUN __tablejump2__
    lsl     r30
    rol     r31
#if defined (__AVR_HAVE_EIJMP_EICALL__)
    ;; Word address of gs() jumptable entry in R24:Z
    rol     r24
    out     __RAMPZ__, r24
#elif defined (__AVR_HAVE_ELPM__)
    ;; Word address of jumptable entry in Z
    clr     __tmp_reg__
    rol     __tmp_reg__
    out     __RAMPZ__, __tmp_reg__
#endif

    ;; Read word address from jumptable and jump

#if defined (__AVR_HAVE_ELPMX__)
    elpm    __tmp_reg__, Z+
    elpm    r31, Z
    mov     r30, __tmp_reg__
#ifdef __AVR_HAVE_RAMPD__
    ;; Reset RAMPZ to 0 so that EBI devices don't read garbage from RAM
    out     __RAMPZ__, __zero_reg__
#endif /* RAMPD */
    XIJMP
#elif defined (__AVR_HAVE_ELPM__)
    elpm
    push    r0
    adiw    r30, 1
    elpm
    push    r0
    ret
#elif defined (__AVR_HAVE_LPMX__)
    lpm     __tmp_reg__, Z+
    lpm     r31, Z
    mov     r30, __tmp_reg__
    ijmp
#elif defined (__AVR_TINY__)
    wsubi 30, -(__AVR_TINY_PM_BASE_ADDRESS__) ; Add PM offset to Z
    ld __tmp_reg__, Z+
    ld r31, Z   ; Use ld instead of lpm to load Z
    mov r30, __tmp_reg__    
    ijmp
#else
    lpm
    push    r0
    adiw    r30, 1
    lpm
    push    r0
    ret
#endif
ENDF __tablejump2__
#endif /* L_tablejump2 */

#if defined(__AVR_TINY__)
#ifdef L_copy_data
        .section .init4,"ax",@progbits
        .global __do_copy_data
__do_copy_data:
        ldi     r18, hi8(__data_end)
        ldi     r26, lo8(__data_start)
        ldi     r27, hi8(__data_start)
        ldi     r30, lo8(__data_load_start + __AVR_TINY_PM_BASE_ADDRESS__)
        ldi     r31, hi8(__data_load_start + __AVR_TINY_PM_BASE_ADDRESS__)
        rjmp    .L__do_copy_data_start
.L__do_copy_data_loop:
        ld      r19, z+
        st      X+, r19
.L__do_copy_data_start:
        cpi     r26, lo8(__data_end)
        cpc     r27, r18
        brne    .L__do_copy_data_loop
#endif
#else
#ifdef L_copy_data
	.section .init4,"ax",@progbits
DEFUN __do_copy_data
#if defined(__AVR_HAVE_ELPMX__)
	ldi	r17, hi8(__data_end)
	ldi	r26, lo8(__data_start)
	ldi	r27, hi8(__data_start)
	ldi	r30, lo8(__data_load_start)
	ldi	r31, hi8(__data_load_start)
	ldi	r16, hh8(__data_load_start)
	out	__RAMPZ__, r16
	rjmp	.L__do_copy_data_start
.L__do_copy_data_loop:
	elpm	r0, Z+
	st	X+, r0
.L__do_copy_data_start:
	cpi	r26, lo8(__data_end)
	cpc	r27, r17
	brne	.L__do_copy_data_loop
#elif  !defined(__AVR_HAVE_ELPMX__) && defined(__AVR_HAVE_ELPM__)
	ldi	r17, hi8(__data_end)
	ldi	r26, lo8(__data_start)
	ldi	r27, hi8(__data_start)
	ldi	r30, lo8(__data_load_start)
	ldi	r31, hi8(__data_load_start)
	ldi	r16, hh8(__data_load_start - 0x10000)
.L__do_copy_data_carry:
	inc	r16
	out	__RAMPZ__, r16
	rjmp	.L__do_copy_data_start
.L__do_copy_data_loop:
	elpm
	st	X+, r0
	adiw	r30, 1
	brcs	.L__do_copy_data_carry
.L__do_copy_data_start:
	cpi	r26, lo8(__data_end)
	cpc	r27, r17
	brne	.L__do_copy_data_loop
#elif !defined(__AVR_HAVE_ELPMX__) && !defined(__AVR_HAVE_ELPM__)
	ldi	r17, hi8(__data_end)
	ldi	r26, lo8(__data_start)
	ldi	r27, hi8(__data_start)
	ldi	r30, lo8(__data_load_start)
	ldi	r31, hi8(__data_load_start)
	rjmp	.L__do_copy_data_start
.L__do_copy_data_loop:
#if defined (__AVR_HAVE_LPMX__)
	lpm	r0, Z+
#else
	lpm
	adiw	r30, 1
#endif
	st	X+, r0
.L__do_copy_data_start:
	cpi	r26, lo8(__data_end)
	cpc	r27, r17
	brne	.L__do_copy_data_loop
#endif /* !defined(__AVR_HAVE_ELPMX__) && !defined(__AVR_HAVE_ELPM__) */
#if defined (__AVR_HAVE_ELPM__) && defined (__AVR_HAVE_RAMPD__)
	;; Reset RAMPZ to 0 so that EBI devices don't read garbage from RAM
	out	__RAMPZ__, __zero_reg__
#endif /* ELPM && RAMPD */
ENDF __do_copy_data
#endif /* L_copy_data */
#endif /* !defined (__AVR_TINY__) */

/* __do_clear_bss is only necessary if there is anything in .bss section.  */

#ifdef L_clear_bss
	.section .init4,"ax",@progbits
DEFUN __do_clear_bss
	ldi	r18, hi8(__bss_end)
	ldi	r26, lo8(__bss_start)
	ldi	r27, hi8(__bss_start)
	rjmp	.do_clear_bss_start
.do_clear_bss_loop:
	st	X+, __zero_reg__
.do_clear_bss_start:
	cpi	r26, lo8(__bss_end)
	cpc	r27, r18
	brne	.do_clear_bss_loop
ENDF __do_clear_bss
#endif /* L_clear_bss */

/* __do_global_ctors and __do_global_dtors are only necessary
   if there are any constructors/destructors.  */

#if defined(__AVR_TINY__)
#define cdtors_tst_reg r18
#else
#define cdtors_tst_reg r17
#endif

#ifdef L_ctors
	.section .init6,"ax",@progbits
DEFUN __do_global_ctors
    ldi     cdtors_tst_reg, pm_hi8(__ctors_start)
    ldi     r28, pm_lo8(__ctors_end)
    ldi     r29, pm_hi8(__ctors_end)
#ifdef __AVR_HAVE_EIJMP_EICALL__
    ldi     r16, pm_hh8(__ctors_end)
#endif /* HAVE_EIJMP */
    rjmp    .L__do_global_ctors_start
.L__do_global_ctors_loop:
    wsubi   28, 1
#ifdef __AVR_HAVE_EIJMP_EICALL__
    sbc     r16, __zero_reg__
    mov     r24, r16
#endif /* HAVE_EIJMP */
    mov_h   r31, r29
    mov_l   r30, r28
    XCALL   __tablejump2__
.L__do_global_ctors_start:
    cpi     r28, pm_lo8(__ctors_start)
    cpc     r29, cdtors_tst_reg
#ifdef __AVR_HAVE_EIJMP_EICALL__
    ldi     r24, pm_hh8(__ctors_start)
    cpc     r16, r24
#endif /* HAVE_EIJMP */
    brne    .L__do_global_ctors_loop
ENDF __do_global_ctors
#endif /* L_ctors */

#ifdef L_dtors
	.section .fini6,"ax",@progbits
DEFUN __do_global_dtors
    ldi     cdtors_tst_reg, pm_hi8(__dtors_end)
    ldi     r28, pm_lo8(__dtors_start)
    ldi     r29, pm_hi8(__dtors_start)
#ifdef __AVR_HAVE_EIJMP_EICALL__
    ldi     r16, pm_hh8(__dtors_start)
#endif /* HAVE_EIJMP */
    rjmp    .L__do_global_dtors_start
.L__do_global_dtors_loop:
#ifdef __AVR_HAVE_EIJMP_EICALL__
    mov     r24, r16
#endif /* HAVE_EIJMP */
    mov_h   r31, r29
    mov_l   r30, r28
    XCALL   __tablejump2__
    waddi   28, 1
#ifdef __AVR_HAVE_EIJMP_EICALL__
    adc     r16, __zero_reg__
#endif /* HAVE_EIJMP */
.L__do_global_dtors_start:
    cpi     r28, pm_lo8(__dtors_end)
    cpc     r29, cdtors_tst_reg
#ifdef __AVR_HAVE_EIJMP_EICALL__
    ldi     r24, pm_hh8(__dtors_end)
    cpc     r16, r24
#endif /* HAVE_EIJMP */
    brne    .L__do_global_dtors_loop
ENDF __do_global_dtors
#endif /* L_dtors */

#undef cdtors_tst_reg

.section .text.libgcc, "ax", @progbits

#if !defined (__AVR_TINY__)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading n bytes from Flash; n = 3,4
;; R22... = Flash[Z]
;; Clobbers: __tmp_reg__

#if (defined (L_load_3)        \
     || defined (L_load_4))    \
    && !defined (__AVR_HAVE_LPMX__)

;; Destination
#define D0  22
#define D1  D0+1
#define D2  D0+2
#define D3  D0+3

.macro  .load dest, n
    lpm
    mov     \dest, r0
.if \dest != D0+\n-1
    adiw    r30, 1
.else
    sbiw    r30, \n-1
.endif
.endm

#if defined (L_load_3)
DEFUN __load_3
    push  D3
    XCALL __load_4
    pop   D3
    ret
ENDF __load_3
#endif /* L_load_3 */

#if defined (L_load_4)
DEFUN __load_4
    .load D0, 4
    .load D1, 4
    .load D2, 4
    .load D3, 4
    ret
ENDF __load_4
#endif /* L_load_4 */

#endif /* L_load_3 || L_load_3 */
#endif /* !defined (__AVR_TINY__) */

#if !defined (__AVR_TINY__)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading n bytes from Flash or RAM;  n = 1,2,3,4
;; R22... = Flash[R21:Z] or RAM[Z] depending on R21.7
;; Clobbers: __tmp_reg__, R21, R30, R31

#if (defined (L_xload_1)            \
     || defined (L_xload_2)         \
     || defined (L_xload_3)         \
     || defined (L_xload_4))

;; Destination
#define D0  22
#define D1  D0+1
#define D2  D0+2
#define D3  D0+3

;; Register containing bits 16+ of the address

#define HHI8  21

#if defined (L_xload_1)
DEFUN __xload_1
#if defined (__AVR_HAVE_LPMX__) && !defined (__AVR_HAVE_ELPM__)
    sbrc    HHI8, 7
    ld      D0, Z
    sbrs    HHI8, 7
    lpm     D0, Z
    ret
#else
    .branch_plus HHI8, __fload_1
    ld      D0, Z
    ret
#endif /* LPMx && ! ELPM */
ENDF __xload_1
#endif /* L_xload_1 */

#if defined (L_xload_2)
DEFUN __xload_2
    .branch_plus HHI8, __fload_2
    ld      D0, Z+
    ld      D1, Z+
    ret
ENDF __xload_2
#endif /* L_xload_2 */

#if defined (L_xload_3)
DEFUN __xload_3
    .branch_plus HHI8, __fload_3
    ld      D0, Z+
    ld      D1, Z+
    ld      D2, Z+
    ret
ENDF __xload_3
#endif /* L_xload_3 */

#if defined (L_xload_4)
DEFUN __xload_4
    .branch_plus HHI8, __fload_4
    ld      D0, Z+
    ld      D1, Z+
    ld      D2, Z+
    ld      D3, Z+
    ret
ENDF __xload_4
#endif /* L_xload_4 */

#endif /* L_xload_{1|2|3|4} */
#endif /* if !defined (__AVR_TINY__) */

#if !defined (__AVR_TINY__)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading n bytes from Flash;  n = 1,2,3,4
;; R22... = Flash[R21:Z]
;; Clobbers: __tmp_reg__, R21, R30, R31

#if (defined (L_fload_1)            \
     || defined (L_fload_2)         \
     || defined (L_fload_3)         \
     || defined (L_fload_4))

;; Destination
#define D0  22
#define D1  D0+1
#define D2  D0+2
#define D3  D0+3

;; Register containing bits 16+ of the address

#define HHI8  21

.macro  .fload dest, n
#if defined (__AVR_HAVE_ELPM__)
.if \dest == D0
    out     __RAMPZ__, HHI8
.endif
#endif /* __AVR_HAVE_ELPM__ */
#if defined (__AVR_HAVE_ELPMX__)
    elpm    \dest, Z+
#elif defined (__AVR_HAVE_ELPM__)
    elpm
    mov     \dest, r0
.if \dest != D0+\n-1
    adiw    r30, 1
    adc     HHI8, __zero_reg__
    out     __RAMPZ__, HHI8
.endif
#elif defined (__AVR_HAVE_LPMX__)
    lpm     \dest, Z+
#else
    lpm
    mov     \dest, r0
.if \dest != D0+\n-1
    adiw    r30, 1
.endif
#endif
#if defined (__AVR_HAVE_ELPM__) && defined (__AVR_HAVE_RAMPD__)
.if \dest == D0+\n-1
    ;; Reset RAMPZ to 0 so that EBI devices don't read garbage from RAM
    out     __RAMPZ__, __zero_reg__
.endif
#endif
.endm ; .fload

#if defined (L_fload_1)
DEFUN __fload_1
#if defined (__AVR_HAVE_LPMX__) && !defined (__AVR_HAVE_ELPM__)
    lpm     D0, Z
    ret
#else
    .fload  D0, 1
    ret
#endif /* LPMx && ! ELPM */
ENDF __fload_1
#endif /* L_fload_1 */

#if defined (L_fload_2)
DEFUN __fload_2
    .fload  D0, 2
    .fload  D1, 2
    ret
ENDF __fload_2
#endif /* L_fload_2 */

#if defined (L_fload_3)
DEFUN __fload_3
    .fload  D0, 3
    .fload  D1, 3
    .fload  D2, 3
    ret
ENDF __fload_3
#endif /* L_fload_3 */

#if defined (L_fload_4)
DEFUN __fload_4
    .fload  D0, 4
    .fload  D1, 4
    .fload  D2, 4
    .fload  D3, 4
    ret
ENDF __fload_4
#endif /* L_fload_4 */

#endif /* L_fload_{1|2|3|4} */
#endif /* if !defined (__AVR_TINY__) */


#if !defined (__AVR_TINY__)
#if defined (L_strlen_memx)
DEFUN __strlen_memx
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    tst     r24
    brmi 1f
#else
    sbrs    r24, 7
#endif
    XJMP    strlen_PF           ; AVR-LibC
1:  wmov    24, 22
    XJMP    strlen              ; AVR-LibC
ENDF __strlen_memx
#endif /* L_strlen_memx */
#endif /* if !defined (__AVR_TINY__) */


#if !defined (__AVR_TINY__)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memcopy from Address Space __memx to RAM
;; R23:Z = Source Address
;; X     = Destination Address
;; Clobbers: __tmp_reg__, R23, R24, R25, X, Z

#if defined (L_movmemx)

#define HHI8  23
#define LOOP  24

DEFUN __movmemx_qi
    ;; #Bytes to copy fits in 8 Bits (1..255)
    ;; Zero-extend Loop Counter
    clr     LOOP+1
    ;; FALLTHRU
ENDF __movmemx_qi

DEFUN __movmemx_hi

    .branch_plus    HHI8, __movmemf_hi

    ;; Read 1 Byte from RAM...
1:  ld      r0, Z+
    ;; and store that Byte to RAM Destination
    st      X+, r0
    sbiw    LOOP, 1
    brne    1b
    ret
ENDF __movmemx_hi

#undef HHI8
#undef LOOP

#endif /* L_movmemx */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memcopy from Address Space __flashx to RAM
;; R23:Z = Source Address
;; X     = Destination Address
;; Clobbers: __tmp_reg__, R23, R24, R25, X, Z

#if defined (L_movmemf)

#define HHI8  23
#define LOOP  24

DEFUN __movmemf_qi
    ;; #Bytes to copy fits in 8 Bits (1..255)
    ;; Zero-extend Loop Counter
    clr     LOOP+1
    ;; FALLTHRU
ENDF __movmemf_qi

DEFUN __movmemf_hi

;; Read from Flash

#if defined (__AVR_HAVE_ELPM__)
    out     __RAMPZ__, HHI8
#endif

0:  ;; Load 1 Byte from Flash...

#if defined (__AVR_HAVE_ELPMX__)
    elpm    r0, Z+
#elif defined (__AVR_HAVE_ELPM__)
    elpm
    adiw    r30, 1
    adc     HHI8, __zero_reg__
    out     __RAMPZ__, HHI8
#elif defined (__AVR_HAVE_LPMX__)
    lpm     r0, Z+
#else
    lpm
    adiw    r30, 1
#endif

    ;; ...and store that Byte to RAM Destination
    st      X+, r0
    sbiw    LOOP, 1
    brne    0b
#if defined (__AVR_HAVE_ELPM__) && defined (__AVR_HAVE_RAMPD__)
    ;; Reset RAMPZ to 0 so that EBI devices don't read garbage from RAM
    out	__RAMPZ__, __zero_reg__
#endif /* ELPM && RAMPD */
    ret
ENDF __movmemf_hi

#undef HHI8
#undef LOOP

#endif /* L_movmemf */
#endif /* !defined (__AVR_TINY__) */ 


.section .text.libgcc.builtins, "ax", @progbits

/**********************************
 * Find first set Bit (ffs)
 **********************************/

#if defined (L_ffssi2)
;; find first set bit
;; r25:r24 = ffs32 (r25:r22)
;; clobbers: r22, r26
DEFUN __ffssi2
    clr  r26
    tst  r22
    brne 1f
    subi r26, -8
    or   r22, r23
    brne 1f
    subi r26, -8
    or   r22, r24
    brne 1f
    subi r26, -8
    or   r22, r25
    brne 1f
    ret
1:  mov  r24, r22
    XJMP __loop_ffsqi2
ENDF __ffssi2
#endif /* defined (L_ffssi2) */

#if defined (L_ffshi2)
;; find first set bit
;; r25:r24 = ffs16 (r25:r24)
;; clobbers: r26
DEFUN __ffshi2
    clr  r26
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Some cores have problem skipping 2-word instruction
    tst  r24
    breq 2f
#else
    cpse r24, __zero_reg__
#endif /* __AVR_HAVE_JMP_CALL__ */
1:  XJMP __loop_ffsqi2
2:  ldi  r26, 8
    or   r24, r25
    brne 1b
    ret
ENDF __ffshi2
#endif /* defined (L_ffshi2) */

#if defined (L_loop_ffsqi2)
;; Helper for ffshi2, ffssi2
;; r25:r24 = r26 + zero_extend16 (ffs8(r24))
;; r24 must be != 0
;; clobbers: r26
DEFUN __loop_ffsqi2
    inc  r26
    lsr  r24
    brcc __loop_ffsqi2
    mov  r24, r26
    clr  r25
    ret
ENDF __loop_ffsqi2
#endif /* defined (L_loop_ffsqi2) */


/**********************************
 * Count trailing Zeros (ctz)
 **********************************/

#if defined (L_ctzsi2)
;; count trailing zeros
;; r25:r24 = ctz32 (r25:r22)
;; clobbers: r26, r22
;; ctz(0) = 255
;; Note that ctz(0) in undefined for GCC
DEFUN __ctzsi2
    XCALL __ffssi2
    dec  r24
    ret
ENDF __ctzsi2
#endif /* defined (L_ctzsi2) */

#if defined (L_ctzhi2)
;; count trailing zeros
;; r25:r24 = ctz16 (r25:r24)
;; clobbers: r26
;; ctz(0) = 255
;; Note that ctz(0) in undefined for GCC
DEFUN __ctzhi2
    XCALL __ffshi2
    dec  r24
    ret
ENDF __ctzhi2
#endif /* defined (L_ctzhi2) */


/**********************************
 * Count leading Zeros (clz)
 **********************************/

#if defined (L_clzdi2)
;; count leading zeros
;; r25:r24 = clz64 (r25:r18)
;; clobbers: r22, r23, r26
DEFUN __clzdi2
    XCALL __clzsi2
    sbrs r24, 5
    ret
    mov_l r22, r18
    mov_h r23, r19
    mov_l r24, r20
    mov_h r25, r21
    XCALL __clzsi2
    subi r24, -32
    ret
ENDF __clzdi2
#endif /* defined (L_clzdi2) */

#if defined (L_clzsi2)
;; count leading zeros
;; r25:r24 = clz32 (r25:r22)
;; clobbers: r26
DEFUN __clzsi2
    XCALL __clzhi2
    sbrs r24, 4
    ret
    mov_l r24, r22
    mov_h r25, r23
    XCALL __clzhi2
    subi r24, -16
    ret
ENDF __clzsi2
#endif /* defined (L_clzsi2) */

#if defined (L_clzhi2)
;; count leading zeros
;; r25:r24 = clz16 (r25:r24)
;; clobbers: r26
DEFUN __clzhi2
    clr  r26
    tst  r25
    brne 1f
    or   r25, r24
    breq 0f
    subi r26, -8
1:  cpi  r25, 16
    brsh 3f
    subi r26, -3
    swap r25
2:  inc  r26
3:  lsl  r25
    brcc 2b
    mov  r24, r26
    clr  r25
    ret
0:  ldi  r24, 16
    ret
ENDF __clzhi2
#endif /* defined (L_clzhi2) */


/**********************************
 * Parity
 **********************************/

#if defined (L_paritydi2)
;; r25:r24 = parity64 (r25:r18)
;; clobbers: __tmp_reg__
DEFUN __paritydi2
    eor  r24, r18
    eor  r24, r19
    eor  r24, r20
    eor  r24, r21
    XJMP __paritysi2
ENDF __paritydi2
#endif /* defined (L_paritydi2) */

#if defined (L_paritysi2)
;; r25:r24 = parity32 (r25:r22)
;; clobbers: __tmp_reg__
DEFUN __paritysi2
    eor  r24, r22
    eor  r24, r23
    XJMP __parityhi2
ENDF __paritysi2
#endif /* defined (L_paritysi2) */

#if defined (L_parityhi2)
;; r25:r24 = parity16 (r25:r24)
;; clobbers: __tmp_reg__
DEFUN __parityhi2
    eor  r24, r25
;; FALLTHRU
ENDF __parityhi2

;; r25:r24 = parity8 (r24)
;; clobbers: __tmp_reg__
DEFUN __parityqi2
    ;; parity is in r24[0..7]
    mov  __tmp_reg__, r24
    swap __tmp_reg__
    eor  r24, __tmp_reg__
    ;; parity is in r24[0..3]
    subi r24, -4
    andi r24, -5
    subi r24, -6
    ;; parity is in r24[0,3]
    sbrc r24, 3
    inc  r24
    ;; parity is in r24[0]
    andi r24, 1
    clr  r25
    ret
ENDF __parityqi2
#endif /* defined (L_parityhi2) */


/**********************************
 * Population Count
 **********************************/

#if defined (L_popcounthi2)
;; population count
;; r25:r24 = popcount16 (r25:r24)
;; clobbers: __tmp_reg__
DEFUN __popcounthi2
    XCALL __popcountqi2
    push r24
    mov  r24, r25
    XCALL __popcountqi2
    clr  r25
    ;; FALLTHRU
ENDF __popcounthi2

DEFUN __popcounthi2_tail
    pop   __tmp_reg__
    add   r24, __tmp_reg__
    ret
ENDF __popcounthi2_tail
#endif /* defined (L_popcounthi2) */

#if defined (L_popcountsi2)
;; population count
;; r25:r24 = popcount32 (r25:r22)
;; clobbers: __tmp_reg__
DEFUN __popcountsi2
    XCALL __popcounthi2
    push  r24
    mov_l r24, r22
    mov_h r25, r23
    XCALL __popcounthi2
    XJMP  __popcounthi2_tail
ENDF __popcountsi2
#endif /* defined (L_popcountsi2) */

#if defined (L_popcountdi2)
;; population count
;; r25:r24 = popcount64 (r25:r18)
;; clobbers: r22, r23, __tmp_reg__
DEFUN __popcountdi2
    XCALL __popcountsi2
    push  r24
    mov_l r22, r18
    mov_h r23, r19
    mov_l r24, r20
    mov_h r25, r21
    XCALL __popcountsi2
    XJMP  __popcounthi2_tail
ENDF __popcountdi2
#endif /* defined (L_popcountdi2) */

#if defined (L_popcountqi2)
;; population count
;; r24 = popcount8 (r24)
;; clobbers: __tmp_reg__
DEFUN __popcountqi2
    mov  __tmp_reg__, r24    ; oeoeoeoe
    andi r24, 0xAA           ; o0o0o0o0
    lsr  r24                 ; 0o0o0o0o
    ;; Four values 0, 1 or 2: # bits set o+e
    sub  __tmp_reg__, r24    ; 44332211
    mov  r24, __tmp_reg__    ; 44332211
    andi r24, 0x33           ; 00330011
    eor  __tmp_reg__, r24    ; 44002200
    lsr  __tmp_reg__         ; 04400220
    lsr  __tmp_reg__         ; 00440022
    add  r24, __tmp_reg__    ; 04210421
    mov  __tmp_reg__, r24    ; h421l421
    swap __tmp_reg__         ; l421h421
    add  r24, __tmp_reg__    ; 84218421
    andi r24, 0xf            ; 00008421 /17
    ret
ENDF __popcountqi2
#endif /* defined (L_popcountqi2) */


/**********************************
 * Swap bytes
 **********************************/

;; swap two registers with different register number
.macro bswap a, b
    eor \a, \b
    eor \b, \a
    eor \a, \b
.endm

#if defined (L_bswapsi2)
;; swap bytes
;; r25:r22 = bswap32 (r25:r22)
DEFUN __bswapsi2
    bswap r22, r25
    bswap r23, r24
    ret
ENDF __bswapsi2
#endif /* defined (L_bswapsi2) */

#if defined (L_bswapdi2)
;; swap bytes
;; r25:r18 = bswap64 (r25:r18)
DEFUN __bswapdi2
    bswap r18, r25
    bswap r19, r24
    bswap r20, r23
    bswap r21, r22
    ret
ENDF __bswapdi2
#endif /* defined (L_bswapdi2) */


/**********************************
 * 64-bit shifts
 **********************************/

#if defined (L_ashrdi3)

#define SS __zero_reg__

;; Arithmetic shift right
;; r25:r18 = ashr64 (r25:r18, r17:r16)
DEFUN __ashrdi3
    sbrc    r25, 7
    com     SS
    ;; FALLTHRU
ENDF  __ashrdi3

;; Logic shift right
;; r25:r18 = lshr64 (r25:r18, r17:r16)
DEFUN __lshrdi3
    ;; Signs are in SS (zero_reg)
    mov     __tmp_reg__, r16
0:  cpi     r16, 8
    brlo 2f
    subi    r16, 8
    mov     r18, r19
    mov     r19, r20
    mov     r20, r21
    mov     r21, r22
    mov     r22, r23
    mov     r23, r24
    mov     r24, r25
    mov     r25, SS
    rjmp 0b
1:  asr     SS
    ror     r25
    ror     r24
    ror     r23
    ror     r22
    ror     r21
    ror     r20
    ror     r19
    ror     r18
2:  dec     r16
    brpl 1b
    clr     __zero_reg__
    mov     r16, __tmp_reg__
    ret
ENDF __lshrdi3

#undef SS

#endif /* defined (L_ashrdi3) */

#if defined (L_ashldi3)
;; Shift left
;; r25:r18 = ashl64 (r25:r18, r17:r16)
;; This function does not clobber T.
DEFUN __ashldi3
    mov     __tmp_reg__, r16
0:  cpi     r16, 8
    brlo 2f
    mov     r25, r24
    mov     r24, r23
    mov     r23, r22
    mov     r22, r21
    mov     r21, r20
    mov     r20, r19
    mov     r19, r18
    clr     r18
    subi    r16, 8
    rjmp 0b
1:  lsl     r18
    rol     r19
    rol     r20
    rol     r21
    rol     r22
    rol     r23
    rol     r24
    rol     r25
2:  dec     r16
    brpl 1b
    mov     r16, __tmp_reg__
    ret
ENDF __ashldi3
#endif /* defined (L_ashldi3) */

#if defined (L_rotldi3)
;; Rotate left
;; r25:r18 = rotl64 (r25:r18, r17:r16)
DEFUN __rotldi3
    push    r16
0:  cpi     r16, 8
    brlo 2f
    subi    r16, 8
    mov     __tmp_reg__, r25
    mov     r25, r24
    mov     r24, r23
    mov     r23, r22
    mov     r22, r21
    mov     r21, r20
    mov     r20, r19
    mov     r19, r18
    mov     r18, __tmp_reg__
    rjmp 0b
1:  lsl     r18
    rol     r19
    rol     r20
    rol     r21
    rol     r22
    rol     r23
    rol     r24
    rol     r25
    adc     r18, __zero_reg__
2:  dec     r16
    brpl 1b
    pop     r16
    ret
ENDF __rotldi3
#endif /* defined (L_rotldi3) */


.section .text.libgcc.fmul, "ax", @progbits

/***********************************************************/
;;; Softmul versions of FMUL, FMULS and FMULSU to implement
;;; __builtin_avr_fmul* if !AVR_HAVE_MUL
/***********************************************************/

#define A1 24
#define B1 25
#define C0 22
#define C1 23
#define A0 __tmp_reg__

#ifdef L_fmuls
;;; r23:r22 = fmuls (r24, r25) like in FMULS instruction
;;; Clobbers: r24, r25, __tmp_reg__
DEFUN __fmuls
    ;; A0.7 = negate result?
    mov  A0, A1
    eor  A0, B1
    ;; B1 = |B1|
    sbrc B1, 7
    neg  B1
    XJMP __fmulsu_exit
ENDF __fmuls
#endif /* L_fmuls */

#ifdef L_fmulsu
;;; r23:r22 = fmulsu (r24, r25) like in FMULSU instruction
;;; Clobbers: r24, r25, __tmp_reg__
DEFUN __fmulsu
    ;; A0.7 = negate result?
    mov  A0, A1
;; FALLTHRU
ENDF __fmulsu

;; Helper for __fmuls and __fmulsu
DEFUN __fmulsu_exit
    ;; A1 = |A1|
    sbrc A1, 7
    neg  A1
#ifdef __AVR_ERRATA_SKIP_JMP_CALL__
    ;; Some cores have problem skipping 2-word instruction
    tst  A0
    brmi 1f
#else
    sbrs A0, 7
#endif /* __AVR_HAVE_JMP_CALL__ */
    XJMP  __fmul
1:  XCALL __fmul
    ;; C = -C iff A0.7 = 1
    NEG2 C0
    ret
ENDF __fmulsu_exit
#endif /* L_fmulsu */


#ifdef L_fmul
;;; r22:r23 = fmul (r24, r25) like in FMUL instruction
;;; Clobbers: r24, r25, __tmp_reg__
DEFUN __fmul
    ; clear result
    clr   C0
    clr   C1
    clr   A0
1:  tst   B1
    ;; 1.0 = 0x80, so test for bit 7 of B to see if A must to be added to C.
2:  brpl  3f
    ;; C += A
    add   C0, A0
    adc   C1, A1
3:  ;; A >>= 1
    lsr   A1
    ror   A0
    ;; B <<= 1
    lsl   B1
    brne  2b
    ret
ENDF __fmul
#endif /* L_fmul */

#undef A0
#undef A1
#undef B1
#undef C0
#undef C1



/**********************************
 * Floating-Point
 **********************************/

#if defined (L_powif)
#ifndef __AVR_TINY__

;; float output and arg #1
#define A0      22
#define A1      A0 + 1
#define A2      A0 + 2
#define A3      A0 + 3

;; float arg #2
#define B0      18
#define B1      B0 + 1
#define B2      B0 + 2
#define B3      B0 + 3

;; float X: input and iterated squares
#define X0      10
#define X1      X0 + 1
#define X2      X0 + 2
#define X3      X0 + 3

;; float Y: expand result
#define Y0      14
#define Y1      Y0 + 1
#define Y2      Y0 + 2
#define Y3      Y0 + 3

;; .7 = Sign of I.
;; .0 == 0  =>  Y = 1.0f implicitly.
#define Flags       R9
#define Y_set       0

;;;  Integer exponent input.
#define I0      28
#define I1      I0+1

#define ONE     0x3f800000

DEFUN __powisf2
    ;; Save 11 Registers: R9...R17, R28, R29
    do_prologue_saves 11

    ;; Fill local vars with input parameters.
    wmov    I0, 20
    mov4    X0, A0
    ;; Save sign of exponent for later.
    mov     Flags,  I1
    ;; I := abs (I)
    tst     I1
    brpl 1f
    NEG2    I0
1:
    ;; Y := (I % 2) ? X : 1.0f
    ;; (When we come from below, this is like SET, i.e. Flags.Y_set := 1).
    bst     I0, 0
    ;; Flags.Y_set = false means that we have to assume Y = 1.0f below.
    bld     Flags,  Y_set
2:  ;; We have A == X when we come from above.
    mov4    Y0, A0

.Loop:
    ;; while (I >>= 1)
    lsr     I1
    ror     I0
    sbiw    I0, 0
    breq .Loop_done

    ;; X := X * X
    mov4    A0, X0
#ifdef __WITH_AVRLIBC__
    XCALL   squaref
#else
    mov4    B0, X0
    XCALL   __mulsf3
#endif /* Have AVR-LibC? */
    mov4    X0, A0

    ;; if (I % 2 == 1)  Y := Y * X
    bst     I0, 0
    brtc .Loop
    bst     Flags, Y_set
    ;; When Y is not set  =>  Y := Y * X = 1.0f * X (= A)
    ;; Plus, we have to set Y_set = 1 (= I0.0)
    brtc 1b
    ;; Y is already set: Y := X * Y (= A * Y)
    mov4    B0, Y0
    XCALL   __mulsf3
    rjmp 2b

    ;; End while
.Loop_done:

    ;; A := 1.0f
    ldi     A3, hhi8(ONE)
    ldi     A2, hlo8(ONE)
    ldi     A1, hi8(ONE)
    ldi     A0, lo8(ONE)

    ;; When Y is still not set, the result is 1.0f (= A).
    bst     Flags, Y_set
    brtc .Lret

    ;; if (I was < 0) Y = 1.0f / Y
    tst     Flags
    brmi 1f
    ;; A := Y
    mov4    A0, Y0
    rjmp .Lret
1:  ;; A := 1 / Y = A / Y
    mov4    B0, Y0
    XCALL   __divsf3

.Lret:
    do_epilogue_restores 11
ENDF __powisf2

#undef A0
#undef A1
#undef A2
#undef A3

#undef B0
#undef B1
#undef B2
#undef B3

#undef X0
#undef X1
#undef X2
#undef X3

#undef Y0
#undef Y1
#undef Y2
#undef Y3

#undef I0
#undef I1
#undef ONE

#endif /* __AVR_TINY__ */
#endif /* L_powif */

#include "lib1funcs-fixed.S"
