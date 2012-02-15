/*  -*- Mode: Asm -*-  */
/* Copyright (C) 1998, 1999, 2000, 2007, 2008, 2009
   Free Software Foundation, Inc.
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

#define __zero_reg__ r1
#define __tmp_reg__ r0
#define __SREG__ 0x3f
#define __SP_H__ 0x3e
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

#if defined (__AVR_HAVE_JMP_CALL__)
#define XCALL call
#define XJMP  jmp
#else
#define XCALL rcall
#define XJMP  rjmp
#endif

.macro DEFUN name
.global \name
.func \name
\name:
.endm

.macro ENDF name
.size \name, .-\name
.endfunc
.endm


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

#if defined (L_mulqihi3)
DEFUN __mulqihi3
	clr	r25
	sbrc	r24, 7
	dec	r25
	clr	r23
	sbrc	r22, 7
	dec	r22
	XJMP	__mulhi3
ENDF __mulqihi3:
#endif /* defined (L_mulqihi3) */

#if defined (L_umulqihi3)
DEFUN __umulqihi3
	clr	r25
	clr	r23
	XJMP	__mulhi3
ENDF __umulqihi3
#endif /* defined (L_umulqihi3) */

/*******************************************************
    Multiplication  16 x 16  without MUL
*******************************************************/
#if defined (L_mulhi3)
#define	r_arg1L	r24		/* multiplier Low */
#define	r_arg1H	r25		/* multiplier High */
#define	r_arg2L	r22		/* multiplicand Low */
#define	r_arg2H	r23		/* multiplicand High */
#define r_resL	__tmp_reg__	/* result Low */
#define r_resH  r21		/* result High */

DEFUN __mulhi3
	clr	r_resH		; clear result
	clr	r_resL		; clear result
__mulhi3_loop:
	sbrs	r_arg1L,0
	rjmp	__mulhi3_skip1
	add	r_resL,r_arg2L	; result + multiplicand
	adc	r_resH,r_arg2H
__mulhi3_skip1:	
	add	r_arg2L,r_arg2L	; shift multiplicand
	adc	r_arg2H,r_arg2H

	cp	r_arg2L,__zero_reg__
	cpc	r_arg2H,__zero_reg__
	breq	__mulhi3_exit	; while multiplicand != 0

	lsr	r_arg1H		; gets LSB of multiplier
	ror	r_arg1L
	sbiw	r_arg1L,0
	brne	__mulhi3_loop	; exit if multiplier = 0
__mulhi3_exit:
	mov	r_arg1H,r_resH	; result to return register
	mov	r_arg1L,r_resL
	ret
ENDF __mulhi3

#undef r_arg1L
#undef r_arg1H
#undef r_arg2L
#undef r_arg2H
#undef r_resL 	
#undef r_resH 

#endif /* defined (L_mulhi3) */

/*******************************************************
    Widening Multiplication  32 = 16 x 16  without MUL
*******************************************************/

#if defined (L_mulhisi3)
DEFUN __mulhisi3
;;; FIXME: This is dead code (noone calls it)
    mov_l   r18, r24
    mov_h   r19, r25
    clr     r24
    sbrc    r23, 7
    dec     r24
    mov     r25, r24
    clr     r20
    sbrc    r19, 7
    dec     r20
    mov     r21, r20
    XJMP    __mulsi3
ENDF __mulhisi3
#endif /* defined (L_mulhisi3) */

#if defined (L_umulhisi3)
DEFUN __umulhisi3
;;; FIXME: This is dead code (noone calls it)
    mov_l   r18, r24
    mov_h   r19, r25
    clr     r24
    clr     r25
    mov_l   r20, r24
    mov_h   r21, r25
    XJMP    __mulsi3
ENDF __umulhisi3
#endif /* defined (L_umulhisi3) */

#if defined (L_mulsi3)
/*******************************************************
    Multiplication  32 x 32  without MUL
*******************************************************/
#define r_arg1L  r22		/* multiplier Low */
#define r_arg1H  r23
#define	r_arg1HL r24
#define	r_arg1HH r25		/* multiplier High */

#define	r_arg2L  r18		/* multiplicand Low */
#define	r_arg2H  r19	
#define	r_arg2HL r20
#define	r_arg2HH r21		/* multiplicand High */
	
#define r_resL	 r26		/* result Low */
#define r_resH   r27
#define r_resHL	 r30
#define r_resHH  r31		/* result High */

DEFUN __mulsi3
	clr	r_resHH		; clear result
	clr	r_resHL		; clear result
	clr	r_resH		; clear result
	clr	r_resL		; clear result
__mulsi3_loop:
	sbrs	r_arg1L,0
	rjmp	__mulsi3_skip1
	add	r_resL,r_arg2L		; result + multiplicand
	adc	r_resH,r_arg2H
	adc	r_resHL,r_arg2HL
	adc	r_resHH,r_arg2HH
__mulsi3_skip1:
	add	r_arg2L,r_arg2L		; shift multiplicand
	adc	r_arg2H,r_arg2H
	adc	r_arg2HL,r_arg2HL
	adc	r_arg2HH,r_arg2HH
	
	lsr	r_arg1HH	; gets LSB of multiplier
	ror	r_arg1HL
	ror	r_arg1H
	ror	r_arg1L
	brne	__mulsi3_loop
	sbiw	r_arg1HL,0
	cpc	r_arg1H,r_arg1L
	brne	__mulsi3_loop		; exit if multiplier = 0
__mulsi3_exit:
	mov_h	r_arg1HH,r_resHH	; result to return register
	mov_l	r_arg1HL,r_resHL
	mov_h	r_arg1H,r_resH
	mov_l	r_arg1L,r_resL
	ret
ENDF __mulsi3

#undef r_arg1L 
#undef r_arg1H 
#undef r_arg1HL
#undef r_arg1HH
             
#undef r_arg2L 
#undef r_arg2H 
#undef r_arg2HL
#undef r_arg2HH
             
#undef r_resL  
#undef r_resH  
#undef r_resHL 
#undef r_resHH 

#endif /* defined (L_mulsi3) */

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
    Widening Multiplication  32 = 16 x 16
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
    rcall   1f
    mul     A1, B0
1:  add     C1, r0
    adc     C2, r1
    clr     __zero_reg__
    adc     C3, __zero_reg__
    ret
ENDF __umulhisi3
#endif /* L_umulhisi3 */

/*******************************************************
    Widening Multiplication  32 = 16 x 32
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
    Multiplication  32 x 32
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
       Multiplication 24 x 24
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
#define C0  0
#define C1  C0+1
#define C2  21

;; R24:R22 *= R20:R18
;; Clobbers: __tmp_reg__, R18, R19, R20, R21

DEFUN __mulpsi3

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

#if defined (L_muldi3)

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
    rcall   __muldi3_6

    pop     A0
    pop     A1
    ;; 1 * 1
    wmov    26, B2
    XCALL   __umulhisi3  $  add C4,22 $ adc C5,23 $ adc C6,24 $ adc C7,25

    pop     r26
    pop     r27
    ;; 1 * 0
    rcall   __muldi3_6

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

    clr     __zero_reg__
    pop     r16
    pop     r17
    pop     r28
    pop     r29
    ret

__muldi3_6:
    XCALL   __umulhisi3
    add     C2, 22
    adc     C3, 23
    adc     C4, 24
    adc     C5, 25
    brcc    0f
    adiw    C6, 1
0:  ret
ENDF __muldi3

#undef C7
#undef C6
#undef C5
#undef C4
#undef C3
#undef C2
#undef C1
#undef C0

#else /* !HAVE_MUL */

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

#endif /* HAVE_MUL */

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

#endif /* L_muldi3 */

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
	sub	r_rem,r_rem	; clear remainder and carry
	ldi	r_cnt,9		; init loop counter
	rjmp	__udivmodqi4_ep	; jump to entry point
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
;; R24:R22 = R24:R22  udiv  R20:R18
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
    rcall   __divmodsi4_neg1    ; dividend negative: negate
0:
    sbrc    r_arg2HH,7
    rcall   __divmodsi4_neg2    ; divisor negative: negate
    XCALL   __udivmodsi4        ; do the unsigned div/mod
    sbrc    __tmp_reg__, 7      ; correct quotient sign
    rcall   __divmodsi4_neg2
    brtc    __divmodsi4_exit    ; correct remainder sign
__divmodsi4_neg1:
    ;; correct dividend/remainder sign
    com     r_arg1HH
    com     r_arg1HL
    com     r_arg1H
    neg     r_arg1L
    sbci    r_arg1H, 0xff
    sbci    r_arg1HL,0xff
    sbci    r_arg1HH,0xff
    ret
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
#elif defined (__AVR_HAVE_MOVW__) && !defined (__AVR_HAVE_8BIT_SP__)
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
    ;; No Frame needed (X = 0)
    clr r26
    clr r27
    ldi r30, lo8(gs(1f))
    ldi r31, hi8(gs(1f))
    XJMP __prologue_saves__ + ((18 - 12) * 2)

1:  ;; SS.7 will contain the Sign of the Quotient  (A.sign * B.sign)
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

4:  ;; Epilogue: Restore the Z = 12 Registers and return
    in r28, __SP_L__
#if defined (__AVR_HAVE_8BIT_SP__)
    clr r29
#else
    in r29, __SP_H__
#endif /* #SP = 8/16 */
    ldi r30, 12
    XJMP __epilogue_restores__ + ((18 - 12) * 2)

ENDF __divdi3_moddi3

#undef R_cnt
#undef SS
#undef NN

#endif /* L_divdi3 */

.section .text.libgcc, "ax", @progbits

#define TT __tmp_reg__

#if defined (L_adddi3)
;; (set (reg:DI 18)
;;      (plus:DI (reg:DI 18)
;;               (reg:DI 10)))
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


.section .text.libgcc.prologue, "ax", @progbits
    
/**********************************
 * This is a prologue subroutine
 **********************************/
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
#if defined (__AVR_HAVE_8BIT_SP__)
	in	r28,__SP_L__
	sub	r28,r26
	out	__SP_L__,r28
	clr	r29
#elif defined (__AVR__XMEGA__)
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

#if defined (__AVR_HAVE_EIJMP_EICALL__)
	eijmp
#else
	ijmp
#endif

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
#if defined (__AVR_HAVE_8BIT_SP__)
	ldd	r29,Y+1
	add	r28,r30
	out	__SP_L__,r28
	mov	r28, r26
#elif defined (__AVR__XMEGA__)
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
    
#ifdef L_tablejump
DEFUN __tablejump2__
	lsl	r30
	rol	r31
    ;; FALLTHRU
ENDF __tablejump2__

DEFUN __tablejump__
#if defined (__AVR_HAVE_LPMX__)
	lpm __tmp_reg__, Z+
	lpm r31, Z
	mov r30, __tmp_reg__
#if defined (__AVR_HAVE_EIJMP_EICALL__)
	eijmp
#else
	ijmp
#endif

#else /* !HAVE_LPMX */
	lpm
	adiw r30, 1
	push r0
	lpm
	push r0
#if defined (__AVR_HAVE_EIJMP_EICALL__)
	in   __tmp_reg__, __EIND__
	push __tmp_reg__
#endif
	ret
#endif /* !HAVE_LPMX */
ENDF __tablejump__
#endif /* defined (L_tablejump) */

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
ENDF __do_copy_data
#endif /* L_copy_data */

/* __do_clear_bss is only necessary if there is anything in .bss section.  */

#ifdef L_clear_bss
	.section .init4,"ax",@progbits
DEFUN __do_clear_bss
	ldi	r17, hi8(__bss_end)
	ldi	r26, lo8(__bss_start)
	ldi	r27, hi8(__bss_start)
	rjmp	.do_clear_bss_start
.do_clear_bss_loop:
	st	X+, __zero_reg__
.do_clear_bss_start:
	cpi	r26, lo8(__bss_end)
	cpc	r27, r17
	brne	.do_clear_bss_loop
ENDF __do_clear_bss
#endif /* L_clear_bss */

/* __do_global_ctors and __do_global_dtors are only necessary
   if there are any constructors/destructors.  */

#ifdef L_ctors
	.section .init6,"ax",@progbits
DEFUN __do_global_ctors
#if defined(__AVR_HAVE_RAMPZ__)
	ldi	r17, hi8(__ctors_start)
	ldi	r28, lo8(__ctors_end)
	ldi	r29, hi8(__ctors_end)
	ldi	r16, hh8(__ctors_end)
	rjmp	.L__do_global_ctors_start
.L__do_global_ctors_loop:
	sbiw	r28, 2
	sbc     r16, __zero_reg__
	mov_h	r31, r29
	mov_l	r30, r28
	out     __RAMPZ__, r16
	XCALL	__tablejump_elpm__
.L__do_global_ctors_start:
	cpi	r28, lo8(__ctors_start)
	cpc	r29, r17
	ldi	r24, hh8(__ctors_start)
	cpc	r16, r24
	brne	.L__do_global_ctors_loop
#else
	ldi	r17, hi8(__ctors_start)
	ldi	r28, lo8(__ctors_end)
	ldi	r29, hi8(__ctors_end)
	rjmp	.L__do_global_ctors_start
.L__do_global_ctors_loop:
	sbiw	r28, 2
	mov_h	r31, r29
	mov_l	r30, r28
	XCALL	__tablejump__
.L__do_global_ctors_start:
	cpi	r28, lo8(__ctors_start)
	cpc	r29, r17
	brne	.L__do_global_ctors_loop
#endif /* defined(__AVR_HAVE_RAMPZ__) */
ENDF __do_global_ctors
#endif /* L_ctors */

#ifdef L_dtors
	.section .fini6,"ax",@progbits
DEFUN __do_global_dtors
#if defined(__AVR_HAVE_RAMPZ__)
	ldi	r17, hi8(__dtors_end)
	ldi	r28, lo8(__dtors_start)
	ldi	r29, hi8(__dtors_start)
	ldi	r16, hh8(__dtors_start)
	rjmp	.L__do_global_dtors_start
.L__do_global_dtors_loop:
	sbiw	r28, 2
	sbc     r16, __zero_reg__
	mov_h	r31, r29
	mov_l	r30, r28
	out     __RAMPZ__, r16
	XCALL	__tablejump_elpm__
.L__do_global_dtors_start:
	cpi	r28, lo8(__dtors_end)
	cpc	r29, r17
	ldi	r24, hh8(__dtors_end)
	cpc	r16, r24
	brne	.L__do_global_dtors_loop
#else
	ldi	r17, hi8(__dtors_end)
	ldi	r28, lo8(__dtors_start)
	ldi	r29, hi8(__dtors_start)
	rjmp	.L__do_global_dtors_start
.L__do_global_dtors_loop:
	mov_h	r31, r29
	mov_l	r30, r28
	XCALL	__tablejump__
	adiw	r28, 2
.L__do_global_dtors_start:
	cpi	r28, lo8(__dtors_end)
	cpc	r29, r17
	brne	.L__do_global_dtors_loop
#endif /* defined(__AVR_HAVE_RAMPZ__) */
ENDF __do_global_dtors
#endif /* L_dtors */

.section .text.libgcc, "ax", @progbits
    
#ifdef L_tablejump_elpm
DEFUN __tablejump_elpm__
#if defined (__AVR_HAVE_ELPM__)
#if defined (__AVR_HAVE_LPMX__)
	elpm	__tmp_reg__, Z+
	elpm	r31, Z
	mov	r30, __tmp_reg__
#if defined (__AVR_HAVE_EIJMP_EICALL__)
	eijmp
#else
	ijmp
#endif

#else
	elpm
	adiw	r30, 1
	push	r0
	elpm
	push	r0
#if defined (__AVR_HAVE_EIJMP_EICALL__)
	in      __tmp_reg__, __EIND__
	push    __tmp_reg__
#endif
	ret
#endif
#endif /* defined (__AVR_HAVE_ELPM__) */
ENDF __tablejump_elpm__
#endif /* defined (L_tablejump_elpm) */

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

.macro  .xload dest, n
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
.endm ; .xload

#if defined (L_xload_1)
DEFUN __xload_1
#if defined (__AVR_HAVE_LPMX__) && !defined (__AVR_HAVE_RAMPZ__)
    ld      D0, Z
    sbrs    HHI8, 7
    lpm     D0, Z
    ret
#else
    sbrc    HHI8, 7
    rjmp    1f
#if defined (__AVR_HAVE_RAMPZ__)
    out     __RAMPZ__, HHI8
#endif /* __AVR_HAVE_RAMPZ__ */
    .xload  D0, 1
    ret
1:  ld      D0, Z
    ret
#endif /* LPMx && ! RAMPZ */
ENDF __xload_1
#endif /* L_xload_1 */

#if defined (L_xload_2)
DEFUN __xload_2
    sbrc    HHI8, 7
    rjmp    1f
#if defined (__AVR_HAVE_RAMPZ__)
    out     __RAMPZ__, HHI8
#endif /* __AVR_HAVE_RAMPZ__ */
    .xload  D0, 2
    .xload  D1, 2
    ret
1:  ld      D0, Z+
    ld      D1, Z+
    ret
ENDF __xload_2
#endif /* L_xload_2 */

#if defined (L_xload_3)
DEFUN __xload_3
    sbrc    HHI8, 7
    rjmp    1f
#if defined (__AVR_HAVE_RAMPZ__)
    out     __RAMPZ__, HHI8
#endif /* __AVR_HAVE_RAMPZ__ */
    .xload  D0, 3
    .xload  D1, 3
    .xload  D2, 3
    ret
1:  ld      D0, Z+
    ld      D1, Z+
    ld      D2, Z+
    ret
ENDF __xload_3
#endif /* L_xload_3 */

#if defined (L_xload_4)
DEFUN __xload_4
    sbrc    HHI8, 7
    rjmp    1f
#if defined (__AVR_HAVE_RAMPZ__)
    out     __RAMPZ__, HHI8
#endif /* __AVR_HAVE_RAMPZ__ */
    .xload  D0, 4
    .xload  D1, 4
    .xload  D2, 4
    .xload  D3, 4
    ret
1:  ld      D0, Z+
    ld      D1, Z+
    ld      D2, Z+
    ld      D3, Z+
    ret
ENDF __xload_4
#endif /* L_xload_4 */

#endif /* L_xload_{1|2|3|4} */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memcopy from Address Space __pgmx to RAM
;; R23:Z = Source Address
;; X     = Destination Address
;; Clobbers: __tmp_reg__, R23, R24, R25, X, Z

#if defined (L_movmemx)

#define HHI8  23
#define LOOP  24

DEFUN __movmemx_qi
    ;; #Bytes to copy fity in 8 Bits (1..255)
    ;; Zero-extend Loop Counter
    clr     LOOP+1
    ;; FALLTHRU
ENDF __movmemx_qi

DEFUN __movmemx_hi

;; Read from where?
    sbrc    HHI8, 7
    rjmp    1f

;; Read from Flash

#if defined (__AVR_HAVE_RAMPZ__)
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
    ret

;; Read from RAM

1:  ;; Read 1 Byte from RAM...
    ld      r0, Z+
    ;; and store that Byte to RAM Destination
    st      X+, r0
    sbiw    LOOP, 1
    brne    0b
    ret
ENDF __movmemx_hi

#undef HHI8
#undef LOOP

#endif /* L_movmemx */


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
    subi r26, -8
    or   r25, r24
    brne 1f
    ldi  r24, 16
    ret
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
    mov  __tmp_reg__, r24
    andi r24, 1
    lsr  __tmp_reg__    
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __zero_reg__
    lsr  __tmp_reg__    
    adc  r24, __tmp_reg__    
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
;; Arithmetic shift right
;; r25:r18 = ashr64 (r25:r18, r17:r16)
DEFUN __ashrdi3
    push r16
    andi r16, 63
    breq 2f
1:  asr  r25
    ror  r24
    ror  r23
    ror  r22
    ror  r21
    ror  r20
    ror  r19
    ror  r18
    dec  r16
    brne 1b
2:  pop  r16
    ret
ENDF __ashrdi3
#endif /* defined (L_ashrdi3) */

#if defined (L_lshrdi3)
;; Logic shift right
;; r25:r18 = lshr64 (r25:r18, r17:r16)
DEFUN __lshrdi3
    push r16
    andi r16, 63
    breq 2f
1:  lsr  r25
    ror  r24
    ror  r23
    ror  r22
    ror  r21
    ror  r20
    ror  r19
    ror  r18
    dec  r16
    brne 1b
2:  pop  r16
    ret
ENDF __lshrdi3
#endif /* defined (L_lshrdi3) */

#if defined (L_ashldi3)
;; Shift left
;; r25:r18 = ashl64 (r25:r18, r17:r16)
DEFUN __ashldi3
    push r16
    andi r16, 63
    breq 2f
1:  lsl  r18
    rol  r19
    rol  r20
    rol  r21
    rol  r22
    rol  r23
    rol  r24
    rol  r25
    dec  r16
    brne 1b
2:  pop  r16
    ret
ENDF __ashldi3
#endif /* defined (L_ashldi3) */

#if defined (L_rotldi3)
;; Shift left
;; r25:r18 = rotl64 (r25:r18, r17:r16)
DEFUN __rotldi3
    push r16
    andi r16, 63
    breq 2f
1:  lsl  r18
    rol  r19
    rol  r20
    rol  r21
    rol  r22
    rol  r23
    rol  r24
    rol  r25
    adc  r18, __zero_reg__
    dec  r16
    brne 1b
2:  pop  r16
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
    com  C1
    neg  C0
    sbci C1, -1
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
