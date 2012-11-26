/*  -*- Mode: Asm -*-  */
;;    Copyright (C) 2012
;;    Free Software Foundation, Inc.
;;    Contributed by Sean D'Epagnier  (sean@depagnier.com)
;;                   Georg-Johann Lay (avr@gjlay.de)

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; In addition to the permissions in the GNU General Public License, the
;; Free Software Foundation gives you unlimited permission to link the
;; compiled version of this file into combinations with other programs,
;; and to distribute those combinations without any restriction coming
;; from the use of this file.  (The General Public License restrictions
;; do apply in other respects; for example, they cover modification of
;; the file, and distribution when not linked into a combine
;; executable.)

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixed point library routines for AVR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.section .text.libgcc.fixed, "ax", @progbits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions to float
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#if defined (L_fractqqsf)
DEFUN __fractqqsf
    ;; Move in place for SA -> SF conversion
    clr     r22
    mov     r23, r24
    ;; Sign-extend
    lsl     r24
    sbc     r24, r24
    mov     r25, r24
    XJMP    __fractsasf
ENDF __fractqqsf
#endif  /* L_fractqqsf */

#if defined (L_fractuqqsf)
DEFUN __fractuqqsf
    ;; Move in place for USA -> SF conversion
    clr     r22
    mov     r23, r24
    ;; Zero-extend
    clr     r24
    clr     r25
    XJMP    __fractusasf
ENDF __fractuqqsf
#endif  /* L_fractuqqsf */

#if defined (L_fracthqsf)
DEFUN __fracthqsf
    ;; Move in place for SA -> SF conversion
    wmov    22, 24
    ;; Sign-extend
    lsl     r25
    sbc     r24, r24
    mov     r25, r24
    XJMP    __fractsasf
ENDF __fracthqsf
#endif  /* L_fracthqsf */

#if defined (L_fractuhqsf)
DEFUN __fractuhqsf
    ;; Move in place for USA -> SF conversion
    wmov    22, 24
    ;; Zero-extend
    clr     r24
    clr     r25
    XJMP    __fractusasf
ENDF __fractuhqsf
#endif  /* L_fractuhqsf */

#if defined (L_fracthasf)
DEFUN __fracthasf
    ;; Move in place for SA -> SF conversion
    clr     r22
    mov     r23, r24
    mov     r24, r25
    ;; Sign-extend
    lsl     r25
    sbc     r25, r25
    XJMP    __fractsasf
ENDF __fracthasf
#endif  /* L_fracthasf */

#if defined (L_fractuhasf)
DEFUN __fractuhasf
    ;; Move in place for USA -> SF conversion
    clr     r22
    mov     r23, r24
    mov     r24, r25
    ;; Zero-extend
    clr     r25
    XJMP    __fractusasf
ENDF __fractuhasf
#endif  /* L_fractuhasf */


#if defined (L_fractsqsf)
DEFUN __fractsqsf
    XCALL   __floatsisf
    ;; Divide non-zero results by 2^31 to move the
    ;; decimal point into place
    tst     r25
    breq    0f
    subi    r24, exp_lo (31)
    sbci    r25, exp_hi (31)
0:  ret
ENDF __fractsqsf
#endif  /* L_fractsqsf */

#if defined (L_fractusqsf)
DEFUN __fractusqsf
    XCALL   __floatunsisf
    ;; Divide non-zero results by 2^32 to move the
    ;; decimal point into place
    cpse    r25, __zero_reg__
    subi    r25, exp_hi (32)
    ret
ENDF __fractusqsf
#endif  /* L_fractusqsf */

#if defined (L_fractsasf)
DEFUN __fractsasf
    XCALL   __floatsisf
    ;; Divide non-zero results by 2^15 to move the
    ;; decimal point into place
    tst     r25
    breq    0f
    subi    r24, exp_lo (15)
    sbci    r25, exp_hi (15)
0:  ret
ENDF __fractsasf
#endif  /* L_fractsasf */

#if defined (L_fractusasf)
DEFUN __fractusasf
    XCALL   __floatunsisf
    ;; Divide non-zero results by 2^16 to move the
    ;; decimal point into place
    cpse    r25, __zero_reg__
    subi    r25, exp_hi (16)
    ret
ENDF __fractusasf
#endif  /* L_fractusasf */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions from float
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
#if defined (L_fractsfqq)
DEFUN __fractsfqq
    ;; Multiply with 2^{24+7} to get a QQ result in r25
    subi    r24, exp_lo (-31)
    sbci    r25, exp_hi (-31)
    XCALL   __fixsfsi
    mov     r24, r25
    ret
ENDF __fractsfqq
#endif  /* L_fractsfqq */

#if defined (L_fractsfuqq)
DEFUN __fractsfuqq
    ;; Multiply with 2^{24+8} to get a UQQ result in r25
    subi    r25, exp_hi (-32)
    XCALL   __fixunssfsi
    mov     r24, r25
    ret
ENDF __fractsfuqq
#endif  /* L_fractsfuqq */

#if defined (L_fractsfha)
DEFUN __fractsfha
    ;; Multiply with 2^{16+7} to get a HA result in r25:r24
    subi    r24, exp_lo (-23)
    sbci    r25, exp_hi (-23)
    XJMP    __fixsfsi
ENDF __fractsfha
#endif  /* L_fractsfha */

#if defined (L_fractsfuha)
DEFUN __fractsfuha
    ;; Multiply with 2^24 to get a UHA result in r25:r24
    subi    r25, exp_hi (-24)
    XJMP    __fixunssfsi
ENDF __fractsfuha
#endif  /* L_fractsfuha */

#if defined (L_fractsfhq)
FALIAS __fractsfsq

DEFUN __fractsfhq
    ;; Multiply with 2^{16+15} to get a HQ result in r25:r24
    ;; resp. with 2^31 to get a SQ result in r25:r22
    subi    r24, exp_lo (-31)
    sbci    r25, exp_hi (-31)
    XJMP    __fixsfsi
ENDF __fractsfhq
#endif  /* L_fractsfhq */

#if defined (L_fractsfuhq)
FALIAS __fractsfusq

DEFUN __fractsfuhq
    ;; Multiply with 2^{16+16} to get a UHQ result in r25:r24
    ;; resp. with 2^32 to get a USQ result in r25:r22
    subi    r25, exp_hi (-32)
    XJMP    __fixunssfsi
ENDF __fractsfuhq
#endif  /* L_fractsfuhq */

#if defined (L_fractsfsa)
DEFUN __fractsfsa
    ;; Multiply with 2^15 to get a SA result in r25:r22
    subi    r24, exp_lo (-15)
    sbci    r25, exp_hi (-15)
    XJMP    __fixsfsi
ENDF __fractsfsa
#endif  /* L_fractsfsa */

#if defined (L_fractsfusa)
DEFUN __fractsfusa
    ;; Multiply with 2^16 to get a USA result in r25:r22
    subi    r25, exp_hi (-16)
    XJMP    __fixunssfsi
ENDF __fractsfusa
#endif  /* L_fractsfusa */


;; For multiplication the functions here are called directly from
;; avr-fixed.md instead of using the standard libcall mechanisms.
;; This can make better code because GCC knows exactly which
;; of the call-used registers (not all of them) are clobbered.  */

/*******************************************************
    Fractional  Multiplication  8 x 8  without MUL
*******************************************************/

#if defined (L_mulqq3) && !defined (__AVR_HAVE_MUL__)
;;; R23 = R24 * R25
;;; Clobbers: __tmp_reg__, R22, R24, R25
;;; Rounding: ???
DEFUN __mulqq3
    XCALL   __fmuls
    ;; TR 18037 requires that  (-1) * (-1)  does not overflow
    ;; The only input that can produce  -1  is  (-1)^2.
    dec     r23
    brvs    0f
    inc     r23
0:  ret
ENDF  __mulqq3
#endif /* L_mulqq3 && ! HAVE_MUL */

/*******************************************************
    Fractional Multiply  .16 x .16  with and without MUL
*******************************************************/

#if defined (L_mulhq3)
;;; Same code with and without MUL, but the interfaces differ:
;;; no MUL: (R25:R24) = (R22:R23) * (R24:R25)
;;;         Clobbers: ABI, called by optabs
;;; MUL:    (R25:R24) = (R19:R18) * (R27:R26)
;;;         Clobbers: __tmp_reg__, R22, R23
;;; Rounding:  -0.5 LSB  <= error  <=  0.5 LSB
DEFUN   __mulhq3
    XCALL   __mulhisi3
    ;; Shift result into place
    lsl     r23
    rol     r24
    rol     r25
    brvs    1f
    ;; Round
    sbrc    r23, 7
    adiw    r24, 1
    ret
1:  ;; Overflow.  TR 18037 requires  (-1)^2  not to overflow
    ldi     r24, lo8 (0x7fff)
    ldi     r25, hi8 (0x7fff)
    ret
ENDF __mulhq3
#endif  /* defined (L_mulhq3) */

#if defined (L_muluhq3)
;;; Same code with and without MUL, but the interfaces differ:
;;; no MUL: (R25:R24) *= (R23:R22)
;;;         Clobbers: ABI, called by optabs
;;; MUL:    (R25:R24) = (R19:R18) * (R27:R26)
;;;         Clobbers: __tmp_reg__, R22, R23
;;; Rounding:  -0.5 LSB  <  error  <=  0.5 LSB
DEFUN   __muluhq3
    XCALL   __umulhisi3
    ;; Round
    sbrc    r23, 7
    adiw    r24, 1
    ret
ENDF __muluhq3
#endif  /* L_muluhq3 */


/*******************************************************
    Fixed  Multiply  8.8 x 8.8  with and without MUL
*******************************************************/

#if defined (L_mulha3)
;;; Same code with and without MUL, but the interfaces differ:
;;; no MUL: (R25:R24) = (R22:R23) * (R24:R25)
;;;         Clobbers: ABI, called by optabs
;;; MUL:    (R25:R24) = (R19:R18) * (R27:R26)
;;;         Clobbers: __tmp_reg__, R22, R23
;;; Rounding:  -0.5 LSB  <=  error  <=  0.5 LSB
DEFUN   __mulha3
    XCALL   __mulhisi3
    lsl     r22
    rol     r23
    rol     r24
    XJMP    __muluha3_round
ENDF __mulha3
#endif  /* L_mulha3 */

#if defined (L_muluha3)
;;; Same code with and without MUL, but the interfaces differ:
;;; no MUL: (R25:R24) *= (R23:R22)
;;;         Clobbers: ABI, called by optabs
;;; MUL:    (R25:R24) = (R19:R18) * (R27:R26)
;;;         Clobbers: __tmp_reg__, R22, R23
;;; Rounding:  -0.5 LSB  <  error  <=  0.5 LSB
DEFUN   __muluha3
    XCALL   __umulhisi3
    XJMP    __muluha3_round
ENDF __muluha3
#endif  /* L_muluha3 */

#if defined (L_muluha3_round)
DEFUN   __muluha3_round
    ;; Shift result into place
    mov     r25, r24
    mov     r24, r23
    ;; Round
    sbrc    r22, 7
    adiw    r24, 1
    ret
ENDF __muluha3_round
#endif  /* L_muluha3_round */


/*******************************************************
    Fixed  Multiplication  16.16 x 16.16
*******************************************************/

;; Bits outside the result (below LSB), used in the signed version
#define GUARD __tmp_reg__

#if defined (__AVR_HAVE_MUL__)

;; Multiplier
#define A0  16
#define A1  A0+1
#define A2  A1+1
#define A3  A2+1

;; Multiplicand
#define B0  20
#define B1  B0+1
#define B2  B1+1
#define B3  B2+1

;; Result
#define C0  24
#define C1  C0+1
#define C2  C1+1
#define C3  C2+1

#if defined (L_mulusa3)
;;; (C3:C0) = (A3:A0) * (B3:B0)
DEFUN __mulusa3
    set
    ;; Fallthru
ENDF  __mulusa3

;;; Round for last digit iff T = 1
;;; Return guard bits in GUARD (__tmp_reg__).
;;; Rounding, T = 0:  -1.0 LSB  <  error  <=  0   LSB
;;; Rounding, T = 1:  -0.5 LSB  <  error  <=  0.5 LSB
DEFUN __mulusa3_round
    ;; Some of the MUL instructions have LSBs outside the result.
    ;; Don't ignore these LSBs in order to tame rounding error.
    ;; Use C2/C3 for these LSBs.

    clr C0
    clr C1
    mul A0, B0  $  movw C2, r0

    mul A1, B0  $  add  C3, r0  $  adc C0, r1
    mul A0, B1  $  add  C3, r0  $  adc C0, r1  $  rol C1
    
    ;; Round if T = 1.  Store guarding bits outside the result for rounding
    ;; and left-shift by the signed version (function below).
    brtc 0f
    sbrc C3, 7
    adiw C0, 1
0:  push C3
    
    ;; The following MULs don't have LSBs outside the result.
    ;; C2/C3 is the high part.

    mul  A0, B2  $  add C0, r0  $  adc C1, r1  $  sbc  C2, C2
    mul  A1, B1  $  add C0, r0  $  adc C1, r1  $  sbci C2, 0
    mul  A2, B0  $  add C0, r0  $  adc C1, r1  $  sbci C2, 0
    neg  C2

    mul  A0, B3  $  add C1, r0  $  adc C2, r1  $  sbc  C3, C3
    mul  A1, B2  $  add C1, r0  $  adc C2, r1  $  sbci C3, 0
    mul  A2, B1  $  add C1, r0  $  adc C2, r1  $  sbci C3, 0
    mul  A3, B0  $  add C1, r0  $  adc C2, r1  $  sbci C3, 0
    neg  C3
    
    mul  A1, B3  $  add C2, r0  $  adc C3, r1
    mul  A2, B2  $  add C2, r0  $  adc C3, r1
    mul  A3, B1  $  add C2, r0  $  adc C3, r1
    
    mul  A2, B3  $  add C3, r0
    mul  A3, B2  $  add C3, r0

    ;; Guard bits used in the signed version below.
    pop  GUARD
    clr  __zero_reg__
    ret
ENDF __mulusa3_round
#endif /* L_mulusa3 */

#if defined (L_mulsa3)
;;; (C3:C0) = (A3:A0) * (B3:B0)
;;; Clobbers: __tmp_reg__, T
;;; Rounding:  -0.5 LSB  <=  error  <=  0.5 LSB
DEFUN __mulsa3
    clt
    XCALL   __mulusa3_round
    ;; A posteriori sign extension of the operands
    tst     B3
    brpl 1f
    sub     C2, A0
    sbc     C3, A1
1:  sbrs    A3, 7
    rjmp 2f
    sub     C2, B0
    sbc     C3, B1
2:
    ;;  Shift 1 bit left to adjust for 15 fractional bits
    lsl     GUARD
    rol     C0
    rol     C1
    rol     C2
    rol     C3
    ;; Round last digit
    lsl     GUARD
    adc     C0, __zero_reg__
    adc     C1, __zero_reg__
    adc     C2, __zero_reg__
    adc     C3, __zero_reg__
    ret
ENDF __mulsa3
#endif /* L_mulsa3 */

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

#else /* __AVR_HAVE_MUL__ */

#define A0 18
#define A1 A0+1
#define A2 A0+2
#define A3 A0+3

#define B0 22
#define B1 B0+1
#define B2 B0+2
#define B3 B0+3

#define C0  22
#define C1  C0+1
#define C2  C0+2
#define C3  C0+3

;; __tmp_reg__
#define CC0  0
;; __zero_reg__
#define CC1  1
#define CC2  16
#define CC3  17

#define AA0  26
#define AA1  AA0+1
#define AA2  30
#define AA3  AA2+1

#if defined (L_mulsa3)
;;; (R25:R22)  *=  (R21:R18)
;;; Clobbers: ABI, called by optabs
;;; Rounding:  -1 LSB  <=  error  <=  1 LSB
DEFUN   __mulsa3
    push    B0
    push    B1
    push    B3
    clt
    XCALL   __mulusa3_round
    pop     r30
    ;; sign-extend B
    bst     r30, 7
    brtc 1f
    ;; A1, A0 survived in  R27:R26
    sub     C2, AA0
    sbc     C3, AA1
1:
    pop     AA1  ;; B1
    pop     AA0  ;; B0

    ;; sign-extend A.  A3 survived in  R31
    bst     AA3, 7
    brtc 2f
    sub     C2, AA0
    sbc     C3, AA1
2:
    ;;  Shift 1 bit left to adjust for 15 fractional bits
    lsl     GUARD
    rol     C0
    rol     C1
    rol     C2
    rol     C3
    ;; Round last digit
    lsl     GUARD
    adc     C0, __zero_reg__
    adc     C1, __zero_reg__
    adc     C2, __zero_reg__
    adc     C3, __zero_reg__
    ret
ENDF __mulsa3
#endif  /* L_mulsa3 */

#if defined (L_mulusa3)
;;; (R25:R22)  *=  (R21:R18)
;;; Clobbers: ABI, called by optabs
;;; Rounding:  -1 LSB  <=  error  <=  1 LSB
DEFUN __mulusa3
    set
    ;; Fallthru
ENDF  __mulusa3

;;; A[] survives in 26, 27, 30, 31
;;; Also used by __mulsa3 with T = 0
;;; Round if T = 1
;;; Return Guard bits in GUARD (__tmp_reg__), used by signed version.
DEFUN __mulusa3_round
    push    CC2
    push    CC3
    ; clear result
    clr     __tmp_reg__
    wmov    CC2, CC0
    ; save multiplicand
    wmov    AA0, A0
    wmov    AA2, A2
    rjmp 3f

    ;; Loop the integral part

1:  ;; CC += A * 2^n;  n >= 0
    add  CC0,A0  $  adc CC1,A1  $  adc  CC2,A2  $  adc  CC3,A3

2:  ;; A <<= 1
    lsl  A0      $  rol A1      $  rol  A2      $  rol  A3

3:  ;; IBIT(B) >>= 1
    ;; Carry = n-th bit of B;  n >= 0
    lsr     B3
    ror     B2
    brcs 1b
    sbci    B3, 0
    brne 2b

    ;; Loop the fractional part
    ;; B2/B3 is 0 now, use as guard bits for rounding
    ;; Restore multiplicand
    wmov    A0, AA0
    wmov    A2, AA2
    rjmp 5f

4:  ;; CC += A:Guard * 2^n;  n < 0
    add  B3,B2 $  adc  CC0,A0  $  adc  CC1,A1  $  adc  CC2,A2  $  adc  CC3,A3
5:
    ;; A:Guard >>= 1
    lsr  A3   $  ror  A2  $  ror  A1  $  ror   A0  $   ror  B2

    ;; FBIT(B) <<= 1
    ;; Carry = n-th bit of B;  n < 0
    lsl     B0
    rol     B1
    brcs 4b
    sbci    B0, 0
    brne 5b

    ;; Save guard bits and set carry for rounding
    push    B3
    lsl     B3
    ;; Move result into place
    wmov    C2, CC2
    wmov    C0, CC0
    clr     __zero_reg__
    brtc 6f
    ;; Round iff T = 1
    adc     C0, __zero_reg__
    adc     C1, __zero_reg__
    adc     C2, __zero_reg__
    adc     C3, __zero_reg__
6:  
    pop     GUARD
    ;; Epilogue
    pop     CC3
    pop     CC2
    ret
ENDF __mulusa3_round
#endif  /* L_mulusa3 */

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
#undef AA0
#undef AA1
#undef AA2
#undef AA3
#undef CC0
#undef CC1
#undef CC2
#undef CC3

#endif /* __AVR_HAVE_MUL__ */

#undef GUARD

/*******************************************************
      Fractional Division 8 / 8
*******************************************************/

#define r_divd  r25     /* dividend */
#define r_quo   r24     /* quotient */
#define r_div   r22     /* divisor */
#define r_sign  __tmp_reg__

#if defined (L_divqq3)
DEFUN   __divqq3
    mov     r_sign, r_divd
    eor     r_sign, r_div
    sbrc    r_div, 7
    neg     r_div
    sbrc    r_divd, 7
    neg     r_divd
    XCALL   __divqq_helper
    lsr     r_quo
    sbrc    r_sign, 7   ; negate result if needed
    neg     r_quo
    ret
ENDF __divqq3
#endif  /* L_divqq3 */

#if defined (L_udivuqq3)
DEFUN   __udivuqq3
    cp      r_divd, r_div
    brsh    0f
    XJMP __divqq_helper
    ;; Result is out of [0, 1)  ==>  Return 1 - eps.
0:  ldi     r_quo, 0xff
    ret
ENDF __udivuqq3
#endif  /* L_udivuqq3 */


#if defined (L_divqq_helper)
DEFUN   __divqq_helper
    clr     r_quo           ; clear quotient
    inc     __zero_reg__    ; init loop counter, used per shift
__udivuqq3_loop:
    lsl     r_divd          ; shift dividend
    brcs    0f              ; dividend overflow
    cp      r_divd,r_div    ; compare dividend & divisor
    brcc    0f              ; dividend >= divisor
    rol     r_quo           ; shift quotient (with CARRY)
    rjmp    __udivuqq3_cont
0:
    sub     r_divd,r_div    ; restore dividend
    lsl     r_quo           ; shift quotient (without CARRY)
__udivuqq3_cont:
    lsl     __zero_reg__    ; shift loop-counter bit
    brne    __udivuqq3_loop
    com     r_quo           ; complement result
                            ; because C flag was complemented in loop
    ret
ENDF __divqq_helper
#endif  /* L_divqq_helper */

#undef  r_divd
#undef  r_quo
#undef  r_div
#undef  r_sign


/*******************************************************
    Fractional Division 16 / 16
*******************************************************/
#define r_divdL 26     /* dividend Low */
#define r_divdH 27     /* dividend Hig */
#define r_quoL  24     /* quotient Low */
#define r_quoH  25     /* quotient High */
#define r_divL  22     /* divisor */
#define r_divH  23     /* divisor */
#define r_cnt   21

#if defined (L_divhq3)
DEFUN   __divhq3
    mov     r0, r_divdH
    eor     r0, r_divH
    sbrs    r_divH, 7
    rjmp    1f
    NEG2    r_divL
1:
    sbrs    r_divdH, 7
    rjmp    2f
    NEG2    r_divdL
2:
    cp      r_divdL, r_divL
    cpc     r_divdH, r_divH
    breq    __divhq3_minus1  ; if equal return -1
    XCALL   __udivuhq3
    lsr     r_quoH
    ror     r_quoL
    brpl    9f
    ;; negate result if needed
    NEG2    r_quoL
9:
    ret
__divhq3_minus1:
    ldi     r_quoH, 0x80
    clr     r_quoL
    ret
ENDF __divhq3
#endif  /* defined (L_divhq3) */

#if defined (L_udivuhq3)
DEFUN   __udivuhq3
    sub     r_quoH,r_quoH   ; clear quotient and carry
    ;; FALLTHRU
ENDF __udivuhq3

DEFUN   __udivuha3_common
    clr     r_quoL          ; clear quotient
    ldi     r_cnt,16        ; init loop counter
__udivuhq3_loop:
    rol     r_divdL         ; shift dividend (with CARRY)
    rol     r_divdH
    brcs    __udivuhq3_ep   ; dividend overflow
    cp      r_divdL,r_divL  ; compare dividend & divisor
    cpc     r_divdH,r_divH
    brcc    __udivuhq3_ep   ; dividend >= divisor
    rol     r_quoL          ; shift quotient (with CARRY)
    rjmp    __udivuhq3_cont
__udivuhq3_ep:
    sub     r_divdL,r_divL  ; restore dividend
    sbc     r_divdH,r_divH
    lsl     r_quoL          ; shift quotient (without CARRY)
__udivuhq3_cont:
    rol     r_quoH          ; shift quotient
    dec     r_cnt           ; decrement loop counter
    brne    __udivuhq3_loop
    com     r_quoL          ; complement result
    com     r_quoH          ; because C flag was complemented in loop
    ret
ENDF __udivuha3_common
#endif  /* defined (L_udivuhq3) */

/*******************************************************
    Fixed Division 8.8 / 8.8
*******************************************************/
#if defined (L_divha3)
DEFUN   __divha3
    mov     r0, r_divdH
    eor     r0, r_divH
    sbrs    r_divH, 7
    rjmp    1f
    NEG2    r_divL
1:
    sbrs    r_divdH, 7
    rjmp    2f
    NEG2    r_divdL
2:
    XCALL   __udivuha3
    lsr     r_quoH  ; adjust to 7 fractional bits
    ror     r_quoL
    sbrs    r0, 7   ; negate result if needed
    ret
    NEG2    r_quoL
    ret
ENDF __divha3
#endif  /* defined (L_divha3) */

#if defined (L_udivuha3)
DEFUN   __udivuha3
    mov     r_quoH, r_divdL
    mov     r_divdL, r_divdH
    clr     r_divdH
    lsl     r_quoH     ; shift quotient into carry
    XJMP    __udivuha3_common ; same as fractional after rearrange
ENDF __udivuha3
#endif  /* defined (L_udivuha3) */

#undef  r_divdL
#undef  r_divdH
#undef  r_quoL
#undef  r_quoH
#undef  r_divL
#undef  r_divH
#undef  r_cnt

/*******************************************************
    Fixed Division 16.16 / 16.16
*******************************************************/

#define r_arg1L  24    /* arg1 gets passed already in place */
#define r_arg1H  25
#define r_arg1HL 26
#define r_arg1HH 27
#define r_divdL  26    /* dividend Low */
#define r_divdH  27
#define r_divdHL 30
#define r_divdHH 31    /* dividend High */
#define r_quoL   22    /* quotient Low */
#define r_quoH   23
#define r_quoHL  24
#define r_quoHH  25    /* quotient High */
#define r_divL   18    /* divisor Low */
#define r_divH   19
#define r_divHL  20
#define r_divHH  21    /* divisor High */
#define r_cnt  __zero_reg__  /* loop count (0 after the loop!) */

#if defined (L_divsa3)
DEFUN   __divsa3
    mov     r0, r_arg1HH
    eor     r0, r_divHH
    sbrs    r_divHH, 7
    rjmp    1f
    NEG4    r_divL
1:
    sbrs    r_arg1HH, 7
    rjmp    2f
    NEG4    r_arg1L
2:
    XCALL   __udivusa3
    lsr     r_quoHH ; adjust to 15 fractional bits
    ror     r_quoHL
    ror     r_quoH
    ror     r_quoL
    sbrs    r0, 7   ; negate result if needed
    ret
    ;; negate r_quoL
    XJMP    __negsi2
ENDF __divsa3
#endif  /* defined (L_divsa3) */

#if defined (L_udivusa3)
DEFUN   __udivusa3
    ldi     r_divdHL, 32    ; init loop counter
    mov     r_cnt, r_divdHL
    clr     r_divdHL
    clr     r_divdHH
    wmov    r_quoL, r_divdHL
    lsl     r_quoHL         ; shift quotient into carry
    rol     r_quoHH
__udivusa3_loop:
    rol     r_divdL         ; shift dividend (with CARRY)
    rol     r_divdH
    rol     r_divdHL
    rol     r_divdHH
    brcs    __udivusa3_ep   ; dividend overflow
    cp      r_divdL,r_divL  ; compare dividend & divisor
    cpc     r_divdH,r_divH
    cpc     r_divdHL,r_divHL
    cpc     r_divdHH,r_divHH
    brcc    __udivusa3_ep   ; dividend >= divisor
    rol     r_quoL          ; shift quotient (with CARRY)
    rjmp    __udivusa3_cont
__udivusa3_ep:
    sub     r_divdL,r_divL  ; restore dividend
    sbc     r_divdH,r_divH
    sbc     r_divdHL,r_divHL
    sbc     r_divdHH,r_divHH
    lsl     r_quoL          ; shift quotient (without CARRY)
__udivusa3_cont:
    rol     r_quoH          ; shift quotient
    rol     r_quoHL
    rol     r_quoHH
    dec     r_cnt           ; decrement loop counter
    brne    __udivusa3_loop
    com     r_quoL          ; complement result
    com     r_quoH          ; because C flag was complemented in loop
    com     r_quoHL
    com     r_quoHH
    ret
ENDF __udivusa3
#endif  /* defined (L_udivusa3) */

#undef  r_arg1L
#undef  r_arg1H
#undef  r_arg1HL
#undef  r_arg1HH
#undef  r_divdL
#undef  r_divdH
#undef  r_divdHL
#undef  r_divdHH
#undef  r_quoL
#undef  r_quoH
#undef  r_quoHL
#undef  r_quoHH
#undef  r_divL
#undef  r_divH
#undef  r_divHL
#undef  r_divHH
#undef  r_cnt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturation, 2 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First Argument and Return Register
#define A0  24
#define A1  A0+1

#if defined (L_ssneg_2)
DEFUN __ssneg_2
    NEG2    A0
    brvc 0f
    sbiw    A0, 1
0:  ret
ENDF __ssneg_2
#endif /* L_ssneg_2 */

#if defined (L_ssabs_2)
DEFUN __ssabs_2
    sbrs    A1, 7
    ret
    XJMP    __ssneg_2
ENDF __ssabs_2
#endif /* L_ssabs_2 */

#undef A0
#undef A1



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturation, 4 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First Argument and Return Register
#define A0  22
#define A1  A0+1
#define A2  A0+2
#define A3  A0+3

#if defined (L_ssneg_4)
DEFUN __ssneg_4
    XCALL   __negsi2
    brvc 0f
    ldi     A3, 0x7f
    ldi     A2, 0xff
    ldi     A1, 0xff
    ldi     A0, 0xff
0:  ret
ENDF __ssneg_4
#endif /* L_ssneg_4 */

#if defined (L_ssabs_4)
DEFUN __ssabs_4
    sbrs    A3, 7
    ret
    XJMP    __ssneg_4
ENDF __ssabs_4
#endif /* L_ssabs_4 */

#undef A0
#undef A1
#undef A2
#undef A3



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturation, 8 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First Argument and Return Register
#define A0  18
#define A1  A0+1
#define A2  A0+2
#define A3  A0+3
#define A4  A0+4
#define A5  A0+5
#define A6  A0+6
#define A7  A0+7

#if defined (L_clr_8)
FALIAS __usneguta2
FALIAS __usneguda2
FALIAS __usnegudq2

;; Clear Carry and all Bytes
DEFUN __clr_8
    ;; Clear Carry and set Z
    sub     A7, A7
    ;; FALLTHRU
ENDF  __clr_8
;; Propagate Carry to all Bytes, Carry unaltered
DEFUN __sbc_8
    sbc     A7, A7
    sbc     A6, A6
    wmov    A4, A6
    wmov    A2, A6
    wmov    A0, A6
    ret
ENDF __sbc_8
#endif /* L_clr_8 */

#if defined (L_ssneg_8)
FALIAS __ssnegta2
FALIAS __ssnegda2
FALIAS __ssnegdq2

DEFUN __ssneg_8
    XCALL   __negdi2
    brvc 0f
    ;; A[] = 0x7fffffff
    sec
    XCALL   __sbc_8
    ldi     A7, 0x7f
0:  ret
ENDF __ssneg_8
#endif /* L_ssneg_8 */

#if defined (L_ssabs_8)
FALIAS __ssabsta2
FALIAS __ssabsda2
FALIAS __ssabsdq2

DEFUN __ssabs_8
    sbrs    A7, 7
    ret
    XJMP    __ssneg_8
ENDF __ssabs_8
#endif /* L_ssabs_8 */

;; Second Argument
#define B0  10
#define B1  B0+1
#define B2  B0+2
#define B3  B0+3
#define B4  B0+4
#define B5  B0+5
#define B6  B0+6
#define B7  B0+7

#if defined (L_usadd_8)
FALIAS __usadduta3
FALIAS __usadduda3
FALIAS __usaddudq3

DEFUN __usadd_8
    XCALL   __adddi3
    brcs 0f
    ret
0:  ;; A[] = 0xffffffff
    XJMP    __sbc_8
ENDF __usadd_8
#endif /* L_usadd_8 */

#if defined (L_ussub_8)
FALIAS __ussubuta3
FALIAS __ussubuda3
FALIAS __ussubudq3

DEFUN __ussub_8
    XCALL   __subdi3
    brcs 0f
    ret
0:  ;; A[] = 0
    XJMP    __clr_8
ENDF __ussub_8
#endif /* L_ussub_8 */

#if defined (L_ssadd_8)
FALIAS __ssaddta3
FALIAS __ssaddda3
FALIAS __ssadddq3

DEFUN __ssadd_8
    XCALL   __adddi3
    brvc 0f
    ;; A = (B >= 0) ? INT64_MAX : INT64_MIN
    cpi     B7, 0x80
    XCALL   __sbc_8
    subi    A7, 0x80
0:  ret
ENDF __ssadd_8
#endif /* L_ssadd_8 */

#if defined (L_sssub_8)
FALIAS __sssubta3
FALIAS __sssubda3
FALIAS __sssubdq3

DEFUN __sssub_8
    XCALL   __subdi3
    brvc 0f
    ;; A = (B < 0) ? INT64_MAX : INT64_MIN
    ldi     A7, 0x7f
    cp      A7, B7
    XCALL   __sbc_8
    subi    A7, 0x80
0:  ret
ENDF __sssub_8
#endif /* L_sssub_8 */

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
