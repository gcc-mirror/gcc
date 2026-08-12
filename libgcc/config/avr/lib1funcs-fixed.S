/*  -*- Mode: Asm -*-  */
;;    Copyright (C) 2012-2026 Free Software Foundation, Inc.
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

#ifndef __AVR_TINY__

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

#endif /* ! AVR_TINY */

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
#ifndef __AVR_TINY__
    sbrc    r23, 7
    adiw    r24, 1
#else
    lsl     r23
    adc     r24, __zero_reg__
    adc     r25, __zero_reg__
#endif /* AVR_TINY? */
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
#ifndef __AVR_TINY__
    sbrc    r23, 7
    adiw    r24, 1
#else
    lsl     r23
    adc     r24, __zero_reg__
    adc     r25, __zero_reg__
#endif /* AVR_TINY? */
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
#ifndef __AVR_TINY__
    sbrc    r22, 7
    adiw    r24, 1
#else
    lsl     r22
    adc     r24, __zero_reg__
    adc     r25, __zero_reg__
#endif /* AVR_TINY? */
    ret
ENDF __muluha3_round
#endif  /* L_muluha3_round */


/*******************************************************
    Fixed  Multiplication  16.16 x 16.16
*******************************************************/

#ifndef __AVR_TINY__

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

#endif /* ! AVR_TINY */

/***********************************************************
    Fixed  unsigned saturated Multiplication  8.8 x 8.8
***********************************************************/

#define C0  22
#define C1  C0+1
#define C2  C0+2
#define C3  C0+3
#define SS __tmp_reg__

#if defined (L_usmuluha3)
DEFUN __usmuluha3
    ;; Widening multiply
#ifdef __AVR_HAVE_MUL__
    ;; Adjust interface
    movw    R26, R22
    movw    R18, R24
#endif /* HAVE MUL */
    XCALL   __umulhisi3
    tst     C3
    brne .Lmax
    ;; Round, target is in C1..C2
    lsl     C0
    adc     C1, __zero_reg__
    adc     C2, __zero_reg__
    brcs .Lmax
    ;; Move result into place
    mov     C3, C2
    mov     C2, C1
    ret
.Lmax:
    ;; Saturate
    ldi     C2, 0xff
    ldi     C3, 0xff
    ret
ENDF  __usmuluha3
#endif /* L_usmuluha3 */

/***********************************************************
    Fixed signed saturated Multiplication  s8.7 x s8.7
***********************************************************/

#if defined (L_ssmulha3)
DEFUN __ssmulha3
    ;; Widening multiply
#ifdef __AVR_HAVE_MUL__
    ;; Adjust interface
    movw    R26, R22
    movw    R18, R24
#endif /* HAVE MUL */
    XCALL   __mulhisi3
    ;; Adjust decimal point
    lsl     C0
    rol     C1
    rol     C2
    brvs .LsatC3.3
    ;; The 9 MSBs must be the same
    rol     C3
    sbc     SS, SS
    cp      C3, SS
    brne .LsatSS
    ;; Round
    lsl     C0
    adc     C1, __zero_reg__
    adc     C2, __zero_reg__
    brvs .Lmax
    ;; Move result into place
    mov    C3, C2
    mov    C2, C1
    ret
.Lmax:
    ;; Load 0x7fff
    clr     C3
.LsatC3.3:
    ;; C3 <  0 -->  0x8000
    ;; C3 >= 0 -->  0x7fff
    mov     SS, C3
.LsatSS:
    ;; Load min / max value:
    ;; SS = -1  -->  0x8000
    ;; SS =  0  -->  0x7fff
    ldi     C3, 0x7f
    ldi     C2, 0xff
#ifndef __AVR_TINY__
    sbrc    SS, 7
    adiw    C2, 1
#else
    lsl     SS
    adc     C2, __zero_reg__
    adc     C3, __zero_reg__
#endif /* AVR_TINY? */
    ret
ENDF  __ssmulha3
#endif /* L_ssmulha3 */

#undef C0
#undef C1
#undef C2
#undef C3
#undef SS

/***********************************************************
    Fixed  unsigned saturated Multiplication  16.16 x 16.16
***********************************************************/

#ifndef __AVR_TINY__

#define C0  18
#define C1  C0+1
#define C2  C0+2
#define C3  C0+3
#define C4  C0+4
#define C5  C0+5
#define C6  C0+6
#define C7  C0+7
#define SS __tmp_reg__

#if defined (L_usmulusa3)
;; R22[4] = R22[4] *{ssat} R18[4]
;; Ordinary ABI function
DEFUN __usmulusa3
    ;; Widening multiply
    XCALL   __umulsidi3
    or      C7, C6
    brne .Lmax
    ;; Round, target is in C2..C5
    lsl     C1
    adc     C2, __zero_reg__
    adc     C3, __zero_reg__
    adc     C4, __zero_reg__
    adc     C5, __zero_reg__
    brcs .Lmax
    ;; Move result into place
    wmov    C6, C4
    wmov    C4, C2
    ret
.Lmax:
    ;; Saturate
    ldi     C7, 0xff
    ldi     C6, 0xff
    wmov    C4, C6
    ret
ENDF  __usmulusa3
#endif /* L_usmulusa3 */

/***********************************************************
    Fixed signed saturated Multiplication  s16.15 x s16.15
***********************************************************/

#if defined (L_ssmulsa3)
;; R22[4] = R22[4] *{ssat} R18[4]
;; Ordinary ABI function
DEFUN __ssmulsa3
    ;; Widening multiply
    XCALL   __mulsidi3
    ;; Adjust decimal point
    lsl     C1
    rol     C2
    rol     C3
    rol     C4
    rol     C5
    brvs .LsatC7.7
    ;; The 17 MSBs must be the same
    rol     C6
    rol     C7
    sbc     SS, SS
    cp      C6, SS
    cpc     C7, SS
    brne .LsatSS
    ;; Round
    lsl     C1
    adc     C2, __zero_reg__
    adc     C3, __zero_reg__
    adc     C4, __zero_reg__
    adc     C5, __zero_reg__
    brvs .Lmax
    ;; Move result into place
    wmov    C6, C4
    wmov    C4, C2
    ret

.Lmax:
    ;; Load 0x7fffffff
    clr     C7
.LsatC7.7:
    ;; C7 <  0 -->  0x80000000
    ;; C7 >= 0 -->  0x7fffffff
    lsl     C7
    sbc     SS, SS
.LsatSS:
    ;; Load min / max value:
    ;; SS = -1  -->  0x80000000
    ;; SS =  0  -->  0x7fffffff
    com     SS
    mov     C4, SS
    mov     C5, C4
    wmov    C6, C4
    subi    C7, 0x80
    ret
ENDF  __ssmulsa3
#endif /* L_ssmulsa3 */

#undef C0
#undef C1
#undef C2
#undef C3
#undef C4
#undef C5
#undef C6
#undef C7
#undef SS

/*******************************************************
    Fixed 64 x 64 saturated Multiplication
*******************************************************/

.pushsection .text.libgcc.mul64, "ax", @progbits

#define A0  18
#define A1  A0+1
#define A2  A0+2
#define A3  A0+3
#define A4  A0+4
#define A5  A0+5
#define A6  A0+6
#define A7  A0+7

#define B0  10
#define B1  B0+1
#define B2  B0+2
#define B3  B0+3
#define B4  B0+4
#define B5  B0+5
#define B6  B0+6
#define B7  B0+7

#define C0  10
#define C1  C0+1
#define C2  C0+2
#define C3  C0+3
#define C4  C0+4
#define C5  C0+5
#define C6  C0+6
#define C7  C0+7
#define C8  2
#define C9  C8+1
#define Ca  C8+2
#define Cb  C8+3
#define Cc  C8+4
#define Cd  C8+5
#define Ce  C8+6
#define Cf  C8+7

#define M0  18
#define M1  M0+1
#define M2  M0+2
#define M3  M0+3
#define M4  M0+4
#define M5  M0+5
#define M6  M0+6
#define M7  M0+7

#if defined (L_umulditi3_raw)

.macro .Mul a, b
    wmov    r24, Y
    adiw    r24, \a
    wmov    r30, Y
    adiw    r30, \b
    rcall   __umulsidi3_pBE
.endm

;;; uint128_t R10[16] = (uint128_t) R10[8] * R18[8]
;;; No regs are restored except Y.
DEFUN __umulditi3_raw
    push    A0
    push    A1
    push    A2
    push    A3
    push    A4
    push    A5
    push    A6
    push    A7
    do_prologue_saves 10
    ;; Now we have the following stack layout:
    ;; Y+1..Y+2    saved Y
    ;; Y+3..Y+10   B[8] as big endian bytes
    ;; Y+11..Y+18  A[8] as big endian bytes
    A.hi = 11
    A.lo = A.hi + 4
    B.hi = 3
    B.lo = B.hi + 4

    ;; In terms of uint32_t we have to perform 4 multiplications:

    .Mul    A.lo, B.lo
    mov8    C0, M0

    .Mul    A.hi, B.hi
    mov8    C8, M0

    .Mul    A.hi, B.lo
    rcall   __umulditi3_12

    .Mul    A.lo, B.hi
    rcall   __umulditi3_12

    ;; Move result into place.
    mov8    C0 + 0x8, C8

    ;; Epilogue:  We pushed 18 bytes.  Let epilogue_restores pop
    ;; them by adjusting SP accordingly.
    ldi     r30, 18
    ;; Pretend we pushed only 2 bytes (Y) so that the low part of
    ;; the return value in R10..R17 won't be overridden by LDDs.
    XJMP    __epilogue_restores__ + ((18 - 2) * 2)

__umulditi3_12:
    ADD     C4, M0
    adc     C5, M1
    adc     C6, M2
    adc     C7, M3
    adc     C8, M4
    adc     C9, M5
    adc     Ca, M6
    adc     Cb, M7
    adc     Cc, __zero_reg__
    adc     Cd, __zero_reg__
    adc     Ce, __zero_reg__
    adc     Cf, __zero_reg__
    ret

;;; uint64_t R18 = uint32_t BE *Z  *  uint32_t BE *W
__umulsidi3_pBE:
    ldd     A0, Z + 3
    ldd     A1, Z + 2
    ldd     A2, Z + 1
    ldd     A3, Z + 0
    wmov    r30, r24
    ldd     A4, Z + 3
    ldd     A5, Z + 2
    ldd     A6, Z + 1
    ldd     A7, Z + 0
    XJMP    __umulsidi3
ENDF __umulditi3_raw
#endif /* L_umulditi3_raw */

#if defined (L_muluda3)
;;; uint128_t R10 = umulsidi3 (uint32_t R22, uint32_t R18)
FALIAS __usmuluda3
DEFUN __muluda3
    clt
ENTRY __muluda3.2
    do_prologue_saves 18
    bld     __tmp_reg__, 7
    push    __tmp_reg__
    XCALL __umulditi3_raw
    or      A7, A6
    or      A7, A5
    or      A7, A4
    mov8    A0, r14
    mov     r27, r13
    XJMP    __muluQ64_tail
ENDF __muluda3
#endif /* L_muluda3 */

#if defined (L_muluta3)
FALIAS __usmuluta3
DEFUN __muluta3
    clt
ENTRY __muluta3.2
    do_prologue_saves 18
    bld     __tmp_reg__, 7
    push    __tmp_reg__
    XCALL __umulditi3_raw
    or      A7, A6
    mov8    A0, r16
    mov     r27, r15
    XJMP    __muluQ64_tail
ENDF __muluta3
#endif /* L_muluta3 */

#if defined (L_muludq3)
FALIAS __usmuludq3
DEFUN __muludq3
    clt
ENTRY __muludq3.2
    do_prologue_saves 18
    bld     __tmp_reg__, 7
    push    __tmp_reg__
    XCALL __umulditi3_raw
    sez
    mov     r27, r17
    XJMP    __muluQ64_tail
ENDF __muludq3
#endif /* L_muludq3 */

#if defined (L_muluQ64_tail)
;;; Inputs:
;;;    R27.7 = LSB (bit -1)
;;;    Z = 0 -> Overflow
;;;    Stack.7: On behalf of signed routines?
DEFUN __muluQ64_tail
    pop     __tmp_reg__
    brne 1f
    ;; tmp.7 = for signed?
    ;; For signed: return LSB in T, no rounding
    bst     r27, 7
    tst     __tmp_reg__
    brmi 9f

    ;; For unsigned: round
    lsl     r27
    brcc 9f
    ldi     r26, 1
    XCALL   __adddi3_s8
    brcs 1f
9:  do_epilogue_restores 18

1:  ;; Unsigned overflow: Saturate to 0xff...
    sec
    XCALL   __sbc_8
    set
    rjmp    9b
ENDF __muluQ64_tail
#endif /* L_muluQ64_tail */

#if defined (L_muldq3)
FALIAS __ssmuldq3
DEFUN __muldq3
    ldi     r30, lo8(gs(__muludq3.2))
    ldi     r31, hi8(gs(__muludq3.2))
    XJMP    __mulQ64_work
ENDF  __muldq3
#endif /* L_muldq3 */

#if defined (L_mulda3)
FALIAS __ssmulda3
DEFUN __mulda3
    ldi     r30, lo8(gs(__muluda3.2))
    ldi     r31, hi8(gs(__muluda3.2))
    XJMP    __mulQ64_work
ENDF  __mulda3
#endif /* L_mulda3 */

#if defined (L_multa3)
FALIAS __ssmulta3
DEFUN __multa3
    ldi     r30, lo8(gs(__muluta3.2))
    ldi     r31, hi8(gs(__muluta3.2))
    XJMP    __mulQ64_work
ENDF  __multa3
#endif /* L_multa3 */

#if defined (L_mulQ64_work)
DEFUN __mulQ64_work
    mov     __tmp_reg__, A7
    eor     __tmp_reg__, B7
    push    __tmp_reg__

    .call_if_neg  A7, __negdi2          ; A = |A|
    .call_if_neg  B7, __negdi2_r10      ; B = |B|

    set    ; for signed
    XICALL ; __muluxx3.2

    ;; Adjust for signed Q formats that have one FBIT less.
    lsl     A0
    ;; Insert LSB from the unsigned mult.
    bld     A0, 0
    rol     A1
    rol     A2
    rol     A3
    rol     A4
    rol     A5
    rol     A6
    rol     A7

    ;; r30.7 = result sign
    pop     r30

    ;; Handle overflow.
    brcs .Lsaturate
    ;; In the negative result case, A = 0x80.. is no overflow,
    ;; but treating it as such keeps the value unchanged.
    brmi .Lsaturate

    ;; Handle negation.
    sbrs    r30, 7
    ret
    XJMP    __negdi2

.Lsaturate:
    ;; r30.7 = 0 -> 0x7f...
    ;; r30.7 = 1 -> 0x80...
    cpi     r30, 0x80
    ;; C = 1 -> 0x7f...
    ;; C = 0 -> 0x80...
    XCALL   __sbc_8
    subi    A7, 0x80
    ret
ENDF __mulQ64_work
#endif /* L_mulQ64_work */

#undef C0
#undef C1
#undef C2
#undef C3
#undef C4
#undef C5
#undef C6
#undef C7
#undef C8
#undef C9
#undef Ca
#undef Cb
#undef Cc
#undef Cd
#undef Ce
#undef Cf

#undef M0
#undef M1
#undef M2
#undef M3
#undef M4
#undef M5
#undef M6
#undef M7

.popsection ; .text.libgcc.mul64

/*******************************************************
    Fixed 64 / 64 saturated Divisions
*******************************************************/

;;; Cnt counts from 65 - BBB down to CStop = -FBIT.
#define Cnt     r26
#define CStop   r27
#define BBB     24

#if defined (L_udivuta3)
;;; unsigned long [sat] accum
DEFUN __udivuta3
ENTRY __usdivuta3
    push    r8
    push    r9
    push    r28
    push    r29
    ldi     CStop, -__UTA_FBIT__
    XCALL   __udivuQ64_common
    pop     r29
    pop     r28
    pop     r9
    pop     r8
    ret
ENDF __udivuta3
#endif /* L_udivuta3 */

#if defined (L_udivudq3)
;;; unsigned long long [sat] fract
DEFUN __udivudq3
ENTRY __usdivudq3
    push    r8
    push    r9
    push    r28
    push    r29
    ldi     CStop, -__UDQ_FBIT__
    XCALL   __udivuQ64_common
    pop     r29
    pop     r28
    pop     r9
    pop     r8
    ret
ENDF __udivudq3
#endif /* L_udivudq3 */

#if defined (L_udivuda3)
;;; unsigned long [sat] accum
DEFUN __udivuda3
ENTRY __usdivuda3
    push    r8
    push    r9
    push    r28
    push    r29
    ldi     CStop, -__UDA_FBIT__
    XCALL   __udivuQ64_common
    pop     r29
    pop     r28
    pop     r9
    pop     r8
    ret
ENDF __udivuda3
#endif /* L_udivuda3 */

#if defined (L_udivuQ64_common)
;;; Remainder
#define M0  A5
#define M1  A6
#define M2  A7
#define M3  28
#define M4  M3+1
#define M5  0
#define M6  M5+1
#define M7  9

;;; Quotient
#define Q0  30
#define Q1  Q0+1
#define Q2  8
#define Q3  A0
#define Q4  A1
#define Q5  A2
#define Q6  A3
#define Q7  A4

;;; Perform unsigned saturated 64-bit fixed-point division.
;;; Rounds towards zero.  Division by 0 is saturated to 0xff..
;;; R18  = R18  / R10
;;; A[8] = A[8] / B[8]
;;; CStop R27 is -FBIT.
;;; No callee-saved regs are saved.
;;; T is not touched.
;;;
;;; Registers overlap as follows:
;;;                     A7  A6  A5  A4  A3  A2  A1  A0
;;; M7  M6  M5  M4  M3  M2  M1  M0  Q7  Q6  Q5  Q4  Q3  Q2  Q1  Q0
;;; r9  r1  r0  r29 r28 r25 r24 r23 r22 r21 r20 r19 r18 r8  r31 r30
;;; This means the algorithm has a head start of BBB = 24 bits.
;;; (32 bits is too much to pass the tests.)
DEFUN __udivuQ64_common
    ldi     Cnt, 65 - BBB
    ;; M[7..3] = 0.
    ;; Q[2..0] = 0.
    sub     M5, M5              ; CLC
    ;; M6 = 0 already.
    wmov    r8, M5              ; M7, Q2
    wmov    M3, M5              ; M4, M3
    wmov    Q0, M5              ; Q1, Q0
    rjmp .Lenter

.Loop:
    ;; Shift one more bit of A into M from the low end.
    ;; When A's bits are exhausted, shift in 0s.
    ROL  M0         $ rol  M1       $ rol  M2       $ rol  M3
    rol  M4         $ rol  M5       $ rol  M6       $ rol  M7

    ;; When M overflows then B will always fit.
    brcs    .LsubB
    CP   M0, B0     $ cpc  M1, B1   $ cpc  M2, B2   $ cpc  M3, B3
    cpc  M4, B4     $ cpc  M5, B5   $ cpc  M6, B6   $ cpc  M7, B7
    brlo    .LshiftQ

.LsubB:
    SUB  M0, B0     $ sbc  M1, B1   $ sbc  M2, B2   $ sbc  M3, B3
    sbc  M4, B4     $ sbc  M5, B5   $ sbc  M6, B6   $ sbc  M7, B7

    SEC
    ROL  Q0
    skip
.LshiftQ:
    LSL  Q0         $ rol  Q1       $ rol  Q2
.Lenter:                                            $ rol  Q3
    rol  Q4         $ rol  Q5       $ rol  Q6       $ rol  Q7

    dec     Cnt
    ;; Cnt >= 0: Feed one more bit of A (plus a final 0 for latency) into M.
    brpl .Loop

    cpse Cnt, CStop
    ;; Cnt < 0: Feed 0s into M.
    brcc .Loop

    ;; Done
    clr     __zero_reg__
    ;; When quotient 1s drop out of Q, then it's overflow.
    brcs .Loverflow

    ;; Move result into place.
    mov  A7, Q7     $ mov  A6, Q6   $ mov  A5, Q5   $ mov  A4, Q4
    mov  A3, Q3     $ mov  A2, Q2   $                 wmov A0, Q0
    ret

.Loverflow:
    ;; A = 0xff..
    XJMP    __sbc_8
ENDF __udivuQ64_common

#undef Q0
#undef Q1
#undef Q2
#undef Q3
#undef Q4
#undef Q5
#undef Q6
#undef Q7

#undef M0
#undef M1
#undef M2
#undef M3
#undef M4
#undef M5
#undef M6
#undef M7
#endif /* L_udivuQ64_common */

;;; Perform signed saturated 64-bit fixed-point division.
;;; Division by 0 is saturated according to the sign of the dividend.
;;; Rounds towards zero.

#if defined (L_divda3)
;;; signed long [sat] accum
DEFUN __divda3
ENTRY __ssdivda3
    ;; Save R8, R9, B and Y.
    do_prologue_saves 12
    ldi     CStop, -__DA_FBIT__
    XJMP    __divQ64_tail
ENDF __divda3
#endif /* L_divda3 */

#if defined (L_divta3)
;;; signed long long [sat] accum
DEFUN __divta3
ENTRY __ssdivta3
    ;; Save R8, R9, B and Y.
    do_prologue_saves 12
    ldi     CStop, -__TA_FBIT__
    XJMP    __divQ64_tail
ENDF __divta3
#endif /* L_divta3 */

#if defined (L_divdq3)
;;; signed long long [sat] fract
DEFUN __divdq3
ENTRY __ssdivdq3
    ;; Save R8, R9, B and Y.
    do_prologue_saves 12
    ldi     CStop, -__DQ_FBIT__
    XJMP    __divQ64_tail
ENDF __divdq3
#endif /* L_divdq3 */

#if defined (L_divQ64_tail)
DEFUN __divQ64_tail
    mov     __tmp_reg__, A7
    eor     __tmp_reg__, B7
    bst     __tmp_reg__, 7

    .call_if_neg  A7, __negdi2          ; A = |A|
    .call_if_neg  B7, __negdi2_r10      ; B = |B|
    XCALL   __udivuQ64_common           ; A /= B as unsigned

    ;; r30.7 = result sign
    bld     r30, 7

    ;; Handle overflow.
    tst     A7
    ;; In the negative result case, A = 0x80.. is no overflow,
    ;; but treating it as such keeps the value unchanged.
    brmi .Lsaturate

    ;; Handle result sign.
    brtc 9f
    XCALL   __negdi2

9:  do_epilogue_restores 12

.Lsaturate:
    ;; r30.7 = 0 -> 0x7f..
    ;; r30.7 = 1 -> 0x80..
    cpi     r30, 0x80
    ;; C = 1 -> 0x7f..
    ;; C = 0 -> 0x80..
    XCALL   __sbc_8
    subi    A7, 0x80
    rjmp    9b
ENDF __divQ64_tail
#endif /* L_divQ64_tail */

#undef Cnt
#undef CStop
#undef BBB

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

#endif /* ! AVR_TINY */

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
;;; Argument 1: Dividend and remainder
#define A0 26
#define A1 A0+1

;;; Argument 2: Divisor
#define B0 22
#define B1 B0+1

;;; Return value: Quotient
#define Q0 24
#define Q1 Q0+1

#define Cnt 21
#define Sign __tmp_reg__

#if defined (L_divhq3)
;;;  (set (reg:HQ 24)
;;;       (ss_div:HQ (reg:HQ 26)
;;;                  (reg:HQ 22)))
;;;  (clobber (reg:HI 26))
;;;  (clobber (reg:QI 21))
;;; Rounds towards zero.  Overflow saturates.
DEFUN __divhq3
ENTRY __ssdivhq3
    mov     Sign, A1
    tst     B1
    brpl 1f
    com     Sign
    NEG2    B0
1:
    tst     A1
    brpl 2f
    NEG2    A0
2:
    lsr     A1                  ; Adjust for FBIT = 15.
    ror     A0                  ; Pass LSB in C.
    clr     Q1
    XCALL   __udivuhq3.2
    XJMP    __divQ16_tail
ENDF __divhq3
#endif  /* defined (L_divhq3) */

#if defined (L_divQ16_tail)
DEFUN __divQ16_tail
    ;; In the negative result case, Q = 0x80.. is no overflow,
    ;; but treating it as such keeps the value unchanged.
    brmi    .Lsaturate          ; N = sign (Q)
    ;; Negate result as needed.
    sbrs    Sign, 7
    ret
    NEG2    Q0
    ret
.Lsaturate:
    ;; Sign.7 = 0 -> 0x7f..
    ;; Sign.7 = 1 -> 0x80..
#ifndef __AVR_TINY__
    ldi     Q0, lo8(0x7fff)
    ldi     Q1, hi8(0x7fff)
    sbrc    Sign, 7
    adiw    Q0, 1
#else
    cpi     Sign, 0x80
    sbc     Q0, Q0
    sbc     Q1, Q1
    subi    Q1, 0x80
#endif
    ret
ENDF __divQ16_tail
#endif /* L_divQ16_tail */

#if defined (L_udivuhq3)
;;;  (set (reg:UHQ 24)
;;;       (us_div:UHQ (reg:UHQ 26)
;;;                   (reg:UHQ 22)))
;;;  (clobber (reg:HI 26))
;;;  (clobber (reg:QI 21))
;;; Rounds towards zero.  Overflow saturates.
DEFUN __udivuhq3
ENTRY __usdivuhq3
    sub     Q1, Q1      ; CLC
ENTRY __udivuhq3.2
    clr     Q0          ; Clear quotient
    ldi     Cnt, 16     ; Init loop counter.
    ;; C = 0 (unsigned).
    ;; C = LSB from the caller (signed).
.Loop:
    rol     A0          ; Shift one more dividend bit into the remainder.
    rol     A1
    brcs .Lsub          ; Divisor always fits in remainder?
    cp      A0, B0      ; Remainder >= B?
    cpc     A1, B1
    brcc .Lsub
    rol     Q0          ; No: Shift quotient with C = 1 (0 in result).
    rjmp .Lcont
.Lsub:
    sub     A0, B0      ; Yes: Restore remainder by subtracting B...
    sbc     A1, B1
    lsl     Q0          ; ...and shift quotient with C = 0 (1 in result).
.Lcont:
    rol     Q1          ; Shift quotient.
    dec     Cnt         ; Decrement loop counter.
    brne .Loop
    com     Q0          ; Complement result because C was
    com     Q1          ; complemented in the loop.
    ret                 ; N = sign (Q).
ENDF __udivuhq3
#endif  /* defined (L_udivuhq3) */

/*******************************************************
    Fixed Division 8.8 / 8.8
*******************************************************/
#if defined (L_divha3)
;;;  (set (reg:HA 24)
;;;       (ss_div:HA (reg:HA 26)
;;;                  (reg:HA 22)))
;;;  (clobber (reg:HI 26))
;;;  (clobber (reg:QI 21))
;;; Rounds towards zero.  Overflow saturates.
DEFUN __divha3
ENTRY __ssdivha3
    mov     Sign, A1
    tst     B1
    brpl 1f
    com     Sign
    NEG2    B0
1:
    tst     A1
    brpl 2f
    NEG2    A0
2:
    lsr     A1              ; Adjust to FBIT = 7.
    ror     A0              ; Pass LSB in C.
    XCALL   __udivuha3.2    ; N = sign (Q)
    XJMP    __divQ16_tail
ENDF __divha3
#endif  /* defined (L_divha3) */

#if defined (L_udivuha3)
;;;  (set (reg:UHA 24)
;;;       (us_div:UHA (reg:UHA 26)
;;;                   (reg:UHA 22)))
;;;  (clobber (reg:HI 26))
;;;  (clobber (reg:QI 21))
;;; Rounds towards zero.  Overflow saturates.
DEFUN __udivuha3
ENTRY __usdivuha3
    clc
ENTRY __udivuha3.2
    mov     Q1, A0          ; Put dividend in A0:Q1.
    mov     A0, A1
    clr     A1
    rol     Q1              ; Shift quotient into carry.
    XJMP    __udivuhq3.2    ; Same as fractional after rearrange.
ENDF __udivuha3
#endif  /* defined (L_udivuha3) */

#undef A0
#undef A1
#undef Q0
#undef Q1
#undef B0
#undef B1
#undef Cnt
#undef Sign


/*********************************************************
    Fixed 16.16 and s16.15 saturated [U]SAmode Divisions
*********************************************************/

;;; Argument 1: Dividend gets passed already in place.
#define A0 24
#define A1 A0+1
#define A2 26
#define A3 A2+1

;;; Argument 2: Divisor
#define B0 18
#define B1 B0+1
#define B2 20
#define B3 B2+1

;;; Return value: Quotient
#define Q0 22
#define Q1 Q0+1
#define Q2 A0
#define Q3 A0+1

;;; Remainder
#define M0 A2
#define M1 A2+1
#define M2 __tmp_reg__
#define M3 __zero_reg__

#define Cnt  r31
#define Sign r30

#if defined (L_divsa3)
;;; (set (reg:SA 22)
;;;      (ss_div:SA (reg:SA 24)
;;;                 (reg:SA 18)))
DEFUN   __divsa3
ENTRY   __ssdivsa3
    mov     Sign, B3
    .call_if_neg B3, __negsi2_r18
    sbrs    A3, 7
    rjmp    2f
    com     Sign
    NEG4    A0
2:  lsr     A3              ; Adjust to 15 fractional bits.
    ror     A2
    ror     A1
    ror     A0              ; Pass LSB in C.
    XCALL   __udivusa3.2    ; N = sign (Q)
    ;; In the negative result case, Q = 0x80.. is no overflow,
    ;; but treating it as such keeps the value unchanged.
    brmi .Lsaturate          ; Overflow?
    ;; Handle result sign.
    sbrs    Sign, 7
    ret
    XJMP    __negsi2
.Lsaturate:
    ;; Sign.7 = 0 -> C = 1 -> 0x7f..
    ;; Sign.7 = 1 -> C = 0 -> 0x80..
    cpi     Sign, 0x80
    sbc     Q0, Q0
    sbc     Q1, Q1
    wmov    Q2, Q0
    subi    Q3, 0x80
    ret
ENDF __divsa3
#endif  /* defined (L_divsa3) */

#if defined (L_udivusa3)
;;; (set (reg:USA 22)
;;;      (us_div:USA (reg:USA 24)
;;;                  (reg:USA 18)))
;;; Register layout:
;;;         A3  A2  A1  A0          [---const----]
;;; M3  M2  M1  M0  Q3  Q2  Q1  Q0  B3  B2  B1  B0
;;; r1  r0  27  26  25  24  23  22  21  20  19  18
DEFUN   __udivusa3
ENTRY   __usdivusa3
    clc
ENTRY   __udivusa3.2
    ldi     Cnt, 33     ; Init loop counter.
    clr     M2
    wmov    Q0, M2      ; M3 === R1 = 0 already.
    ;; C = 0 (unsigned).
    ;; C = LSB (from signed caller).
    rjmp    .Lstart
.Loop:
    rol     M0          ; Shift dividend with Carry from Q.
    rol     M1
    rol     M2
    rol     M3
    brcs .Lsub          ; Remainder overflow: B will always fit.
    cp      M0, B0      ; Compare remainder and divisor.
    cpc     M1, B1
    cpc     M2, B2
    cpc     M3, B3
    brcc .Lsub          ; Remainder >= divisor?
    rol     Q0          ; No: Shift quotient (C = 1, 0 in result)
    rjmp .Lcont
.Lsub:
    sub     M0, B0      ; Yes: Restore remainder.
    sbc     M1, B1
    sbc     M2, B2
    sbc     M3, B3
    lsl     Q0          ; Shift quotient (C = 0, 1 in result).
.Lcont:
    rol     Q1          ; Shift quotient into C into M.
.Lstart:
    rol     Q2
    rol     Q3
    dec     Cnt         ; Decrement loop counter until 0.
    brne .Loop
    clr     __zero_reg__
    com     Q0          ; Complement result because Carry
    com     Q1          ; was complemented in the loop.
    com     Q2
    com     Q3          ; N is the sign of Q
    ret
ENDF __udivusa3
#endif  /* defined (L_udivusa3) */

#undef A0
#undef A1
#undef A2
#undef A3
#undef M0
#undef M1
#undef M2
#undef M3
#undef Q0
#undef Q1
#undef Q2
#undef Q3
#undef B0
#undef B1
#undef B2
#undef B3
#undef Cnt
#undef Sign


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturation, 1 Byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First Argument and Return Register
#define A0  24

#if defined (L_ssabs_1)
DEFUN __ssabs_1
    sbrs    A0, 7
    ret
    neg     A0
    sbrc    A0,7
    dec     A0
    ret
ENDF __ssabs_1
#endif /* L_ssabs_1 */

#undef A0



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
    wsubi   A0, 1
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

#ifndef __AVR_TINY__

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

#endif /* ! AVR_TINY */


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rounding Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef L_mask1

#define AA 24
#define CC 25

;; R25 = 1 << (R24 & 7)
;; CC  = 1 << (AA  & 7)
;; Clobbers: None
DEFUN __mask1
    ;; CC = 2 ^ AA.1
    ldi     CC, 1 << 2
    sbrs    AA, 1
    ldi     CC, 1 << 0
    ;; CC *= 2 ^ AA.0
    sbrc    AA, 0
    lsl     CC
    ;; CC *= 2 ^ AA.2
    sbrc    AA, 2
    swap    CC
    ret
ENDF __mask1

#undef AA
#undef CC
#endif /* L_mask1 */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The rounding point. Any bits smaller than
;; 2^{-RP} will be cleared.
#define RP R24

#define A0 22
#define A1 A0 + 1

#define C0 24
#define C1 C0 + 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rounding, 1 Byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef L_roundqq3

;; R24 = round (R22, R24)
;; Clobbers: R22, __tmp_reg__
DEFUN  __roundqq3
    mov     __tmp_reg__, C1
    subi    RP, __QQ_FBIT__ - 1
    neg     RP
    ;; R25 = 1 << RP  (Total offset is FBIT-1 - RP)
    XCALL   __mask1
    mov     C0, C1
    ;; Add-Saturate 2^{-RP-1}
    add     A0, C0
    brvc 0f
    ldi     C0, 0x7f
    rjmp 9f
0:  ;; Mask out bits beyond RP
    lsl     C0
    neg     C0
    and     C0, A0
9:  mov     C1, __tmp_reg__
    ret
ENDF  __roundqq3
#endif /* L_roundqq3 */

#ifdef L_rounduqq3

;; R24 = round (R22, R24)
;; Clobbers: R22, __tmp_reg__
DEFUN  __rounduqq3
    mov     __tmp_reg__, C1
    subi    RP, __UQQ_FBIT__ - 1
    neg     RP
    ;; R25 = 1 << RP  (Total offset is FBIT-1 - RP)
    XCALL   __mask1
    mov     C0, C1
    ;; Add-Saturate 2^{-RP-1}
    add     A0, C0
    brcc 0f
    ldi     C0, 0xff
    rjmp 9f
0:  ;; Mask out bits beyond RP
    lsl     C0
    neg     C0
    and     C0, A0
9:  mov     C1, __tmp_reg__
    ret
ENDF  __rounduqq3
#endif /* L_rounduqq3 */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rounding, 2 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifdef L_addmask_2

;; [ R25:R24 =  1 << (R24 & 15)
;;   R23:R22 += 1 << (R24 & 15) ]
;; SREG is set according to the addition
DEFUN __addmask_2
    ;; R25 = 1 << (R24 & 7)
    XCALL   __mask1
    cpi     RP, 1 << 3
    sbc     C0, C0
    ;; Swap C0 and C1 if RP.3 was set
    and     C0, C1
    eor     C1, C0
    ;; Finally, add the power-of-two:  A[] += C[]
    add     A0, C0
    adc     A1, C1
    ret
ENDF  __addmask_2
#endif /* L_addmask_2 */

#ifdef L_round_s2

;; R25:R24 = round (R23:R22, R24)
;; Clobbers: R23, R22
DEFUN  __roundhq3
    subi    RP, __HQ_FBIT__ - __HA_FBIT__
ENDF   __roundhq3
DEFUN  __roundha3
    subi    RP, __HA_FBIT__ - 1
    neg     RP
    ;; [ R25:R24  = 1 << (FBIT-1 - RP)
    ;;   R23:R22 += 1 << (FBIT-1 - RP) ]
    XCALL   __addmask_2
    XJMP    __round_s2_const
ENDF  __roundha3

#endif /* L_round_s2 */

#ifdef L_round_u2

;; R25:R24 = round (R23:R22, R24)
;; Clobbers: R23, R22
DEFUN  __rounduhq3
    subi    RP, __UHQ_FBIT__ - __UHA_FBIT__
ENDF   __rounduhq3
DEFUN  __rounduha3
    subi    RP, __UHA_FBIT__ - 1
    neg     RP
    ;; [ R25:R24  = 1 << (FBIT-1 - RP)
    ;;   R23:R22 += 1 << (FBIT-1 - RP) ]
    XCALL   __addmask_2
    XJMP    __round_u2_const
ENDF  __rounduha3

#endif /* L_round_u2 */


#ifdef L_round_2_const

;; Helpers for 2 byte wide rounding

DEFUN  __round_s2_const
    brvc 2f
    ldi     C1, 0x7f
    rjmp 1f
    ;; FALLTHRU (Barrier)
ENDF  __round_s2_const

DEFUN __round_u2_const
    brcc 2f
    ldi     C1, 0xff
1:
    ldi     C0, 0xff
    rjmp 9f
2:
    ;; Saturation is performed now.
    ;; Currently, we have C[] = 2^{-RP-1}
    ;; C[] = 2^{-RP}
    lsl     C0
    rol     C1
    ;;
    NEG2    C0
    ;; Clear the bits beyond the rounding point.
    and     C0, A0
    and     C1, A1
9:  ret
ENDF  __round_u2_const

#endif /* L_round_2_const */

#undef A0
#undef A1
#undef C0
#undef C1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rounding, 4 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifndef __AVR_TINY__

#define A0 18
#define A1 A0 + 1
#define A2 A0 + 2
#define A3 A0 + 3

#define C0 22
#define C1 C0 + 1
#define C2 C0 + 2
#define C3 C0 + 3

#ifdef L_addmask_4

;; [ R25:R22 =  1 << (R24 & 31)
;;   R21:R18 += 1 << (R24 & 31) ]
;; SREG is set according to the addition
DEFUN __addmask_4
    ;; R25 = 1 << (R24 & 7)
    XCALL   __mask1
    cpi     RP, 1 << 4
    sbc     C0, C0
    sbc     C1, C1
    ;; Swap C2 with C3 if RP.3 is not set
    cpi     RP, 1 << 3
    sbc     C2, C2
    and     C2, C3
    eor     C3, C2
    ;; Swap C3:C2 with C1:C0 if RP.4 is not set
    and     C0, C2  $  eor     C2, C0
    and     C1, C3  $  eor     C3, C1
    ;; Finally, add the power-of-two:  A[] += C[]
    add     A0, C0
    adc     A1, C1
    adc     A2, C2
    adc     A3, C3
    ret
ENDF  __addmask_4
#endif /* L_addmask_4 */

#ifdef L_round_s4

;; R25:R22 = round (R21:R18, R24)
;; Clobbers: R18...R21
DEFUN  __roundsq3
    subi    RP, __SQ_FBIT__ - __SA_FBIT__
ENDF   __roundsq3
DEFUN  __roundsa3
    subi    RP, __SA_FBIT__ - 1
    neg     RP
    ;; [ R25:R22  = 1 << (FBIT-1 - RP)
    ;;   R21:R18 += 1 << (FBIT-1 - RP) ]
    XCALL   __addmask_4
    XJMP    __round_s4_const
ENDF  __roundsa3

#endif /* L_round_s4 */

#ifdef L_round_u4

;; R25:R22 = round (R21:R18, R24)
;; Clobbers: R18...R21
DEFUN  __roundusq3
    subi    RP, __USQ_FBIT__ - __USA_FBIT__
ENDF   __roundusq3
DEFUN  __roundusa3
    subi    RP, __USA_FBIT__ - 1
    neg     RP
    ;; [ R25:R22  = 1 << (FBIT-1 - RP)
    ;;   R21:R18 += 1 << (FBIT-1 - RP) ]
    XCALL   __addmask_4
    XJMP    __round_u4_const
ENDF  __roundusa3

#endif /* L_round_u4 */


#ifdef L_round_4_const

;; Helpers for 4 byte wide rounding

DEFUN  __round_s4_const
    brvc 2f
    ldi     C3, 0x7f
    rjmp 1f
    ;; FALLTHRU (Barrier)
ENDF  __round_s4_const

DEFUN __round_u4_const
    brcc 2f
    ldi     C3, 0xff
1:
    ldi     C2, 0xff
    ldi     C1, 0xff
    ldi     C0, 0xff
    rjmp 9f
2:
    ;; Saturation is performed now.
    ;; Currently, we have C[] = 2^{-RP-1}
    ;; C[] = 2^{-RP}
    lsl     C0
    rol     C1
    rol     C2
    rol     C3
    XCALL   __negsi2
    ;; Clear the bits beyond the rounding point.
    and     C0, A0
    and     C1, A1
    and     C2, A2
    and     C3, A3
9:  ret
ENDF  __round_u4_const

#endif /* L_round_4_const */

#undef A0
#undef A1
#undef A2
#undef A3
#undef C0
#undef C1
#undef C2
#undef C3

#undef RP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rounding, 8 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#define RP     16
#define FBITm1 31

#define C0 18
#define C1 C0 + 1
#define C2 C0 + 2
#define C3 C0 + 3
#define C4 C0 + 4
#define C5 C0 + 5
#define C6 C0 + 6
#define C7 C0 + 7

#define A0 16
#define A1 17
#define A2 26
#define A3 27
#define A4 28
#define A5 29
#define A6 30
#define A7 31


#ifdef L_rounddq3
;; R25:R18 = round (R25:R18, R16)
;; Clobbers: ABI
DEFUN  __rounddq3
    ldi     FBITm1, __DQ_FBIT__ - 1
    clt
    XJMP    __round_x8
ENDF  __rounddq3
#endif /* L_rounddq3 */

#ifdef L_roundudq3
;; R25:R18 = round (R25:R18, R16)
;; Clobbers: ABI
DEFUN  __roundudq3
    ldi     FBITm1, __UDQ_FBIT__ - 1
    set
    XJMP    __round_x8
ENDF  __roundudq3
#endif /* L_roundudq3 */

#ifdef L_roundda3
;; R25:R18 = round (R25:R18, R16)
;; Clobbers: ABI
DEFUN  __roundda3
    ldi     FBITm1, __DA_FBIT__ - 1
    clt
    XJMP    __round_x8
ENDF  __roundda3
#endif /* L_roundda3 */

#ifdef L_rounduda3
;; R25:R18 = round (R25:R18, R16)
;; Clobbers: ABI
DEFUN  __rounduda3
    ldi     FBITm1, __UDA_FBIT__ - 1
    set
    XJMP    __round_x8
ENDF  __rounduda3
#endif /* L_rounduda3 */

#ifdef L_roundta3
;; R25:R18 = round (R25:R18, R16)
;; Clobbers: ABI
DEFUN  __roundta3
    ldi     FBITm1, __TA_FBIT__ - 1
    clt
    XJMP    __round_x8
ENDF  __roundta3
#endif /* L_roundta3 */

#ifdef L_rounduta3
;; R25:R18 = round (R25:R18, R16)
;; Clobbers: ABI
DEFUN  __rounduta3
    ldi     FBITm1, __UTA_FBIT__ - 1
    set
    XJMP    __round_x8
ENDF  __rounduta3
#endif /* L_rounduta3 */


#ifdef L_round_x8
DEFUN __round_x8
    push r16
    push r17
    push r28
    push r29
    ;; Compute log2 of addend from rounding point
    sub     RP, FBITm1
    neg     RP
    ;; Move input to work register A[]
    push    C0
    mov     A1, C1
    wmov    A2, C2
    wmov    A4, C4
    wmov    A6, C6
    ;; C[] = 1 << (FBIT-1 - RP)
    XCALL   __clr_8
    inc     C0
    XCALL   __ashldi3
    pop     A0
    ;; A[] += C[]
    add     A0, C0
    adc     A1, C1
    adc     A2, C2
    adc     A3, C3
    adc     A4, C4
    adc     A5, C5
    adc     A6, C6
    adc     A7, C7
    brts    1f
    ;; Signed
    brvc    3f
    ;; Signed overflow: A[] = 0x7f...
    brvs    2f
1:  ;; Unsigned
    brcc    3f
    ;; Unsigned overflow: A[] = 0xff...
2:  ldi     C7, 0xff
    ldi     C6, 0xff
    wmov    C0, C6
    wmov    C2, C6
    wmov    C4, C6
    bld     C7, 7
    rjmp 9f
3:
    ;;  C[] = -C[] - C[]
    push    A0
    ldi     r16, 1
    XCALL   __ashldi3
    pop     A0
    XCALL   __negdi2
    ;; Clear the bits beyond the rounding point.
    and     C0, A0
    and     C1, A1
    and     C2, A2
    and     C3, A3
    and     C4, A4
    and     C5, A5
    and     C6, A6
    and     C7, A7
9:  ;; Epilogue
    pop r29
    pop r28
    pop r17
    pop r16
    ret
ENDF  __round_x8

#endif /* L_round_x8 */

#undef A0
#undef A1
#undef A2
#undef A3
#undef A4
#undef A5
#undef A6
#undef A7

#undef C0
#undef C1
#undef C2
#undef C3
#undef C4
#undef C5
#undef C6
#undef C7

#undef RP
#undef FBITm1

#endif /* ! AVR_TINY */

;; Supply implementations / symbols for the bit-banging functions
;; __builtin_avr_bitsfx and __builtin_avr_fxbits
#ifdef L_ret
DEFUN __ret
    ret
ENDF  __ret
#endif /* L_ret */


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturated Shift Left, 1 Byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#define A0 24
#define Off r22

#ifdef L_usshift_1
;;; (set (reg:UQQ 24)
;;;      (us_ashift:UQQ (reg:UQQ 24)
;;;                     (reg:QI 22)))
;;; T = T
;;; Clobbers: R22.

DEFUN __usashluqq3
    tst     Off
    breq 9f
.Loop_bits:
    lsl     A0
    brcs .Lsat
    dec     Off
    brne .Loop_bits
    ret

.Lsat:
    ldi     A0, 0xff
9:  ret
ENDF __usashluqq3
#endif /* L_usshift_1 */

#ifdef L_ssshift_1
;;; (set (reg:QQ 24)
;;;      (ss_ashift:QQ (reg:QQ 24)
;;;                    (reg:QI 22)))
;;; Clobbers: R22.
DEFUN __ssashlqq3
    bst     A0, 7
    sbrc    A0, 7
    neg     A0
    XCALL   __usashluqq3
    ;; In the negative result case, A = 0x80 is no overflow,
    ;; but treating it as such keeps the value unchanged.
    tst     A0
    brmi .Lsat
    brtc 9f
    neg     A0
    ret

.Lsat:
    ;; Saturate:
    ;; T = 0 -> 0x7f
    ;; T = 1 -> 0x80
    ldi     A0, 0x7f
    brtc 9f
    inc     A0
9:  ret
ENDF __ssashlqq3
#endif /* L_ssshift_1 */

#undef A0
#undef Off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturated Shift Left, 2 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#define A0 24
#define A1 A0+1
#define Off r22

#ifdef L_usshift_2
;;; (set (reg:UH* 24)
;;;      (us_ashift:UH* (reg:UH* 24)
;;;                     (reg:QI 22)))
;;; T = T
;;; Clobbers: R22.

DEFUN __usshift_2
.Loop_bytes:
    tst     A1
    brne .Lsat
    mov     A1, A0
    clr     A0
ENTRY __usashluha3
ENTRY __usashluhq3
    subi    Off, 8
    brcc .Loop_bytes
    ;; Undo the extra Off -= 8 from above.
    subi    Off, -8
    breq 9f
.Loop_bits:
    lsl     A0
    rol     A1
    brcs .Lsat
    dec     Off
    brne .Loop_bits
    ret

.Lsat:
    ;; A = 0xff..
    ldi     A0, lo8(0xffff)
    ldi     A1, hi8(0xffff)
9:  ret
ENDF __usshift_2
#endif /* L_usshift_2 */

#ifdef L_ssshift_2
;;; (set (reg:HA/HQ 24)
;;;      (ss_ashift:HA/HQ (reg:HA/HQ 24)
;;;                       (reg:QI 22)))
;;; Clobbers: R22.
DEFUN __ssashlha3
ENTRY __ssashlhq3
    bst     A1, 7
    brtc 1f
    NEG2    A0
1:  XCALL   __usashluha3
    ;; In the negative result case, A = 0x80.. is no overflow,
    ;; but treating it as such keeps the value unchanged.
    tst     A1
    brmi .Lsat
    brtc 9f
    NEG2    A0
    ret

.Lsat:
    ;; Saturate:
    ;; T = 0 -> 0x7f..
    ;; T = 1 -> 0x80..
    ldi     A0, lo8(0x7fff)
    ldi     A1, hi8(0x7fff)
    brtc 9f
    waddi   A0, 1
9:  ret
ENDF __ssashlha3
#endif /* L_ssshift_2 */

#undef A0
#undef A1
#undef Off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturated Shift Left, 4 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#define A0 22
#define A1 A0+1
#define A2 A0+2
#define A3 A0+3
#define Off r20

#ifdef L_usshift_4
;;; (set (reg:US* 22)
;;;      (us_ashift:US* (reg:US* 22)
;;;                     (reg:QI 20)))
;;; T = T
;;; Clobbers: R20.
DEFUN __usshift_4
.Loop_bytes:
    tst     A3
    brne .Lsat
    mov     A3, A2
    mov     A2, A1
    mov     A1, A0
    clr     A0
ENTRY __usashlusa3
ENTRY __usashlusq3
    subi    Off, 8
    brcc .Loop_bytes
    ;; Undo the extra Off -= 8 from above.
    subi    Off, -8
    breq 9f
.Loop_bits:
    lsl     A0
    rol     A1
    rol     A2
    rol     A3
    brcs .Lsat
    dec     Off
    brne .Loop_bits
    ret

.Lsat:
    ldi     A0, 0xff
    ldi     A1, 0xff
    wmov    A2, A0
9:  ret
ENDF __usshift_4
#endif /* L_usshift_4 */

#ifdef L_ssshift_4
;;; (set (reg:SA/SQ 22)
;;;      (ss_ashift:SA/SQ (reg:SA/SQ 22)
;;;                       (reg:QI 20)))
;;; Clobbers: R20.
DEFUN __ssashlsa3
ENTRY __ssashlsq3
    bst     A3, 7
    brtc 1f
    XCALL   __negsi2
1:  XCALL   __usashlusa3
    ;; In the negative result case, A = 0x80.. is no overflow,
    ;; but treating it as such keeps the value unchanged.
    tst     A3
    brpl 2f
    ;; Saturate:
    ;; T = 0 -> 0x7f..
    ;; T = 1 -> 0x80..
    bld     A3, 7
    cpi     A3, 0x80
    sbc     A0, A0
    sbc     A1, A1
    wmov    A2, A0
    subi    A3, 0x80
9:  ret
2:  brtc 9b
    XJMP    __negsi2
ENDF __ssashlsa3
#endif /* L_ssshift_4 */

#undef A0
#undef A1
#undef A2
#undef A3
#undef Off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saturated Shift Left, 8 Bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#ifndef __AVR_TINY__

#define A0 18
#define A1 A0+1
#define A2 A0+2
#define A3 A0+3
#define A4 A0+4
#define A5 A0+5
#define A6 A0+6
#define A7 A0+7
#define Off r31

#ifdef L_usshift_8
;;; (set (reg:UD* 18)
;;;      (us_ashift:UD* (reg:UD* 18)
;;;                     (reg:QI 16)))
;;; T = T
;;; Clobbers: R31
DEFUN __usashluda3
ENTRY __usashluta3
ENTRY __usashludq3
    mov     Off, r16
    rjmp    .Lstart
.Loop_bytes:
    tst     A7
    brne .Lsat
    mov     A7, A6
    mov     A6, A5
    mov     A5, A4
    mov     A4, A3
    mov     A3, A2
    mov     A2, A1
    mov     A1, A0
    clr     A0
.Lstart:
    subi    Off, 8
    brcc .Loop_bytes
    ;; Undo the extra Off -= 8 from above.
    subi    Off, -8
    breq 9f
.Loop_bits:
    lsl     A0
    rol     A1
    rol     A2
    rol     A3
    rol     A4
    rol     A5
    rol     A6
    rol     A7
    brcs .Lsat
    dec     Off
    brne .Loop_bits
    ret

.Lsat:
    ldi     A0, 0xff
    ldi     A1, 0xff
    wmov    A2, A0
    wmov    A4, A0
    wmov    A6, A0
9:  ret
ENDF __usashluda3
#endif /* L_usshift_8 */

#ifdef L_ssshift_8
;;; (set (reg:DA/DQ 18)
;;;      (ss_ashift:DA/DQ (reg:DA/DQ 18)
;;;                       (reg:QI 16)))
DEFUN __ssashlda3
ENTRY __ssashldq3
ENTRY __ssashlta3
    bst     A7, 7
    brtc 1f
    XCALL   __negdi2
1:  XCALL   __usashluda3
    ;; In the negative result case, A = 0x80.. is no overflow,
    ;; but treating it as such keeps the value unchanged.
    tst     A7
    brpl 2f
    ;; Saturate:
    ;; T = 0 -> 0x7f..
    ;; T = 1 -> 0x80..
    bld     A7, 7
    cpi     A7, 0x80
    sbc     A0, A0
    sbc     A1, A1
    wmov    A2, A0
    wmov    A4, A0
    wmov    A6, A0
    subi    A7, 0x80
9:  ret
2:  brtc 9b
    XJMP    __negdi2
ENDF __ssashlda3
#endif /* L_ssshift_8 */

#undef A0
#undef A1
#undef A2
#undef A3
#undef A4
#undef A5
#undef A6
#undef A7
#undef Off

#endif /* !__AVR_TINY__ */
