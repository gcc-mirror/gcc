; SF format is:
;
; [sign] 1.[23bits] E[8bits(n-127)]
;
; SEEEEEEE Emmmmmmm mmmmmmmm mmmmmmmm
;
; [A+0] mmmmmmmm
; [A+1] mmmmmmmm
; [A+2] Emmmmmmm
; [A+3] SEEEEEEE
;
; Special values (xxx != 0):
;
;  s1111111 10000000 00000000 00000000	infinity
;  s1111111 1xxxxxxx xxxxxxxx xxxxxxxx	NaN
;  s0000000 00000000 00000000 00000000	zero
;  s0000000 0xxxxxxx xxxxxxxx xxxxxxxx	denormals
;
; Note that CMPtype is "signed char" for rl78
;
	
#include "vregs.h"

#define Z	PSW.6

START_FUNC	___negsf2

	;; Negate the floating point value.
	;; Input at [SP+4]..[SP+7].
	;; Output to R8..R11.

	movw	ax, [SP+4]
	movw	r8, ax
	movw	ax, [SP+6]
	xor	a, #0x80
	movw	r10, ax
	ret

END_FUNC	___negsf2

;; ------------------internal functions used by later code --------------

START_FUNC	__int_isnan

	;; [HL] points to value, returns Z if it's a NaN

	mov	a, [hl+2]
	and	a, #0x80
	mov	x, a
	mov	a, [hl+3]
	and	a, #0x7f
	cmpw	ax, #0x7f80
	skz
	ret			; return NZ if not NaN
	mov	a, [hl+2]
	and	a, #0x7f
	or	a, [hl+1]
	or	a, [hl]
	bnz	$1f
	clr1	Z		; Z, normal
	ret
1:
	set1	Z		; nan
	ret

END_FUNC	__int_isnan

START_FUNC	__int_eithernan

	;; call from toplevel functions, returns Z if either number is a NaN,
	;; or NZ if both are OK.

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	$!__int_isnan
	bz	$1f

	movw	ax, sp
	addw	ax, #12
	movw	hl, ax
	call	$!__int_isnan
1:
	ret

END_FUNC	__int_eithernan

START_FUNC	__int_iszero

	;; [HL] points to value, returns Z if it's zero

	mov	a, [hl+3]
	and	a, #0x7f
	or	a, [hl+2]
	or	a, [hl+1]
	or	a, [hl]
	ret

END_FUNC	__int_iszero

START_FUNC	__int_cmpsf

	;; This is always called from some other function here,
	;; so the stack offsets are adjusted accordingly.

	;; X [SP+8] <=> Y [SP+12] : <a> <=> 0

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	$!__int_iszero
	bnz	$1f

	movw	ax, sp
	addw	ax, #12
	movw	hl, ax
	call	$!__int_iszero
	bnz	$2f
	;; At this point, both args are zero.
	mov	a, #0
	ret

2:
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
1:
	;; At least one arg is non-zero so we can just compare magnitudes.
	;; Args are [HL] and [HL+4].

	mov	a, [HL+3]
	xor	a, [HL+7]
	mov1	cy, a.7
	bnc	$1f

	mov	a, [HL+3]
	sar	a, 7
	or	a, #1
	ret

1:	;; Signs the same, compare magnitude.  It's safe to lump
	;; the sign bits, exponent, and mantissa together here, since they're
	;; stored in the right sequence.
	movw	ax, [HL+2]
	cmpw	ax, [HL+6]
	bc	$ybig_cmpsf	; branch if X < Y
	bnz	$xbig_cmpsf	; branch if X > Y

	movw	ax, [HL]
	cmpw	ax, [HL+4]
	bc	$ybig_cmpsf	; branch if X < Y
	bnz	$xbig_cmpsf	; branch if X > Y

	mov	a, #0
	ret

xbig_cmpsf:			; |X| > |Y| so return A = 1 if pos, 0xff if neg
	mov	a, [HL+3]
	sar	a, 7
	or	a, #1
	ret
ybig_cmpsf:			; |X| < |Y| so return A = 0xff if pos, 1 if neg
	mov	a, [HL+3]
	xor	a, #0x80
	sar	a, 7
	or	a, #1
	ret

END_FUNC	__int_cmpsf

;; ----------------------------------------------------------

START_FUNC	___cmpsf2
	;; This functions calculates "A <=> B".  That is, if A is less than B
	;; they return -1, if A is greater than B, they return 1, and if A
	;; and B are equal they return 0.  If either argument is NaN the
	;; behaviour is undefined.

	;; Input at [SP+4]..[SP+7].
	;; Output to R8..R9.

	call	$!__int_eithernan
	bnz	$1f
	movw	r8, #1
	ret
1:
	call	$!__int_cmpsf
	mov	r8, a
	sar	a, 7
	mov	r9, a
	ret

END_FUNC	___cmpsf2

;; ----------------------------------------------------------

	;; These functions are all basically the same as ___cmpsf2
	;; except that they define how they handle NaNs.

START_FUNC		___eqsf2
	;; Returns zero iff neither argument is NaN
	;; and both arguments are equal.
START_ANOTHER_FUNC	___nesf2
	;; Returns non-zero iff either argument is NaN or the arguments are
	;; unequal.  Effectively __nesf2 is the same as __eqsf2
START_ANOTHER_FUNC	___lesf2
	;; Returns a value less than or equal to zero if neither
	;; argument is NaN, and the first is less than or equal to the second.
START_ANOTHER_FUNC	___ltsf2
	;; Returns a value less than zero if neither argument is
	;; NaN, and the first is strictly less than the second.

	;; Input at [SP+4]..[SP+7].
	;; Output to R8.

	mov	r8, #1

;;;  Fall through

START_ANOTHER_FUNC	__int_cmp_common

	call	$!__int_eithernan
	sknz
	;; return value (pre-filled-in below) for "either is nan"
	ret

	call	$!__int_cmpsf
	mov	r8, a
	ret

END_ANOTHER_FUNC	__int_cmp_common
END_ANOTHER_FUNC	___ltsf2
END_ANOTHER_FUNC	___lesf2
END_ANOTHER_FUNC	___nesf2
END_FUNC		___eqsf2

START_FUNC		___gesf2
	;; Returns a value greater than or equal to zero if neither argument
	;; is a NaN and the first is greater than or equal to the second.
START_ANOTHER_FUNC	___gtsf2
	;; Returns a value greater than zero if neither argument
	;; is NaN, and the first is strictly greater than the second.

	mov	r8, #0xffff
	br	$__int_cmp_common

END_ANOTHER_FUNC	___gtsf2
END_FUNC		___gesf2

;; ----------------------------------------------------------

START_FUNC	___unordsf2
	;; Returns a nonzero value if either argument is NaN, otherwise 0.

	call	$!__int_eithernan
	movw	r8, #0
	sknz			; this is from the call, not the movw
	movw	r8, #1
	ret
	
END_FUNC	___unordsf2

;; ----------------------------------------------------------

START_FUNC	___fixsfsi
	;; Converts its floating point argument into a signed long,
	;; rounding toward zero.
	;; The behaviour with NaNs and Infinities is not well defined.
	;; We choose to return 0 for NaNs, -INTMAX for -inf and INTMAX for +inf.
	;; This matches the behaviour of the C function in libgcc2.c.

	;; Input at [SP+4]..[SP+7], result is in (lsb) R8..R11 (msb).

	;; Special case handling for infinities as __fixunssfsi
	;; will not give us the values that we want.
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_isinf
	bnz	$1f
	mov	a, [SP+7]
	bt	a.7, $2f
	;; +inf
	movw	r8, #-1
	movw	r10, #0x7fff
	ret
	;; -inf
2:	mov	r8, #0
	mov	r10, #0x8000
	ret
	
	;; Load the value into r10:r11:X:A
1:	movw	ax, [SP+4]
	movw	r10, ax
	movw	ax, [SP+6]

	;; If the value is positive we can just use __fixunssfsi
	bf	a.7, $__int_fixunssfsi

	;; Otherwise we negate the value, call __fixunssfsi and
	;; then negate its result.
	clr1	a.7
	call	$!__int_fixunssfsi

	movw	ax, #0
	subw	ax, r8
	movw	r8, ax
	movw	ax, #0
        sknc
        decw    ax
        subw    ax, r10
	movw	r10, ax
	
	;; Check for a positive result (which should only happen when
	;; __fixunssfsi returns UINTMAX or 0).  In such cases just return 0.
	mov	a, r11
	bt      a.7, $1f
	movw	r10,#0x0
	movw	r8, #0x0

1:	ret

END_FUNC   	___fixsfsi

START_FUNC 	___fixunssfsi
	;; Converts its floating point argument into an unsigned long
	;; rounding towards zero.  Negative arguments all become zero.
	;; We choose to return 0 for NaNs and -inf, but UINTMAX for +inf.
	;; This matches the behaviour of the C function in libgcc2.c.

	;; Input at [SP+4]..[SP+7], result is in (lsb) R8..R11 (msb)
	
	;; Get the input value.
	movw	ax, [SP+4]
	movw	r10, ax
	movw	ax, [SP+6]

	;; Fall through into the internal function.
	
	.global __int_fixunssfsi
__int_fixunssfsi:
	;; Input in (lsb) r10.r11.x.a (msb).

	;; Test for a negative input.  We shift the other bits at the
	;; same time so that A ends up holding the whole exponent:
	;;
	;; before:
	;;   SEEEEEEE EMMMMMMM MMMMMMMM MMMMMMMM
	;;       A       X        R11     R10
	;;
	;; after:
	;;   EEEEEEEE MMMMMMM0 MMMMMMMM MMMMMMMM
	;;       A       X        R11     R10
	shlw	ax, 1
	bnc	$1f

	;; Return zero.
2:	movw	r8, #0
	movw	r10, #0
	ret

	;; An exponent of -1 is either a NaN or infinity.
1:	cmp	a, #-1
	bnz	$3f
	;; For NaN we return 0.  For infinity we return UINTMAX.
	mov	a, x
	or	a, r10
	or	a, r11
	cmp0	a
	bnz	$2b

6:	movw	r8, #-1		; -1 => UINT_MAX
	movw	r10, #-1
	ret
	
	;; If the exponent is negative the value is < 1 and so the
	;; converted value is 0.  Note we must allow for the bias
	;; applied to the exponent.  Thus a value of 127 in the
	;; EEEEEEEE bits actually represents an exponent of 0, whilst
	;; a value less than 127 actually represents a negative exponent.
	;; Also if the EEEEEEEE bits are all zero then this represents
	;; either a denormal value or 0.0.  Either way for these values
	;; we return 0.
3:	sub     a, #127
	bc	$2b

	;; A now holds the bias adjusted exponent, which is known to be >= 0.
	;; If the exponent is > 31 then the conversion will overflow.
	cmp 	a, #32
	bnc	$6b
4:
	;; Save the exponent in H.  We increment it by one because we want
	;; to be sure that the loop below will always execute at least once.
 	inc	a
	mov	h, a

	;; Get the top 24 bits of the mantissa into A:X:R10
	;; Include the implicit 1-bit that is inherent in the IEEE fp format.
	;;
	;; before:
	;;   EEEEEEEE MMMMMMM0 MMMMMMMM MMMMMMMM
	;;       H       X        R11     R10
	;; after:
	;;   EEEEEEEE 1MMMMMMM MMMMMMMM MMMMMMMM
	;;       H       A        X       R10

	mov	a, r11
	xch	a, x
	shr	a, 1
	set1	a.7

	;; Clear B:C:R12:R13
	movw	bc, #0
	movw	r12, #0

	;; Shift bits from the mantissa (A:X:R10) into (B:C:R12:R13),
	;; decrementing the exponent as we go.

	;; before:
	;;   MMMMMMMM MMMMMMMM MMMMMMMM   xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
	;;       A        X      R10          B       C       R12      R13
	;; first iter:
	;;   MMMMMMMM MMMMMMMM MMMMMMM0   xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxM
	;;       A        X      R10          B       C       R12      R13
	;; second iter:
	;;   MMMMMMMM MMMMMMMM MMMMMM00   xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxMM
	;;       A        X      R10          B       C       R12      R13
	;; etc.
5:
	xch	a, r10
	shl	a, 1
	xch	a, r10
	
	rolwc	ax, 1
	
	xch	a, r13
	rolc	a, 1
	xch	a, r13

	xch	a, r12
	rolc	a, 1
	xch	a, r12

	rolwc	bc, 1
	
	dec	h
	bnz	$5b

	;; Result is currently in (lsb) r13.r12. c.  b.  (msb),
	;; Move it into           (lsb) r8. r9. r10. r11 (msb).

	mov	a, r13
	mov	r8, a

	mov	a, r12
	mov	r9, a
	
	mov	a, c
	mov	r10, a

	mov	a, b
	mov	r11, a

	ret

END_FUNC	___fixunssfsi

;; ------------------------------------------------------------------------

START_FUNC	___floatsisf
	;; Converts its signed long argument into a floating point.
	;; Argument in [SP+4]..[SP+7].  Result in R8..R11.

	;; Get the argument.
	movw	ax, [SP+4]
	movw	bc, ax
	movw	ax, [SP+6]

	;; Test the sign bit.  If the value is positive then drop into
	;; the unsigned conversion routine.
	bf 	a.7, $2f

	;; If negative convert to positive ...
	movw 	hl, ax
	movw	ax, #0
	subw	ax, bc
	movw	bc, ax
	movw	ax, #0
	sknc
	decw	ax
	subw	ax, hl

	;; If the result is negative then the input was 0x80000000 and
	;; we want to return -0.0, which will not happen if we call
	;; __int_floatunsisf.
	bt	a.7, $1f

	;;  Call the unsigned conversion routine.
	call	$!__int_floatunsisf

	;; Negate the result.
	set1	r11.7

	;; Done.
	ret

1:	;; Return -0.0 aka 0xcf000000

	clrb	a
	mov	r8, a
	mov	r9, a
	mov	r10, a
	mov	a, #0xcf
	mov	r11, a
	ret
	
START_ANOTHER_FUNC	___floatunsisf
	;; Converts its unsigned long argument into a floating point.
	;; Argument in [SP+4]..[SP+7].  Result in R8..R11.

	;; Get the argument.
	movw	ax, [SP+4]
	movw	bc, ax
	movw	ax, [SP+6]

2:	;; Internal entry point from __floatsisf
	;; Input in AX (high) and BC (low)
	.global __int_floatunsisf
__int_floatunsisf:
	
	;; Special case handling for zero.
	cmpw	ax, #0
	bnz	$1f
	movw	ax, bc
	cmpw	ax, #0
	movw	ax, #0
	bnz	$1f

	;; Return 0.0
	movw	r8, ax
	movw	r10, ax
	ret

1:	;; Pre-load the loop count/exponent.
	;; Exponents are biased by 0x80 and we start the loop knowing that
	;; we are going to skip the highest set bit.  Hence the highest value
	;; that we can get for the exponent is 0x1e (bits from input) + 0x80 = 0x9e.
	mov     h, #0x9e

	;; Move bits off the top of AX:BC until we hit a 1 bit.
	;; Decrement the count of remaining bits as we go.

2:	shlw	bc, 1
	rolwc	ax, 1
	bc	$3f
	dec	h
	br	$2b

	;; Ignore the first one bit - it is implicit in the IEEE format.
	;; The count of remaining bits is the exponent.

	;; Assemble the final floating point value.  We have...
	;; before:
	;;   EEEEEEEE MMMMMMMM MMMMMMMM MMMMMMMM xxxxxxxx
	;;       H        A       X        B         C
	;; after:
	;;   0EEEEEEE EMMMMMMM MMMMMMMM MMMMMMMM
	;;      R11      R10      R9       R8

	
3:	shrw	ax, 1
	mov	r10, a
	mov	a, x
	mov	r9, a	

	mov	a, b
	rorc	a, 1	

	;; If the bottom bit of B was set before we shifted it out then we
	;; need to round the result up.  Unless none of the bits in C are set.
	;; In this case we are exactly half-way between two values, and we
	;; round towards an even value.  We round up by increasing the
	;; mantissa by 1.  If this results in a zero mantissa we have to
	;; increment the exponent.  We round down by ignoring the dropped bits.
	
	bnc	$4f
	cmp0	c
	sknz	
	bf	a.0, $4f

5:	;; Round the mantissa up by 1.
	add	a, #1
	addc	r9, #0
	addc	r10, #0
	bf	r10.7, $4f
	inc	h
	clr1	r10.7

4:	mov	r8, a
	mov	a, h
	shr	a, 1
	mov	r11, a
	sknc
	set1	r10.7
	ret

END_ANOTHER_FUNC	___floatunsisf	
END_FUNC		___floatsisf
