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
;  r11      r10      r9       r8
;  [HL+3]   [HL+2]   [HL+1]   [HL+0]   
;  s1111111 10000000 00000000 00000000	infinity
;  s1111111 1xxxxxxx xxxxxxxx xxxxxxxx	NaN
;  s0000000 00000000 00000000 00000000	zero
;  s0000000 0xxxxxxx xxxxxxxx xxxxxxxx	denormals
;
; Note that CMPtype is "signed char" for rl78
;
	
#include "vregs.h"

#define Z	PSW.6

; External Functions:
;
;  __int_isnan  [HL] -> Z if NaN
;  __int_iszero  [HL] -> Z if zero

START_FUNC	__int_isinf
	;; [HL] points to value, returns Z if it's #Inf

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
	ret

END_FUNC	__int_isinf

#define A_SIGN		[hl+0]	/* byte */
#define A_EXP		[hl+2]	/* word */
#define A_FRAC_L	[hl+4]	/* word */
#define A_FRAC_LH	[hl+5]	/* byte */
#define A_FRAC_H	[hl+6]	/* word or byte */
#define A_FRAC_HH	[hl+7]	/* byte */

#define B_SIGN		[hl+8]
#define B_EXP		[hl+10]
#define B_FRAC_L	[hl+12]
#define B_FRAC_LH	[hl+13]
#define B_FRAC_H	[hl+14]
#define B_FRAC_HH	[hl+15]

START_FUNC	_int_unpack_sf
	;; convert 32-bit SFmode [DE] to 6-byte struct [HL] ("A")

	mov	a, [de+3]
	sar	a, 7
	mov	A_SIGN, a

	movw	ax, [de+2]
	and	a, #0x7f
	shrw	ax, 7
	movw	bc, ax		; remember if the exponent is all zeros
	subw	ax, #127	; exponent is now non-biased
	movw	A_EXP, ax

	movw	ax, [de]
	movw	A_FRAC_L, ax

	mov	a, [de+2]
	and	a, #0x7f
	cmp0	c		; if the exp is all zeros, it's denormal
	skz
	or	a, #0x80
	mov	A_FRAC_H, a

	mov	a, #0
	mov	A_FRAC_HH, a

	;; rounding-bit-shift
	movw	ax, A_FRAC_L
	shlw	ax, 1
	movw	A_FRAC_L, ax
	mov	a, A_FRAC_H
	rolc	a, 1
	mov	A_FRAC_H, a
	mov	a, A_FRAC_HH
	rolc	a, 1
	mov	A_FRAC_HH, a

	ret

END_FUNC	_int_unpack_sf

;	func(SF a,SF b)
;	[SP+4..7]	a
;	[SP+8..11]	b

START_FUNC		___subsf3

	;; a - b => a + (-b)

	;; Note - we cannot just change the sign of B on the stack and
	;; then fall through into __addsf3.  The stack'ed value may be
	;; used again (it was created by our caller after all).  Instead
	;; we have to allocate some stack space of our own, copy A and B,
	;; change the sign of B, call __addsf3, release the allocated stack
	;; and then return.

	subw	sp, #8
	movw	ax, [sp+4+8]
	movw	[sp], ax
	movw	ax, [sp+4+2+8]
	movw	[sp+2], ax
	movw	ax, [sp+4+4+8]
	movw	[sp+4], ax
	mov 	a, [sp+4+6+8]
	mov	[sp+6], a
	mov	a, [sp+4+7+8]
	xor	a, #0x80
	mov	[sp+7], a
	call	$!___addsf3
	addw	sp, #8
	ret
END_FUNC	___subsf3

START_FUNC	___addsf3

	;; if (isnan(a)) return a
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_isnan
	bnz	$1f
ret_a:
	movw	ax, [sp+4]
	movw	r8, ax
	movw	ax, [sp+6]
	movw	r10, ax
	ret

1:	;; if (isnan (b)) return b;
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_isnan
	bnz	$2f
ret_b:
	movw	ax, [sp+8]
	movw	r8, ax
	movw	ax, [sp+10]
	movw	r10, ax
	ret

2:	;; if (isinf (a))
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	$!__int_isinf
	bnz	$3f

	;;   if (isinf (b) && a->sign != b->sign) return NaN
	
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	$!__int_isinf
	bnz	$ret_a

	mov	a, [sp+7]
	mov	h, a
	mov	a, [sp+11]
	xor	a, h
	bf	a.7, $ret_a

	movw	r8,  #0x0001
	movw	r10, #0x7f80
	ret

3:	;; if (isinf (b)) return b;
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	$!__int_isinf
	bz	$ret_b

	;; if (iszero (b))
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_iszero
	bnz	$4f

	;;   if (iszero (a))
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_iszero
	bnz	$ret_a

	movw	ax, [sp+4]
	movw	r8, ax
	mov	a, [sp+7]
	mov	h, a
	movw	ax, [sp+10]
	and	a, h
	movw	r10, ax
	ret

4:	;; if (iszero (a)) return b;
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_iszero
	bz	$ret_b

; Normalize the two numbers relative to each other.  At this point,
; we need the numbers converted to their "unpacked" format.

	subw	sp, #16		; Save room for two unpacked values.

	movw	ax, sp
	movw	hl, ax
	addw	ax, #16+4
	movw	de, ax
	call	$!_int_unpack_sf

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	addw	ax, #16+8-8
	movw	de, ax
	call	$!_int_unpack_sf

	movw	ax, sp
	movw	hl, ax

	;; diff = a.exponent - b.exponent
	movw	ax, B_EXP	; sign/exponent word
	movw	bc, ax
	movw	ax, A_EXP	; sign/exponent word
	
	subw	ax, bc		; a = a.exp - b.exp
	movw	de, ax		; d = sdiff

	;;  if (diff < 0) diff = -diff
	bf	a.7, $1f
	xor	a, #0xff
	xor	r_0, #0xff	; x
	incw	ax		; a = diff
1:	
	;; if (diff >= 23) zero the smaller one
	cmpw	ax, #24
	bc	$.L661		; if a < 23 goto 661

	;; zero out the smaller one

	movw	ax, de
	bt	a.7, $1f	; if sdiff < 0 (a_exp < b_exp) goto 1f
	;; "zero out" b
	movw	ax, A_EXP
	movw	B_EXP, ax
	movw	ax, #0
	movw	B_FRAC_L, ax
	movw	B_FRAC_H, ax
	br	$5f
1:	
	;; "zero out" a
	movw	ax, B_EXP
	movw	A_EXP, ax
	movw	ax, #0
	movw	A_FRAC_L, ax
	movw	A_FRAC_H, ax

	br	$5f
.L661:
	;; shift the smaller one so they have the same exponents
1:	
	movw	ax, de
	bt	a.7, $1f
	cmpw	ax, #0		; sdiff > 0
	bnh	$1f		; if (sdiff <= 0) goto 1f

	decw	de
	incw	B_EXP		; because it's [HL+byte]

	movw	ax, B_FRAC_H
	shrw	ax, 1
	movw	B_FRAC_H, ax
	mov	a, B_FRAC_LH
	rorc	a, 1
	mov	B_FRAC_LH, a
	mov	a, B_FRAC_L
	rorc	a, 1
	mov	B_FRAC_L, a
	
	br	$1b
1:	
	movw	ax, de
	bf	a.7, $1f

	incw	de
	incw	A_EXP		; because it's [HL+byte]

	movw	ax, A_FRAC_H
	shrw	ax, 1
	movw	A_FRAC_H, ax
	mov	a, A_FRAC_LH
	rorc	a, 1
	mov	A_FRAC_LH, a
	mov	a, A_FRAC_L
	rorc	a, 1
	mov	A_FRAC_L, a
	
	br	$1b
1:	

5:	;; At this point, A and B have the same exponent.

	mov	a, A_SIGN
	cmp	a, B_SIGN
	bnz	$1f

	;; Same sign, just add.
	movw	ax, A_FRAC_L
	addw	ax, B_FRAC_L
	movw	A_FRAC_L, ax
	mov	a, A_FRAC_H
	addc	a, B_FRAC_H
	mov	A_FRAC_H, a
	mov	a, A_FRAC_HH
	addc	a, B_FRAC_HH
	mov	A_FRAC_HH, a

	br	$.L728

1:	;; Signs differ - A has A_SIGN still.
	bf	a.7, $.L696

	;; A is negative, do B-A
	movw	ax, B_FRAC_L
	subw	ax, A_FRAC_L
	movw	A_FRAC_L, ax
	mov	a, B_FRAC_H
	subc	a, A_FRAC_H
	mov	A_FRAC_H, a
	mov	a, B_FRAC_HH
	subc	a, A_FRAC_HH
	mov	A_FRAC_HH, a

	br	$.L698
.L696:
	;; B is negative, do A-B
	movw	ax, A_FRAC_L
	subw	ax, B_FRAC_L
	movw	A_FRAC_L, ax
	mov	a, A_FRAC_H
	subc	a, B_FRAC_H
	mov	A_FRAC_H, a
	mov	a, A_FRAC_HH
	subc	a, B_FRAC_HH
	mov	A_FRAC_HH, a

.L698:	
	;; A is still A_FRAC_HH
	bt	a.7, $.L706
	
	;; subtraction was positive
	mov	a, #0
	mov	A_SIGN, a
	br	$.L712

.L706:
	;; subtraction was negative
	mov	a, #0xff
	mov	A_SIGN, a

	;; This negates A_FRAC
	mov	a, A_FRAC_L
	xor	a, #0xff		; XOR doesn't mess with carry
	add	a, #1			; INC doesn't set the carry
	mov	A_FRAC_L, a
	mov	a, A_FRAC_LH
	xor	a, #0xff
	addc	a, #0
	mov	A_FRAC_LH, a
	mov	a, A_FRAC_H
	xor	a, #0xff
	addc	a, #0
	mov	A_FRAC_H, a
	mov	a, A_FRAC_HH
	xor	a, #0xff
	addc	a, #0
	mov	A_FRAC_HH, a

.L712:
	;; Renormalize the subtraction

	mov	a, A_FRAC_L
	or	a, A_FRAC_LH
	or	a, A_FRAC_H
	or	a, A_FRAC_HH
	bz	$.L728

	;; Mantissa is not zero, left shift until the MSB is in the
	;; right place
1:
	movw	ax, A_FRAC_H
	cmpw	ax, #0x0200
	bnc	$.L728

	decw	A_EXP

	movw	ax, A_FRAC_L
	shlw	ax, 1
	movw	A_FRAC_L, ax
	movw	ax, A_FRAC_H
	rolwc	ax, 1
	movw	A_FRAC_H, ax
	br	$1b

.L728:
	;; normalize A and pack it

	movw	ax, A_FRAC_H
	cmpw	ax, #0x01ff
	bnh	$1f
	;; overflow in the mantissa; adjust
	movw	ax, A_FRAC_H
	shrw	ax, 1
	movw	A_FRAC_H, ax
	mov	a, A_FRAC_LH
	rorc	a, 1
	mov	A_FRAC_LH, a
	mov	a, A_FRAC_L
	rorc	a, 1
	mov	A_FRAC_L, a
	incw	A_EXP
1:	

	call	$!__rl78_int_pack_a_r8
	addw	sp, #16
	ret

END_FUNC	___addsf3

START_FUNC	__rl78_int_pack_a_r8
	;; pack A to R8
	movw	ax, A_EXP
	addw	ax, #126	; not 127, we want the "bt/bf" test to check for denormals

	bf	a.7, $1f
	;; make a denormal
2:
	movw	bc, ax
	movw	ax, A_FRAC_H
	shrw	ax, 1
	movw	A_FRAC_H, ax
	mov	a, A_FRAC_LH
	rorc	a, 1
	mov	A_FRAC_LH, a
	mov	a, A_FRAC_L
	rorc	a, 1
	mov	A_FRAC_L, a
	movw	ax, bc
	incw	ax
	bt	a.7, $2b
	decw	ax
1:	
	incw	ax		; now it's as if we added 127
	movw	A_EXP, ax

	cmpw	ax, #0xfe
	bnh	$1f
	;; store #Inf instead
	mov	a, A_SIGN
	or	a, #0x7f
	mov	x, #0x80
	movw	r10, ax
	movw	r8, #0
	ret

1:
	bf	a.7, $1f	; note AX has EXP at top of loop
	;; underflow, denormal?
	movw	ax, A_FRAC_H
	shrw	ax, 1
	movw	A_FRAC_H, ax
	mov	a, A_FRAC_LH
	rorc	a, 1
	movw	A_FRAC_LH, ax
	mov	a, A_FRAC_L
	rorc	a, 1
	movw	A_FRAC_L, ax
	incw	A_EXP
	movw	ax, A_EXP
	br	$1b

1:
	;; undo the rounding-bit-shift
	mov	a, A_FRAC_L
	bf	a.0, $1f
	;; round up
	movw	ax, A_FRAC_L
	addw	ax, #1
	movw	A_FRAC_L, ax
	bnc	$1f
	incw	A_FRAC_H

	;; If the rounding set the bit beyond the end of the fraction, increment the exponent.
	mov	a, A_FRAC_HH
	bf	a.1, $1f
	incw	A_EXP
	
1:	
	movw	ax, A_FRAC_H
	shrw	ax, 1
	movw	A_FRAC_H, ax
	mov	a, A_FRAC_LH
	rorc	a, 1
	mov	A_FRAC_LH, a
	mov	a, A_FRAC_L
	rorc	a, 1
	mov	A_FRAC_L, a

	movw	ax, A_FRAC_L
	movw	r8, ax

	or	a, x
	or	a, A_FRAC_H
	or	a, A_FRAC_HH
	bnz	$1f
	movw	ax, #0
	movw	A_EXP, ax
1:	
	mov	a, A_FRAC_H
	and	a, #0x7f
	mov	b, a
	mov	a, A_EXP
	shl	a, 7
	or	a, b
	mov	r10, a

	mov	a, A_SIGN
	and	a, #0x80
	mov	b, a
	mov	a, A_EXP
	shr	a, 1
	or	a, b
	mov	r11, a

	ret
END_FUNC	__rl78_int_pack_a_r8

START_FUNC	___mulsf3

	;; if (isnan(a)) return a
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_isnan
	bnz	$1f
mret_a:
	movw	ax, [sp+4]
	movw	r8, ax
	mov	a, [sp+11]
	and	a, #0x80
	mov	b, a
	movw	ax, [sp+6]
	xor	a, b		; sign is always a ^ b
	movw	r10, ax
	ret
1:	
	;; if (isnan (b)) return b;
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_isnan
	bnz	$1f
mret_b:
	movw	ax, [sp+8]
	movw	r8, ax
	mov	a, [sp+7]
	and	a, #0x80
	mov	b, a
	movw	ax, [sp+10]
	xor	a, b		; sign is always a ^ b
	movw	r10, ax
	ret
1:	
	;; if (isinf (a)) return (b==0) ? nan : a
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	$!__int_isinf
	bnz	$.L805

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_iszero
	bnz	$mret_a

	movw	r8,  #0x0001	; return NaN
	movw	r10, #0x7f80
	ret

.L805:	
	;; if (isinf (b)) return (a==0) ? nan : b
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	$!__int_isinf
	bnz	$.L814

	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_iszero
	bnz	$mret_b

	movw	r8,  #0x0001	; return NaN
	movw	r10, #0x7f80
	ret

.L814:
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_iszero
	bz	$mret_a
	
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_iszero
	bz	$mret_b

	;; at this point, we're doing the multiplication.

	subw	sp, #16	; save room for two unpacked values

	movw	ax, sp
	movw	hl, ax
	addw	ax, #16+4
	movw	de, ax
	call	$!_int_unpack_sf

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	addw	ax, #16+8-8
	movw	de, ax
	call	$!_int_unpack_sf

	movw	ax, sp
	movw	hl, ax

	;; multiply SI a.FRAC * SI b.FRAC to DI r8

	subw	sp, #16
	movw	ax, A_FRAC_L
	movw	[sp+0], ax
	movw	ax, A_FRAC_H
	movw	[sp+2], ax

	movw	ax, B_FRAC_L
	movw	[sp+8], ax
	movw	ax, B_FRAC_H
	movw	[sp+10], ax

	movw	ax, #0
	movw	[sp+4], ax
	movw	[sp+6], ax
	movw	[sp+12], ax
	movw	[sp+14], ax

	call	!!___muldi3	; MTMPa * MTMPb -> R8..R15
	addw	sp, #16

	movw	ax, sp
	movw	hl, ax

	;;  add the exponents together
	movw	ax, A_EXP
	addw	ax, B_EXP
	movw	bc, ax		; exponent in BC

	;; now, re-normalize the DI value in R8..R15 to have the
	;; MSB in the "right" place, adjusting BC as we shift it.

	;; The value will normally be in this range:
	;; R15              R8
	;; 0001_0000_0000_0000
	;; 0003_ffff_fc00_0001

	;; so to speed it up, we normalize to:
	;; 0001_xxxx_xxxx_xxxx
	;; then extract the bytes we want (r11-r14)

1:
	mov	a, r15
	cmp0	a
	bnz	$2f
	mov	a, r14
	and	a, #0xfe
	bz	$1f
2:	
	;; shift right, inc exponent
	movw	ax, r14
	shrw	ax, 1
	movw	r14, ax
	mov	a, r13
	rorc	a, 1
	mov	r13, a
	mov	a, r12
	rorc	a, 1
	mov	r12, a
	mov	a, r11
	rorc	a, 1
	mov	r11, a
	;; we don't care about r8/r9/r10 if we're shifting this way
	incw	bc
	br	$1b
1:	
	mov	a, r15
	or	a, r14
	bnz	$1f
	;; shift left, dec exponent
	movw	ax, r8
	shlw	ax, 1
	movw	r8, ax
	movw	ax, r10
	rolwc	ax, 1
	movw	r10, ax
	movw	ax, r12
	rolwc	ax, 1
	movw	r12, ax
	movw	ax, r14
	rolwc	ax, 1
	movw	r14, ax
	decw	bc
	br	$1b
1:
	;; at this point, FRAC is in R11..R14 and EXP is in BC
	movw	ax, bc
	movw	A_EXP, ax

	mov	a, r11
	mov	A_FRAC_L, a
	mov	a, r12
	mov	A_FRAC_LH, a
	mov	a, r13
	mov	A_FRAC_H, a
	mov	a, r14
	mov	A_FRAC_HH, a

	mov	a, A_SIGN
	xor	a, B_SIGN
	mov	A_SIGN, a

	call	$!__rl78_int_pack_a_r8

	addw	sp, #16
	ret

END_FUNC		___mulsf3
	
START_FUNC		___divsf3

	;; if (isnan(a)) return a
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_isnan
	bnz	$1f
dret_a:
	movw	ax, [sp+4]
	movw	r8, ax
	mov	a, [sp+11]
	and	a, #0x80
	mov	b, a
	movw	ax, [sp+6]
	xor	a, b		; sign is always a ^ b
	movw	r10, ax
	ret
1:	
	;; if (isnan (b)) return b;
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_isnan
	bnz	$1f
dret_b:
	movw	ax, [sp+8]
	movw	r8, ax
	mov	a, [sp+7]
	and	a, #0x80
	mov	b, a
	movw	ax, [sp+10]
	xor	a, b		; sign is always a ^ b
	movw	r10, ax
	ret
1:	

	;; if (isinf (a)) return isinf(b) ? nan : a

	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	$!__int_isinf
	bnz	$1f
	
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	$!__int_isinf
	bnz	$dret_a
dret_nan:	
	movw	r8,  #0x0001	; return NaN
	movw	r10, #0x7f80
	ret
	
1:	

	;; if (iszero (a)) return iszero(b) ? nan : a

	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	call	!!__int_iszero
	bnz	$1f
	
	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_iszero
	bnz	$dret_a
	br	$dret_nan
	
1:	
	;; if (isinf (b)) return 0

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	$!__int_isinf
	bnz	$1f
	
	mov	a, [sp+7]
	mov	b, a
	mov	a, [sp+11]
	xor	a, b
	and	a, #0x80
	mov	r11, a
	movw	r8, #0
	mov	r10, #0
	ret
	
1:	
	;; if (iszero (b)) return Inf

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	call	!!__int_iszero
	bnz	$1f

	mov	a, [sp+7]
	mov	b, a
	mov	a, [sp+11]
	xor	a, b
	or	a, #0x7f
	mov	r11, a
	movw	r8, #0
	mov	r10, #0x80
	ret
1:	

	;; at this point, we're doing the division.  Normalized
	;; mantissas look like:
	;; 01.xx.xx.xx
	;; so we divide:
	;; 01.xx.xx.xx.00.00.00.00
	;; by          01.xx.xx.xx
	;; to get approx 00.80.00.00.00 to 01.ff.ff.ff.00


	subw	sp, #16	; save room for two unpacked values

	movw	ax, sp
	movw	hl, ax
	addw	ax, #16+4
	movw	de, ax
	call	$!_int_unpack_sf

	movw	ax, sp
	addw	ax, #8
	movw	hl, ax
	addw	ax, #16+8-8
	movw	de, ax
	call	$!_int_unpack_sf

	movw	ax, sp
	movw	hl, ax

	;; divide DI a.FRAC / SI b.FRAC to DI r8

	subw	sp, #16
	movw	ax, A_FRAC_L
	movw	[sp+4], ax
	movw	ax, A_FRAC_H
	movw	[sp+6], ax

	movw	ax, B_FRAC_L
	movw	[sp+8], ax
	movw	ax, B_FRAC_H
	movw	[sp+10], ax

	movw	ax, #0
	movw	[sp+0], ax
	movw	[sp+2], ax
	movw	[sp+12], ax
	movw	[sp+14], ax

	call	!!___divdi3	; MTMPa / MTMPb -> R8..R15
	addw	sp, #16

	movw	ax, sp
	movw	hl, ax

	;;  subtract the exponents A - B
	movw	ax, A_EXP
	subw	ax, B_EXP
	movw	bc, ax		; exponent in BC

	;; now, re-normalize the DI value in R8..R15 to have the
	;; MSB in the "right" place, adjusting BC as we shift it.

	;; The value will normally be in this range:
	;; R15              R8
	;; 0000_0000_8000_0000
	;; 0000_0001_ffff_ff00

	;; so to speed it up, we normalize to:
	;; 0000_0001_xxxx_xxxx
	;; then extract the bytes we want (r9-r12)

1:
	movw	ax, r14
	cmpw	ax, #0
	bnz	$2f
	movw	ax, r12
	cmpw	ax, #1
	bnh	$1f
2:	
	;; shift right, inc exponent
	movw	ax, r14
	shrw	ax, 1
	movw	r14, ax
	mov	a, r13
	rorc	a, 1
	mov	r13, a
	mov	a, r12
	rorc	a, 1
	mov	r12, a
	mov	a, r11
	rorc	a, 1
	mov	r11, a
	mov	a, r10
	rorc	a, 1
	mov	r10, a
	mov	a, r9
	rorc	a, 1
	mov	r9, a
	mov	a, r8
	rorc	a, 1
	mov	r8, a

	incw	bc
	br	$1b
1:	
	;; the previous loop leaves r15.r13 zero
	mov	a, r12
	cmp0	a
	bnz	$1f
	;; shift left, dec exponent
	movw	ax, r8
	shlw	ax, 1
	movw	r8, ax
	movw	ax, r10
	rolwc	ax, 1
	movw	r10, ax
	movw	ax, r12
	rolwc	ax, 1
	movw	r12, ax
	;; don't need to do r14
	decw	bc
	br	$1b
1:
	;; at this point, FRAC is in R8..R11 and EXP is in BC
	movw	ax, bc
	movw	A_EXP, ax

	mov	a, r9
	mov	A_FRAC_L, a
	mov	a, r10
	mov	A_FRAC_LH, a
	mov	a, r11
	mov	A_FRAC_H, a
	mov	a, r12
	mov	A_FRAC_HH, a

	mov	a, A_SIGN
	xor	a, B_SIGN
	mov	A_SIGN, a

	call	$!__rl78_int_pack_a_r8

	addw	sp, #16
	ret
	
END_FUNC	___divsf3
