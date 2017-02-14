;; Copyright (C) 2001-2017 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; Under Section 7 of GPL version 3, you are granted additional
;; permissions described in the GCC Runtime Library Exception, version
;; 3.1, as published by the Free Software Foundation.
;;
;; You should have received a copy of the GNU General Public License and
;; a copy of the GCC Runtime Library Exception along with this program;
;; see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This code is derived from mulsi3.S, observing that the mstep*16-based
;; multiplications there, from which it is formed, are actually
;; zero-extending; in gcc-speak "umulhisi3".  The difference to *this*
;; function is just a missing top mstep*16 sequence and shifts and 64-bit
;; additions for the high part.  Compared to an implementation based on
;; calling __Mul four times (see default implementation of umul_ppmm in
;; longlong.h), this will complete in a time between a fourth and a third
;; of that, assuming the value-based optimizations don't strike.  If they
;; all strike there (very often) but none here, we still win, though by a
;; lesser margin, due to lesser total overhead.

#define L(x) .x
#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

#ifdef __USER_LABEL_PREFIX__
# define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)
#else
# define SYM(x) x
#endif

	.global SYM(__umulsidi3)
	.type	SYM(__umulsidi3),@function
SYM(__umulsidi3):
#if defined (__CRIS_arch_version) && __CRIS_arch_version >= 10
;; Can't have the mulu.d last on a cache-line, due to a hardware bug.  See
;; the documentation for -mmul-bug-workaround.
;; Not worthwhile to conditionalize here.
	.p2alignw 2,0x050f
	mulu.d $r11,$r10
	ret
	move $mof,$r11
#else
	move.d $r11,$r9
	bound.d $r10,$r9
	cmpu.w 65535,$r9
	bls L(L3)
	move.d $r10,$r12

	move.d $r10,$r13
	movu.w $r11,$r9 ; ab*cd = (a*c)<<32 (a*d + b*c)<<16 + b*d

;; We're called for floating point numbers very often with the "low" 16
;; bits zero, so it's worthwhile to optimize for that.

	beq L(L6)	; d == 0?
	lslq 16,$r13

	beq L(L7)	; b == 0?
	clear.w $r10

	mstep $r9,$r13	; d*b
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13
	mstep $r9,$r13

L(L7):
	test.d $r10
	mstep $r9,$r10	; d*a
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10
	mstep $r9,$r10

;; d*a in $r10, d*b in $r13, ab in $r12 and cd in $r11
;; $r9 = d, need to do b*c and a*c; we can drop d.
;; so $r9 is up for use and we can shift down $r11 as the mstep
;; source for the next mstep-part.

L(L8):
	lsrq 16,$r11
	move.d $r12,$r9
	lslq 16,$r9
	beq L(L9)	; b == 0?
	mstep $r11,$r9

	mstep $r11,$r9	; b*c
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
	mstep $r11,$r9
L(L9):

;; d*a in $r10, d*b in $r13, c*b in $r9, ab in $r12 and c in $r11,
;; need to do a*c.  We want that to end up in $r11, so we shift up $r11 to
;; now use as the destination operand.  We'd need a test insn to update N
;; to do it the other way round.

	lsrq 16,$r12
	lslq 16,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11
	mstep $r12,$r11

;; d*a in $r10, d*b in $r13, c*b in $r9, a*c in $r11 ($r12 free).
;; Need (a*d + b*c)<<16 + b*d into $r10 and
;; a*c + (a*d + b*c)>>16 plus carry from the additions into $r11.

	add.d $r9,$r10	; (a*d + b*c) - may produce a carry.
	scs $r12	; The carry corresponds to bit 16 of $r11.
	lslq 16,$r12
	add.d $r12,$r11	; $r11 = a*c + carry from (a*d + b*c).

#if defined (__CRIS_arch_version) && __CRIS_arch_version >= 8
	swapw $r10
	addu.w $r10,$r11 ; $r11 = a*c + (a*d + b*c) >> 16 including carry.
	clear.w $r10	; $r10 = (a*d + b*c) << 16
#else
	move.d $r10,$r9
	lsrq 16,$r9
	add.d $r9,$r11	; $r11 = a*c + (a*d + b*c) >> 16 including carry.
	lslq 16,$r10	; $r10 = (a*d + b*c) << 16
#endif
	add.d $r13,$r10	; $r10 = (a*d + b*c) << 16 + b*d - may produce a carry.
	scs $r9
	ret
	add.d $r9,$r11	; Last carry added to the high-order 32 bits.

L(L6):
	clear.d $r13
	ba L(L8)
	clear.d $r10

L(L11):
	clear.d $r10
	ret
	clear.d $r11

L(L3):
;; Form the maximum in $r10, by knowing the minimum, $r9.
;; (We don't know which one of $r10 or $r11 it is.)
;; Check if the largest operand is still just 16 bits.

	xor $r9,$r10
	xor $r11,$r10
	cmpu.w 65535,$r10
	bls L(L5)
	movu.w $r9,$r13

;; We have ab*cd = (a*c)<<32 + (a*d + b*c)<<16 + b*d, but c==0
;; so we only need (a*d)<<16 + b*d with d = $r13, ab = $r10.
;; Remember that the upper part of (a*d)<<16 goes into the lower part
;; of $r11 and there may be a carry from adding the low 32 parts.
	beq L(L11)	; d == 0?
	move.d $r10,$r9

	lslq 16,$r9
	beq L(L10)	; b == 0?
	clear.w $r10

	mstep $r13,$r9	; b*d
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
	mstep $r13,$r9
L(L10):
	test.d $r10
	mstep $r13,$r10	; a*d
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	move.d $r10,$r11
	lsrq 16,$r11
	lslq 16,$r10
	add.d $r9,$r10
	scs $r12
	ret
	add.d $r12,$r11

L(L5):
;; We have ab*cd = (a*c)<<32 + (a*d + b*c)<<16 + b*d, but a and c==0
;; so b*d (with min=b=$r13, max=d=$r10) it is.  As it won't overflow the
;; 32-bit part, just set $r11 to 0.

	lslq 16,$r10
	clear.d $r11

	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	mstep $r13,$r10
	ret
	mstep $r13,$r10
#endif
L(Lfe1):
	.size	SYM(__umulsidi3),L(Lfe1)-SYM(__umulsidi3)
