; libgcc1 routines for Synopsys DesignWare ARC cpu.

/* Copyright (C) 1995-2025 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


 /* ANSI concatenation macros.  */

 #define CONCAT1(a, b) CONCAT2(a, b)
 #define CONCAT2(a, b) a ## b

 /* Use the right prefix for global labels.  */

 #define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

#ifndef WORKING_ASSEMBLER
#define abs_l abs
#define asl_l asl
#define mov_l mov
#endif

#define FUNC(X)         .type SYM(X),@function
#define HIDDEN_FUNC(X)	FUNC(X)` .hidden X
#define ENDFUNC0(X)     .Lfe_##X: .size X,.Lfe_##X-X
#define ENDFUNC(X)      ENDFUNC0(X)

#ifdef __ARC_RF16__
	/* Use object attributes to inform other tools this file is
	safe for RF16 configuration.  */
	.arc_attribute Tag_ARC_ABI_rf16, 1
#endif

#ifdef  L_mulsi3
	.section .text
	.align 4

	.global SYM(__mulsi3)
SYM(__mulsi3):

/* This the simple version.

  while (a)
    {
      if (a & 1)
	r += b;
      a >>= 1;
      b <<= 1;
    }
*/

#if defined (__ARC_MUL64__)
	FUNC(__mulsi3)
	mulu64 r0,r1
	j_s.d [blink]
	mov_s r0,mlo
	ENDFUNC(__mulsi3)
#elif defined (__ARC_MPY__)
	HIDDEN_FUNC(__mulsi3)
	mpyu	r0,r0,r1
	nop_s
	j_s	[blink]
	ENDFUNC(__mulsi3)
#elif defined (__ARC_NORM__)
	FUNC(__mulsi3)
	norm.f	r2,r0
	rsub	lp_count,r2,31
	mov.mi	lp_count,32
	mov_s	r2,r0
	mov_s	r0,0
	lpnz	@.Lend		; loop is aligned
	lsr.f	r2,r2
	add.cs	r0,r0,r1
	add_s	r1,r1,r1
.Lend:	j_s [blink]
	ENDFUNC(__mulsi3)
#elif !defined (__OPTIMIZE_SIZE__) && defined (__ARC_BARREL_SHIFTER__)
	/* Up to 3.5 times faster than the simpler code below, but larger.  */
	FUNC(__mulsi3)
	ror.f	r2,r0,4
	mov_s	r0,0
	add3.mi	r0,r0,r1
	asl.f	r2,r2,2
	add2.cs	r0,r0,r1
	jeq_s	[blink]
.Loop:
	add1.mi	r0,r0,r1
	asl.f	r2,r2,2
	add.cs	r0,r0,r1
	asl_s	r1,r1,4
	ror.f	r2,r2,8
	add3.mi	r0,r0,r1
	asl.f	r2,r2,2
	bne.d	.Loop
	add2.cs	r0,r0,r1
	j_s	[blink]
	ENDFUNC(__mulsi3)
#elif !defined (__OPTIMIZE_SIZE__) /* __ARC601__ */
	FUNC(__mulsi3)
	lsr.f r2,r0
	mov_s r0,0
	mov_s r3,0
	add.cs r0,r0,r1
.Loop:
	lsr.f r2,r2
	add1.cs r0,r0,r1
	lsr.f r2,r2
	add2.cs r0,r0,r1
	lsr.f r2,r2
	add3.cs r0,r0,r1
	bne.d .Loop
	add3 r1,r3,r1
	j_s	[blink]
	ENDFUNC(__mulsi3)
#else
/********************************************************/
	FUNC(__mulsi3)
	mov_s r2,0		; Accumulate result here.
.Lloop:
	bbit0 r0,0,@.Ly
	add_s r2,r2,r1		; r += b
.Ly:
	lsr_s r0,r0		; a >>= 1
	asl_s r1,r1		; b <<= 1
	brne_s r0,0,@.Lloop
.Ldone:
	j_s.d [blink]
	mov_s r0,r2
	ENDFUNC(__mulsi3)
/********************************************************/
#endif

#endif /* L_mulsi3 */

#ifdef  L_umulsidi3
	.section .text
	.align 4

	.global SYM(__umulsidi3)
SYM(__umulsidi3):
	HIDDEN_FUNC(__umulsidi3)
/* We need ARC700 /ARC_MUL64 definitions of __umulsidi3 / __umulsi3_highpart
   in case some code has been compiled without multiply support enabled,
   but linked with the multiply-support enabled libraries.
   For ARC601 (i.e. without a barrel shifter), we also use umuldisi3 as our
   umulsi3_highpart implementation; the use of the latter label doesn't
   actually benefit ARC601 platforms, but is useful when ARC601 code is linked
   against other libraries.  */
#if defined (__ARC_MPY__) || defined (__ARC_MUL64__) \
	|| !defined (__ARC_BARREL_SHIFTER__)
	.global SYM(__umulsi3_highpart)
SYM(__umulsi3_highpart):
	HIDDEN_FUNC(__umulsi3_highpart)
#endif

/* This the simple version.

  while (a)
    {
      if (a & 1)
	r += b;
      a >>= 1;
      b <<= 1;
    }
*/
#include "ieee-754/arc-ieee-754.h"

#ifdef __ARC_MPY__
	mov_s	r12,DBL0L
	mpyu	DBL0L,r12,DBL0H
	j_s.d	[blink]
	MPYHU	DBL0H,r12,DBL0H
#elif defined (__ARC_MUL64__)
/* Likewise for __ARC_MUL64__ */
	mulu64 r0,r1
	mov_s DBL0L,mlo
	j_s.d [blink]
	mov_s DBL0H,mhi
#else /* !__ARC_MPY__ && !__ARC_MUL64__ */
/* Although it might look tempting to extend this to handle muldi3,
   using mulsi3 twice with 2.25 cycles per 32 bit add is faster
   than one loop with 3 or four cycles per 32 bit add.  */
	asl.f r12,0		; Top part of b.
	mov_s r2,0		; Accumulate result here.
	bbit1.d r0,0,@.Ladd
	mov_s r3,0
.Llooptst:
	rlc r12,r12
	breq r0,0,@.Ldone	; while (a)
.Lloop:
	asl.f r1,r1		; b <<= 1
	bbit0.d r0,1,@.Llooptst
	lsr r0,r0		; a >>= 1
	rlc r12,r12
.Ladd:
	add.f r3,r3,r1	; r += b
	brne.d r0,0,@.Lloop	; while (a);
	adc   r2,r2,r12
.Ldone:
	mov_s DBL0L,r3
	j_s.d [blink]
	mov DBL0H,r2
#endif /* !__ARC_MPY__*/
	ENDFUNC(__umulsidi3)
#if defined (__ARC_MPY__) || defined (__ARC_MUL64__) \
	|| !defined (__ARC_BARREL_SHIFTER__)
	ENDFUNC(__umulsi3_highpart)
#endif
#endif /* L_umulsidi3 */

#ifndef __ARC_RF16__
#ifdef L_muldi3
	.section .text
	.align 4
	.global SYM(__muldi3)
SYM(__muldi3):
#ifdef __LITTLE_ENDIAN__
        push_s blink
        mov_s r4,r3     ;4
        mov_s r5,r2     ;4
        mov_s r9,r0     ;4
        mov_s r8,r1     ;4
        bl.d @__umulsidi3
        mov_s r1,r2     ;4
        mov_s r6,r0     ;4
        mov_s r7,r1     ;4
        mov_s r0,r9     ;4
        bl.d @__mulsi3
        mov_s r1,r4     ;4
        mov_s r4,r0     ;4
        mov_s r1,r8     ;4
        bl.d @__mulsi3
        mov_s r0,r5     ;4
        pop_s blink
        add_s r0,r0,r4 ;2
        add r1,r0,r7
        j_s.d [blink]
        mov_s r0,r6     ;4
#else
	push_s  blink
	mov_s   r5,r3
	mov_s   r9,r2
	mov_s   r4,r1
	mov_s   r8,r0
	mov_s   r0,r1
	bl.d 	@__umulsidi3
	mov_s   r1,r3
	mov_s   r7,r0
	mov_s   r6,r1
	mov_s   r0,r4
	bl.d    @__mulsi3
	mov_s   r1,r9
	mov_s   r4,r0
	mov_s   r1,r8
	bl.d    @__mulsi3
	mov_s   r0,r5
	pop_s   blink
	add_s   r0,r0,r4
	add_s   r0,r0,r7
	j_s.d   [blink]
	mov_s   r1,r6
#endif /* __LITTLE_ENDIAN__ */
ENDFUNC(__muldi3)
#endif /* L_muldi3 */
#endif /* !__ARC_RF16__ */

#ifdef  L_umulsi3_highpart
#include "ieee-754/arc-ieee-754.h"
/* For use without a barrel shifter, and for ARC700 / ARC_MUL64, the
   mulsidi3 algorithms above look better, so for these, there is an
   extra label up there.  */
#if !defined (__ARC_MPY__) && !defined (__ARC_MUL64__) \
	&& defined (__ARC_BARREL_SHIFTER__)
	.global SYM(__umulsi3_highpart)
SYM(__umulsi3_highpart):
	HIDDEN_FUNC(__umulsi3_highpart)
	mov_s r2,0
	mov_s r3,32
.Loop:
	lsr.f r0,r0
	add.cs.f r2,r2,r1
	sub_s r3,r3,1
	brne.d r0,0,.Loop
	rrc r2,r2
	j_s.d	[blink]
/* Make the result register peephole-compatible with mulsidi3.  */
	lsr DBL0H,r2,r3
	ENDFUNC(__umulsi3_highpart)
#endif /* !__ARC_MPY__  && __ARC_BARREL_SHIFTER__ */
#endif /* L_umulsi3_highpart */

#ifdef L_divmod_tools

; Utilities used by all routines.

	.section .text

/*
unsigned long
udivmodsi4(int modwanted, unsigned long num, unsigned long den)
{
  unsigned long bit = 1;
  unsigned long res = 0;

  while (den < num && bit && !(den & (1L<<31)))
    {
      den <<=1;
      bit <<=1;
    }
  while (bit)
    {
      if (num >= den)
	{
	  num -= den;
	  res |= bit;
	}
      bit >>=1;
      den >>=1;
    }
  if (modwanted) return num;
  return res;
}
*/

; inputs: r0 = numerator, r1 = denominator
; outputs: r0 = quotient, r1 = remainder, r2/r3 trashed

	.balign 4
	.global SYM(__udivmodsi4)
	FUNC(__udivmodsi4)
SYM(__udivmodsi4):

#if defined (__ARC_EA__)
/* Normalize divisor and divident, and then use the appropriate number of
   divaw (the number of result bits, or one more) to produce the result.
   There are some special conditions that need to be tested:
   - We can only directly normalize unsigned numbers that fit in 31 bit.  For
     the divisor, we test early on that it is not 'negative'.
   - divaw can't corrrectly process a divident that is larger than the divisor.
     We handle this be checking that the divident prior to normalization is
     not larger than the normalized divisor.  As we then already know then
     that the divisor fits 31 bit, this check also makes sure that the
     divident fits.
   - ordinary normalization of the divident could make it larger than the
     normalized divisor, which again would be unsuitable for divaw.
     Thus, we want to shift left the divident by one less, except that we
     want to leave it alone if it is already 31 bit.  To this end, we
     double the input to norm with adds.
   - If the divident has less bits than the divisor, that would leave us
     with a negative number of divaw to execute.  Although we could use a
     conditional loop to avoid excess divaw, and then the quotient could
     be extracted correctly as there'd be more than enough zero bits, the
     remainder would be shifted left too far, requiring a conditional shift
     right.  The cost of that shift and the possible mispredict on the
     conditional loop cost as much as putting in an early check for a zero
     result.  */
	bmsk	r3,r0,29
	brne.d	r3,r0,.Large_dividend
	norm.f	r2,r1
	brlo	r0,r1,.Lret0
	norm	r3,r0
	asl_s	r1,r1,r2
	sub_s	r3,r3,1
	asl_l	r0,r0,r3	; not short to keep loop aligned
	sub	lp_count,r2,r3
	lp	.Ldiv_end
	divaw	r0,r0,r1
.Ldiv_end:sub_s	r3,r2,1
	lsr	r1,r0,r2
	j_s.d	[blink]
	bmsk	r0,r0,r3

	.balign 4
.Large_dividend:
	bmi	.Ltrivial
	asl_s	r1,r1,r2
	mov_s	r3,0
	sub1.f	r4,r0,r1
	mov.lo	r4,r0
	mov.hs	r3,2
	cmp	r4,r1
	sub.hs	r4,r4,r1
	add.hs	r3,r3,1
	mov.f	lp_count,r2
	lpne	.Ldiv_end2
	divaw	r4,r4,r1
.Ldiv_end2:asl	r0,r3,r2
	lsr	r1,r4,r2
	sub_s	r2,r2,1
	bmsk	r4,r4,r2
	j_s.d	[blink]
	or.ne	r0,r0,r4

.Lret0:
	mov_s	r1,r0
	j_s.d	[blink]
	mov_l	r0,0
	.balign	4
.Ltrivial:
	sub.f	r1,r0,r1
	mov.c	r1,r0
	mov_s	r0,1
	j_s.d	[blink]
	mov.c	r0,0
#elif !defined (__OPTIMIZE_SIZE__) && !defined (__ARC_RF16__)
#if defined (__ARC_NORM__) && defined (__ARC_BARREL_SHIFTER__)
	lsr_s r2,r0
	brhs.d r1,r2,.Lret0_3
	norm r2,r2
	norm r3,r1
	sub_s r3,r3,r2
	asl_s r1,r1,r3
	sub1.f 0,r0,r1
	lsr.cs r1,r1,1
	sbc r2,r3,0
	sub1 r0,r0,r1
	cmp_s r0,r1
	mov.f lp_count,r2
#else /* ! __ARC_NORM__ */
	lsr_s r2,r0
	brhs.d r1,r2,.Lret0_3
	mov lp_count,32
.Lloop1:
	asl_s r1,r1		; den <<= 1
	brls.d r1,r2,@.Lloop1
	sub lp_count,lp_count,1
	sub_s r0,r0,r1
	lsr_s r1,r1
	cmp_s r0,r1
	xor.f r2,lp_count,31
#if !defined (__ARCEM__) && !defined (__ARCHS__)
	mov_s lp_count,r2
#else
	mov lp_count,r2
	nop_s
#endif /* !__ARCEM__ && !__ARCHS__ */
#endif /* !__ARC_NORM__ */
	sub.cc r0,r0,r1
	mov_s r3,3
	sbc r3,r3,0
#if defined (__ARC_BARREL_SHIFTER__)
	asl_s r3,r3,r2
	rsub r1,r1,1
	lpne @.Lloop2_end
	add1.f r0,r1,r0
	sub.cc r0,r0,r1
.Lloop2_end:
	lsr r1,r0,r2
#else
	rsub r1,r1,1
	lpne @.Lloop2_end
	asl_s r3,r3
	add1.f r0,r1,r0
	sub.cc r0,r0,r1
.Lloop2_end:
	lsr_s r1,r0
	lsr.f lp_count,r2
	mov.cc r1,r0
	lpnz 1f
	lsr_s r1,r1
	lsr_s r1,r1
1:
#endif
	bmsk r0,r0,r2
	bclr r0,r0,r2
	j_s.d [blink]
	or_s r0,r0,r3
.Lret0_3:
#if 0 /* Slightly shorter, but slower.  */
	lp .Loop3_end
	brhi.d r1,r0,.Loop3_end
	sub_s r0,r0,r1
.Loop3_end
	add_s r1,r1,r0
	j_s.d [blink]
	rsub r0,lp_count,32-1
#else
	mov_s r4,r1
	sub.f r1,r0,r1
	sbc r0,r0,r0
	sub.cc.f r1,r1,r4
	sbc r0,r0,0
	sub.cc.f r1,r1,r4
	sbc r0,r0,-3
	j_s.d [blink]
	add.cs r1,r1,r4
#endif
#else /* Arctangent-A5 */
	breq_s r1,0,@.Ldivmodend
	mov_s r2,1		; bit = 1
	mov_s r3,0		; res = 0
.Lloop1:
	brhs r1,r0,@.Lloop2
	bbit1 r1,31,@.Lloop2
	asl_s r1,r1		; den <<= 1
	b.d @.Lloop1
	asl_s r2,r2		; bit <<= 1
.Lloop2:
	brlo r0,r1,@.Lshiftdown
	sub_s r0,r0,r1		; num -= den
	or_s r3,r3,r2		; res |= bit
.Lshiftdown:
	lsr_s r2,r2		; bit >>= 1
	lsr_s r1,r1		; den >>= 1
	brne_s r2,0,@.Lloop2
.Ldivmodend:
	mov_s r1,r0		; r1 = mod
	j.d [blink]
	mov_s r0,r3		; r0 = res
/******************************************************/
#endif
	ENDFUNC(__udivmodsi4)

#endif

#ifdef  L_udivsi3
	.section .text
	.align 4

	.global SYM(__udivsi3)
	FUNC(__udivsi3)
SYM(__udivsi3):
	b @SYM(__udivmodsi4)
	ENDFUNC(__udivsi3)

#endif /* L_udivsi3 */

#ifdef  L_divsi3
	.section .text
	.align 4

	.global SYM(__divsi3)
	FUNC(__divsi3)

#ifndef __ARC_EA__
SYM(__divsi3):
	/* A5 / ARC60? */
	mov r12,blink
	xor r11,r0,r1
	abs_s r0,r0
	bl.d @SYM(__udivmodsi4)
	abs_s r1,r1
	tst r11,r11
	j.d [r12]
	neg.mi r0,r0
#else 	/* !ifndef __ARC_EA__ */
	;; We can use the abs, norm, divaw and mpy instructions for ARC700
#define MULDIV
#ifdef MULDIV
/* This table has been generated by divtab-arc700.c.  */
/* 1/512 .. 1/256, normalized.  There is a leading 1 in bit 31.
   For powers of two, we list unnormalized numbers instead.  The values
   for powers of 2 are loaded, but not used.  The value for 1 is actually
   the first instruction after .Lmuldiv.  */
	.balign 4
.Ldivtab:

	.long	0x1000000
	.long	0x80808081
	.long	0x81020409
	.long	0x81848DA9
	.long	0x82082083
	.long	0x828CBFBF
	.long	0x83126E98
	.long	0x83993053
	.long	0x84210843
	.long	0x84A9F9C9
	.long	0x85340854
	.long	0x85BF3762
	.long	0x864B8A7E
	.long	0x86D90545
	.long	0x8767AB60
	.long	0x87F78088
	.long	0x88888889
	.long	0x891AC73B
	.long	0x89AE408A
	.long	0x8A42F871
	.long	0x8AD8F2FC
	.long	0x8B70344B
	.long	0x8C08C08D
	.long	0x8CA29C05
	.long	0x8D3DCB09
	.long	0x8DDA5203
	.long	0x8E78356E
	.long	0x8F1779DA
	.long	0x8FB823EF
	.long	0x905A3864
	.long	0x90FDBC0A
	.long	0x91A2B3C5
	.long	0x92492493
	.long	0x92F11385
	.long	0x939A85C5
	.long	0x94458095
	.long	0x94F20950
	.long	0x95A02569
	.long	0x964FDA6D
	.long	0x97012E03
	.long	0x97B425EE
	.long	0x9868C80A
	.long	0x991F1A52
	.long	0x99D722DB
	.long	0x9A90E7DA
	.long	0x9B4C6F9F
	.long	0x9C09C09D
	.long	0x9CC8E161
	.long	0x9D89D89E
	.long	0x9E4CAD24
	.long	0x9F1165E8
	.long	0x9FD809FE
	.long	0xA0A0A0A1
	.long	0xA16B312F
	.long	0xA237C32C
	.long	0xA3065E40
	.long	0xA3D70A3E
	.long	0xA4A9CF1E
	.long	0xA57EB503
	.long	0xA655C43A
	.long	0xA72F053A
	.long	0xA80A80A9
	.long	0xA8E83F58
	.long	0xA9C84A48
	.long	0xAAAAAAAB
	.long	0xAB8F69E3
	.long	0xAC769185
	.long	0xAD602B59
	.long	0xAE4C415D
	.long	0xAF3ADDC7
	.long	0xB02C0B03
	.long	0xB11FD3B9
	.long	0xB21642C9
	.long	0xB30F6353
	.long	0xB40B40B5
	.long	0xB509E68B
	.long	0xB60B60B7
	.long	0xB70FBB5B
	.long	0xB81702E1
	.long	0xB92143FB
	.long	0xBA2E8BA3
	.long	0xBB3EE722
	.long	0xBC52640C
	.long	0xBD691048
	.long	0xBE82FA0C
	.long	0xBFA02FE9
	.long	0xC0C0C0C1
	.long	0xC1E4BBD6
	.long	0xC30C30C4
	.long	0xC4372F86
	.long	0xC565C87C
	.long	0xC6980C6A
	.long	0xC7CE0C7D
	.long	0xC907DA4F
	.long	0xCA4587E7
	.long	0xCB8727C1
	.long	0xCCCCCCCD
	.long	0xCE168A78
	.long	0xCF6474A9
	.long	0xD0B69FCC
	.long	0xD20D20D3
	.long	0xD3680D37
	.long	0xD4C77B04
	.long	0xD62B80D7
	.long	0xD79435E6
	.long	0xD901B204
	.long	0xDA740DA8
	.long	0xDBEB61EF
	.long	0xDD67C8A7
	.long	0xDEE95C4D
	.long	0xE070381D
	.long	0xE1FC780F
	.long	0xE38E38E4
	.long	0xE525982B
	.long	0xE6C2B449
	.long	0xE865AC7C
	.long	0xEA0EA0EB
	.long	0xEBBDB2A6
	.long	0xED7303B6
	.long	0xEF2EB720
	.long	0xF0F0F0F1
	.long	0xF2B9D649
	.long	0xF4898D60
	.long	0xF6603D99
	.long	0xF83E0F84
	.long	0xFA232CF3
	.long	0xFC0FC0FD
	.long	0xFE03F810
	.long	0x2000000
	.long	0x81020409
	.long	0x82082083
	.long	0x83126E98
	.long	0x84210843
	.long	0x85340854
	.long	0x864B8A7E
	.long	0x8767AB60
	.long	0x88888889
	.long	0x89AE408A
	.long	0x8AD8F2FC
	.long	0x8C08C08D
	.long	0x8D3DCB09
	.long	0x8E78356E
	.long	0x8FB823EF
	.long	0x90FDBC0A
	.long	0x92492493
	.long	0x939A85C5
	.long	0x94F20950
	.long	0x964FDA6D
	.long	0x97B425EE
	.long	0x991F1A52
	.long	0x9A90E7DA
	.long	0x9C09C09D
	.long	0x9D89D89E
	.long	0x9F1165E8
	.long	0xA0A0A0A1
	.long	0xA237C32C
	.long	0xA3D70A3E
	.long	0xA57EB503
	.long	0xA72F053A
	.long	0xA8E83F58
	.long	0xAAAAAAAB
	.long	0xAC769185
	.long	0xAE4C415D
	.long	0xB02C0B03
	.long	0xB21642C9
	.long	0xB40B40B5
	.long	0xB60B60B7
	.long	0xB81702E1
	.long	0xBA2E8BA3
	.long	0xBC52640C
	.long	0xBE82FA0C
	.long	0xC0C0C0C1
	.long	0xC30C30C4
	.long	0xC565C87C
	.long	0xC7CE0C7D
	.long	0xCA4587E7
	.long	0xCCCCCCCD
	.long	0xCF6474A9
	.long	0xD20D20D3
	.long	0xD4C77B04
	.long	0xD79435E6
	.long	0xDA740DA8
	.long	0xDD67C8A7
	.long	0xE070381D
	.long	0xE38E38E4
	.long	0xE6C2B449
	.long	0xEA0EA0EB
	.long	0xED7303B6
	.long	0xF0F0F0F1
	.long	0xF4898D60
	.long	0xF83E0F84
	.long	0xFC0FC0FD
	.long	0x4000000
	.long	0x82082083
	.long	0x84210843
	.long	0x864B8A7E
	.long	0x88888889
	.long	0x8AD8F2FC
	.long	0x8D3DCB09
	.long	0x8FB823EF
	.long	0x92492493
	.long	0x94F20950
	.long	0x97B425EE
	.long	0x9A90E7DA
	.long	0x9D89D89E
	.long	0xA0A0A0A1
	.long	0xA3D70A3E
	.long	0xA72F053A
	.long	0xAAAAAAAB
	.long	0xAE4C415D
	.long	0xB21642C9
	.long	0xB60B60B7
	.long	0xBA2E8BA3
	.long	0xBE82FA0C
	.long	0xC30C30C4
	.long	0xC7CE0C7D
	.long	0xCCCCCCCD
	.long	0xD20D20D3
	.long	0xD79435E6
	.long	0xDD67C8A7
	.long	0xE38E38E4
	.long	0xEA0EA0EB
	.long	0xF0F0F0F1
	.long	0xF83E0F84
	.long	0x8000000
	.long	0x84210843
	.long	0x88888889
	.long	0x8D3DCB09
	.long	0x92492493
	.long	0x97B425EE
	.long	0x9D89D89E
	.long	0xA3D70A3E
	.long	0xAAAAAAAB
	.long	0xB21642C9
	.long	0xBA2E8BA3
	.long	0xC30C30C4
	.long	0xCCCCCCCD
	.long	0xD79435E6
	.long	0xE38E38E4
	.long	0xF0F0F0F1
	.long	0x10000000
	.long	0x88888889
	.long	0x92492493
	.long	0x9D89D89E
	.long	0xAAAAAAAB
	.long	0xBA2E8BA3
	.long	0xCCCCCCCD
	.long	0xE38E38E4
	.long	0x20000000
	.long	0x92492493
	.long	0xAAAAAAAB
	.long	0xCCCCCCCD
	.long	0x40000000
	.long	0xAAAAAAAB
	.long	0x80000000
__muldiv:
	neg	r4,r2
	ld.as	r5,[pcl,r4]
	abs_s	r12,r0
	bic.f	0,r2,r4
	mpyhu.ne r12,r12,r5
	norm	r3,r2
	xor.f	0,r0,r1
	; write port allocation stall
	rsub	r3,r3,30
	lsr	r0,r12,r3
	j_s.d	[blink]
	neg.mi	r0,r0

	.balign	4
SYM(__divsi3):
	norm	r3,r1
	abs_s	r2,r1
	brhs	r3,23,__muldiv
	norm	r4,r0
	abs_l	r12,r0
	brhs	r4,r3,.Lonebit
	asl_s	r2,r2,r3
	asl	r12,r12,r4
	sub	lp_count,r3,r4
	sub.f	r12,r12,r2
	brge.d	r12,r2,.Lsbit
	sub	r4,r3,r4
	add.lo	r12,r12,r2
	lp	.Ldivend
.Ldivstart:divaw r12,r12,r2
.Ldivend:xor_s	r1,r1,r0
	sub	r0,r4,1
	bmsk	r0,r12,r0
	bset.hs	r0,r0,r4
	tst_s	r1,r1
	j_s.d	[blink]
	neg.mi	r0,r0
.Lonebit:
	xor_s	r1,r1,r0
	asr_s	r1,r1,31
	sub1.f	0,r12,r2	; special case:	-2**(n+1) / 2**n
	or	r0,r1,1
	add.eq	r0,r0,r0
	cmp_s	r12,r2
	j_s.d	[blink]
	mov.lo	r0,0
.Lsbit:
	; Need to handle special cases involving negative powers of two:
	; r12,r2 are normalized dividend / divisor;
	; divide anything by 0x80000000, or divide 0x80000000 by 0x40000000
	add_s	r12,r12,r2
	xor_s	r1,r1,r0
	rsub	r4,r4,-1
	ror	r0,r12,r4
	tst_s	r2,r2
	bmsk	r0,r0,r3
	add.pl	r0,r0,r0
	tst_s	r1,r1
	j_s.d	[blink]
	neg.mi	r0,r0
#else /* !MULDIV */
/* This version requires that divaw works with a divisor of 0x80000000U  */
	abs_s	r2,r1
	norm	r4,r0
	neg_s	r3,r2
	norm	r3,r3
	abs_s	r12,r0
	brhs	r4,r3,.Lonebit
	asl_s	r2,r2,r3
	asl	r12,r12,r4
	sub	lp_count,r3,r4
	cmp_s	r12,r2
	sub.hs	r12,r12,r2
	lp	.Ldivend
.Ldivstart:divaw r12,r12,r2
.Ldivend:xor_s	r1,r1,r0
	sub_s	r0,r3,1
	bmsk	r0,r12,r0
	bset.hs	r0,r0,r3
	tst_s	r1,r1
	j_s.d	[blink]
	negmi	r0,r0
.Lonebit:
	xor_s	r1,r1,r0
	asr_s	r1,r1,31
	cmp_s	r12,r2
	mov_s	r0,0
	j_s.d	[blink]
	orhs	r0,r1,1
#endif /* MULDIV */

#endif	/* ifndef __ARC700__ */
	ENDFUNC(__divsi3)


#endif /* L_divsi3 */

#ifndef __ARC_RF16__
#ifdef  L_umodsi3
	.section .text
	.align 4

	.global SYM(__umodsi3)
	FUNC(__umodsi3)
SYM(__umodsi3):
	mov r7,blink
	bl.nd @SYM(__udivmodsi4)
	j.d [r7]
	mov r0,r1
	ENDFUNC(__umodsi3)

#endif /* L_umodsi3 */
#endif /* !__ARC_RF16__ */

#ifdef  L_modsi3
	.section .text
	.align 4

	.global SYM (__modsi3)
	FUNC(__modsi3)
SYM(__modsi3):
#ifndef __ARC_EA__
	/* A5 / ARC60? */
	mov_s r12,blink
	mov_s r11,r0
	abs_s r0,r0
	bl.d @SYM(__udivmodsi4)
	abs_s r1,r1
	tst r11,r11
	neg_s r0,r1
	j_s.d [r12]
	mov.pl r0,r1
#else /* __ARC_EA__ */
	abs_s	r2,r1
	norm.f	r4,r0
	neg	r5,r2
	norm	r3,r5
	abs_l	r12,r0
	brhs	r4,r3,.Lonebit
	asl_s	r2,r2,r3
	asl	r12,r12,r4
	sub	lp_count,r3,r4
	cmp_s	r12,r2
	sub.hs	r12,r12,r2
	tst_s	r0,r0
	lp	.Ldivend
.Ldivstart:divaw r12,r12,r2
.Ldivend:
	lsr	r0,r12,r3
	j_s.d	[blink]
	neg.mi	r0,r0
	.balign	4
.Lonebit:neg.pl	r5,r5
	cmp_s	r12,r2
	j_s.d	[blink]
	sub.hs	r0,r0,r5
#endif /* !__ARC_EA__ */
	ENDFUNC(__modsi3)

#endif /* L_modsi3 */

#ifdef L_clzsi2
       .section .text
       .align 4
       .global SYM (__clzsi2)
SYM(__clzsi2):
#ifdef __ARC_NORM__
	HIDDEN_FUNC(__clzsi2)
	norm.f	r0,r0
	mov.n	r0,0
	j_s.d	[blink]
	add.pl	r0,r0,1
	ENDFUNC(__clzsi2)
#elif !defined (__ARC_BARREL_SHIFTER__)
	FUNC(__clzsi2)
	mov lp_count,10
	mov_l r1,0
	bset r2,r1,29
	lp .Loop_end
	brhs r0,r2,.Loop_end
	add3 r0,r1,r0
.Loop_end:
	asl.f 0,r0
	sub2 r0,lp_count,lp_count
	sub.cs.f r0,r0,1
	add r0,r0,31
	j_s.d [blink]
	add.pl r0,r0,1
	ENDFUNC(__clzsi2)
#else
	FUNC(__clzsi2)
	asl.f 0,r0,2
	mov r1,-1
.Lcheck:
	bbit1.d r0,31,.Ldone
	asl.pl r0,r0,3
	bcs.d .Ldone_1
	add_s r1,r1,3
	bpnz.d .Lcheck
	asl.f 0,r0,2
	mov_s r0,32
	j_s.d [blink]
	mov.ne r0,r1
.Ldone:
	j_s.d [blink]
	add_s r0,r1,1
.Ldone_1:
	j_s.d [blink]
	sub_s r0,r1,1
	ENDFUNC(__clzsi2)
#endif
#endif /* L_clzsi2 */
       .section .text


;;; MILLICODE THUNK LIB ;***************

;;; 	.macro push_regs from, to, offset
;;; 		st_s "\from", [sp, \offset]
;;; 		.if \to-\from
;;; 			push_regs "(\from+1)", \to, "(\offset+4)"
;;; 		.endif
;;; 	.endm
;;; 	push_regs 13, 18, 0
;;;

;;;;   	.macro sum from, to, three
;;;;   		.long \from
;;;;   		.long \three
;;;;   		.local regno
;;;;   		.set regno, \from+1
;;;;   		.set shift, 32
;;;;   		.set shift, shift - 1
;;;;   #		st_s %shift @3 lsl #shift
;;;;   		.if \to-\from
;;;;   		sum "(\from+1)", \to, "(\three)"
;;;;   		.endif
;;;;   	.endm
;;;;
;;;;   	SUM 0,5, 9
;;;;
;	.altmacro
;;  	.macro push_regs from=0, to=3, offset
;;  		st_s r\from, [sp, \offset]
;;  		.if \to-\from
;;  			push_regs "\from+1 ",\to,"(\offset+4)"
;;  		.endif
;;  	.endm
;;
;;  	.macro expand_to_push from=13, to
;;  ;		.section .text
;;  ;		.align 4
;;  ;		.global st_
;;  ;		.type foo,
;;  	st_13_to_25:
;;  ;		push_regs \from, \to, 0
;;  	push_regs 0,3		;
;;  	.endm
;;
;;  	expand_to_push 13,18
;;
;#endif

#ifndef __ARC_RF16__
#ifdef L_millicodethunk_st
	.section .text
	.align 4
	.global SYM(__st_r13_to_r15)
	.global SYM(__st_r13_to_r16)
	.global SYM(__st_r13_to_r17)
	.global SYM(__st_r13_to_r18)
	.global SYM(__st_r13_to_r19)
	.global SYM(__st_r13_to_r20)
	.global SYM(__st_r13_to_r21)
	.global SYM(__st_r13_to_r22)
	.global SYM(__st_r13_to_r23)
	.global SYM(__st_r13_to_r24)
	.global SYM(__st_r13_to_r25)
	HIDDEN_FUNC(__st_r13_to_r15)
	HIDDEN_FUNC(__st_r13_to_r16)
	HIDDEN_FUNC(__st_r13_to_r17)
	HIDDEN_FUNC(__st_r13_to_r18)
	HIDDEN_FUNC(__st_r13_to_r19)
	HIDDEN_FUNC(__st_r13_to_r20)
	HIDDEN_FUNC(__st_r13_to_r21)
	HIDDEN_FUNC(__st_r13_to_r22)
	HIDDEN_FUNC(__st_r13_to_r23)
	HIDDEN_FUNC(__st_r13_to_r24)
	HIDDEN_FUNC(__st_r13_to_r25)
	.align 4
SYM(__st_r13_to_r25):
	st r25, [sp,48]
SYM(__st_r13_to_r24):
	st r24, [sp,44]
SYM(__st_r13_to_r23):
	st r23, [sp,40]
SYM(__st_r13_to_r22):
	st r22, [sp,36]
SYM(__st_r13_to_r21):
	st r21, [sp,32]
SYM(__st_r13_to_r20):
	st r20, [sp,28]
SYM(__st_r13_to_r19):
	st r19, [sp,24]
SYM(__st_r13_to_r18):
	st r18, [sp,20]
SYM(__st_r13_to_r17):
	st r17, [sp,16]
SYM(__st_r13_to_r16):
	st r16, [sp,12]
SYM(__st_r13_to_r15):
#ifdef __ARC700__
	st r15, [sp,8] ; minimum function size to avoid stall: 6 bytes.
#else
	st_s r15, [sp,8]
#endif
	st_s r14, [sp,4]
	j_s.d [%blink]
	st_s r13, [sp,0]
	ENDFUNC(__st_r13_to_r15)
	ENDFUNC(__st_r13_to_r16)
	ENDFUNC(__st_r13_to_r17)
	ENDFUNC(__st_r13_to_r18)
	ENDFUNC(__st_r13_to_r19)
	ENDFUNC(__st_r13_to_r20)
	ENDFUNC(__st_r13_to_r21)
	ENDFUNC(__st_r13_to_r22)
	ENDFUNC(__st_r13_to_r23)
	ENDFUNC(__st_r13_to_r24)
	ENDFUNC(__st_r13_to_r25)
#endif  /* L_millicodethunk_st */


#ifdef L_millicodethunk_ld
	.section .text
	.align 4
;	==================================
;	the loads

	.global SYM(__ld_r13_to_r15)
	.global SYM(__ld_r13_to_r16)
	.global SYM(__ld_r13_to_r17)
	.global SYM(__ld_r13_to_r18)
	.global SYM(__ld_r13_to_r19)
	.global SYM(__ld_r13_to_r20)
	.global SYM(__ld_r13_to_r21)
	.global SYM(__ld_r13_to_r22)
	.global SYM(__ld_r13_to_r23)
	.global SYM(__ld_r13_to_r24)
	.global SYM(__ld_r13_to_r25)
	HIDDEN_FUNC(__ld_r13_to_r15)
	HIDDEN_FUNC(__ld_r13_to_r16)
	HIDDEN_FUNC(__ld_r13_to_r17)
	HIDDEN_FUNC(__ld_r13_to_r18)
	HIDDEN_FUNC(__ld_r13_to_r19)
	HIDDEN_FUNC(__ld_r13_to_r20)
	HIDDEN_FUNC(__ld_r13_to_r21)
	HIDDEN_FUNC(__ld_r13_to_r22)
	HIDDEN_FUNC(__ld_r13_to_r23)
	HIDDEN_FUNC(__ld_r13_to_r24)
	HIDDEN_FUNC(__ld_r13_to_r25)
SYM(__ld_r13_to_r25):
	ld r25, [sp,48]
SYM(__ld_r13_to_r24):
	ld r24, [sp,44]
SYM(__ld_r13_to_r23):
	ld r23, [sp,40]
SYM(__ld_r13_to_r22):
	ld r22, [sp,36]
SYM(__ld_r13_to_r21):
	ld r21, [sp,32]
SYM(__ld_r13_to_r20):
	ld r20, [sp,28]
SYM(__ld_r13_to_r19):
	ld r19, [sp,24]
SYM(__ld_r13_to_r18):
	ld r18, [sp,20]
SYM(__ld_r13_to_r17):
	ld r17, [sp,16]
SYM(__ld_r13_to_r16):
	ld r16, [sp,12]
SYM(__ld_r13_to_r15):
#ifdef __ARC700__
	ld r15, [sp,8] ; minimum function size to avoid stall: 6 bytes.
#else
	ld_s r15, [sp,8]
#endif
	ld_s r14, [sp,4]
	j_s.d [%blink]
	ld_s r13, [sp,0]
	ENDFUNC(__ld_r13_to_r15)
	ENDFUNC(__ld_r13_to_r16)
	ENDFUNC(__ld_r13_to_r17)
	ENDFUNC(__ld_r13_to_r18)
	ENDFUNC(__ld_r13_to_r19)
	ENDFUNC(__ld_r13_to_r20)
	ENDFUNC(__ld_r13_to_r21)
	ENDFUNC(__ld_r13_to_r22)
	ENDFUNC(__ld_r13_to_r23)
	ENDFUNC(__ld_r13_to_r24)
	ENDFUNC(__ld_r13_to_r25)

#endif /* L_millicodethunk_ld */
#ifdef L_millicodethunk_ret
	.global SYM(__ld_r13_to_r14_ret)
	.global SYM(__ld_r13_to_r15_ret)
	.global SYM(__ld_r13_to_r16_ret)
	.global SYM(__ld_r13_to_r17_ret)
	.global SYM(__ld_r13_to_r18_ret)
	.global SYM(__ld_r13_to_r19_ret)
	.global SYM(__ld_r13_to_r20_ret)
	.global SYM(__ld_r13_to_r21_ret)
	.global SYM(__ld_r13_to_r22_ret)
	.global SYM(__ld_r13_to_r23_ret)
	.global SYM(__ld_r13_to_r24_ret)
	.global SYM(__ld_r13_to_r25_ret)
	HIDDEN_FUNC(__ld_r13_to_r14_ret)
	HIDDEN_FUNC(__ld_r13_to_r15_ret)
	HIDDEN_FUNC(__ld_r13_to_r16_ret)
	HIDDEN_FUNC(__ld_r13_to_r17_ret)
	HIDDEN_FUNC(__ld_r13_to_r18_ret)
	HIDDEN_FUNC(__ld_r13_to_r19_ret)
	HIDDEN_FUNC(__ld_r13_to_r20_ret)
	HIDDEN_FUNC(__ld_r13_to_r21_ret)
	HIDDEN_FUNC(__ld_r13_to_r22_ret)
	HIDDEN_FUNC(__ld_r13_to_r23_ret)
	HIDDEN_FUNC(__ld_r13_to_r24_ret)
	HIDDEN_FUNC(__ld_r13_to_r25_ret)
	.section .text
	.align 4
SYM(__ld_r13_to_r25_ret):
	ld r25, [sp,48]
SYM(__ld_r13_to_r24_ret):
	ld r24, [sp,44]
SYM(__ld_r13_to_r23_ret):
	ld r23, [sp,40]
SYM(__ld_r13_to_r22_ret):
	ld r22, [sp,36]
SYM(__ld_r13_to_r21_ret):
	ld r21, [sp,32]
SYM(__ld_r13_to_r20_ret):
	ld r20, [sp,28]
SYM(__ld_r13_to_r19_ret):
	ld r19, [sp,24]
SYM(__ld_r13_to_r18_ret):
	ld r18, [sp,20]
SYM(__ld_r13_to_r17_ret):
	ld r17, [sp,16]
SYM(__ld_r13_to_r16_ret):
	ld r16, [sp,12]
SYM(__ld_r13_to_r15_ret):
	ld r15, [sp,8]
SYM(__ld_r13_to_r14_ret):
	ld blink,[sp,r12]
	ld_s r14, [sp,4]
	ld.ab r13, [sp,r12]
	j_s.d [%blink]
	add_s sp,sp,4
	ENDFUNC(__ld_r13_to_r14_ret)
	ENDFUNC(__ld_r13_to_r15_ret)
	ENDFUNC(__ld_r13_to_r16_ret)
	ENDFUNC(__ld_r13_to_r17_ret)
	ENDFUNC(__ld_r13_to_r18_ret)
	ENDFUNC(__ld_r13_to_r19_ret)
	ENDFUNC(__ld_r13_to_r20_ret)
	ENDFUNC(__ld_r13_to_r21_ret)
	ENDFUNC(__ld_r13_to_r22_ret)
	ENDFUNC(__ld_r13_to_r23_ret)
	ENDFUNC(__ld_r13_to_r24_ret)
	ENDFUNC(__ld_r13_to_r25_ret)

#endif /* L_millicodethunk_ret */

#if defined (__ARC700__) || defined (__ARC_FPX_QUARK__)
#ifdef  L_adddf3
#ifdef __ARC_NORM__
#include "ieee-754/adddf3.S"
#endif
#endif

#ifdef  L_muldf3
#ifdef __ARC_MPY__
#include "ieee-754/muldf3.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL64__)
#include "ieee-754/arc600-mul64/muldf3.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL32BY16__)
#include "ieee-754/arc600-dsp/muldf3.S"
#endif
#endif

#ifdef  L_addsf3
#ifdef __ARC_NORM__
#include "ieee-754/addsf3.S"
#endif
#endif

#ifdef  L_mulsf3
#ifdef  __ARC_MPY__
#include "ieee-754/mulsf3.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL64__)
#include "ieee-754/arc600-mul64/mulsf3.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL32BY16__)
#include "ieee-754/arc600-dsp/mulsf3.S"
#elif defined (__ARC_NORM__)
#include "ieee-754/arc600/mulsf3.S"
#endif
#endif

#ifdef  L_divdf3
#ifdef  __ARC_MPY__
#include "ieee-754/divdf3.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL64__)
#include "ieee-754/arc600-mul64/divdf3.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL32BY16__)
#include "ieee-754/arc600-dsp/divdf3.S"
#endif
#endif

#ifdef  L_divsf3
#ifdef  __ARC_MPY__
#include "ieee-754/divsf3-stdmul.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL64__)
#include "ieee-754/arc600-mul64/divsf3.S"
#elif defined (__ARC_NORM__) && defined(__ARC_MUL32BY16__)
#include "ieee-754/arc600-dsp/divsf3.S"
#elif defined (__ARC_NORM__)
#include "ieee-754/arc600/divsf3.S"
#endif
#endif

#ifdef L_extendsfdf2
#ifdef __ARC_NORM__
#include "ieee-754/extendsfdf2.S"
#endif
#endif

#ifdef L_truncdfsf2
#ifdef __ARC_NORM__
#include "ieee-754/truncdfsf2.S"
#endif
#endif

#ifdef L_floatsidf
#ifdef __ARC_NORM__
#include "ieee-754/floatsidf.S"
#endif
#endif

#ifdef L_floatsisf
#ifdef __ARC_NORM__
#include "ieee-754/floatsisf.S"
#endif
#endif

#ifdef L_floatunsidf
#ifdef __ARC_NORM__
#include "ieee-754/floatunsidf.S"
#endif
#endif

#ifdef L_fixdfsi
#ifdef __ARC_NORM__
#include "ieee-754/fixdfsi.S"
#endif
#endif

#ifdef L_fixsfsi
#ifdef __ARC_NORM__
#include "ieee-754/fixsfsi.S"
#endif
#endif

#ifdef L_fixunsdfsi
#ifdef __ARC_NORM__
#include "ieee-754/fixunsdfsi.S"
#endif
#endif

#ifdef L_eqdf2
#ifdef __ARC_NORM__
#include "ieee-754/eqdf2.S"
#endif
#endif

#ifdef L_eqsf2
#ifdef __ARC_NORM__
#include "ieee-754/eqsf2.S"
#endif
#endif

#ifdef L_gtdf2
#ifdef __ARC_NORM__
#include "ieee-754/gtdf2.S"
#endif
#endif

#ifdef L_gtsf2
#ifdef __ARC_NORM__
#include "ieee-754/gtsf2.S"
#endif
#endif

#ifdef L_gedf2
#ifdef __ARC_NORM__
#include "ieee-754/gedf2.S"
#endif
#endif

#ifdef L_gesf2
#ifdef __ARC_NORM__
#include "ieee-754/gesf2.S"
#endif
#endif

#ifdef L_uneqdf2
#ifdef __ARC_NORM__
#include "ieee-754/uneqdf2.S"
#endif
#endif

#ifdef L_uneqsf2
#ifdef __ARC_NORM__
#include "ieee-754/uneqsf2.S"
#endif
#endif

#ifdef L_orddf2
#ifdef __ARC_NORM__
#include "ieee-754/orddf2.S"
#endif
#endif

#ifdef L_ordsf2
#ifdef __ARC_NORM__
#include "ieee-754/ordsf2.S"
#endif
#endif
#endif /* ARC_OPTFPE */

#endif /* !__ARC_RF16__ */
