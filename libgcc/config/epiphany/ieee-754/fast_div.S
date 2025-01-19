/* Copyright (C) 2011-2025 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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

#include "../epiphany-asm.h"

.section _fast_div_text,"a",@progbits;
  .balign 8;
_fast_div_table:
.word 0x007fffff//   mantissa mask
.word 0x40257ebb//   hold constant a = 2.58586

.word 0x3f000000//   hold constant 126 shifted to bits [30:23]
.word 0xc0ba2e88//   hold constant b = -5.81818

.word 0x4087c1e8//   hold constant c = 4.24242
.word 0x40000000//  to hold constant 2 for Newton-Raphson iterations

 .global SYM(__fast_recipsf2)
 FUNC(__fast_recipsf2)
SYM(__fast_recipsf2):

//###################
//# input operands:
//###################
// Divisor
//R0
// Function address (used with negative offsets to read _fast_div_table)
//R1
/* Scratch registers:  two single (TMP0/TMP5) and two pairs.  */
#define P0L TMP1
#define P0H TMP2
#define P1L TMP3
#define P1H TMP4

//#########################################
//# Constants to be used in the algorithm
//#########################################
ldrd P0L , [ R1 , -3 ]

ldrd P1L , [ R1 , -2 ]



//#############################################################################
//#                       The Algorithm
//#
//# Operation: C=A/B
//# stage 1 - find the reciprocal 1/B according to the following scheme:
//#  B = (2^E)*m                                (1<m<2, E=e-127)
//#  1/B = 1/((2^E)*m) = 1/((2^(E+1))*m1)          (0.5<m1<1)
//#      = (2^-(E+1))*(1/m1) = (2^E1)*(1/m1)
//#
//# Now we can find the new exponent:
//# e1 = E1+127 = -E-1+127 = -e+127-1+127 = 253-e **
//# 1/m1 alreadt has the exponent 127, so we have to add 126-e.
//# the exponent might underflow, which we can detect as a sign change.
//# Since the architeture uses flush-to-zero for subnormals, we can
//# give the result 0. then.
//#
//# The 1/m1 term with 0.5<m1<1 is approximated with the Chebyshev polynomial
//# 1/m1 = 2.58586*(m1^2) - 5.81818*m1 + 4.24242
//#
//# Next step is to use two iterations of Newton-Raphson algorithm to complete
//# the reciprocal calculation.
//#
//# Final result is achieved by multiplying A with 1/B
//#############################################################################



// R0 exponent and sign "replacement" into TMP0
AND TMP0,R0,P0L		 ;
ORR TMP0,TMP0,P1L
SUB TMP5,R0,TMP0 // R0 sign/exponent extraction into TMP5
// Calculate new mantissa
FMADD P1H,TMP0,P0H	         ;
		// Calculate new exponent offset 126 - "old exponent"
		SUB P1L,P1L,TMP5
	ldrd P0L , [ R1 , -1 ]
FMADD P0L,TMP0,P1H	         ;
		eor P1H,r0,P1L // check for overflow (N-BIT).
		blt .Lret_0
// P0L exponent and sign "replacement"
sub P0L,P0L,TMP5

// Newton-Raphson iteration #1
MOV TMP0,P0H	         ;
FMSUB P0H,R0,P0L	 ;
FMUL  P0L,P0H,P0L	 ;
// Newton-Raphson iteration #2
FMSUB TMP0,R0,P0L	;
FMUL  R0,TMP0,P0L	         ;
jr lr
.Lret_0:ldrd P0L , [ R1 , -3 ]
	lsr TMP0,r0,31 ; extract sign
	lsl TMP0,TMP0,31
	add P0L,P0L,r0 ; check for NaN input
	eor P0L,P0L,r0
	movgte r0,TMP0
	jr lr
// Quotient calculation is expected by the caller: FMUL quotient,divident,R0
        ;
	ENDFUNC(__fast_recipsf2)
