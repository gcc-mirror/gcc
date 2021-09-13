/* Libgcc Target specific implementation.
   Copyright (C) 2012-2021 Free Software Foundation, Inc.
   Contributed by KPIT Cummins Infosystems Limited.

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

#ifdef  L_mulsi3
	.text
	.align  4
	.globl  ___mulsi3
___mulsi3:
	movw    r4,r0
	movw    r2,r1
	/* Extended multiplication between the 2 lower words */
	muluw   r1,(r1,r0)

	/* Multiply the lower word of each parameter  */  
	mulw    r2,r5   
    
	/* With the higher word of the other  */
	mulw    r3,r4     
  
	/* Add products to the higher part of the final result  */
	addw    r4,r1       
	addw    r5,r1
	jump    (ra)
#endif

#ifdef L_divdi3
	.text
	.align 4
	.globl ___divdi3

___divdi3:
	push	$4, r7, ra

	/* Param #1 Long Long low bit first */
	loadd   12(sp), (r1, r0)
	loadd   16(sp), (r3, r2)

	/* Param #2 Long Long low bit first */
	loadd   20(sp), (r5, r4)
	loadd   24(sp), (r7, r6)
	
	/* Set neg to 0 */
	movw $0, r10
	
	subd $16, (sp)

	/* Compare if param1 is greater than 0 */	
	cmpw $0, r3
	ble L4

	/* Invert param1 and neg */
	movd $-1, (r9, r8) 	/* Temp set to FFFFFFFF */
	xord (r9, r8), (r1, r0)	/* Xor low bits of param 1 with temp */
	xord (r9, r8), (r3, r2)	/* Xor high bits of param 1 with temp */
	addd $1, (r1, r0)	/* Add 1 to low bits of param 1 */
	xorw $1, r10		/* Invert neg */
	bcc L4			/* If no carry occurred go to L4 */
	addd $1, (r3, r2)	/* Add 1 to high bits of param 1 */

L4:	stord (r1, r0), 0(sp)
	stord (r3, r2), 4(sp)
	
	/* Compare if param2 is greater than 0 */	
	cmpw $0, r7
	ble L5

	/* Invert param2 and neg */
	movd $-1, (r9, r8)	/* Temp set to FFFFFFFF */
	xord (r9, r8), (r5, r4)	/* Xor low bits of param 2 with temp */
	xord (r9, r8), (r7, r6)	/* Xor high bits of param 2 with temp */
	addd $1, (r5, r4)	/* Add 1 to low bits of param 2 */
	xorw $1, r10		/* Invert neg */
	bcc L5			/* If no carry occurred go to L5 */
	addd $1, (r7, r6)	/* Add 1 to high bits of param 2 */

L5:	stord (r5, r4), 8(sp)
	stord (r7, r6), 12(sp)
	movw $0, r2

	/* Call udivmoddi3 */
#ifdef __PIC__
	loadd	___udivmoddi3@cGOT(r12), (r1,r0)
	jal	(r1,r0)
#else
	bal (ra), ___udivmoddi3
#endif

	/* If (neg) */
	addd	$16, (sp)
	cmpw $0, r10		/* Compare 0 with neg */
	beq Lexit__
	
	/* Neg = -Neg */
	xord (r9, r8), (r1, r0)	/* Xor low bits of ures with temp */
	xord (r9, r8), (r3, r2)	/* Xor high bits of ures with temp */
	addd $1, (r1, r0)	/* Add 1 to low bits of ures */
	bcc Lexit__
	addd $1, (r3, r2)	/* Add 1 to high bit of ures */

Lexit__:
#  ifdef __ID_SHARED_LIB__
	pop	$2, r12
#  endif
	popret $4, r7, ra
#endif

#ifdef L_lshrdi3
	.text
	.align 4
	.globl ___lshrdi3

___lshrdi3:
	push	$3, r7

	/* Load parameters from stack in this order */
	movw r2, r6		/* Number of shifts */
	loadd	6(sp), (r1, r0)	/* Low bits */
	loadd	10(sp), (r3, r2)/* High bits */

	xorw $-1, r6		/* Invert number of shifts */
	addw $1, r6		/* Add 1 by number of shifts */

	movw r6, r7		/* Copy number of shifts */

	tbit $15, r6		/* Test if number is negative */
	bfs L2			/* If negative jump to L2 */

	movd (r1, r0), (r9, r8)	/* Copy low bits */

	subw $32, r7		/* Calc how many bits will overflow */
	/* Shift the temp low bit to the right to see the overflowing bits  */
	lshd r7, (r9, r8)	

	cmpw $32, r6		/* If number of shifts is higher than 31 */
	blt L1			/* Shift by moving */

	lshd r6, (r3, r2)	/* Shift high bits */
	lshd r6, (r1, r0)	/* Shift low bits */
	addd (r9, r8), (r3, r2)	/* Add overflow to the high bits */
	popret	$3, r7		/* Return */

L1:	movd $0, (r1, r0)	/* Reset low bit */
	movd (r9, r8), (r3, r2)	/* Add the overflow from the low bit */
	popret	$3, r7		/* Return */

L2:	movd (r3, r2), (r9, r8)	/* Copy high bits */

	addw $32, r7		/* Calc how many bits will overflow */
	/* Shift the temp low bit to the left to see the overflowing bits  */
	lshd r7, (r9, r8)	

	cmpw $-32, r6		/* If number of shifts is lower than -31 */
	bgt L3			/* Shift by moving */

	lshd r6, (r1, r0)	/* Shift low bits */
	lshd r6, (r3, r2)	/* Shift high bits */
	addd (r9, r8), (r1, r0)	/* Add overflow to the low bits */
	popret	$3, r7		/* Return */
	
L3:	movd $0, (r3, r2)	/* Reset the high bit */
	movd (r9, r8), (r1, r0)	/* Add the overflow from the high bit */
	popret	$3, r7		/* Return */
#endif

#ifdef L_moddi3
	.text
	.align 4
	.globl ___moddi3

___moddi3:
	push	$4, r7, ra

	/* Param #1 Long Long low bit first */
	loadd	12(sp), (r1, r0)
	loadd	16(sp), (r3, r2)

	/* Param #2 Long Long low bit first */
	loadd	20(sp), (r5, r4)
	loadd	24(sp), (r7, r6)
	
	subd	$18, (sp)

	/* Set neg to 0 */
	storw $0, 16(sp)
	
	movd	$-1, (r9, r8) 	/* Temp set to FFFFFFFF */

	/* Compare if param1 is greater than 0 */	
	cmpw $0, r3
	ble L4

	/* Invert param1 and neg */
	xord (r9, r8), (r1, r0)	/* Xor low bits of param 1 with temp */
	xord (r9, r8), (r3, r2)	/* Xor high bits of param 1 with temp */
	addd $1, (r1, r0)	/* Add 1 to low bits of param 1 */
	storw $1, 16(sp)
	bcc L4			/* If no carry occurred go to L4 */
	addd $1, (r3, r2)	/* Add 1 to high bits of param 1 */

L4:	stord (r1, r0), 0(sp)
	stord (r3, r2), 4(sp)
	
	/* Compare if param2 is greater than 0 */	
	cmpw $0, r7
	ble L5

	/* Invert param2 and neg */
	xord (r9, r8), (r5, r4)	/* Xor low bits of param 2 with temp */
	xord (r9, r8), (r7, r6)	/* Xor high bits of param 2 with temp */
	addd $1, (r5, r4)	/* Add 1 to low bits of param 2 */
	bcc L5			/* If no carry occurred go to L5 */
	addd $1, (r7, r6)	/* Add 1 to high bits of param 2 */

L5:	stord (r5, r4), 8(sp)
	stord (r7, r6), 12(sp)
	movw $1, r2

	/* Call udivmoddi3 */
#ifdef __PIC__
	loadd	___udivmoddi3@cGOT(r12), (r1,r0)
	jal	(r1,r0)
#else
	bal (ra), ___udivmoddi3
#endif

	/* If (neg) */
	loadw 16(sp), r10	/* Load neg from stack */
	addd	$18, (sp)
	cmpw $0, r10		/* Compare 0 with neg */
	beq	Lexit__
	
	/* Neg = -Neg */
	xord (r9, r8), (r1, r0)	/* Xor low bits of ures with temp */
	xord (r9, r8), (r3, r2)	/* Xor high bits of ures with temp */
	addd $1, (r1, r0)	/* Add 1 to low bits of ures */
	bcc	Lexit__
	addd $1, (r3, r2)	/* Add 1 to high bit of ures */
Lexit__:
#  ifdef __ID_SHARED_LIB__
	pop	$2, r12
#  endif
	popret	$4, r7, ra
#endif

#ifdef L_muldi3
	.text
	.align 4
	.globl ___muldi3

___muldi3:
	push	$2, r13
	push	$7, r7

	/* Param #1 Long Long low bit first */
	loadd   18(sp), (r1, r0)
	loadd   22(sp), (r3, r2)

	/* Param #2 Long Long low bit first */
	loadd   26(sp), (r5, r4)
	loadd   30(sp), (r7, r6)

	/* Clear r13, r12 */	
	movd $0, (r12)	
	movd $0, (r13)

	/* Set neg */
	movw $0, r10

	/* Compare if param1 is greater than 0 */	
	cmpw $0, r3
	ble L1

	/* Invert param1 and neg */
	movd $-1, (r9, r8) 	/* Temp set to FFFFFFFF */
	xord (r9, r8), (r1, r0)	/* Xor low bits of param 1 with temp */
	xord (r9, r8), (r3, r2)	/* Xor high bits of param 1 with temp */
	addd $1, (r1, r0)	/* Add 1 to low bits of param 1 */
	xorw $1, r10		/* Invert neg */
	bcc L1			/* If no carry occurred go to L1 */
	addd $1, (r3, r2)	/* Add 1 to high bits of param 1 */

L1:	/* Compare if param2 is greater than 0 */	
	cmpw $0, r7
	ble L2	
	
	/* Invert param2 and neg */
	movd $-1, (r9, r8)	/* Temp set to FFFFFFFF */
	xord (r9, r8), (r5, r4)	/* Xor low bits of param 2 with temp */
	xord (r9, r8), (r7, r6)	/* Xor high bits of param 2 with temp */
	addd $1, (r5, r4)	/* Add 1 to low bits of param 2 */
	xorw $1, r10		/* Invert neg */
	bcc L2			/* If no carry occurred go to L2 */
	addd $1, (r7, r6)	/* Add 1 to high bits of param 2 */
	
L2:	storw r10, 18(sp)	/* Store neg to stack so we can use r10 */

	/* B*D */
	/* Bl*Dl */
	macuw r0, r4, (r12)	/* Multiply r0 and r4 and add to r12 */
		
	/* Bh*Dl */
	movd $0, (r9, r8)	/* Clear r9, r8 */
	macuw r1, r4, (r9, r8)	/* Multiply Bh*Dl and add result to (r9, r8) */
	movw r9,  r10		/* Shift left: r9 to r10 */
	lshd $16, (r9, r8)	/* Shift left: r8 to r9 */
	movw $0, r11		/* Clear r11 */
	addd (r9, r8), (r12)	/* Add (r9, r8) to r12 */
	bcc L3			/* If no carry occurred go to L3 */
	addd $1, (r13)		/* If carry occurred add 1 to r13 */

L3:	addd (r11, r10), (r13)	/* Add (r11, r10) to r13 */
		
	/* Bl*Dh */
	movd $0, (r9, r8)	/* Clear (r9, r8) */
	macuw r0, r5, (r9, r8)	/* Multiply r0 and r5 and stor in (r9, r8) */
	movw r9, r10		/* Shift left: r9 to r10 */
	lshd $16, (r9, r8)	/* Shift left: r8 to r9 */
	addd (r9, r8), (r12)	/* Add (r9, r8) to r12 */
	bcc L4			/* If no carry occurred go to L4 */ 
	addd $1, (r13)		/* If carry occurred add 1 to r13 */

L4:	addd (r11, r10), (r13)	/* Add (r11, r10) to r13 */

	/* Bh*Dh */
	movd $0, (r9, r8)	/* Clear (r9, r8) */
	macuw r1, r5, (r9, r8)	/* Multiply r1 and r5 and add to r13 */
	addd (r9, r8), (r13)	/* Add (r9, r8) to result */

	/* A*D */
	/* Al*Dl */
	movd $0, (r11, r10)	/* Clear (r11, r10) */
	macuw r2, r4, (r11, r10)/* Multiply r2 and r4 and add to (r11, r10) */
	
	addd (r13), (r11, r10)	/* Copy r13 to (r11, r10) */

	/* Al*Dh */
	movd $0, (r9, r8)	/* Clear (r9, r8) */
	macuw r2, r5, (r9, r8)	/* Multiply r2 and r5 and add to (r9, r8) */
	addw r8, r11		/* Add r8 to r11 */
	
	/* Ah*Dl */
	muluw r3, (r5, r4)	/* Multiply r3 and r4 and stor in (r5, r4) */
	addw r4, r11		/* Add r4 to r11 */

	/* B*C */
	/* Bl*Cl */
	movd $0, (r9, r8)	/* Clear (r9, r8) */
	macuw r0, r6, (r9, r8)	/* Multiply r0 and r6 and add to (r9, r8) */
	addd (r9, r8), (r11, r10)/* Add (r9, r8) to result */

	/* Bl*Ch */
	movd $0, (r9, r8)	/* Clear (r9, r8) */
	macuw r0, r7, (r9, r8)	/* Multiply r0 and r7 and add to (r9, r8) */
	addw r8, r11		/* Add r8 to r11 */

	loadw 18(sp), r8	/* Load neg from stack */

	/* Bh*Cl */
	muluw r1, (r7, r6)	/* Multiply r1 and r6 and stor in (r7, r6) */
	addw r6, r11		/* Add r6 to r11 */

E1:	movd (r11, r10), (r3, r2)
	movd (r12), (r1, r0)	

	/* If (neg) */
	cmpw $0, r8		/* Compare 0 with neg */
	beq	Lexit__
	
	/* Neg = -Neg */
	movd $-1, (r9, r8) 	/* Temp set to FFFFFFFF */
	xord (r9, r8), (r1, r0)	/* Xor low bits of result with temp */
	xord (r9, r8), (r3, r2) /* Xor high bits of result with temp */
	addd $1, (r1, r0)	/* Add 1 to low bits of result */
	bcc	Lexit__
	addd $1, (r3, r2)	/* Add 1 to high bit of result */
Lexit__:
	pop	$7, r7
	popret	$2, r13
#endif

#ifdef L_negdi2
	.text
	.align 4
	.globl ___negdi2

___negdi2:
	/* Load parameter from the registers in this order */
	loadd 0(sp), (r1, r0)
	loadd 4(sp), (r3, r2)
	
	movd $-1, (r6, r5)	/* Set temp to FFFFFFFF */
	xord (r6, r5), (r1, r0)	/* Xor low bits with temp */
	xord (r6, r5), (r3, r2)	/* Xor high bits with temp */
	addd $1, (r1, r0)	/* Add one */
	jcc (ra)
	addd $1, (r3, r2)	/* Add the carry to the high bits */
	jump (ra)
#endif

#ifdef L_udivdi3
	.text
	.align 4
	.globl ___udivdi3

___udivdi3:
	movw $0, r2
	br ___udivmoddi3
#endif

#ifdef L_udivmoddi3
	.text
	.align 4
	.globl ___udivmoddi3

___udivmoddi3:
	push	$2, r13
	push	$7, r7

	/* Param #1 Long Long low bit first */
	loadd	18(sp), (r1, r0)
	storw	r2, 18(sp)	/* Store modulo on stack */
	loadd	22(sp), (r3, r2)

	/* Param #2 Long Long low bit first */
	loadd	26(sp), (r5, r4)
	loadd	30(sp), (r7, r6)

	/* Set ures to 0 */
	movd $0, (r13)
	movd $0, (r12)
	
	cmpd (r12), (r5, r4)
	beq LE

L5:	movd $1, (r9, r8)	/* Store 1 in low bits from bit */
	movd $0, (r11, r10)	/* Store 0 in high bits from bit */
	
L6:	/* While (den < num && (!den & (1LL<<63))) */
	/* Compare high bits from param 1 and param 2 */
	cmpd (r7, r6), (r3, r2)	
	bhi L10			/* If param 2 is greater go to L10 */
	bne L8			/* If param 1 is greater go to L8 */
	cmpd (r5, r4), (r1, r0)	/* Compare low bits from param 1 and param 2 */
	/*  If param 2 is greater or the same go to L1 */
	bhs L10			 

L8:	/* Check if most significant bit of param 2 is set */
	tbit $15, r7		
	bfs L10			/* If PSR is set go to L10 */

	/* Shift bit */
	lshd $1, (r11, r10)	/* Shift left: high bits of bit */ 
	/* Check if most significant bit of bit is set */
	tbit $15, r9		
	lshd $1, (r9, r8)	/* Shift left: low bits of bit */
	bfs L28			/* If PSR is set go to L28 */

L9:	/* Shift b */
	lshd $1, (r7, r6)	/* Shift left: high bits of param 2 */
	/* Check if most significant bit of param 2 is set */
	tbit $15, r5		
	lshd $1, (r5, r4)	/* Shift left: low bits of param 2 */
	bfc L6			/* If PSR is set go to L6 */
	addw $1, r6		/* Add 1 to the highest bits of b */
	br L6			/* Go to L6 */
	
L10:	/* While (bit) */
	cmpd $0, (r11, r10)	
	bne L11
	cmpd $0, (r9, r8)
	beq E1

L11:	/* If (num >= den) */
	cmpd (r3, r2), (r7, r6)	/* Compare high bits of param 1 and param 2 */
	blo L15			/* If param 1 lower than param 2 go to L15 */
	bne L12			/* If not equal go to L12 */	
	cmpd (r1, r0), (r5, r4)	/* Compare low bits of param 1 and param 2 */
	blo L15			/* If param 1 lower than param 2 go to L15 */

L12:	/* Ures |= bit */
	ord (r11, r10), (r13)
	ord (r9, r8), (r12)	

	/* Num -= den */
	subd (r7, r6), (r3, r2) /* Subtract highest 32 bits from each other */
	subd (r5, r4), (r1, r0)	/* Subtract lowest 32 bits from each other */
	bcc L15			/* If no carry occurred go to L15 */
	subd $1, (r3, r2)	/* Subtract the carry */
	
L15:	/* Shift bit to the right */
	lshd $-1, (r9, r8)	/* Shift right: low bits of bit */
	/* Check if least significant bit of high bits is set */
	tbit $0, r10		
	lshd $-1, (r11, r10)	/* Shift right: high bits of bit */
	bfs L18			/* If PSR is set go to L18 */

L17:	/* Shift param#2 to the right */
	lshd $-1, (r5, r4)	/* Shift right: low bits of param 2 */
	/* Check if least significant bit of high bits is set */
	tbit $0, r6		
	lshd $-1, (r7, r6)	/* Shift right: high bits of param 2 */
	bfc L10			/* If PSR is not set go to L10 */
	/* Or with 0x8000 to set most significant bit */
	orw $32768, r5		
	br L10			/* Go to L10 */

L18:	/* Or with 0x8000 to set most significant bit */
	orw $32768, r9		
	br L17

L28: 	/* Left shift bit */
	addw $1, r10		/* Add 1 to highest bits of bit */
	br L9			/* Go to L9 */

LE:	cmpd (r12), (r7, r6)
	bne L5
	excp dvz
	br	Lexit__

E1:	loadw	18(sp), r4
	cmpw $0, r4
	bne	Lexit__

	/* Return result */
	movd (r12), (r1, r0)
	movd (r13), (r3, r2)
Lexit__:
	pop	$7, r7
	popret	$2, r13
#endif

#ifdef L_umoddi3
	.text
	.align 4
	.globl ___umoddi3

___umoddi3:
	movw $1, r2
	br ___udivmoddi3
#endif

