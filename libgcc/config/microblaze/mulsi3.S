###################################-*-asm*- 
# 
#  Copyright (C) 2009-2013 Free Software Foundation, Inc.
#
#  Contributed by Michael Eager <eager@eagercon.com>.
#
#  This file is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3, or (at your option) any
#  later version.
#
#  GCC is distributed in the hope that it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
#  License for more details.
#
#  Under Section 7 of GPL version 3, you are granted additional
#  permissions described in the GCC Runtime Library Exception, version
#  3.1, as published by the Free Software Foundation.
#
#  You should have received a copy of the GNU General Public License and
#  a copy of the GCC Runtime Library Exception along with this program;
#  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
#  <http://www.gnu.org/licenses/>. 
# 
#  mulsi3.S
# 
#  Multiply operation for 32 bit integers.
#	Input :	Operand1 in Reg r5
#		Operand2 in Reg r6
#	Output: Result [op1 * op2] in Reg r3
# 
#######################################

	.globl	__mulsi3
	.ent	__mulsi3
	.type	__mulsi3,@function
__mulsi3:
	.frame	r1,0,r15
	add	r3,r0,r0
	BEQI	r5,$L_Result_Is_Zero      # Multiply by Zero
	BEQI	r6,$L_Result_Is_Zero      # Multiply by Zero
	BGEId	r5,$L_R5_Pos 
	XOR	r4,r5,r6                  # Get the sign of the result
	RSUBI	r5,r5,0	                  # Make r5 positive
$L_R5_Pos:
	BGEI	r6,$L_R6_Pos
	RSUBI	r6,r6,0	                  # Make r6 positive
$L_R6_Pos:	
	bri	$L1
$L2:	
	add	r5,r5,r5
$L1:	
	srl	r6,r6
	addc	r7,r0,r0
	beqi	r7,$L2
	bneid	r6,$L2
	add	r3,r3,r5	
	blti	r4,$L_NegateResult			
	rtsd	r15,8
	nop
$L_NegateResult:
	rtsd	r15,8
	rsub	r3,r3,r0
$L_Result_Is_Zero:
	rtsd	r15,8
	addi	r3,r0,0
	.end __mulsi3
	.size	__mulsi3, . - __mulsi3
