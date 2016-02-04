###################################
# 
#  Copyright (C) 2009-2016 Free Software Foundation, Inc.
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
#  modsi3.S
# 
#  modulo operation for 64 bit integers.
# 
#######################################


	.globl	__moddi3
	.ent	__moddi3
__moddi3:
	.frame	r1,0,r15	

#Change the stack pointer value and Save callee saved regs
	addik	r1,r1,-24
	swi	r25,r1,0
	swi	r26,r1,4
	swi	r27,r1,8	# used for sign
	swi	r28,r1,12	# used for loop count
	swi	r29,r1,16	# Used for div value High
	swi	r30,r1,20	# Used for div value Low

#Check for Zero Value in the divisor/dividend
	OR	r9,r5,r6			# Check for the op1 being zero
	BEQID	r9,$LaResult_Is_Zero		# Result is zero
	OR	r9,r7,r8			# Check for the dividend being zero
	BEQI	r9,$LaDiv_By_Zero	        # Div_by_Zero   # Division Error
	BGEId	r5,$La1_Pos 
	XOR	r27,r5,r7			# Get the sign of the result
	RSUBI	r6,r6,0				# Make dividend positive
	RSUBIC	r5,r5,0				# Make dividend positive
$La1_Pos:
	BGEI	r7,$La2_Pos
	RSUBI	r8,r8,0				# Make Divisor Positive
	RSUBIC	r9,r9,0				# Make Divisor Positive
$La2_Pos:
	ADDIK	r4,r0,0				# Clear mod low
	ADDIK	r3,r0,0                	        # Clear mod high
	ADDIK	r29,r0,0			# clear div high
	ADDIK	r30,r0,0			# clear div low
	ADDIK	r28,r0,64			# Initialize the loop count
   # First part try to find the first '1' in the r5/r6
$LaDIV1:
	ADD	r6,r6,r6
	ADDC	r5,r5,r5			# left shift logical r5
	BGEID	r5,$LaDIV1			
	ADDIK	r28,r28,-1
$LaDIV2:
	ADD	r6,r6,r6
	ADDC	r5,r5,r5	# left shift logical r5/r6 get the '1' into the Carry
	ADDC	r4,r4,r4	# Move that bit into the Mod register
	ADDC	r3,r3,r3	# Move carry into high mod register
	rsub	r18,r7,r3	# Compare the High Parts of Mod and Divisor
	bnei	r18,$L_High_EQ
	rsub	r18,r6,r4	# Compare Low Parts only if Mod[h] == Divisor[h]
$L_High_EQ:	
	rSUB	r26,r8,r4	# Subtract divisor[L] from Mod[L]
	rsubc	r25,r7,r3	# Subtract divisor[H] from Mod[H]
	BLTi	r25,$LaMOD_TOO_SMALL
	OR	r3,r0,r25	# move r25 to mod [h]
	OR	r4,r0,r26	# move r26 to mod [l]
	ADDI	r30,r30,1
	ADDC	r29,r29,r0
$LaMOD_TOO_SMALL:
	ADDIK	r28,r28,-1
	BEQi	r28,$LaLOOP_END
	ADD	r30,r30,r30		# Shift in the '1' into div [low]
	ADDC	r29,r29,r29		# Move the carry generated into high
	BRI	$LaDIV2   # Div2
$LaLOOP_END:
	BGEI	r27,$LaRETURN_HERE
	rsubi	r30,r30,0
	rsubc	r29,r29,r0
	BRI	$LaRETURN_HERE
$LaDiv_By_Zero:
$LaResult_Is_Zero:
	or	r29,r0,r0	# set result to 0 [High]
	or	r30,r0,r0	# set result to 0 [Low]
$LaRETURN_HERE:
# Restore values of CSRs and that of r29 and the divisor and the dividend
	
	lwi	r25,r1,0
	lwi	r26,r1,4
	lwi	r27,r1,8
	lwi	r28,r1,12
	lwi	r29,r1,16
	lwi	r30,r1,20
	rtsd	r15,8
	addik r1,r1,24
        .end __moddi3
	
