// picoChip ASM file
//
//   Support for 32-bit unsigned division/modulus.
//
//   Copyright (C) 2003-2014 Free Software Foundation, Inc.
//   Contributed by Picochip Ltd.
//   Maintained by Daniel Towner (daniel.towner@picochip.com)
//
//   This file is free software; you can redistribute it and/or modify it
//   under the terms of the GNU General Public License as published by the
//   Free Software Foundation; either version 3, or (at your option) any
//   later version.
//
//   This file is distributed in the hope that it will be useful, but
//   WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   Under Section 7 of GPL version 3, you are granted additional
//   permissions described in the GCC Runtime Library Exception, version
//   3.1, as published by the Free Software Foundation.
//
//   You should have received a copy of the GNU General Public License and
//   a copy of the GCC Runtime Library Exception along with this program;
//   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
//   <http://www.gnu.org/licenses/>.
	
.section .text

.align 8
.global __udivmodsi4
__udivmodsi4:
_picoMark_FUNCTION_BEGIN=
// picoChip Function Prologue : &__udivmodsi4 = 24 bytes
	
	// Schedule the register saves alongside the special cases, so that
	// if the special cases fail, the registers will have already
	// been stored onto the stack.
	SUB.0 R3,R1,r15 \ STL R[13:12],(FP)-1
	BHS skipCommonCase \ STL R[9:8],(FP)-4
=->	SUB.0 R2,1,r15 \ STL R[11:10],(FP)-3
	
_L2:
	// Flags set above, and in _L2 caller.
	BNE restOfCode
=->	SUB.0 R3,0,r15
	BNE restOfCode 
=->	COPY.0 R0,R4 \ COPY.1 R1,R5
	JR (R12)	// Return to caller
=->	COPY.0 0,R6 \ COPY.1 0,R7
	// Never reach here

skipCommonCase:
	SUB.0 R3,R1,r15
	BNE _L3	// (Reversed branch) 
=->	SUB.0 R2,R0,r15 // Must be set in delay slot, so ready by _L9

_L9:
	BLO _L2	// (Reversed branch)
=->	SUB.0 R2,1,r15
	
_L3:
	SUB.0 R2,R0,r15
	BEQ _L10	// (Reversed branch)
=->	SUB.0 R1,R3,r15 // Set flags for branch at _L10 
	
_L4:
	// greater than
	COPY.0 0,R4 \ COPY.1 0,R5 \ JR (R12)	// Return to caller
=->	COPY.0 R0,R6 \ COPY.1 R1,R7
	// Doesn't reach here.
		
_L10:
	// Flags set in _L10 call delay slot.
	BNE _L4 
=->	COPY.0 1,R4 \ COPY.1 0,R5
	JR (R12)	// Return to caller
=->	COPY.0 0,R6 \ COPY.1 0,R7

restOfCode:	

// Prologue
	
	// Register saves scheduled alongside special cases above.
	ADD.0 FP,-20,FP \ STW R14,(FP)-4

	// The following can be scheduled together.
	// dividend in R[9:8] (from R[1:0])
	// divisor in R[7:6] (from R[3:2])
	// R14 := clzsi2 (dividend)	
	// R0 := clzsi2 (divisor)
	JL (&__clzsi2) \ COPY.0 R0,R8 \ COPY.1 R1,R9
=->	COPY.0 R2,R6 \ COPY.1 R3,R7
	COPY.0 R0,R14 \ JL (&__clzsi2)
=->	COPY.0 R6,R0 \ COPY.1 R7,R1

	// R14 := R0 - R14
	SUB.0 R0,R14,R14

	ADD.0 R14,1,R0	// R0 := R14 + 1 (HI)
	
	// R[11:10] = R[7,6] << R14
	SUB.0 15,R14,r15
	LSL.0 R6,R14,R11 \ BLT setupDivstepLoop
=->	SUB.0 0,R14,R4 \ COPY.1 0,R10

	// Zero shift is a special case. Shifting by zero within a 16-bit
	// source object is fine, but don't execute the OR of the right-shift
	// into the final result.
	LSL.0 R7,R14,R11 \ BEQ setupDivstepLoop
=->	LSL.0 R6,R14,R10
	
	LSR.0 R6,R4,R4
	OR.0 R11,R4,R11
	
setupDivstepLoop:

	// R[5:4] := R[9:8] (SI)
	COPY.0 R8,R4 \ COPY.1 R9,R5
	COPY.0 0,R6 \ COPY.1 R0,R8

	// Store original value of loopCount for use after the loop.
	// The Subtraction is handled in the tail of the loop iteration
	// after this point.
	SUB.0 R4,R10,R0 \ COPY.1 R8,R14
	
	// workingResult in R4,5,6
	// temps in r0,1,2 and r7
	// alignedDivisor in R10,11
	// loopCount in r8
	// r3, r9 scratch, used for renaming.
	
loopStart:	
	// R0 := R4 - zeroExtend (R10) - only need 33-bits (i.e., 48-bits)
	SUBB.0 R5,R11,R1 \ LSR.1 R0,15,R3
	SUBB.0 R6,0,R2 \ LSR.1 R1,15,R6

	// if (carry) goto shiftOnly
	SUB.0 R8,1,R8 \ BNE shiftOnly
=->	LSR.0 R4,15,R7 \ LSL.1 R1,1,R9
	
	OR.0 [LSL R0,1],1,R4 \ BNE loopStart
=->	SUB.0 R4,R10,R0 \ OR.1 R9,R3,R5
	
	BRA loopEnd
	
shiftOnly:	

	OR.0 [LSL R5,1],R7,R5 \ BNE loopStart \ LSR.1 R5,15,R6
=->	SUB.0 [LSL R4,1],R10,R0 \LSL.1 R4,1,R4
	
// End of loop
loopEnd:

	// Schedule the computation of the upper word after shifting
	// alongside the decision over whether to branch, and the register
	// restores.
	// R10 is filled with a useful constant.
	SUB.0 15,r14,r15 \ LDL (FP)4,R[13:12]
	SUB.1 0,R14,R1 // Don't set flags!
	LSL.0 R6,R1,R3 \ LDL (FP)-4,R[9:8]

	BLT remainderHasMoreThan16Bits \ LSR.0 R5,R14,R7 \ COPY.1 -1,R10
=->	LSL.0 R5,R1,R2 \ OR.1 R7,R3,R3

	LSR.0 R4,R14,R3 \ COPY.1 R3,R7
	BRA epilogue \ LSR.0 -1,R1,R0 \ COPY.1 0,R5
=->	OR.0 R3,R2,R6 \ AND.1 R0,R4,R4
	
remainderHasMoreThan16Bits:	

	LSL.0 R10,R14,R1 \ COPY.1 R3,R6
	XOR.0 R10,R1,R1 \ COPY.1 0,R7
	AND.0 R1,R5,R5

epilogue:
	
	JR (R12) \ LDW (FP)-4,R14
=->	LDL (FP)-3,R[11:10]

_picoMark_FUNCTION_END=
	
// picoChip Function Epilogue : udivmodsi4

//============================================================================
// All DWARF information between this marker, and the END OF DWARF
// marker should be included in the source file. Search for
// FUNCTION_STACK_SIZE_GOES_HERE and FUNCTION NAME GOES HERE, and
// provide the relevant information. Add markers called
// _picoMark_FUNCTION_BEGIN and _picoMark_FUNCTION_END around the
// function in question.
//============================================================================

//============================================================================
// Frame information. 
//============================================================================

.section .debug_frame
_picoMark_DebugFrame=

// Common CIE header.
.unalignedInitLong _picoMark_CieEnd-_picoMark_CieBegin
_picoMark_CieBegin=
.unalignedInitLong 0xffffffff
.initByte 0x1	// CIE Version
.ascii 16#0#	// CIE Augmentation
.uleb128 0x1	// CIE Code Alignment Factor
.sleb128 2	// CIE Data Alignment Factor
.initByte 0xc	// CIE RA Column
.initByte 0xc	// DW_CFA_def_cfa
.uleb128 0xd
.uleb128 0x0
.align 2
_picoMark_CieEnd=

// FDE 
_picoMark_LSFDE0I900821033007563=
.unalignedInitLong _picoMark_FdeEnd-_picoMark_FdeBegin
_picoMark_FdeBegin=
.unalignedInitLong _picoMark_DebugFrame	// FDE CIE offset
.unalignedInitWord _picoMark_FUNCTION_BEGIN	// FDE initial location
.unalignedInitWord _picoMark_FUNCTION_END-_picoMark_FUNCTION_BEGIN
.initByte 0xe	// DW_CFA_def_cfa_offset
.uleb128 0x18	// <-- FUNCTION_STACK_SIZE_GOES_HERE
.initByte 0x4	// DW_CFA_advance_loc4
.unalignedInitLong _picoMark_FUNCTION_END-_picoMark_FUNCTION_BEGIN
.initByte 0xe	// DW_CFA_def_cfa_offset
.uleb128 0x0
.align 2
_picoMark_FdeEnd=

//============================================================================
// Abbrevation information.
//============================================================================

.section .debug_abbrev
_picoMark_ABBREVIATIONS=

.section .debug_abbrev
	.uleb128 0x1	// (abbrev code)
	.uleb128 0x11	// (TAG: DW_TAG_compile_unit)
	.initByte 0x1	// DW_children_yes
	.uleb128 0x10	// (DW_AT_stmt_list)
	.uleb128 0x6	// (DW_FORM_data4)
	.uleb128 0x12	// (DW_AT_high_pc)
	.uleb128 0x1	// (DW_FORM_addr)
	.uleb128 0x11	// (DW_AT_low_pc)
	.uleb128 0x1	// (DW_FORM_addr)
	.uleb128 0x25	// (DW_AT_producer)
	.uleb128 0x8	// (DW_FORM_string)
	.uleb128 0x13	// (DW_AT_language)
	.uleb128 0x5	// (DW_FORM_data2)
	.uleb128 0x3	// (DW_AT_name)
	.uleb128 0x8	// (DW_FORM_string)
.initByte 0x0
.initByte 0x0

	.uleb128 0x2	;# (abbrev code)
	.uleb128 0x2e	;# (TAG: DW_TAG_subprogram)
.initByte 0x0	;# DW_children_no
	.uleb128 0x3	;# (DW_AT_name)
	.uleb128 0x8	;# (DW_FORM_string)
	.uleb128 0x11	;# (DW_AT_low_pc)
	.uleb128 0x1	;# (DW_FORM_addr)
	.uleb128 0x12	;# (DW_AT_high_pc)
	.uleb128 0x1	;# (DW_FORM_addr)
.initByte 0x0
.initByte 0x0

.initByte 0x0

//============================================================================
// Line information. DwarfLib requires this to be present, but it can
// be empty.
//============================================================================

.section .debug_line
_picoMark_LINES=

//============================================================================
// Debug Information
//============================================================================
.section .debug_info

//Fixed header.
.unalignedInitLong _picoMark_DEBUG_INFO_END-_picoMark_DEBUG_INFO_BEGIN
_picoMark_DEBUG_INFO_BEGIN=
.unalignedInitWord 0x2
.unalignedInitLong _picoMark_ABBREVIATIONS
.initByte 0x2

// Compile unit information.
.uleb128 0x1	// (DIE 0xb) DW_TAG_compile_unit)
.unalignedInitLong _picoMark_LINES
.unalignedInitWord _picoMark_FUNCTION_END
.unalignedInitWord _picoMark_FUNCTION_BEGIN
// Producer is `picoChip'
.ascii 16#70# 16#69# 16#63# 16#6f# 16#43# 16#68# 16#69# 16#70# 16#00#
.unalignedInitWord 0xcafe // ASM language
.ascii 16#0# // Name. DwarfLib expects this to be present.

.uleb128 0x2	;# (DIE DW_TAG_subprogram)

// FUNCTION NAME GOES HERE. Use `echo name | od -t x1' to get the hex. Each hex
// digit is specified using the format 16#XX#
.ascii 16#5f# 16#75# 16#64# 16#69# 16#76# 16#6d# 16#6f# 16#64# 16#73# 16#69# 16#34# 16#0# // Function name `_udivmodsi4'
.unalignedInitWord _picoMark_FUNCTION_BEGIN	// DW_AT_low_pc
.unalignedInitWord _picoMark_FUNCTION_END	// DW_AT_high_pc

.initByte 0x0	// end of compile unit children.

_picoMark_DEBUG_INFO_END=

//============================================================================
// END OF DWARF
//============================================================================
.section .endFile
// End of picoChip ASM file
