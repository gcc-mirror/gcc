// picoChip ASM file
//
//   Support for 32-bit signed division/modulus.
//
//   Copyright (C) 2003-2013 Free Software Foundation, Inc.
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
.global __divmodsi4
__divmodsi4:
_picoMark_FUNCTION_BEGIN=
// picoChip Function Prologue : &__divmodsi4 = 8 bytes

	// Note: optimising for size is preferred over optimising for speed.

	// Note: the frame is setup throughout the following instructions,
	// and is complete at the point the udivmodsi4 function is called. 	

	// Note that R9 is encoded with a pattern which indicates
	// whether the remainder and quotient should be negated on
	// completion. The MSB is set to the sign of the dividend
	// (i.e., the sign of the remainder), while the LSB encodes
	// the XOR of the two input's signs (i.e., the sign of the
	// quotient.
	
	// If dividend is negative, invert the dividend and flag.
	ASR.0 r1,15,r4
	BEQ dividendNotNegative
=->	STL R[9:8],(FP)-2

	// Dividend is negative - negate dividend.
        SUB.0 0,R0,R0
        SUBB.0 0,R1,R1

dividendNotNegative:
			
	// If divisor is negative, invert the divisor.
	AND.0 [lsr r3,15],1,r5
	SUB.0 R3,0, r15
	BGE divisorNotNegative
=->	XOR.0 r4,r5,r9

	// Divisor is negative - negate divisor.
        SUB.0 0,R2,R2
        SUBB.0 0,R3,R3

divisorNotNegative:
	
        STL R[13:12],(FP)-1 \ JL (&__udivmodsi4)
=->	SUB.0 FP,8,FP  // udivmodsi expects the frame to be valid still.
	
	// The LSB of R9 indicates whether the quotient should be negated.
	AND.0 r9,1,r15
	BEQ skipQuotientNegation
=->	LDL (FP)1,R[13:12]	// Convenient point to restore link/fp
	
        SUB.0 0,R4,R4
        SUBB.0 0,R5,R5	

skipQuotientNegation:		

	// The MSB of R9 indicates whether the remainder should be negated.
	ASR.0 R9,15,r15
	BEQ epilogue

        SUB.0 0,R6,R6
        SUBB.0 0,R7,R7

epilogue:	

	JR (R12)
=->	LDL (FP)-2,R[9:8]

_picoMark_FUNCTION_END=
// picoChip Function Epilogue : __divmodsi4

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
.uleb128 0x8	// <-- FUNCTION_STACK_SIZE_GOES_HERE
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
.ascii 16#5f# 16#64# 16#69# 16#76# 16#6d# 16#6f# 16#64# 16#73# 16#69# 16#34# 16#0# // Function name `_divmodsi4'
.unalignedInitWord _picoMark_FUNCTION_BEGIN	// DW_AT_low_pc
.unalignedInitWord _picoMark_FUNCTION_END	// DW_AT_high_pc

.initByte 0x0	// end of compile unit children.

_picoMark_DEBUG_INFO_END=

//============================================================================
// END OF DWARF
//============================================================================

.section .endFile
// End of picoChip ASM file
