// picoChip ASM file
//
//   Support for 16-bit signed division/modulus.
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
.global __divmodhi4
__divmodhi4:
_picoMark_FUNCTION_BEGIN=

// picoChip Function Prologue : &__divmodhi4 = 4 bytes

	// 16-bit signed division. Most of the special cases are dealt
	// with by the 15-bit signed division library (e.g., division by
	// zero, division by 1, and so on). This wrapper simply inverts	
	// any negative inputs, calls the 15-bit library, and flips any
	// results as necessary.  The
	// only special cases to be handled here are where either the 
	// divisor or the dividend are the maximum negative values.

	// Encode r5 with a bit pattern which indicates whether the
	// outputs of the division must be negated. The MSB will be set
	// to the sign of the dividend (which controls the remainder's
	// sign), while the LSB will store the XOR of the two signs,
	// which indicates the quotient's sign. R5 is not modified by the
	// 15-bit divmod routine.
	sub.0 r1,16#8000#,r15 \ asr.1 r0,15,r4
	beq divisorIsLargestNegative \ lsr.0 r1,15,r3
=->	sub.0 r0,16#8000#,r15 \ xor.1 r3,r4,r5

	// Handle least negative dividend with a special case. Note that the
	// absolute value of the divisor is also computed here.
	add.0 [asr r1,15],r1,r3	\ beq dividendIsLargestNegative
=->	xor.0 [asr r1,15],r3,r1 \ stw lr,(fp)-1	
	
	// Compute the absolute value of the dividend, and call the main
	// divide routine.
	add.0 r4,r0,r2 \ jl (&__divmod15)  // fn_call &__divmod15
=->	xor.0 r4,r2,r0

handleNegatedResults:	
	// Speculatively store the negation of the results.
	sub.0 0,r0,r2 \ sub.1 0,r1,r3

	// Does the quotient need negating? The LSB indicates this.
	and.0 r5,1,r15 \ ldw (fp)-1,lr
	copyne r2,r0
		
	asr.0 r5,15,r15 \ jr (lr)
=->	copyne r3,r1
	
dividendIsLargestNegative:

	// Divide the constant -32768. Use the Hacker's Delight
	// algorithm (i.e., ((dividend / 2) / divisor) * 2) gives
	// approximate answer). This code is a special case, so no
	// great effort is made to make it fast, only to make it
	// small.

	lsr.0 r0,1,r0 \ jl (&__divmod15)  // fn_call &__divmod15
=->	stw r1,(fp)-2

	// Load the original divisor, and compute the new quotient and
	// remainder.	
	lsl.0 r0,1,r0 \ ldw (fp)-2,r3
	lsl.0 r1,1,r1 // Fill stall slot

	// The error in the quotient is 0 or 1. The error can be determined
	// by comparing the remainder to the original divisor. If the
	// remainder is bigger, then an error of 1 has been introduced,
	// which must be fixed.
	sub.0 r1,r3,r15
	blo noCompensationForError
=->	nop	
	add.0 r0,1,r0 \ sub.1 r1,r3,r1
noCompensationForError:
	bra handleNegatedResults
=->	nop

divisorIsLargestNegative:	
	// The flags indicate whether the dividend is also the maximum negative
	copy.0 r0,r1 \ copy.1 0,r0
	copyeq r0,r1 \ jr (lr)
=->	copyeq 1,r0

_picoMark_FUNCTION_END=
// picoChip Function Epilogue : __divmodhi4
	

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
.uleb128 0x4	// <-- FUNCTION_STACK_SIZE_GOES_HERE
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
.ascii 16#5f# 16#64# 16#69# 16#76# 16#6d# 16#6f# 16#64# 16#68# 16#69# 16#34# 16#0# // Function name `_divmodhi4'
.unalignedInitWord _picoMark_FUNCTION_BEGIN	// DW_AT_low_pc
.unalignedInitWord _picoMark_FUNCTION_END	// DW_AT_high_pc

.initByte 0x0	// end of compile unit children.

_picoMark_DEBUG_INFO_END=

//============================================================================
// END OF DWARF
//============================================================================
.section .endFile
