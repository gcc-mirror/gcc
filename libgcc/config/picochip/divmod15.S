// picoChip ASM file
//
//   Support for 16-bit unsigned division/modulus.
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

.global __divmod15
__divmod15:
_picoMark_FUNCTION_BEGIN=
	
// picoChip Function Prologue : &__divmod15 = 0 bytes

	// The picoChip instruction set has a divstep instruction which
	// is used to perform one iteration of a binary division algorithm.
	// The instruction allows 16-bit signed division to be implemented.
	// It does not directly allow 16-bit unsigned division to be
	// implemented. Thus, this function pulls out the common division
	// iteration for 15-bits unsigned, and then special wrappers
	// provide the logic to change this into a 16-bit signed or
	// unsigned division, as appropriate. This allows the two
	// versions of division to share a common implementation, reducing
	// code size when the two are used together. It also reduces
	// the maintenance overhead.

	// Input:
	//	r0 - dividend
	//	r1 - divisor
	// Output:
	//	r0 - quotient
	//	r1 - remainder
	// R5 is unused
	
	// Check for special cases. The emphasis is on detecting these as
	// quickly as possible, so that the main division can be started. If 
	// the user requests division by one, division by self, and so on
	// then they will just have to accept that this won't be particularly
	// quick (relatively), whereas a real division (e.g., dividing a 
	// large value by a small value) will run as fast as possible
	// (i.e., special case detection should not slow down the common case)
	//
	// Special cases to consider:
	//
	//	Division by zero.
	//	Division of zero.
	//	Inputs are equal
	//	Divisor is bigger than dividend
	//	Division by power of two (can be shifted instead).
	//	Division by 1 (special case of power of two division)
	//
	// Division/modulus by zero is undefined (ISO C:6.5.5), so
	// don't bother handling this special case.
	//
	// The special cases of division by a power of 2 are ignored, since 
	// they cause the general case to slow down. Omitting these
	// special cases also reduces code size considerably.

	// Handle divisor >= dividend separately. Note that this also handles 
	// the case where the dividend is zero.	Note that the flags must be
	// preserved, since they are also used at the branch destination.
	sub.0 r1,r0,r15
	sbc r0,r2 \ bge divisorGeDividend
=->	sbc r1,r4
	
	// Compute the shift count. The amount by which the divisor
	// must be shifted left to be aligned with the dividend.	
	sub.0 r4,r2,r3
		
	// Align the divisor to the dividend. Execute a divstep (since at 
	// least one will always be executed). Skip the remaining loop
	// if the shift count is zero.
	lsl.0 r1,r3,r1 \ beq skipLoop
=->	divstep r0,r1 \ add.1 r3,1,r2

	// Execute the divstep loop until temp is 0. This assumes that the
	// loop count is at least one.
	sub.0 r3,1,r4
divLoop:	
	divstep r0,r1 \ bne divLoop
=->	sub.0 r4,1,r4

skipLoop:
				
	// The top bits of the result are the remainder. The bottom
	// bits are the quotient.
	lsr.0 r0,r2,r1 \ sub.1 16,r2,r4
	jr (lr ) \ lsl.0 r0,r4,r0
=->	lsr.0 r0,r4,r0

// Special case.

divisorGeDividend:	
	// The divisor is greater than or equal to the dividend. The flags
	// indicate which of these alternatives it is. The COPYNE can be used 
	// to set the result appropriately, without introducing any more
	// branches.
	copy.0 r0,r1 \ copy.1 0,r0
	jr (lr) \ copyeq r0,r1
=->	copyeq 1,r0

_picoMark_FUNCTION_END=
// picoChip Function Epilogue : __divmod15

	
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
.uleb128 0x0	// <-- FUNCTION_STACK_SIZE_GOES_HERE
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
.ascii 16#5f# 16#64# 16#69# 16#76# 16#6d# 16#6f# 16#64# 16#31# 16#35# 16#0# // Function name `_divmod15'
.unalignedInitWord _picoMark_FUNCTION_BEGIN	// DW_AT_low_pc
.unalignedInitWord _picoMark_FUNCTION_END	// DW_AT_high_pc

.initByte 0x0	// end of compile unit children.

_picoMark_DEBUG_INFO_END=

//============================================================================
// END OF DWARF
//============================================================================

.section .endFile
// End of picoChip ASM file
