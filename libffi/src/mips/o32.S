/* -----------------------------------------------------------------------
   o32.S - Copyright (c) 1996, 1998  Red Hat, Inc.
   
   MIPS Foreign Function Interface 

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL CYGNUS SOLUTIONS BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#define LIBFFI_ASM	
#include <fficonfig.h>
#include <ffi.h>

/* Only build this code if we are compiling for o32 */	

#if defined(FFI_MIPS_O32)
	
#define callback a0
#define bytes	 a2
#define flags	 a3
		
#define SIZEOF_FRAME	( 4 * FFI_SIZEOF_ARG + 2 * FFI_SIZEOF_ARG )

	.text
	.align	2
	.globl	ffi_call_O32
	.ent	ffi_call_O32
ffi_call_O32:	
$LFB0:
	# Prologue
	SUBU	$sp, SIZEOF_FRAME			# Frame size
$LCFI0:
	REG_S	$fp, SIZEOF_FRAME - 2*FFI_SIZEOF_ARG($sp)	# Save frame pointer
$LCFI1:
	REG_S	ra, SIZEOF_FRAME - 1*FFI_SIZEOF_ARG($sp)	# Save return address
$LCFI2:
	move	$fp, $sp

$LCFI3:
	move	t9, callback	# callback function pointer
	REG_S	flags, SIZEOF_FRAME + 3*FFI_SIZEOF_ARG($fp) # flags

	# Allocate at least 4 words in the argstack
	move	v0, bytes
	bge	bytes, 4 * FFI_SIZEOF_ARG, bigger	
	LI	v0, 4 * FFI_SIZEOF_ARG
	b	sixteen

bigger:	
	ADDU	t0, v0, 2 * FFI_SIZEOF_ARG -1	# make sure it is aligned 
	and	v0, t0, -2 * FFI_SIZEOF_ARG		# to an 8 byte boundry

sixteen:
	SUBU	$sp, $sp, v0	# move the stack pointer to reflect the
				# arg space

	ADDU	a0, $sp, 4 * FFI_SIZEOF_ARG
	ADDU	a3, $fp, SIZEOF_FRAME + 3*FFI_SIZEOF_ARG

	jalr	t9
	
	REG_L	t0, SIZEOF_FRAME + 3*FFI_SIZEOF_ARG($fp)  # load the flags word
	add	t2, t0, 0			   # and copy it into t2

	and     t0, ((1<<4)-1)          # mask out the return type
	SRL	t2, 4			# shift our arg info
		
	ADDU	$sp, $sp, 4 * FFI_SIZEOF_ARG	# adjust $sp to new args

	bnez	t0, pass_d			# make it quick for int
	REG_L	a0, 0*FFI_SIZEOF_ARG($sp)		# just go ahead and load the
	REG_L	a1, 1*FFI_SIZEOF_ARG($sp)		# four regs.
	REG_L	a2, 2*FFI_SIZEOF_ARG($sp)
	REG_L	a3, 3*FFI_SIZEOF_ARG($sp)
	b	call_it

pass_d:
	bne	t0, FFI_ARGS_D, pass_f
	l.d	$f12, 0*FFI_SIZEOF_ARG($sp)	# load $fp regs from args
	REG_L	a2,   2*FFI_SIZEOF_ARG($sp)	# passing a double
	REG_L	a3,   3*FFI_SIZEOF_ARG($sp)
	b	call_it

pass_f:	
	bne	t0, FFI_ARGS_F, pass_d_d
	l.s	$f12, 0*FFI_SIZEOF_ARG($sp)	# load $fp regs from args
	REG_L	a1,   1*FFI_SIZEOF_ARG($sp)	# passing a float
	REG_L	a2,   2*FFI_SIZEOF_ARG($sp)
	REG_L	a3,   3*FFI_SIZEOF_ARG($sp)
	b	call_it		

pass_d_d:		
	bne	t0, FFI_ARGS_DD, pass_f_f
	l.d	$f12, 0*FFI_SIZEOF_ARG($sp)	# load $fp regs from args
	l.d	$f14, 2*FFI_SIZEOF_ARG($sp)	# passing two doubles
	b	call_it

pass_f_f:	
	bne	t0, FFI_ARGS_FF, pass_d_f
	l.s	$f12, 0*FFI_SIZEOF_ARG($sp)	# load $fp regs from args
	l.s	$f14, 1*FFI_SIZEOF_ARG($sp)	# passing two floats
	REG_L	a2,   2*FFI_SIZEOF_ARG($sp)
	REG_L	a3,   3*FFI_SIZEOF_ARG($sp)
	b	call_it

pass_d_f:		
	bne	t0, FFI_ARGS_DF, pass_f_d
	l.d	$f12, 0*FFI_SIZEOF_ARG($sp)	# load $fp regs from args
	l.s	$f14, 2*FFI_SIZEOF_ARG($sp)	# passing double and float
	REG_L	a3,   3*FFI_SIZEOF_ARG($sp)
	b	call_it

pass_f_d:		
 # assume that the only other combination must be float then double
 #	bne	t0, FFI_ARGS_F_D, call_it
	l.s	$f12, 0*FFI_SIZEOF_ARG($sp)	# load $fp regs from args
	l.d	$f14, 2*FFI_SIZEOF_ARG($sp)	# passing double and float

call_it:	
	# Load the function pointer
	REG_L	t9, SIZEOF_FRAME + 5*FFI_SIZEOF_ARG($fp)

	# If the return value pointer is NULL, assume no return value.
	REG_L	t1, SIZEOF_FRAME + 4*FFI_SIZEOF_ARG($fp)
	beqz	t1, noretval

	bne     t2, FFI_TYPE_INT, retlonglong
	jalr	t9
	REG_L	t0, SIZEOF_FRAME + 4*FFI_SIZEOF_ARG($fp)
	REG_S	v0, 0(t0)
	b	epilogue

retlonglong:
	# Really any 64-bit int, signed or not.
	bne	t2, FFI_TYPE_UINT64, retfloat
	jalr	t9
	REG_L	t0, SIZEOF_FRAME + 4*FFI_SIZEOF_ARG($fp)
	REG_S	v1, 4(t0)
	REG_S	v0, 0(t0)
	b	epilogue

retfloat:
	bne     t2, FFI_TYPE_FLOAT, retdouble
	jalr	t9
	REG_L	t0, SIZEOF_FRAME + 4*FFI_SIZEOF_ARG($fp)
	s.s	$f0, 0(t0)
	b	epilogue

retdouble:	
	bne	t2, FFI_TYPE_DOUBLE, noretval
	jalr	t9
	REG_L	t0, SIZEOF_FRAME + 4*FFI_SIZEOF_ARG($fp)
	s.d	$f0, 0(t0)
	b	epilogue
	
noretval:	
	jalr	t9
	
	# Epilogue
epilogue:	
	move	$sp, $fp	
	REG_L	$fp, SIZEOF_FRAME - 2*FFI_SIZEOF_ARG($sp) # Restore frame pointer
	REG_L	ra, SIZEOF_FRAME - 1*FFI_SIZEOF_ARG($sp)  # Restore return address
	ADDU	$sp, SIZEOF_FRAME		      # Fix stack pointer
	j	ra

$LFE0:
	.end	ffi_call_O32


/* ffi_closure_O32. Expects address of the passed-in ffi_closure
	in t0. Stores any arguments passed in registers onto the
	stack, then calls ffi_closure_mips_inner_O32, which
	then decodes them.
	
	Stack layout:

	14 - Start of parameters, original sp
	13 - ra save
	12 - fp save
	11 - $16 (s0) save
	10 - cprestore
	 9 - return value high (v1)
	 8 - return value low (v0)
	 7 - f14 (le high, be low)
	 6 - f14 (le low, be high)
	 5 - f12 (le high, be low)
	 4 - f12 (le low, be high)
	 3 - Called function a3 save
	 2 - Called function a2 save
	 1 - Called function a1 save
	 0 - Called function a0 save our sp, fp point here
	 */
	
#define SIZEOF_FRAME2	( 14 * FFI_SIZEOF_ARG )

	.text
	.align	2
	.globl	ffi_closure_O32
	.ent	ffi_closure_O32
ffi_closure_O32:
$LFB1:
	# Prologue
	.frame	$fp, SIZEOF_FRAME2, $31
	.set	noreorder
	.cpload	$25
	.set	reorder
	SUBU	$sp, SIZEOF_FRAME2
	.cprestore SIZEOF_FRAME2 - 4*FFI_SIZEOF_ARG
$LCFI4:
	REG_S	$16, SIZEOF_FRAME2 - 3*FFI_SIZEOF_ARG($sp)	 # Save s0
	REG_S	$fp, SIZEOF_FRAME2 - 2*FFI_SIZEOF_ARG($sp)	 # Save frame pointer
	REG_S	ra, SIZEOF_FRAME2 - 1*FFI_SIZEOF_ARG($sp)	 # Save return address
$LCFI6:
	move	$fp, $sp

$LCFI7:
	# Store all possible argument registers. If there are more than
	# four arguments, then they should be stored above where we put $7.
	REG_S	$4, SIZEOF_FRAME2 + 0*FFI_SIZEOF_ARG($fp)
	REG_S	$5, SIZEOF_FRAME2 + 1*FFI_SIZEOF_ARG($fp)
	REG_S	$6, SIZEOF_FRAME2 + 2*FFI_SIZEOF_ARG($fp)
	REG_S	$7, SIZEOF_FRAME2 + 3*FFI_SIZEOF_ARG($fp)

	# Load ABI enum to $16
	REG_L	$16, 20($8)	# cif pointer follows tramp.
	REG_L	$16, 0($16)	# abi is first member.

	li	$13, 1		# FFI_O32
	bne	$16, $13, 1f	# Skip fp save if FFI_O32_SOFT_FLOAT
	
	# Store all possible float/double registers.
	s.d	$f12, SIZEOF_FRAME2 - 10*FFI_SIZEOF_ARG($fp)
	s.d	$f14, SIZEOF_FRAME2 -  8*FFI_SIZEOF_ARG($fp)
1:	
	# Call ffi_closure_mips_inner_O32 to do the work.
	la	$25, ffi_closure_mips_inner_O32
	move	$4, $8	 # Pointer to the ffi_closure
	addu	$5, $fp, SIZEOF_FRAME2 -  6*FFI_SIZEOF_ARG
	addu	$6, $fp, SIZEOF_FRAME2 +  0*FFI_SIZEOF_ARG
	addu	$7, $fp, SIZEOF_FRAME2 - 10*FFI_SIZEOF_ARG
	jalr	$31, $25

	# Load the return value into the appropriate register.
	move	$8, $2
	li	$9, FFI_TYPE_VOID
	beq	$8, $9, closure_done

	li	$13, 1		# FFI_O32
	bne	$16, $13, 1f	# Skip fp restore if FFI_O32_SOFT_FLOAT

	li	$9, FFI_TYPE_FLOAT
	l.s	$f0, SIZEOF_FRAME2 - 6*FFI_SIZEOF_ARG($fp)
	beq	$8, $9, closure_done

	li	$9, FFI_TYPE_DOUBLE
	l.d	$f0, SIZEOF_FRAME2 - 6*FFI_SIZEOF_ARG($fp)
	beq	$8, $9, closure_done
1:	
	li	$9, FFI_TYPE_SINT64
	REG_L	$3, SIZEOF_FRAME2 - 5*FFI_SIZEOF_ARG($fp)
	beq	$8, $9, integer
	li	$9, FFI_TYPE_UINT64
	beq	$8, $9, integer

integer:
	REG_L	$2, SIZEOF_FRAME2 - 6*FFI_SIZEOF_ARG($fp)

closure_done:
	# Epilogue
	move	$sp, $fp
	REG_L	$16, SIZEOF_FRAME2 - 3*FFI_SIZEOF_ARG($sp)	 # Restore s0
	REG_L	$fp, SIZEOF_FRAME2 - 2*FFI_SIZEOF_ARG($sp)	 # Restore frame pointer
	REG_L	ra,  SIZEOF_FRAME2 - 1*FFI_SIZEOF_ARG($sp)	 # Restore return address
	ADDU	$sp, SIZEOF_FRAME2
	j	ra
$LFE1:
	.end	ffi_closure_O32

/* DWARF-2 unwind info. */

	.section	.eh_frame,"a",@progbits
$Lframe0:
	.4byte	$LECIE0-$LSCIE0	 # Length of Common Information Entry
$LSCIE0:
	.4byte	0x0	 # CIE Identifier Tag
	.byte	0x1	 # CIE Version
	.ascii "zR\0"	 # CIE Augmentation
	.uleb128 0x1	 # CIE Code Alignment Factor
	.sleb128 4	 # CIE Data Alignment Factor
	.byte	0x1f	 # CIE RA Column
	.uleb128 0x1	 # Augmentation size
	.byte	0x00	 # FDE Encoding (absptr)
	.byte	0xc	 # DW_CFA_def_cfa
	.uleb128 0x1d
	.uleb128 0x0
	.align	2
$LECIE0:
$LSFDE0:
	.4byte	$LEFDE0-$LASFDE0	 # FDE Length
$LASFDE0:
	.4byte	$LASFDE0-$Lframe0	 # FDE CIE offset
	.4byte	$LFB0	 # FDE initial location
	.4byte	$LFE0-$LFB0	 # FDE address range
	.uleb128 0x0	 # Augmentation size
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	$LCFI0-$LFB0
	.byte	0xe	 # DW_CFA_def_cfa_offset
	.uleb128 0x18
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	$LCFI2-$LCFI0
	.byte	0x11	 # DW_CFA_offset_extended_sf
	.uleb128 0x1e	 # $fp
	.sleb128 -2	 # SIZEOF_FRAME2 - 2*FFI_SIZEOF_ARG($sp)
	.byte	0x11	 # DW_CFA_offset_extended_sf
	.uleb128 0x1f	 # $ra
	.sleb128 -1	 # SIZEOF_FRAME2 - 1*FFI_SIZEOF_ARG($sp)
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	$LCFI3-$LCFI2
	.byte	0xc	 # DW_CFA_def_cfa
	.uleb128 0x1e
	.uleb128 0x18
	.align	2
$LEFDE0:
$LSFDE1:
	.4byte	$LEFDE1-$LASFDE1	 # FDE Length
$LASFDE1:
	.4byte	$LASFDE1-$Lframe0	 # FDE CIE offset
	.4byte	$LFB1	 # FDE initial location
	.4byte	$LFE1-$LFB1	 # FDE address range
	.uleb128 0x0	 # Augmentation size
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	$LCFI4-$LFB1
	.byte	0xe	 # DW_CFA_def_cfa_offset
	.uleb128 0x38
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	$LCFI6-$LCFI4
	.byte	0x11	 # DW_CFA_offset_extended_sf
	.uleb128 0x10	 # $16
	.sleb128 -3	 # SIZEOF_FRAME2 - 3*FFI_SIZEOF_ARG($sp)
	.byte	0x11	 # DW_CFA_offset_extended_sf
	.uleb128 0x1e	 # $fp
	.sleb128 -2	 # SIZEOF_FRAME2 - 2*FFI_SIZEOF_ARG($sp)
	.byte	0x11	 # DW_CFA_offset_extended_sf
	.uleb128 0x1f	 # $ra
	.sleb128 -1	 # SIZEOF_FRAME2 - 1*FFI_SIZEOF_ARG($sp)
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	$LCFI7-$LCFI6
	.byte	0xc	 # DW_CFA_def_cfa
	.uleb128 0x1e
	.uleb128 0x38
	.align	2
$LEFDE1:

#endif
