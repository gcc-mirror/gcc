#define LIBFFI_ASM
#include <ffi.h>

	.file	"linux64_closure.S"

#ifdef __powerpc64__
        .hidden ffi_closure_LINUX64, .ffi_closure_LINUX64
        .globl  ffi_closure_LINUX64, .ffi_closure_LINUX64
        .section        ".opd","aw"
        .align  3
ffi_closure_LINUX64:
        .quad   .ffi_closure_LINUX64,.TOC.@tocbase,0
        .size   ffi_closure_LINUX64,24
        .type   .ffi_closure_LINUX64,@function
        .text
.ffi_closure_LINUX64:
.LFB1:
	# save general regs into parm save area
	std	%r3, 48(%r1)
	std	%r4, 56(%r1)
	std	%r5, 64(%r1)
	std	%r6, 72(%r1)
	mflr	%r0

	std	%r7, 80(%r1)
	std	%r8, 88(%r1)
	std	%r9, 96(%r1)
	std	%r10, 104(%r1)
	std	%r0, 16(%r1)

	# mandatory 48 bytes special reg save area + 64 bytes parm save area
	# + 8 bytes retval area + 13*8 bytes fpr save area
	stdu	%r1, -224(%r1)
.LCFI0:

	# next save fpr 1 to fpr 13
	stfd  %f1, 120+(0*8)(%r1)
	stfd  %f2, 120+(1*8)(%r1)
	stfd  %f3, 120+(2*8)(%r1)
	stfd  %f4, 120+(3*8)(%r1)
	stfd  %f5, 120+(4*8)(%r1)
	stfd  %f6, 120+(5*8)(%r1)
	stfd  %f7, 120+(6*8)(%r1)
	stfd  %f8, 120+(7*8)(%r1)
	stfd  %f9, 120+(8*8)(%r1)
	stfd  %f10, 120+(9*8)(%r1)
	stfd  %f11, 120+(10*8)(%r1)
	stfd  %f12, 120+(11*8)(%r1)
	stfd  %f13, 120+(12*8)(%r1)

	# set up registers for the routine that actually does the work
	# get the context pointer from the trampoline
	mr %r3, %r11

	# now load up the pointer to the result storage
	addi %r4, %r1, 112

	# now load up the pointer to the parameter save area
	# in the previous frame
	addi %r5, %r1, 224 + 48

	# now load up the pointer to the saved fpr registers */
	addi %r6, %r1, 120

	# make the call
	bl .ffi_closure_helper_LINUX64

	# now r3 contains the return type
	# so use it to look up in a table
	# so we know how to deal with each type

	# look up the proper starting point in table 
	# by using return type as offset
	addi %r5, %r1, 112	# get pointer to results area
	bl .Lget_ret_type0_addr # get pointer to .Lret_type0 into LR
	mflr %r4		# move to r4
	sldi %r3, %r3, 4	# now multiply return type by 16
	add %r3, %r3, %r4	# add contents of table to table address
	mtctr %r3
	bctr			# jump to it

# Each of the ret_typeX code fragments has to be exactly 16 bytes long
# (4 instructions). For cache effectiveness we align to a 16 byte boundary
# first.
	.align 4

	nop
	nop
	nop
.Lget_ret_type0_addr:
	blrl

.Lret_type0:
# case FFI_TYPE_VOID
	b .Lfinish
	nop
	nop
	nop
# case FFI_TYPE_INT
	lwa %r3, 4(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_FLOAT
	lfs %f1, 4(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_DOUBLE
	lfd %f1, 0(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_LONGDOUBLE
	lfd %f1, 0(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_UINT8
	lbz %r3, 7(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_SINT8
	lbz %r3, 7(%r5)
	extsb %r3,%r3
	b .Lfinish
	nop
# case FFI_TYPE_UINT16
	lhz %r3, 6(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_SINT16
	lha %r3, 6(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_UINT32
	lwz %r3, 4(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_SINT32
	lwa %r3, 4(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_UINT64
	ld %r3, 0(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_SINT64
	ld %r3, 0(%r5)
	b .Lfinish
	nop
	nop
# case FFI_TYPE_STRUCT
	b .Lfinish
	nop
	nop
	nop
# case FFI_TYPE_POINTER
	ld %r3, 0(%r5)
	b .Lfinish
	nop
	nop
# esac
.Lfinish:
	ld %r0, 224+16(%r1)
	mtlr %r0
	addi %r1, %r1, 224
	blr
.LFE1:
	.long	0
	.byte	0,12,0,1,128,0,0,0
	.size	.ffi_closure_LINUX64,.-.ffi_closure_LINUX64

	.section	.eh_frame,EH_FRAME_FLAGS,@progbits
.Lframe1:
	.4byte	.LECIE1-.LSCIE1	 # Length of Common Information Entry
.LSCIE1:
	.4byte	0x0	 # CIE Identifier Tag
	.byte	0x1	 # CIE Version
	.ascii "zR\0"	 # CIE Augmentation
	.uleb128 0x1	 # CIE Code Alignment Factor
	.sleb128 -8	 # CIE Data Alignment Factor
	.byte	0x41	 # CIE RA Column
	.uleb128 0x1	 # Augmentation size
	.byte	0x14	 # FDE Encoding (pcrel udata8)
	.byte	0xc	 # DW_CFA_def_cfa
	.uleb128 0x1
	.uleb128 0x0
	.align 3
.LECIE1:
.LSFDE1:
	.4byte	.LEFDE1-.LASFDE1	 # FDE Length
.LASFDE1:
	.4byte	.LASFDE1-.Lframe1	 # FDE CIE offset
	.8byte	.LFB1-.	 # FDE initial location
	.8byte	.LFE1-.LFB1	 # FDE address range
	.uleb128 0x0	 # Augmentation size
	.byte	0x2	 # DW_CFA_advance_loc1
	.byte	.LCFI0-.LFB1
	.byte	0xe	 # DW_CFA_def_cfa_offset
	.uleb128 224
	.byte	0x11	 # DW_CFA_offset_extended_sf
	.uleb128 0x41
	.sleb128 -2
	.align 3
.LEFDE1:
#endif
