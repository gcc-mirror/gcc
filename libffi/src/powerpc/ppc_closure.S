#define LIBFFI_ASM
#include <powerpc/asm.h>

.globl	ffi_closure_helper_SYSV

ENTRY(ffi_closure_SYSV)
.LFB1:
	stwu %r1,-144(%r1)
.LCFI0:
	mflr %r0
	stw %r31,140(%r1)
.LCFI1:
	stw %r0,148(%r1)

# we want to build up an areas for the parameters passed
# in registers (both floating point and integer)
	
	# so first save gpr 3 to gpr 10 (aligned to 4)
	stw   %r3, 16(%r1)
	stw   %r4, 20(%r1)
	stw   %r5, 24(%r1) 
	stw   %r6, 28(%r1)
	stw   %r7, 32(%r1)
	stw   %r8, 36(%r1) 
	stw   %r9, 40(%r1)
	stw   %r10,44(%r1)

	# next save fpr 1 to fpr 8 (aligned to 8)
	stfd  %f1, 48(%r1)
	stfd  %f2, 56(%r1)
	stfd  %f3, 64(%r1)
	stfd  %f4, 72(%r1)
	stfd  %f5, 80(%r1)
	stfd  %f6, 88(%r1)
	stfd  %f7, 96(%r1)
	stfd  %f8, 104(%r1)

	# set up registers for the routine that actually does the work
	# get the context pointer from the trampoline
	mr %r3,%r11
	
        # now load up the pointer to the result storage
	addi %r4,%r1,112
	
	# now load up the pointer to the saved gpr registers
        addi %r5,%r1,16

        # now load up the pointer to the saved fpr registers */
        addi %r6,%r1,48

	# now load up the pointer to the outgoing parameter 
	# stack in the previous frame
	# i.e. the previous frame pointer + 8
	addi %r7,%r1,152
	
        # make the call
	bl JUMPTARGET(ffi_closure_helper_SYSV)

	# now r3 contains the return type
	# so use it to look up in a table
	# so we know how to deal with each type

        # look up the proper starting point in table 
	# by using return type as offset
	addi %r5,%r1,112   # get pointer to results area
	addis %r4,0,.L60@ha  # get address of jump table
	addi %r4,%r4,.L60@l
	slwi %r3,%r3,2         # now multiply return type by 4
	lwzx %r3,%r4,%r3         # get the contents of that table value
	add %r3,%r3,%r4          # add contents of table to table address
	mtctr %r3
	bctr               # jump to it
.LFE1:
	.align 2
.L60:
	.long .L44-.L60    # FFI_TYPE_VOID
	.long .L50-.L60    # FFI_TYPE_INT
	.long .L47-.L60    # FFI_TYPE_FLOAT
	.long .L46-.L60    # FFI_TYPE_DOUBLE
	.long .L46-.L60    # FFI_TYPE_LONGDOUBLE
	.long .L56-.L60    # FFI_TYPE_UINT8
	.long .L55-.L60    # FFI_TYPE_SINT8
	.long .L58-.L60    # FFI_TYPE_UINT16
	.long .L57-.L60    # FFI_TYPE_SINT16
	.long .L50-.L60    # FFI_TYPE_UINT32
	.long .L50-.L60    # FFI_TYPE_SINT32
	.long .L48-.L60    # FFI_TYPE_UINT64
	.long .L48-.L60    # FFI_TYPE_SINT64
	.long .L44-.L60    # FFI_TYPE_STRUCT
	.long .L50-.L60    # FFI_TYPE_POINTER


# case double
.L46:   
        lfd %f1,0(%r5)
	b .L44

# case float
.L47:
	lfs %f1,0(%r5)
	b .L44
	
# case long long
.L48:
	lwz %r3,0(%r5)
	lwz %r4,4(%r5)
	b .L44
	
# case default / int32 / pointer
.L50:
	lwz %r3,0(%r5)
	b .L44
	
# case signed int8	
.L55:
	addi %r5,%r5,3
	lbz %r3,0(%r5)
	extsb %r3,%r3
	b .L44

# case unsigned int8	
.L56:
	addi %r5,%r5,3
        lbz %r3,0(%r5)
	b .L44

# case signed int16
.L57:
	addi %r5,%r5,2
	lhz %r3,0(%r5)
	extsh %r3,%r3
	b .L44

#case unsigned int16
.L58:	
	addi %r5,%r5,2
	lhz %r3,0(%r5)

# case void / done	
.L44:
	
	lwz %r11,0(%r1)
	lwz %r0,4(%r11)
	mtlr %r0
	lwz %r31,-4(%r11)
	mr %r1,%r11
	blr
END(ffi_closure_SYSV)

	.section	".eh_frame","aw"
__FRAME_BEGIN__:
	.4byte	.LECIE1-.LSCIE1	 # Length of Common Information Entry
.LSCIE1:
	.4byte	0x0	 # CIE Identifier Tag
	.byte	0x1	 # CIE Version
	.ascii "\0"	 # CIE Augmentation
	.byte	0x1	 # uleb128 0x1; CIE Code Alignment Factor
	.byte	0x7c	 # sleb128 -4; CIE Data Alignment Factor
	.byte	0x41	 # CIE RA Column
	.byte	0xc	 # DW_CFA_def_cfa
	.byte	0x1	 # uleb128 0x1
	.byte	0x0	 # uleb128 0x0
	.align 2
.LECIE1:
.LSFDE1:
	.4byte	.LEFDE1-.LASFDE1	 # FDE Length
.LASFDE1:
	.4byte	.LASFDE1-__FRAME_BEGIN__	 # FDE CIE offset
	.4byte	.LFB1	 # FDE initial location
	.4byte	.LFE1-.LFB1	 # FDE address range
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	.LCFI0-.LFB1
	.byte	0xe	 # DW_CFA_def_cfa_offset
	.byte	144,1	 # uleb128 144
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	.LCFI1-.LCFI0
	.byte	0x2f	 # DW_CFA_GNU_negative_offset_extended
	.byte	0x41	 # uleb128 0x41
	.byte	0x1	 # uleb128 0x1
	.align 2
.LEFDE1:
