/* -----------------------------------------------------------------------
   v9.S - Copyright (c) 2000 Cygnus Solutions
   
   Sparc 64bit Foreign Function Interface 

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
#include <ffi.h>

#ifdef SPARC64
/* Only compile this in for 64bit builds, because otherwise the object file
   will have inproper architecture due to used instructions.  */

#define STACKFRAME 128		/* Minimum stack framesize for SPARC */
#define STACK_BIAS 2047
#define ARGS (128)		/* Offset of register area in frame */

.text
        .align 8
.globl ffi_call_V9
.globl _ffi_call_V9

ffi_call_V9:
_ffi_call_V9:
.LLFB1:
	save	%sp, -STACKFRAME, %sp
.LLCFI0:
	
	sub	%sp, %i2, %sp	! alloca() space in stack for frame to set up
	add	%sp, STACKFRAME+STACK_BIAS, %l0	! %l0 has start of 
						! frame to set up

	mov	%l0, %o0	! call routine to set up frame
	call	%i0
	 mov	%i1, %o1	! (delay)
	brz,pt	%o0, 1f
	 ldx	[%l0+ARGS], %o0	! call foreign function

	ldd	[%l0+ARGS], %f0
	ldd	[%l0+ARGS+8], %f2
	ldd	[%l0+ARGS+16], %f4
	ldd	[%l0+ARGS+24], %f6
	ldd	[%l0+ARGS+32], %f8
	ldd	[%l0+ARGS+40], %f10
	ldd	[%l0+ARGS+48], %f12
	ldd	[%l0+ARGS+56], %f14
	ldd	[%l0+ARGS+64], %f16
	ldd	[%l0+ARGS+72], %f18
	ldd	[%l0+ARGS+80], %f20
	ldd	[%l0+ARGS+88], %f22
	ldd	[%l0+ARGS+96], %f24
	ldd	[%l0+ARGS+104], %f26
	ldd	[%l0+ARGS+112], %f28
	ldd	[%l0+ARGS+120], %f30

1:	ldx	[%l0+ARGS+8], %o1
	ldx	[%l0+ARGS+16], %o2
	ldx	[%l0+ARGS+24], %o3
	ldx	[%l0+ARGS+32], %o4
	ldx	[%l0+ARGS+40], %o5
	call	%i5
	 sub	%l0, STACK_BIAS, %sp	! (delay) switch to frame

	! If the return value pointer is NULL, assume no return value.
	brz,pn	%i4, done
	 nop

	cmp	%i3, FFI_TYPE_INT
	be,a,pt	%icc, done
	 stx	%o0, [%i4]	! (delay)

	cmp	%i3, FFI_TYPE_FLOAT
	be,a,pn	%icc, done
	 st	%f0, [%i4+0]	! (delay)

	cmp	%i3, FFI_TYPE_DOUBLE
	be,a,pn	%icc, done
	 std	%f0, [%i4+0]	! (delay)

	cmp	%i3, FFI_TYPE_STRUCT
	be,pn	%icc, dostruct

	 cmp	%i3, FFI_TYPE_LONGDOUBLE
	bne,pt	%icc, done
	 nop
	std	%f0, [%i4+0]
	std	%f2, [%i4+8]

done:	ret
	 restore

dostruct:
	/* This will not work correctly for unions. */
	stx	%o0, [%i4+0]
	stx	%o1, [%i4+8]
	stx	%o2, [%i4+16]
	stx	%o3, [%i4+24]
	std	%f0, [%i4+32]
	std	%f2, [%i4+40]
	std	%f4, [%i4+48]
	std	%f6, [%i4+56]
	ret
	 restore
.LLFE1:

.ffi_call_V9_end:
	.size	ffi_call_V9,.ffi_call_V9_end-ffi_call_V9

	.section	".eh_frame",#alloc,#write
.LLframe1:
	.uaword	.LLECIE1-.LLSCIE1	! Length of Common Information Entry
.LLSCIE1:
	.uaword	0x0	! CIE Identifier Tag
	.byte	0x1	! CIE Version
	.ascii "zR\0"	! CIE Augmentation
	.byte	0x1	! uleb128 0x1; CIE Code Alignment Factor
	.byte	0x78	! sleb128 -8; CIE Data Alignment Factor
	.byte	0xf	! CIE RA Column
	.byte	0x1	! uleb128 0x1; Augmentation size
	.byte	0x50	! FDE Encoding (aligned absolute)
	.byte	0xc	! DW_CFA_def_cfa
	.byte	0xe	! uleb128 0xe
	.byte	0xff,0xf	! uleb128 0x7ff
	.align 8
.LLECIE1:
.LLSFDE1:
	.uaword	.LLEFDE1-.LLASFDE1	! FDE Length
.LLASFDE1:
	.uaword	.LLASFDE1-.LLframe1	! FDE CIE offset
	.align 8
	.xword	.LLFB1
	.uaxword	.LLFE1-.LLFB1	! FDE address range
	.byte	0x0	! uleb128 0x0; Augmentation size
	.byte	0x4	! DW_CFA_advance_loc4
	.uaword	.LLCFI0-.LLFB1
	.byte	0xd	! DW_CFA_def_cfa_register
	.byte	0x1e	! uleb128 0x1e
	.byte	0x2d	! DW_CFA_GNU_window_save
	.byte	0x9	! DW_CFA_register
	.byte	0xf	! uleb128 0xf
	.byte	0x1f	! uleb128 0x1f
	.align 8
.LLEFDE1:

#endif
