/* -----------------------------------------------------------------------
   v8.S - Copyright (c) 1996, 1997 Cygnus Solutions
   
   Sparc Foreign Function Interface 

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

#define STACKFRAME 96		/* Minimum stack framesize for SPARC */
#define ARGS (64+4)		/* Offset of register area in frame */

.text
        .align 8
.globl ffi_call_V8
.globl _ffi_call_V8

ffi_call_V8:
_ffi_call_V8:
.LLFB1:
	save	%sp, -STACKFRAME, %sp
.LLCFI0:
	
	sub	%sp, %i2, %sp	! alloca() space in stack for frame to set up
	add	%sp, STACKFRAME, %l0	! %l0 has start of 
					! frame to set up

	mov	%l0, %o0	! call routine to set up frame
	call	%i0
	mov	%i1, %o1	! (delay)

	ld	[%l0+ARGS], %o0	! call foreign function
	ld	[%l0+ARGS+4], %o1
	ld	[%l0+ARGS+8], %o2
	ld	[%l0+ARGS+12], %o3
	ld	[%l0+ARGS+16], %o4
	ld	[%l0+ARGS+20], %o5
	call	%i5
	mov	%l0, %sp	! (delay) switch to frame
	nop			! STRUCT returning functions skip 12 instead of 8 bytes

	! If the return value pointer is NULL, assume no return value.
	tst	%i4
	bz	done
	nop

	cmp	%i3, FFI_TYPE_INT
	be,a	done
	st	%o0, [%i4]	! (delay)

	cmp	%i3, FFI_TYPE_FLOAT
	be,a	done
	st	%f0, [%i4+0]	! (delay)

	cmp	%i3, FFI_TYPE_SINT64
	be	longlong

	cmp	%i3, FFI_TYPE_DOUBLE
	bne	done
	nop
	st	%f0, [%i4+0]
	st	%f1, [%i4+4]
	
done:
	ret
	restore

longlong:
	st	%o0, [%i4+0]
	st	%o1, [%i4+4]
	ret
	restore
.LLFE1:

.ffi_call_V8_end:
	.size	ffi_call_V8,.ffi_call_V8_end-ffi_call_V8

#ifdef SPARC64
#define WS 8
#define nword	xword
#define uanword	uaxword
#else
#define WS 4
#define nword	long
#define uanword	uaword
#endif

	.section	".eh_frame",#alloc,#write
.LLframe1:
	.uaword	.LLECIE1-.LLSCIE1	! Length of Common Information Entry
.LLSCIE1:
	.uaword	0x0	! CIE Identifier Tag
	.byte	0x1	! CIE Version
	.ascii "zR\0"	! CIE Augmentation
	.byte	0x1	! uleb128 0x1; CIE Code Alignment Factor
	.byte	0x80-WS	! sleb128 -WS; CIE Data Alignment Factor
	.byte	0xf	! CIE RA Column
	.byte	0x1	! uleb128 0x1; Augmentation size
	.byte	0x50	! FDE Encoding (aligned absolute)
	.byte	0xc	! DW_CFA_def_cfa
	.byte	0xe	! uleb128 0xe
	.byte	0x0	! uleb128 0x0
	.align	WS
.LLECIE1:
.LLSFDE1:
	.uaword	.LLEFDE1-.LLASFDE1	! FDE Length
.LLASFDE1:
	.uaword	.LLASFDE1-.LLframe1	! FDE CIE offset
	.align	WS
	.nword	.LLFB1
	.uanword .LLFE1-.LLFB1	! FDE address range
	.byte	0x0	! uleb128 0x0; Augmentation size
	.byte	0x4	! DW_CFA_advance_loc4
	.uaword	.LLCFI0-.LLFB1
	.byte	0xd	! DW_CFA_def_cfa_register
	.byte	0x1e	! uleb128 0x1e
	.byte	0x2d	! DW_CFA_GNU_window_save
	.byte	0x9	! DW_CFA_register
	.byte	0xf	! uleb128 0xf
	.byte	0x1f	! uleb128 0x1f
	.align	WS
.LLEFDE1:
