/* -----------------------------------------------------------------------
   unix64.S - Copyright (c) 2002  Bo Thorsen <bo@suse.de>

   x86-64 Foreign Function Interface 

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

#ifdef __x86_64__
#define LIBFFI_ASM	
#include <ffi.h>

	.section	.rodata
.LC0:
	.string	"asm in progress %lld\n"
.LC1:
	.string	"asm in progress\n"
.text
	.align	2
.globl ffi_call_UNIX64
        .type	ffi_call_UNIX64,@function

ffi_call_UNIX64:
.LFB1:
        pushq	%rbp
.LCFI0:
        movq	%rsp, %rbp
.LCFI1:
	/* Save all arguments */
	subq	$48, %rsp
.LCFI2:
	movq	%rdi, -8(%rbp)		/* ffi_prep_args	 */
	movq	%rsi, -16(%rbp)		/* ffi_fill_return_value */
	movq	%rdx, -24(%rbp)		/* ecif			 */
	movq	%rcx, -32(%rbp)		/* cif->bytes		 */
	movq	%r8, -40(%rbp)		/* ecif.rvalue		 */
	movq	%r9, -48(%rbp)		/* fn			 */

	/* Make room for all of the new args and the register args */
	addl	$176, %ecx
.LCFI3:
	subq	%rcx, %rsp
.LCFI4:
	/* Setup the call to ffi_prep_args.  */
	movq	%rdi, %rax		/* &ffi_prep_args	*/
	movq	%rsp, %rdi		/* stackLayout		*/
	movq	%rdx, %rsi		/* ecif			*/
	call	*%rax			/* ffi_prep_args(stackLayout, ecif);*/ 

	/* ffi_prep_args have put all the register contents into the  */
	/* stackLayout struct. Now put the register values in place.  */
	movq	(%rsp), %rdi
	movq	8(%rsp), %rsi
	movq	16(%rsp), %rdx
	movq	24(%rsp), %rcx
	movq	32(%rsp), %r8
	movq	40(%rsp), %r9
	movaps	48(%rsp), %xmm0
	movaps	64(%rsp), %xmm1
	movaps	80(%rsp), %xmm2
	movaps	96(%rsp), %xmm3
	movaps	112(%rsp), %xmm4
	movaps	128(%rsp), %xmm5
	movaps	144(%rsp), %xmm6
	movaps	160(%rsp), %xmm7

	/* Remove space for stackLayout so stack arguments are placed
	   correctly for the call.  */
.LCFI5:
	addq	$176, %rsp
.LCFI6:
	/* Call the user function.  */
	call	*-48(%rbp)

	/* Make stack space for the return_value struct.  */
	subq	$64, %rsp

	/* Fill in all potential return values to this struct.  */
	movq	%rax, (%rsp)
	movq	%rdx, 8(%rsp)
	movaps	%xmm0, 16(%rsp)
	movaps	%xmm1, 32(%rsp)
	fstpt	48(%rsp)

	/* Now call ffi_fill_return_value.  */
	movq	%rsp, %rdi		/* struct return_value	  */
	movq	-24(%rbp), %rsi		/* ecif			  */
	movq	-16(%rbp), %rax		/* &ffi_fill_return_value */
	call	*%rax			/* call it		  */

	/* And the work is done.  */
        leave
        ret
.LFE1:
.ffi_call_UNIX64_end:
        .size    ffi_call_UNIX64,.ffi_call_UNIX64_end-ffi_call_UNIX64

.text
	.align	2
.globl float2sse
        .type	float2sse,@function
float2sse:
	/* Save the contents of this sse-float in a pointer.  */
	movaps	%xmm0, (%rdi)
	ret

	.align	2
.globl floatfloat2sse
        .type	floatfloat2sse,@function
floatfloat2sse:
	/* Save the contents of these two sse-floats in a pointer.  */
	movq	(%rdi), %xmm0
	movaps	%xmm0, (%rsi)
	ret

	.align	2
.globl double2sse
        .type	double2sse,@function
double2sse:
	/* Save the contents of this sse-double in a pointer.  */
	movaps	%xmm0, (%rdi)
	ret

	.align	2
.globl sse2float
        .type	sse2float,@function
sse2float:
	/* Save the contents of this sse-float in a pointer.  */
	movaps	(%rdi), %xmm0
	ret

	.align	2
.globl sse2double
        .type	sse2double,@function
sse2double:
	/* Save the contents of this pointer in a sse-double.  */
	movaps	(%rdi), %xmm0
	ret

	.align	2
.globl sse2floatfloat
        .type	sse2floatfloat,@function
sse2floatfloat:
	/* Save the contents of this pointer in two sse-floats.  */
	movaps	(%rdi), %xmm0
	movq	%xmm0, (%rsi)
	ret

	.align	2
.globl ffi_closure_UNIX64
        .type	ffi_closure_UNIX64,@function

ffi_closure_UNIX64:
.LFB2:
        pushq   %rbp
.LCFI10:
        movq    %rsp, %rbp
.LCFI11:
        subq    $240, %rsp
.LCFI12:
	movq	%rdi, -176(%rbp)
        movq    %rsi, -168(%rbp)
        movq    %rdx, -160(%rbp)
        movq    %rcx, -152(%rbp)
        movq    %r8, -144(%rbp)
        movq    %r9, -136(%rbp)
        /* FIXME: We can avoid all this stashing of XMM registers by
	   (in ffi_prep_closure) computing the number of
	   floating-point args and moving it into %rax before calling
	   this function.  Once this is done, uncomment the next few
	   lines and only the essential XMM registers will be written
	   to memory.  This is a significant saving.  */
/*         movzbl  %al, %eax  */
/*         movq    %rax, %rdx */
/*         leaq    0(,%rdx,4), %rax */
/*         leaq    2f(%rip), %rdx */
/*         subq    %rax, %rdx */
        leaq    -1(%rbp), %rax
/*         jmp     *%rdx */
        movaps  %xmm7, -15(%rax)
        movaps  %xmm6, -31(%rax)
        movaps  %xmm5, -47(%rax)
        movaps  %xmm4, -63(%rax)
        movaps  %xmm3, -79(%rax)
        movaps  %xmm2, -95(%rax)
        movaps  %xmm1, -111(%rax)
        movaps  %xmm0, -127(%rax)
2:
        movl    %edi, -180(%rbp)
        movl    $0, -224(%rbp)
        movl    $48, -220(%rbp)
        leaq    16(%rbp), %rax
        movq    %rax, -216(%rbp)
        leaq    -176(%rbp), %rdx
        movq    %rdx, -208(%rbp)
        leaq    -224(%rbp), %rsi
	movq	%r10, %rdi
	movq	%rsp, %rdx
        call    ffi_closure_UNIX64_inner@PLT

	cmpl	$FFI_TYPE_FLOAT, %eax
	je	1f
	cmpl	$FFI_TYPE_DOUBLE, %eax
	je	2f
	cmpl	$FFI_TYPE_LONGDOUBLE, %eax
	je	3f
	cmpl	$FFI_TYPE_STRUCT, %eax
	je	4f
	popq	%rax
        leave
        ret
1:
2:
3:	
	movaps	-240(%rbp), %xmm0
        leave
        ret
4:
	leave
	ret
.LFE2:	
		
        .section        .eh_frame,EH_FRAME_FLAGS,@progbits
.Lframe0:
        .long   .LECIE1-.LSCIE1
.LSCIE1:
        .long   0x0
        .byte   0x1
        .string "zR"
        .uleb128 0x1
        .sleb128 -8
        .byte   0x10
        .uleb128 0x1
        .byte   0x1b
        .byte   0xc
        .uleb128 0x7
        .uleb128 0x8
        .byte   0x90
        .uleb128 0x1
        .align 8
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
        .long   .LASFDE1-.Lframe0

        .long   .LFB1-.
        .long   .LFE1-.LFB1
        .uleb128 0x0
        .byte   0x4		# DW_CFA_advance_loc4
        .long   .LCFI0-.LFB1
        .byte   0xe		# DW_CFA_def_cfa_offset
        .uleb128 0x10
        .byte   0x86		# DW_CFA_offset: r6 at cfa-16
        .uleb128 0x2
        .byte   0x4		# DW_CFA_advance_loc4
        .long   .LCFI1-.LCFI0
        .byte   0x86		# DW_CFA_offset: r6 at cfa-16
        .uleb128 0x2
        .byte   0xd		# DW_CFA_def_cfa_reg: r6
        .uleb128 0x6
	.align 8
.LEFDE1:
.LSFDE3:
        .long   .LEFDE3-.LASFDE3        # FDE Length
.LASFDE3:
        .long   .LASFDE3-.Lframe0       # FDE CIE offset

        .long   .LFB2-. # FDE initial location
        .long   .LFE2-.LFB2     # FDE address range
        .uleb128 0x0    # Augmentation size
        .byte   0x4     # DW_CFA_advance_loc4
        .long   .LCFI10-.LFB2
        .byte   0xe     # DW_CFA_def_cfa_offset
        .uleb128 0x10
        .byte   0x86    # DW_CFA_offset, column 0x6
        .uleb128 0x2
        .byte   0x4     # DW_CFA_advance_loc4
        .long   .LCFI11-.LCFI10
        .byte   0xd     # DW_CFA_def_cfa_register
        .uleb128 0x6
        .align 8
.LEFDE3:

#endif /* __x86_64__  */
