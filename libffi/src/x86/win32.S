/* -----------------------------------------------------------------------
   win32.S - Copyright (c) 1996, 1998, 2001, 2002, 2009  Red Hat, Inc.
	     Copyright (c) 2001  John Beniton
	     Copyright (c) 2002  Ranjit Mathew
			
 
   X86 Foreign Function Interface
 
   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:
 
   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.
 
   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
   -----------------------------------------------------------------------
   */
 
#define LIBFFI_ASM
#include <fficonfig.h>
#include <ffi.h>
 
	.text
 
        # This assumes we are using gas.
        .balign 16
	.globl	_ffi_call_SYSV
	.def	_ffi_call_SYSV;	.scl	2;	.type	32;	.endef
_ffi_call_SYSV:
.LFB1:
        pushl %ebp
.LCFI0:
        movl  %esp,%ebp
.LCFI1:
        # Make room for all of the new args.
        movl  16(%ebp),%ecx                                                     
        subl  %ecx,%esp
 
        movl  %esp,%eax
 
        # Place all of the ffi_prep_args in position
        pushl 12(%ebp)
        pushl %eax
        call  *8(%ebp)
 
        # Return stack to previous state and call the function
        addl  $8,%esp
 
        # FIXME: Align the stack to a 128-bit boundary to avoid
        # potential performance hits.

	call  *28(%ebp)
 
        # Remove the space we pushed for the args
        movl  16(%ebp),%ecx
        addl  %ecx,%esp
 
        # Load %ecx with the return type code
        movl  20(%ebp),%ecx
 
        # If the return value pointer is NULL, assume no return value.
        cmpl  $0,24(%ebp)
        jne   retint
 
        # Even if there is no space for the return value, we are
        # obliged to handle floating-point values.
        cmpl  $FFI_TYPE_FLOAT,%ecx
        jne   noretval
        fstp  %st(0)
 
        jmp   epilogue
 
retint:
        cmpl  $FFI_TYPE_INT,%ecx
        jne   retfloat
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movl  %eax,0(%ecx)
        jmp   epilogue
 
retfloat:
        cmpl  $FFI_TYPE_FLOAT,%ecx
        jne   retdouble   
         # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        fstps (%ecx)
        jmp   epilogue
 
retdouble:
        cmpl  $FFI_TYPE_DOUBLE,%ecx
        jne   retlongdouble
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        fstpl (%ecx)
        jmp   epilogue
 
retlongdouble:
        cmpl  $FFI_TYPE_LONGDOUBLE,%ecx
        jne   retint64
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        fstpt (%ecx)
        jmp   epilogue
 
retint64:
        cmpl  $FFI_TYPE_SINT64,%ecx
        jne   retstruct1b
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movl  %eax,0(%ecx)
        movl  %edx,4(%ecx)
 
retstruct1b:
        cmpl  $FFI_TYPE_SINT8,%ecx
        jne   retstruct2b
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movb  %al,0(%ecx)
        jmp   epilogue
 
retstruct2b:
        cmpl  $FFI_TYPE_SINT16,%ecx
        jne   retstruct
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movw  %ax,0(%ecx)
        jmp   epilogue
 
retstruct:
        # Nothing to do!
 
noretval:
epilogue:
        movl %ebp,%esp
        popl %ebp
        ret
.ffi_call_SYSV_end:
.LFE1:

        # This assumes we are using gas.
        .balign 16
	.globl	_ffi_call_STDCALL
	.def	_ffi_call_STDCALL;	.scl	2;	.type	32;	.endef
_ffi_call_STDCALL:
.LFB2:
        pushl %ebp
.LCFI2:
        movl  %esp,%ebp
.LCFI3:
        # Make room for all of the new args.
        movl  16(%ebp),%ecx 
        subl  %ecx,%esp

        movl  %esp,%eax

        # Place all of the ffi_prep_args in position
        pushl 12(%ebp)
        pushl %eax
        call  *8(%ebp)

        # Return stack to previous state and call the function
        addl  $8,%esp

        # FIXME: Align the stack to a 128-bit boundary to avoid
        # potential performance hits.

        call  *28(%ebp)

        # stdcall functions pop arguments off the stack themselves

        # Load %ecx with the return type code
        movl  20(%ebp),%ecx

        # If the return value pointer is NULL, assume no return value.
        cmpl  $0,24(%ebp)
        jne   sc_retint

        # Even if there is no space for the return value, we are
        # obliged to handle floating-point values.
        cmpl  $FFI_TYPE_FLOAT,%ecx
        jne   sc_noretval
        fstp  %st(0)

        jmp   sc_epilogue

sc_retint:
        cmpl  $FFI_TYPE_INT,%ecx
        jne   sc_retfloat
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movl  %eax,0(%ecx)
        jmp   sc_epilogue

sc_retfloat:
        cmpl  $FFI_TYPE_FLOAT,%ecx
        jne   sc_retdouble
         # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        fstps (%ecx)
        jmp   sc_epilogue

sc_retdouble:
        cmpl  $FFI_TYPE_DOUBLE,%ecx
        jne   sc_retlongdouble
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        fstpl (%ecx)
        jmp   sc_epilogue

sc_retlongdouble:
        cmpl  $FFI_TYPE_LONGDOUBLE,%ecx
        jne   sc_retint64
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        fstpt (%ecx)
        jmp   sc_epilogue

sc_retint64:
        cmpl  $FFI_TYPE_SINT64,%ecx
        jne   sc_retstruct1b
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movl  %eax,0(%ecx)
        movl  %edx,4(%ecx)

sc_retstruct1b:
        cmpl  $FFI_TYPE_SINT8,%ecx
        jne   sc_retstruct2b
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movb  %al,0(%ecx)
        jmp   sc_epilogue

sc_retstruct2b:
        cmpl  $FFI_TYPE_SINT16,%ecx
        jne   sc_retstruct
        # Load %ecx with the pointer to storage for the return value
        movl  24(%ebp),%ecx
        movw  %ax,0(%ecx)
        jmp   sc_epilogue

sc_retstruct:
        # Nothing to do!

sc_noretval:
sc_epilogue:
        movl %ebp,%esp
        popl %ebp
        ret
.ffi_call_STDCALL_end:
.LFE2:

        # This assumes we are using gas.
        .balign 16
	.globl	_ffi_closure_SYSV
	.def	_ffi_closure_SYSV;	.scl	2;	.type	32;	.endef
_ffi_closure_SYSV:
.LFB3:
	pushl	%ebp
.LCFI4:
	movl	%esp, %ebp
.LCFI5:
	subl	$40, %esp
	leal	-24(%ebp), %edx
	movl	%edx, -12(%ebp)	/* resp */
	leal	8(%ebp), %edx
	movl	%edx, 4(%esp)	/* args = __builtin_dwarf_cfa () */
	leal	-12(%ebp), %edx
	movl	%edx, (%esp)	/* &resp */
	call	_ffi_closure_SYSV_inner
	movl	-12(%ebp), %ecx
	cmpl	$FFI_TYPE_INT, %eax
	je	.Lcls_retint
	cmpl	$FFI_TYPE_FLOAT, %eax
	je	.Lcls_retfloat
	cmpl	$FFI_TYPE_DOUBLE, %eax
	je	.Lcls_retdouble
	cmpl	$FFI_TYPE_LONGDOUBLE, %eax
	je	.Lcls_retldouble
	cmpl	$FFI_TYPE_SINT64, %eax
	je	.Lcls_retllong
	cmpl	$FFI_TYPE_SINT8, %eax	/* 1-byte struct */
	je	.Lcls_retstruct1
	cmpl	$FFI_TYPE_SINT16, %eax	/* 2-bytes struct */
	je	.Lcls_retstruct2
.Lcls_epilogue:
	movl	%ebp, %esp
	popl	%ebp
	ret
.Lcls_retint:
	movl	(%ecx), %eax
	jmp	.Lcls_epilogue
.Lcls_retfloat:
	flds	(%ecx)
	jmp	.Lcls_epilogue
.Lcls_retdouble:
	fldl	(%ecx)
	jmp	.Lcls_epilogue
.Lcls_retldouble:
	fldt	(%ecx)
	jmp	.Lcls_epilogue
.Lcls_retllong:
	movl	(%ecx), %eax
	movl	4(%ecx), %edx
	jmp	.Lcls_epilogue
.Lcls_retstruct1:
	movsbl	(%ecx), %eax
	jmp	.Lcls_epilogue
.Lcls_retstruct2:
	movswl	(%ecx), %eax
	jmp	.Lcls_epilogue
.ffi_closure_SYSV_end:
.LFE3:

#if !FFI_NO_RAW_API

#define RAW_CLOSURE_CIF_OFFSET ((FFI_TRAMPOLINE_SIZE + 3) & ~3)
#define RAW_CLOSURE_FUN_OFFSET (RAW_CLOSURE_CIF_OFFSET + 4)
#define RAW_CLOSURE_USER_DATA_OFFSET (RAW_CLOSURE_FUN_OFFSET + 4)
#define CIF_FLAGS_OFFSET 20

        # This assumes we are using gas.
        .balign 16
	.globl	_ffi_closure_raw_SYSV
	.def	_ffi_closure_raw_SYSV;	.scl	2;	.type	32;	.endef
_ffi_closure_raw_SYSV:
.LFB4:
	pushl	%ebp
.LCFI6:
	movl	%esp, %ebp
.LCFI7:
	pushl	%esi
.LCFI8:
	subl	$36, %esp
	movl	RAW_CLOSURE_CIF_OFFSET(%eax), %esi	 /* closure->cif */
	movl	RAW_CLOSURE_USER_DATA_OFFSET(%eax), %edx /* closure->user_data */
	movl	%edx, 12(%esp)	/* user_data */
	leal	8(%ebp), %edx	/* __builtin_dwarf_cfa () */
	movl	%edx, 8(%esp)	/* raw_args */
	leal	-24(%ebp), %edx
	movl	%edx, 4(%esp)	/* &res */
	movl	%esi, (%esp)	/* cif */
	call	*RAW_CLOSURE_FUN_OFFSET(%eax)		 /* closure->fun */
	movl	CIF_FLAGS_OFFSET(%esi), %eax		 /* rtype */
	cmpl	$FFI_TYPE_INT, %eax
	je	.Lrcls_retint
	cmpl	$FFI_TYPE_FLOAT, %eax
	je	.Lrcls_retfloat
	cmpl	$FFI_TYPE_DOUBLE, %eax
	je	.Lrcls_retdouble
	cmpl	$FFI_TYPE_LONGDOUBLE, %eax
	je	.Lrcls_retldouble
	cmpl	$FFI_TYPE_SINT64, %eax
	je	.Lrcls_retllong
.Lrcls_epilogue:
	addl	$36, %esp
	popl	%esi
	popl	%ebp
	ret
.Lrcls_retint:
	movl	-24(%ebp), %eax
	jmp	.Lrcls_epilogue
.Lrcls_retfloat:
	flds	-24(%ebp)
	jmp	.Lrcls_epilogue
.Lrcls_retdouble:
	fldl	-24(%ebp)
	jmp	.Lrcls_epilogue
.Lrcls_retldouble:
	fldt	-24(%ebp)
	jmp	.Lrcls_epilogue
.Lrcls_retllong:
	movl	-24(%ebp), %eax
	movl	-20(%ebp), %edx
	jmp	.Lrcls_epilogue
.ffi_closure_raw_SYSV_end:
.LFE4:

#endif /* !FFI_NO_RAW_API */

        # This assumes we are using gas.
	.balign	16
	.globl	_ffi_closure_STDCALL
	.def	_ffi_closure_STDCALL;	.scl	2;	.type	32;	.endef
_ffi_closure_STDCALL:
.LFB5:
	pushl	%ebp
.LCFI9:
	movl	%esp, %ebp
.LCFI10:
	subl	$40, %esp
	leal	-24(%ebp), %edx
	movl	%edx, -12(%ebp)	/* resp */
	leal	12(%ebp), %edx  /* account for stub return address on stack */
	movl	%edx, 4(%esp)	/* args */
	leal	-12(%ebp), %edx
	movl	%edx, (%esp)	/* &resp */
	call	_ffi_closure_SYSV_inner
	movl	-12(%ebp), %ecx
	/* It would be nice to just share this code with the
	   duplicate sequence in _ffi_closure_SYSV, if only
	   there were some way to represent that in the EH info.  */
	cmpl	$FFI_TYPE_INT, %eax
	je	.Lscls_retint
	cmpl	$FFI_TYPE_FLOAT, %eax
	je	.Lscls_retfloat
	cmpl	$FFI_TYPE_DOUBLE, %eax
	je	.Lscls_retdouble
	cmpl	$FFI_TYPE_LONGDOUBLE, %eax
	je	.Lscls_retldouble
	cmpl	$FFI_TYPE_SINT64, %eax
	je	.Lscls_retllong
	cmpl	$FFI_TYPE_SINT8, %eax	/* 1-byte struct */
	je	.Lscls_retstruct1
	cmpl	$FFI_TYPE_SINT16, %eax	/* 2-bytes struct */
	je	.Lscls_retstruct2
.Lscls_epilogue:
	movl	%ebp, %esp
	popl	%ebp
	ret
.Lscls_retint:
	movl	(%ecx), %eax
	jmp	.Lscls_epilogue
.Lscls_retfloat:
	flds	(%ecx)
	jmp	.Lscls_epilogue
.Lscls_retdouble:
	fldl	(%ecx)
	jmp	.Lscls_epilogue
.Lscls_retldouble:
	fldt	(%ecx)
	jmp	.Lscls_epilogue
.Lscls_retllong:
	movl	(%ecx), %eax
	movl	4(%ecx), %edx
	jmp	.Lscls_epilogue
.Lscls_retstruct1:
	movsbl	(%ecx), %eax
	jmp	.Lscls_epilogue
.Lscls_retstruct2:
	movswl	(%ecx), %eax
	jmp	.Lscls_epilogue
.ffi_closure_STDCALL_end:
.LFE5:

	.section	.eh_frame,"w"
.Lframe1:
.LSCIE1:
	.long	.LECIE1-.LASCIE1  /* Length of Common Information Entry */
.LASCIE1:
	.long	0x0	/* CIE Identifier Tag */
	.byte	0x1	/* CIE Version */
#ifdef __PIC__
	.ascii "zR\0"	/* CIE Augmentation */
#else
	.ascii "\0"	/* CIE Augmentation */
#endif
	.byte	0x1	/* .uleb128 0x1; CIE Code Alignment Factor */
	.byte	0x7c	/* .sleb128 -4; CIE Data Alignment Factor */
	.byte	0x8	/* CIE RA Column */
#ifdef __PIC__
	.byte	0x1	/* .uleb128 0x1; Augmentation size */
	.byte	0x1b	/* FDE Encoding (pcrel sdata4) */
#endif
	.byte	0xc	/* DW_CFA_def_cfa CFA = r4 + 4 = 4(%esp) */
	.byte	0x4	/* .uleb128 0x4 */
	.byte	0x4	/* .uleb128 0x4 */
	.byte	0x88	/* DW_CFA_offset, column 0x8 %eip at CFA + 1 * -4 */
	.byte	0x1	/* .uleb128 0x1 */
	.align 4
.LECIE1:

.LSFDE1:
	.long	.LEFDE1-.LASFDE1	/* FDE Length */
.LASFDE1:
	.long	.LASFDE1-.Lframe1	/* FDE CIE offset */
#if defined __PIC__ && defined HAVE_AS_X86_PCREL
	.long	.LFB1-.	/* FDE initial location */
#else
	.long	.LFB1
#endif
	.long	.LFE1-.LFB1	/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	/* DW_CFA_xxx CFI instructions go here.  */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI0-.LFB1
	.byte	0xe	/* DW_CFA_def_cfa_offset CFA = r4 + 8 = 8(%esp) */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 %ebp at CFA + 2 * -4 */
	.byte	0x2	/* .uleb128 0x2 */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI1-.LCFI0
	.byte	0xd	/* DW_CFA_def_cfa_register CFA = r5 = %ebp */
	.byte	0x5	/* .uleb128 0x5 */

	/* End of DW_CFA_xxx CFI instructions.  */
	.align 4
.LEFDE1:


.LSFDE2:
	.long	.LEFDE2-.LASFDE2	/* FDE Length */
.LASFDE2:
	.long	.LASFDE2-.Lframe1	/* FDE CIE offset */
#if defined __PIC__ && defined HAVE_AS_X86_PCREL
	.long	.LFB2-.	/* FDE initial location */
#else
	.long	.LFB2
#endif
	.long	.LFE2-.LFB2	/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	/* DW_CFA_xxx CFI instructions go here.  */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI2-.LFB2
	.byte	0xe	/* DW_CFA_def_cfa_offset CFA = r4 + 8 = 8(%esp) */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 %ebp at CFA + 2 * -4 */
	.byte	0x2	/* .uleb128 0x2 */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI3-.LCFI2
	.byte	0xd	/* DW_CFA_def_cfa_register CFA = r5 = %ebp */
	.byte	0x5	/* .uleb128 0x5 */

	/* End of DW_CFA_xxx CFI instructions.  */
	.align 4
.LEFDE2:


.LSFDE3:
	.long	.LEFDE3-.LASFDE3	/* FDE Length */
.LASFDE3:
	.long	.LASFDE3-.Lframe1	/* FDE CIE offset */
#if defined __PIC__ && defined HAVE_AS_X86_PCREL
	.long	.LFB3-.	/* FDE initial location */
#else
	.long	.LFB3
#endif
	.long	.LFE3-.LFB3	/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	/* DW_CFA_xxx CFI instructions go here.  */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI4-.LFB3
	.byte	0xe	/* DW_CFA_def_cfa_offset CFA = r4 + 8 = 8(%esp) */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 %ebp at CFA + 2 * -4 */
	.byte	0x2	/* .uleb128 0x2 */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI5-.LCFI4
	.byte	0xd	/* DW_CFA_def_cfa_register CFA = r5 = %ebp */
	.byte	0x5	/* .uleb128 0x5 */

	/* End of DW_CFA_xxx CFI instructions.  */
	.align 4
.LEFDE3:

#if !FFI_NO_RAW_API

.LSFDE4:
	.long	.LEFDE4-.LASFDE4	/* FDE Length */
.LASFDE4:
	.long	.LASFDE4-.Lframe1	/* FDE CIE offset */
#if defined __PIC__ && defined HAVE_AS_X86_PCREL
	.long	.LFB4-.	/* FDE initial location */
#else
	.long	.LFB4
#endif
	.long	.LFE4-.LFB4	/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	/* DW_CFA_xxx CFI instructions go here.  */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI6-.LFB4
	.byte	0xe	/* DW_CFA_def_cfa_offset CFA = r4 + 8 = 8(%esp) */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 %ebp at CFA + 2 * -4 */
	.byte	0x2	/* .uleb128 0x2 */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI7-.LCFI6
	.byte	0xd	/* DW_CFA_def_cfa_register CFA = r5 = %ebp */
	.byte	0x5	/* .uleb128 0x5 */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI8-.LCFI7
	.byte	0x86	/* DW_CFA_offset, column 0x6 %esi at CFA + 3 * -4 */
	.byte	0x3	/* .uleb128 0x3 */

	/* End of DW_CFA_xxx CFI instructions.  */
	.align 4
.LEFDE4:

#endif /* !FFI_NO_RAW_API */

.LSFDE5:
	.long	.LEFDE5-.LASFDE5	/* FDE Length */
.LASFDE5:
	.long	.LASFDE5-.Lframe1	/* FDE CIE offset */
#if defined __PIC__ && defined HAVE_AS_X86_PCREL
	.long	.LFB5-.	/* FDE initial location */
#else
	.long	.LFB5
#endif
	.long	.LFE5-.LFB5	/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	/* DW_CFA_xxx CFI instructions go here.  */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI9-.LFB5
	.byte	0xe	/* DW_CFA_def_cfa_offset CFA = r4 + 8 = 8(%esp) */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 %ebp at CFA + 2 * -4 */
	.byte	0x2	/* .uleb128 0x2 */

	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI10-.LCFI9
	.byte	0xd	/* DW_CFA_def_cfa_register CFA = r5 = %ebp */
	.byte	0x5	/* .uleb128 0x5 */

	/* End of DW_CFA_xxx CFI instructions.  */
	.align 4
.LEFDE5:
