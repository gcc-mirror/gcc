/* -----------------------------------------------------------------------
   darwin.S - Copyright (c) 2000 John Hornkvist
   
   PowerPC Assembly glue.

   $Id: darwin.S,v 1.1 2001/10/09 05:32:15 bryce Exp $

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
   IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#define LIBFFI_ASM	
#include <ffi.h>
#define JUMPTARGET(name) name
#define L(x) x
.text
	.align 2
.globl _ffi_prep_args

.text
	.align 2
.globl _ffi_call_DARWIN
.text
	.align 2
_ffi_call_DARWIN:
	mr      r12,r8 // We only need r12 until the call, so it doesn't have to be saved...
	/* Save the old stack pointer as AP.  */
	mr	r8,r1

	/* Allocate the stack space we need.  */
	stwux	r1,r1,r4	
	
	/* Save registers we use.  */
	mflr	r9

	stw	r28,-16(r8)
	stw	r29,-12(r8)
	stw	r30, -8(r8)
	stw	r31, -4(r8)
	
	stw	r9,  8(r8)
	stw	r2, 20(r1)

	/* Save arguments over call...  */
	mr	r31,r5	/* flags, */
	mr	r30,r6	/* rvalue, */
	mr	r29,r7	/* function address, */
	mr	r28,r8	/* our AP. */
		
	/* Call ffi_prep_args.  */
	mr	r4,r1
	li	r9,0
	
	mtctr	r12 // r12 holds address of _ffi_prep_args
	bctrl 
	lwz     r2,20(r1)

	/* Now do the call.  */
	/* Set up cr1 with bits 4-7 of the flags.  */
	mtcrf	0x40,r31
	/* Get the address to call into CTR.  */
	mtctr	r29
	/* Load all those argument registers.  */
        // We have set up a nice stack frame, just load it into registers.
        lwz     r3, 20+(1*4)(r1)
        lwz     r4, 20+(2*4)(r1)
        lwz     r5, 20+(3*4)(r1)
        lwz     r6, 20+(4*4)(r1)
        nop
	lwz     r7, 20+(5*4)(r1)
        lwz     r8, 20+(6*4)(r1)
        lwz     r9, 20+(7*4)(r1)
        lwz     r10,20+(8*4)(r1)
		
L1:
	/* Load all the FP registers.  */
	bf	6,L2 // 2f + 0x18
	lfd	f1,-16-(13*8)(r28)
	lfd	f2,-16-(12*8)(r28)
	lfd	f3,-16-(11*8)(r28)
	lfd	f4,-16-(10*8)(r28)
	nop
	lfd	f5,-16-(9*8)(r28)
	lfd	f6,-16-(8*8)(r28)
	lfd	f7,-16-(7*8)(r28)
	lfd	f8,-16-(6*8)(r28)
	nop
	lfd     f9,-16-(5*8)(r28)
        lfd     f10,-16-(4*8)(r28)
	lfd     f11,-16-(3*8)(r28)
	lfd     f12,-16-(2*8)(r28)
	nop
	lfd     f13,-16-(1*8)(r28)

L2:	
	mr	r12,r29 // Put the target address in r12 as specified.
	mtctr  r12
	nop
	nop
	/* Make the call.  */
	bctrl
        
	/* Now, deal with the return value.  */
	mtcrf	0x01,r31

	bt	30,L(done_return_value)
	bt	29,L(fp_return_value)
	stw	r3,0(r30)
	bf	28,L(done_return_value)
	stw	r4,4(r30)

	/* Fall through...  */

L(done_return_value):
	/* Restore the registers we used and return.  */
	lwz	r9,   8(r28)
	lwz	r31,  -4(r28)
	mtlr	r9
	lwz	r30, -8(r28)
	lwz	r29,-12(r28)
	lwz	r28,-16(r28)
	lwz	r1,0(r1)
	blr

L(fp_return_value):
	bf	28,L(float_return_value)
	stfd	f1,0(r30)
	b	L(done_return_value)
L(float_return_value):
	stfs	f1,0(r30)
	b	L(done_return_value)
//END(_ffi_call_DARWIN)

/* Provide a null definition of _ffi_call_AIX.  */
.text
	.align 2
.globl _ffi_call_AIX
.text
	.align 2
_ffi_call_AIX:
	blr
//END(_ffi_call_AIX)

