/* -----------------------------------------------------------------------
   darwin_closure.S - Copyright (c) 2002 2003 Free Software Foundation, 
   Inc. based on ppc_closure.S
 
   PowerPC Assembly glue.

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
#define JUMPTARGET(name) name
#define L(x) x
.text
.globl _ffi_closure_helper_DARWIN

.text
	.align 2
.globl _ffi_closure_ASM

.text
	.align 2
_ffi_closure_ASM:
LFB1:		
	mflr r0			/* extract return address  */
	stw r0, 8(r1)		/* save the return address  */
LCFI0:	
	/* 24 Bytes (Linkage Area)
	   32 Bytes (outgoing parameter area, always reserved)
	   104 Bytes (13*8 from FPR)	
	   8 Bytes (result)
	   168 Bytes  */
	
	stwu r1,-176(r1)	/* skip over caller save area
				keep stack aligned to 16  */
LCFI1:		
	/* we want to build up an area for the parameters passed
	in registers (both floating point and integer)  */
	
	/* we store gpr 3 to gpr 10 (aligned to 4)
	in the parents outgoing area  */
	stw   r3, 200(r1)
	stw   r4, 204(r1)
	stw   r5, 208(r1) 
	stw   r6, 212(r1)
	stw   r7, 216(r1)
	stw   r8, 220(r1) 
	stw   r9, 224(r1)
	stw   r10, 228(r1)

	/* we save fpr 1 to fpr 13 (aligned to 8) */
	stfd  f1, 56(r1)
	stfd  f2, 64(r1)
	stfd  f3, 72(r1)
	stfd  f4, 80(r1)
	stfd  f5, 88(r1)
	stfd  f6, 96(r1)
	stfd  f7, 104(r1)
	stfd  f8, 112(r1)
	stfd  f9, 120(r1) 
	stfd  f10, 128(r1) 
	stfd  f11, 136(r1) 
	stfd  f12, 144(r1) 
	stfd  f13, 152(r1) 

	/* set up registers for the routine that actually does the work */
	/* get the context pointer from the trampoline */
	mr r3,r11
	
	/* now load up the pointer to the result storage */
	addi r4,r1,160
	
	/* now load up the pointer to the saved gpr registers  */
	addi r5,r1,200

	/* now load up the pointer to the saved fpr registers */
	addi r6,r1,56

	/* now load up the pointer to the outgoing parameter
	stack in the previous frame  */
	addi r7,r1,232
	
	/* make the call */
	bl L(_ffi_closure_helper_DARWIN)

	/* now r3 contains the return type */
	/* so use it to look up in a table */
	/* so we know how to deal with each type */

	/* look up the proper starting point in table  */
	/* by using return type as offset */
	addi r5,r1,160		/* get pointer to results area */
	addis r4,0,ha16(.L60)	/* get address of jump table */
	addi r4,r4,lo16(.L60)
	slwi r3,r3,2		/* now multiply return type by 4 */
	lwzx r3,r4,r3		/* get the contents of that table value */
	add r3,r3,r4		/* add contents of table to table address */
	mtctr r3
	bctr			/* jump to it  */
LFE1:
	.align	2

.L60:
	.long .L44-.L60    /* FFI_TYPE_VOID */
	.long .L50-.L60    /* FFI_TYPE_INT */
	.long .L47-.L60    /* FFI_TYPE_FLOAT */
	.long .L46-.L60    /* FFI_TYPE_DOUBLE */
	.long .L46-.L60    /* FFI_TYPE_LONGDOUBLE */
	.long .L56-.L60    /* FFI_TYPE_UINT8 */
	.long .L55-.L60    /* FFI_TYPE_SINT8 */
	.long .L58-.L60    /* FFI_TYPE_UINT16 */
	.long .L57-.L60    /* FFI_TYPE_SINT16 */
	.long .L50-.L60    /* FFI_TYPE_UINT32 */
	.long .L50-.L60    /* FFI_TYPE_SINT32 */
	.long .L48-.L60    /* FFI_TYPE_UINT64 */
	.long .L48-.L60    /* FFI_TYPE_SINT64 */
	.long .L44-.L60    /* FFI_TYPE_STRUCT */
	.long .L50-.L60    /* FFI_TYPE_POINTER */


/* case double */
.L46:   
	lfd f1,0(r5)
	b .L44

/* case float */
.L47:
	lfs f1,0(r5)
	b .L44
	
/* case long long */
.L48:
	lwz r3,0(r5)
	lwz r4,4(r5)
	b .L44
	
/* case default / int32 / pointer */
.L50:
	lwz r3,0(r5)
	b .L44
	
/* case signed int8	 */
.L55:
	addi r5,r5,3
	lbz r3,0(r5)
	extsb r3,r3
	b .L44

/* case unsigned int8	 */
.L56:
	addi r5,r5,3
	lbz r3,0(r5)
	b .L44

/* case signed int16 */
.L57:
	addi r5,r5,2
	lhz r3,0(r5)
	extsh r3,r3
	b .L44

/* case unsigned int16 */
.L58:	
	addi r5,r5,2
	lhz r3,0(r5)

/* case void / done	 */
.L44:
	
	addi r1,r1,176		/* restore stack pointer  */
	lwz r0,8(r1)		/* get return address  */
	mtlr r0			/* reset link register  */
	blr
	
/* END(ffi_closure_ASM)  */
