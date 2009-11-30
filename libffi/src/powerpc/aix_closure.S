/* -----------------------------------------------------------------------
   aix_closure.S - Copyright (c) 2002, 2003, 2009 Free Software Foundation, Inc.
   based on darwin_closure.S

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

	.set r0,0
	.set r1,1
	.set r2,2
	.set r3,3
	.set r4,4
	.set r5,5
	.set r6,6
	.set r7,7
	.set r8,8
	.set r9,9
	.set r10,10
	.set r11,11
	.set r12,12
	.set r13,13
	.set r14,14
	.set r15,15
	.set r16,16
	.set r17,17
	.set r18,18
	.set r19,19
	.set r20,20
	.set r21,21
	.set r22,22
	.set r23,23
	.set r24,24
	.set r25,25
	.set r26,26
	.set r27,27
	.set r28,28
	.set r29,29
	.set r30,30
	.set r31,31
	.set f0,0
	.set f1,1
	.set f2,2
	.set f3,3
	.set f4,4
	.set f5,5
	.set f6,6
	.set f7,7
	.set f8,8
	.set f9,9
	.set f10,10
	.set f11,11
	.set f12,12
	.set f13,13
	.set f14,14
	.set f15,15
	.set f16,16
	.set f17,17
	.set f18,18
	.set f19,19
	.set f20,20
	.set f21,21

#define LIBFFI_ASM
#define JUMPTARGET(name) name
#define L(x) x
	.file "aix_closure.S"
	.toc
LC..60:
	.tc L..60[TC],L..60
	.csect .text[PR]
	.align 2

.csect .text[PR]
	.align 2
	.globl ffi_closure_ASM
	.globl .ffi_closure_ASM
.csect ffi_closure_ASM[DS]
ffi_closure_ASM:
#ifdef __64BIT__
	.llong .ffi_closure_ASM, TOC[tc0], 0
	.csect .text[PR]
.ffi_closure_ASM:

	mflr r0			/* extract return address */
	std r0,16(r1)		/* save the return address */

	/* 48  Bytes (Linkage Area) */
	/* 64  Bytes (params) */
	/* 104 Bytes (13*8 from FPR) */
	/* 32  Bytes (result) */
	/* 248 Bytes */

	stdu r1,-248(r1)	/* skip over caller save area
				   keep stack aligned to 16  */

/* we want to build up an area for the parameters passed */
/* in registers (both floating point and integer) */

	/* we store gpr 3 to gpr 10 (aligned to 4)
	in the parents outgoing area  */
	std   r3, (304+0*8)(r1)
	std   r4, (304+1*8)(r1)
	std   r5, (304+2*8)(r1)
	std   r6, (304+3*8)(r1)
	std   r7, (304+4*8)(r1)
	std   r8, (304+5*8)(r1)
	std   r9, (304+6*8)(r1)
	std   r10, (304+7*8)(r1)

	/* next save fpr 1 to fpr 13 (aligned to 8) */
	stfd  f1, (112+0*8)(r1)
	stfd  f2, (112+1*8)(r1)
	stfd  f3, (112+2*8)(r1)
	stfd  f4, (112+3*8)(r1)
	stfd  f5, (112+4*8)(r1)
	stfd  f6, (112+5*8)(r1)
	stfd  f7, (112+6*8)(r1)
	stfd  f8, (112+7*8)(r1)
	stfd  f9, (112+8*8)(r1)
	stfd  f10, (112+9*8)(r1)
	stfd  f11, (112+10*8)(r1)
	stfd  f12, (112+11*8)(r1)
	stfd  f13, (112+12*8)(r1)

	/* set up registers for the routine that actually does the work */
	/* get the context pointer from the trampoline */
	mr r3,r11

	/* now load up the pointer to the result storage */
	addi r4,r1,216

	/* now load up the pointer to the saved gpr registers */
	addi r5,r1,304

	/* now load up the pointer to the saved fpr registers */
	addi r6,r1,112

	/* make the call */
	bl .ffi_closure_helper_DARWIN
	nop

	/* now r3 contains the return type */
	/* so use it to look up in a table */
	/* so we know how to deal with each type */

	/* look up the proper starting point in table  */
	/* by using return type as offset */
	addi r5,r1,216		/* get pointer to results area */
	ld r4,LC..60(2)		/* get address of jump table */
	sldi r3,r3,2		/* now multiply return type by 4 */
	lwzx r3,r4,r3		/* get the contents of that table value */
	add r3,r3,r4		/* add contents of table to table address */
	mtctr r3
	bctr			/* jump to it */

L..60:
	.long L..44-L..60    /* FFI_TYPE_VOID */
	.long L..51-L..60    /* FFI_TYPE_INT */
	.long L..47-L..60    /* FFI_TYPE_FLOAT */
	.long L..46-L..60    /* FFI_TYPE_DOUBLE */
	.long L..45-L..60    /* FFI_TYPE_LONGDOUBLE */
	.long L..56-L..60    /* FFI_TYPE_UINT8 */
	.long L..55-L..60    /* FFI_TYPE_SINT8 */
	.long L..58-L..60    /* FFI_TYPE_UINT16 */
	.long L..57-L..60    /* FFI_TYPE_SINT16 */
	.long L..50-L..60    /* FFI_TYPE_UINT32 */
	.long L..51-L..60    /* FFI_TYPE_SINT32 */
	.long L..48-L..60    /* FFI_TYPE_UINT64 */
	.long L..48-L..60    /* FFI_TYPE_SINT64 */
	.long L..44-L..60    /* FFI_TYPE_STRUCT */
	.long L..48-L..60    /* FFI_TYPE_POINTER */


/* case long double */
L..45:
	lfd f1,0(r5)
	lfd f2,8(r5)
	b L..44

/* case double */
L..46:
	lfd f1,0(r5)
	b L..44

/* case float */
L..47:
	lfs f1,0(r5)
	b L..44

/* case long long / pointer */
L..48:
	ld r3,0(r5)
	b L..44

/* case uint32 */
L..50:
	lwz r3,4(r5)
	b L..44

/* case int / sint32 */
L..51:
	lwa r3,4(r5)
	b L..44

/* case signed int8 */
L..55:
	lbz r3,7(r5)
	extsb r3,r3
	b L..44

/* case unsigned int8 */
L..56:
	lbz r3,7(r5)
	b L..44

/* case signed int16 */
L..57:
	lha r3,6(r5)
	b L..44

/* case unsigned int16 */
L..58:
	lhz r3,6(r5)

/* case void / done	 */
L..44:
	addi r1,r1,248		/* restore stack pointer */
	ld r0,16(r1)		/* get return address */
	mtlr r0			/* reset link register */
	blr

#else /* ! __64BIT__ */
	
	.long .ffi_closure_ASM, TOC[tc0], 0
	.csect .text[PR]
.ffi_closure_ASM:

	mflr r0			/* extract return address */
	stw r0, 8(r1)		/* save the return address */

	/* 24 Bytes (Linkage Area) */
	/* 32 Bytes (params) */
	/* 104 Bytes (13*8 from FPR) */
	/* 16  Bytes (result) */
	/* 176 Bytes */

	stwu r1,-176(r1)	/* skip over caller save area
				keep stack aligned to 16  */

/* we want to build up an area for the parameters passed */
/* in registers (both floating point and integer) */

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

	/* next save fpr 1 to fpr 13 (aligned to 8) */
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

	/* now load up the pointer to the saved gpr registers */
	addi r5,r1,200

	/* now load up the pointer to the saved fpr registers */
	addi r6,r1,56

	/* make the call */
	bl .ffi_closure_helper_DARWIN
	nop

	/* now r3 contains the return type */
	/* so use it to look up in a table */
	/* so we know how to deal with each type */

	/* look up the proper starting point in table  */
	/* by using return type as offset */
	addi r5,r1,160		/* get pointer to results area */
	lwz r4,LC..60(2)	/* get address of jump table */
	slwi r3,r3,2		/* now multiply return type by 4 */
	lwzx r3,r4,r3		/* get the contents of that table value */
	add r3,r3,r4		/* add contents of table to table address */
	mtctr r3
	bctr			/* jump to it */

L..60:
	.long L..44-L..60    /* FFI_TYPE_VOID */
	.long L..50-L..60    /* FFI_TYPE_INT */
	.long L..47-L..60    /* FFI_TYPE_FLOAT */
	.long L..46-L..60    /* FFI_TYPE_DOUBLE */
	.long L..45-L..60    /* FFI_TYPE_LONGDOUBLE */
	.long L..56-L..60    /* FFI_TYPE_UINT8 */
	.long L..55-L..60    /* FFI_TYPE_SINT8 */
	.long L..58-L..60    /* FFI_TYPE_UINT16 */
	.long L..57-L..60    /* FFI_TYPE_SINT16 */
	.long L..50-L..60    /* FFI_TYPE_UINT32 */
	.long L..50-L..60    /* FFI_TYPE_SINT32 */
	.long L..48-L..60    /* FFI_TYPE_UINT64 */
	.long L..48-L..60    /* FFI_TYPE_SINT64 */
	.long L..44-L..60    /* FFI_TYPE_STRUCT */
	.long L..50-L..60    /* FFI_TYPE_POINTER */


/* case long double */
L..45:
	lfd f1,0(r5)
	lfd f2,8(r5)
	b L..44

/* case double */
L..46:
	lfd f1,0(r5)
	b L..44

/* case float */
L..47:
	lfs f1,0(r5)
	b L..44

/* case long long */
L..48:
	lwz r3,0(r5)
	lwz r4,4(r5)
	b L..44

/* case default / int32 / pointer */
L..50:
	lwz r3,0(r5)
	b L..44

/* case signed int8 */
L..55:
	lbz r3,3(r5)
	extsb r3,r3
	b L..44

/* case unsigned int8 */
L..56:
	lbz r3,3(r5)
	b L..44

/* case signed int16 */
L..57:
	lha r3,2(r5)
	b L..44

/* case unsigned int16 */
L..58:
	lhz r3,2(r5)

/* case void / done	 */
L..44:
	addi r1,r1,176		/* restore stack pointer */
	lwz r0,8(r1)		/* get return address */
	mtlr r0			/* reset link register */
	blr
#endif
/* END(ffi_closure_ASM) */
