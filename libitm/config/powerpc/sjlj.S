/* Copyright (C) 2012 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

	.text

#include "asmcfi.h"

#if defined(__powerpc64__) && defined(__ELF__)
.macro FUNC name
        .globl  \name, .\name
        .section ".opd","aw"
        .align  3
\name:
        .quad   .\name, .TOC.@tocbase, 0
        .size   \name, 24
        .type   .\name, @function
        .text
.\name:
.endm
.macro END name
	.size	.\name, . - .\name
.endm
.macro HIDDEN name
	.hidden	\name, .\name
.endm
.macro CALL name
	bl	\name
	nop
.endm
#elif defined(__ELF__)
.macro FUNC name
	.globl	\name
	.type	\name, @function
\name:
.endm
.macro END name
	.size	\name, . - \name
.endm
.macro HIDDEN name
	.hidden	\name
.endm
.macro CALL name
	bl	\name
.endm
#elif defined(_CALL_DARWIN)
.macro FUNC name
	.globl	_$0
_$0:
.endmacro
.macro END name
.endmacro
.macro HIDDEN name
	.private_extern _$0
.endmacro
.macro CALL name
	bl	_$0
.endmacro
# ifdef __ppc64__
	.machine ppc64
# else
	.machine ppc7400
# endif
#else
#error "unsupported system"
#endif

/* Parameterize the naming of registers.  */
#if defined(__ELF__)
# define r(N)	%r##N
# define f(N)	%f##N
# define v(N)	%v##N
#elif defined(__MACH__)
# define r(N)	r##N
# define f(N)	f##N
# define v(N)	v##N
#else
# define r(N)	N
# define f(N)	N
# define v(N)	N
#endif

/* Parameterize the code for 32-bit vs 64-bit.  */
#if defined(__powerpc64__) || defined(__ppc64__)
#define ldreg	ld
#define	streg	std
#define	stregu	stdu
#define WS	8
#else
#define ldreg	lwz
#define streg	stw
#define stregu	stwu
#define WS	4
#endif

/* Parameterize the code for call frame constants.  */
#if defined(_CALL_AIXDESC)
# define BASE		6*WS
# define LR_SAVE	2*WS
#elif defined(_CALL_SYSV)
# define BASE		2*WS
# define LR_SAVE	1*WS
#elif defined(_CALL_DARWIN)
# define BASE		(6*WS + 2*WS)
# define LR_SAVE	2*WS
#else
# error "unsupported system"
#endif

#if defined(__ALTIVEC__) || defined(__VSX__)
# define OFS_VR		0
# define OFS_VSCR	12*16
# define OFS_VR_END	OFS_VSCR + 8
#else
# define OFS_VR_END	0
#endif
#ifndef _SOFT_FLOAT
# define OFS_FR		OFS_VR_END
# define OFS_FPSCR	OFS_FR + 18*8
# define OFS_FR_END	OFS_FPSCR + 8
#else
# define OFS_FR_END	OFS_VR_END
#endif
#define OFS_GR		OFS_FR_END
#define OFS_CFA		OFS_GR + 18*WS
#define OFS_LR		OFS_CFA + WS
#define OFS_TOC		OFS_LR + WS
#define OFS_CR		OFS_TOC + WS
#define OFS_END		(((OFS_CR + WS + 15) / 16) * 16)

#define FRAME		(((BASE + OFS_END + 15) / 16) * 16)
#define VRSAVE		256

	.align	4
FUNC _ITM_beginTransaction
	cfi_startproc
	mflr	r(0)
	mfcr	r(5)
	addi	r(4), r(1), -OFS_END
	mr	r(6), r(1)
	streg	r(0), LR_SAVE(r(1))
	stregu	r(1), -FRAME(r(1))
	cfi_def_cfa_offset(FRAME)
	cfi_offset(65, LR_SAVE)
	streg	r(6),  OFS_CFA(r(4))
	streg	r(0),   OFS_LR(r(4))
#ifdef _CALL_DARWIN
	streg	r(13),  OFS_TOC(r(4))
#else
	streg	r(2),   OFS_TOC(r(4))
#endif
	streg	r(5),   OFS_CR(r(4))
	streg	r(14),  0*WS+OFS_GR(r(4))
	streg	r(15),  1*WS+OFS_GR(r(4))
	streg	r(16),  2*WS+OFS_GR(r(4))
	streg	r(17),  3*WS+OFS_GR(r(4))
	streg	r(18),  4*WS+OFS_GR(r(4))
	streg	r(19),  5*WS+OFS_GR(r(4))
	streg	r(20),  6*WS+OFS_GR(r(4))
	streg	r(21),  7*WS+OFS_GR(r(4))
	streg	r(22),  8*WS+OFS_GR(r(4))
	streg	r(23),  9*WS+OFS_GR(r(4))
	streg	r(24), 10*WS+OFS_GR(r(4))
	streg	r(25), 11*WS+OFS_GR(r(4))
	streg	r(26), 12*WS+OFS_GR(r(4))
	streg	r(27), 13*WS+OFS_GR(r(4))
	streg	r(28), 14*WS+OFS_GR(r(4))
	streg	r(29), 15*WS+OFS_GR(r(4))
	streg	r(30), 16*WS+OFS_GR(r(4))
	streg	r(31), 17*WS+OFS_GR(r(4))

#ifndef _SOFT_FLOAT
	/* ??? Determine when FPRs not present.  */
	/* ??? Test r(3) for pr_hasNoFloatUpdate and skip the fp save.
	   This is not yet set by the compiler.  */
	mffs	f(0)
	stfd	f(14),  0+OFS_FR(r(4))
	stfd	f(15),  8+OFS_FR(r(4))
	stfd	f(16), 16+OFS_FR(r(4))
	stfd	f(17), 24+OFS_FR(r(4))
	stfd	f(18), 32+OFS_FR(r(4))
	stfd	f(19), 40+OFS_FR(r(4))
	stfd	f(20), 48+OFS_FR(r(4))
	stfd	f(21), 56+OFS_FR(r(4))
	stfd	f(22), 64+OFS_FR(r(4))
	stfd	f(23), 72+OFS_FR(r(4))
	stfd	f(24), 80+OFS_FR(r(4))
	stfd	f(25), 88+OFS_FR(r(4))
	stfd	f(26), 96+OFS_FR(r(4))
	stfd	f(27),104+OFS_FR(r(4))
	stfd	f(28),112+OFS_FR(r(4))
	stfd	f(29),120+OFS_FR(r(4))
	stfd	f(30),128+OFS_FR(r(4))
	stfd	f(31),136+OFS_FR(r(4))
	stfd	f(0), OFS_FPSCR(r(4))
#endif

#if defined(__ALTIVEC__)
	/* ??? Determine when VRs not present.  */
	/* ??? Test r(3) for pr_hasNoVectorUpdate and skip the vr save.
	   This is not yet set by the compiler.  */
	addi	r(5), r(4), OFS_VR
	addi	r(6), r(4), OFS_VR+16
	mfspr	r(0), VRSAVE
	stvx	v(20), 0, r(5)
	addi	r(5), r(5), 32
	stvx	v(21), 0, r(6)
	addi	r(6), r(6), 32
	stvx	v(22), 0, r(5)
	addi	r(5), r(5), 32
	stvx	v(23), 0, r(6)
	addi	r(6), r(6), 32
	stvx	v(25), 0, r(5)
	addi	r(5), r(5), 32
	stvx	v(26), 0, r(6)
	addi	r(6), r(6), 32
	stvx	v(26), 0, r(5)
	addi	r(5), r(5), 32
	stvx	v(27), 0, r(6)
	addi	r(6), r(6), 32
	stvx	v(28), 0, r(5)
	addi	r(5), r(5), 32
	stvx	v(29), 0, r(6)
	addi	r(6), r(6), 32
	stvx	v(30), 0, r(5)
	stvx	v(31), 0, r(6)
	streg	r(0), OFS_VSCR(r(4))
#endif

	CALL	GTM_begin_transaction

	ldreg	r(0), LR_SAVE+FRAME(r(1))
	mtlr	r(0)
	addi	r(1), r(1), FRAME
	cfi_def_cfa_offset(0)
	cfi_restore(65)
	blr
	cfi_endproc
END _ITM_beginTransaction

	.align 4
	HIDDEN	GTM_longjmp
FUNC GTM_longjmp
	cfi_startproc
#if defined(__ALTIVEC__) || defined(__VSX__)
	/* ??? Determine when VRs not present.  */
	/* ??? Test r(5) for pr_hasNoVectorUpdate and skip the vr restore.
	   This is not yet set by the compiler.  */
	addi	r(6), r(4), OFS_VR
	addi	r(7), r(4), OFS_VR+16
	ldreg	r(0), OFS_VSCR(r(4))
	cfi_undefined(v(20))
	cfi_undefined(v(21))
	cfi_undefined(v(22))
	cfi_undefined(v(23))
	cfi_undefined(v(24))
	cfi_undefined(v(25))
	cfi_undefined(v(26))
	cfi_undefined(v(27))
	cfi_undefined(v(28))
	cfi_undefined(v(29))
	cfi_undefined(v(30))
	cfi_undefined(v(31))
	lvx	v(20), 0, r(6)
	addi	r(6), r(6), 32
	lvx	v(21), 0, r(7)
	addi	r(7), r(7), 32
	lvx	v(22), 0, r(6)
	addi	r(6), r(6), 32
	lvx	v(23), 0, r(7)
	addi	r(7), r(7), 32
	lvx	v(24), 0, r(6)
	addi	r(6), r(6), 32
	lvx	v(25), 0, r(7)
	addi	r(7), r(7), 32
	lvx	v(26), 0, r(6)
	addi	r(6), r(6), 32
	lvx	v(27), 0, r(7)
	addi	r(7), r(7), 32
	lvx	v(28), 0, r(6)
	addi	r(6), r(6), 32
	lvx	v(29), 0, r(7)
	addi	r(7), r(7), 32
	lvx	v(30), 0, r(6)
	lvx	v(31), 0, r(7)
	mtspr	VRSAVE, r(0)
#endif

#ifndef _SOFT_FLOAT
	/* ??? Determine when FPRs not present.  */
	/* ??? Test r(5) for pr_hasNoFloatUpdate and skip the fp load.
	   This is not yet set by the compiler.  */
	lfd	f(0), OFS_FPSCR(r(4))
	cfi_undefined(f(14))
	cfi_undefined(f(15))
	cfi_undefined(f(16))
	cfi_undefined(f(17))
	cfi_undefined(f(18))
	cfi_undefined(f(19))
	cfi_undefined(f(20))
	cfi_undefined(f(21))
	cfi_undefined(f(22))
	cfi_undefined(f(23))
	cfi_undefined(f(24))
	cfi_undefined(f(25))
	cfi_undefined(f(26))
	cfi_undefined(f(27))
	cfi_undefined(f(28))
	cfi_undefined(f(29))
	cfi_undefined(f(30))
	cfi_undefined(f(31))
	lfd	f(14),  0+OFS_FR(r(4))
	lfd	f(15),  8+OFS_FR(r(4))
	lfd	f(16), 16+OFS_FR(r(4))
	lfd	f(17), 24+OFS_FR(r(4))
	lfd	f(18), 32+OFS_FR(r(4))
	lfd	f(19), 40+OFS_FR(r(4))
	lfd	f(20), 48+OFS_FR(r(4))
	lfd	f(21), 56+OFS_FR(r(4))
	lfd	f(22), 64+OFS_FR(r(4))
	lfd	f(23), 72+OFS_FR(r(4))
	lfd	f(24), 80+OFS_FR(r(4))
	lfd	f(25), 88+OFS_FR(r(4))
	lfd	f(26), 96+OFS_FR(r(4))
	lfd	f(27),104+OFS_FR(r(4))
	lfd	f(28),112+OFS_FR(r(4))
	lfd	f(29),120+OFS_FR(r(4))
	lfd	f(30),128+OFS_FR(r(4))
	lfd	f(31),136+OFS_FR(r(4))
	mtfsf	0xff, f(0)
#endif

	ldreg	r(6),   OFS_CFA(r(4))
	ldreg	r(0),   OFS_LR(r(4))
#ifdef _CALL_DARWIN
	ldreg	r(13),  OFS_TOC(r(4))
#else
	ldreg	r(2),   OFS_TOC(r(4))
#endif
	ldreg	r(7),   OFS_CR(r(4))
	/* At the instant we restore the LR, the only coherent view of
	   the world we have is into the new stack frame.  Define the
	   CFA in terms of the not-yet-restored stack pointer.  This will
	   last until the end of the function.  */
	mtlr	r(0)
	cfi_def_cfa(r(6), 0)
	cfi_undefined(r(14))
	cfi_undefined(r(15))
	cfi_undefined(r(16))
	cfi_undefined(r(17))
	cfi_undefined(r(18))
	cfi_undefined(r(19))
	cfi_undefined(r(20))
	cfi_undefined(r(21))
	cfi_undefined(r(22))
	cfi_undefined(r(23))
	cfi_undefined(r(24))
	cfi_undefined(r(25))
	cfi_undefined(r(26))
	cfi_undefined(r(27))
	cfi_undefined(r(28))
	cfi_undefined(r(29))
	cfi_undefined(r(30))
	cfi_undefined(r(31))
	mtcr	r(7)
	ldreg	r(14),  0*WS+OFS_GR(r(4))
	ldreg	r(15),  1*WS+OFS_GR(r(4))
	ldreg	r(16),  2*WS+OFS_GR(r(4))
	ldreg	r(17),  3*WS+OFS_GR(r(4))
	ldreg	r(18),  4*WS+OFS_GR(r(4))
	ldreg	r(19),  5*WS+OFS_GR(r(4))
	ldreg	r(20),  6*WS+OFS_GR(r(4))
	ldreg	r(21),  7*WS+OFS_GR(r(4))
	ldreg	r(22),  8*WS+OFS_GR(r(4))
	ldreg	r(23),  9*WS+OFS_GR(r(4))
	ldreg	r(24), 10*WS+OFS_GR(r(4))
	ldreg	r(25), 11*WS+OFS_GR(r(4))
	ldreg	r(26), 12*WS+OFS_GR(r(4))
	ldreg	r(27), 13*WS+OFS_GR(r(4))
	ldreg	r(28), 14*WS+OFS_GR(r(4))
	ldreg	r(29), 15*WS+OFS_GR(r(4))
	ldreg	r(30), 16*WS+OFS_GR(r(4))
	ldreg	r(31), 17*WS+OFS_GR(r(4))
	mr	r(1), r(6)
	blr
	cfi_endproc
END GTM_longjmp

#ifdef __linux__
.section .note.GNU-stack, "", @progbits
#endif
