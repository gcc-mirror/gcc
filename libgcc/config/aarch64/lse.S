/* Out-of-line LSE atomics for AArch64 architecture.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
   Contributed by Linaro Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * The problem that we are trying to solve is operating system deployment
 * of ARMv8.1-Atomics, also known as Large System Exensions (LSE).
 *
 * There are a number of potential solutions for this problem which have
 * been proposed and rejected for various reasons.  To recap:
 *
 * (1) Multiple builds.  The dynamic linker will examine /lib64/atomics/
 * if HWCAP_ATOMICS is set, allowing entire libraries to be overwritten.
 * However, not all Linux distributions are happy with multiple builds,
 * and anyway it has no effect on main applications.
 *
 * (2) IFUNC.  We could put these functions into libgcc_s.so, and have
 * a single copy of each function for all DSOs.  However, ARM is concerned
 * that the branch-to-indirect-branch that is implied by using a PLT,
 * as required by IFUNC, is too much overhead for smaller cpus.
 *
 * (3) Statically predicted direct branches.  This is the approach that
 * is taken here.  These functions are linked into every DSO that uses them.
 * All of the symbols are hidden, so that the functions are called via a
 * direct branch.  The choice of LSE vs non-LSE is done via one byte load
 * followed by a well-predicted direct branch.  The functions are compiled
 * separately to minimize code size.
 *
 * Since these functions have hidden visibility and are never called
 * indirectly, they do not need to start with a BTI instruction.
 */

#include "aarch64-asm.h"
#include "auto-target.h"

/* Tell the assembler to accept LSE instructions.  */
#ifdef HAVE_AS_LSE
	.arch armv8-a+lse
#else
	.arch armv8-a
#endif

/* Declare the symbol gating the LSE implementations.  */
	HIDDEN(__aarch64_have_lse_atomics)

/* Turn size and memory model defines into mnemonic fragments.  */
#if SIZE == 1
# define S     b
# define UXT   uxtb
# define B     0x00000000
#elif SIZE == 2
# define S     h
# define UXT   uxth
# define B     0x40000000
#elif SIZE == 4 || SIZE == 8 || SIZE == 16
# define S
# define UXT   mov
# if SIZE == 4
#  define B    0x80000000
# elif SIZE == 8
#  define B    0xc0000000
# endif
#else
# error
#endif

#if MODEL == 1
# define SUFF  _relax
# define A
# define L
# define M     0x000000
# define N     0x000000
# define BARRIER
#elif MODEL == 2
# define SUFF  _acq
# define A     a
# define L
# define M     0x400000
# define N     0x800000
# define BARRIER
#elif MODEL == 3
# define SUFF  _rel
# define A
# define L     l
# define M     0x008000
# define N     0x400000
# define BARRIER
#elif MODEL == 4
# define SUFF  _acq_rel
# define A     a
# define L     l
# define M     0x408000
# define N     0xc00000
# define BARRIER
#elif MODEL == 5
# define SUFF  _sync
#ifdef L_swp
/* swp has _acq semantics.  */
#  define A    a
#  define L
#  define M    0x400000
#  define N    0x800000
#else
/* All other _sync functions have _seq semantics.  */
#  define A    a
#  define L    l
#  define M    0x408000
#  define N    0xc00000
#endif
# define BARRIER dmb		ish
#else
# error
#endif

/* Concatenate symbols.  */
#define glue2_(A, B)		A ## B
#define glue2(A, B)		glue2_(A, B)
#define glue3_(A, B, C)		A ## B ## C
#define glue3(A, B, C)		glue3_(A, B, C)
#define glue4_(A, B, C, D)	A ## B ## C ## D
#define glue4(A, B, C, D)	glue4_(A, B, C, D)

/* Select the size of a register, given a regno.  */
#define x(N)			glue2(x, N)
#define w(N)			glue2(w, N)
#if SIZE < 8
# define s(N)			w(N)
#else
# define s(N)			x(N)
#endif

#define NAME(BASE)		glue4(__aarch64_, BASE, SIZE, SUFF)
#if MODEL == 5
/* Drop A for _sync functions.  */
# define LDXR			glue3(ld, xr, S)
#else
# define LDXR			glue4(ld, A, xr, S)
#endif
#define STXR			glue4(st, L, xr, S)

/* Temporary registers used.  Other than these, only the return value
   register (x0) and the flags are modified.  */
#define tmp0	16
#define tmp1	17
#define tmp2	15
#define tmp3	14
#define tmp4	13

/* Start and end a function.  */
.macro	STARTFN name
	.text
	.balign	16
	.globl	\name
	HIDDEN(\name)
	SYMBOL_TYPE(\name, %function)
	.cfi_startproc
\name:
.endm

.macro	ENDFN name
	.cfi_endproc
	SYMBOL_SIZE(\name)
.endm

/* Branch to LABEL if LSE is disabled.  */
.macro	JUMP_IF_NOT_LSE label
	adrp	x(tmp0), __aarch64_have_lse_atomics
	ldrb	w(tmp0), [x(tmp0), :lo12:__aarch64_have_lse_atomics]
	cbz	w(tmp0), \label
.endm

#ifdef L_cas

STARTFN	NAME(cas)
	JUMP_IF_NOT_LSE	8f

#if SIZE < 16
#ifdef HAVE_AS_LSE
# define CAS	glue4(cas, A, L, S)	s(0), s(1), [x2]
#else
# define CAS	.inst 0x08a07c41 + B + M
#endif

	CAS		/* s(0), s(1), [x2] */
	ret

8:	UXT		s(tmp0), s(0)
0:	LDXR		s(0), [x2]
	cmp		s(0), s(tmp0)
	bne		1f
	STXR		w(tmp1), s(1), [x2]
	cbnz		w(tmp1), 0b
1:	BARRIER
	ret

#else
#if MODEL == 5
/* Drop A for _sync functions.  */
# define LDXP	glue2(ld, xp)
#else
# define LDXP	glue3(ld, A, xp)
#endif
#define STXP	glue3(st, L, xp)
#ifdef HAVE_AS_LSE
# define CASP	glue3(casp, A, L)	x0, x1, x2, x3, [x4]
#else
# define CASP	.inst 0x48207c82 + M
#endif

	CASP		/* x0, x1, x2, x3, [x4] */
	ret

8:	mov		x(tmp0), x0
	mov		x(tmp1), x1
0:	LDXP		x0, x1, [x4]
	cmp		x0, x(tmp0)
	ccmp		x1, x(tmp1), #0, eq
	csel		x(tmp2), x2, x0, eq
	csel		x(tmp3), x3, x1, eq
	STXP		w(tmp4), x(tmp2), x(tmp3), [x4]
	cbnz		w(tmp4), 0b
	BARRIER
	ret

#endif

ENDFN	NAME(cas)
#endif

#ifdef L_swp
#ifdef HAVE_AS_LSE
# define SWP	glue4(swp, A, L, S)	s(0), s(0), [x1]
#else
# define SWP	.inst 0x38208020 + B + N
#endif

STARTFN	NAME(swp)
	JUMP_IF_NOT_LSE	8f

	SWP		/* s(0), s(0), [x1] */
	ret

8:	mov		s(tmp0), s(0)
0:	LDXR		s(0), [x1]
	STXR		w(tmp1), s(tmp0), [x1]
	cbnz		w(tmp1), 0b
	BARRIER
	ret

ENDFN	NAME(swp)
#endif

#if defined(L_ldadd) || defined(L_ldclr) \
    || defined(L_ldeor) || defined(L_ldset)

#ifdef L_ldadd
#define LDNM	ldadd
#define OP	add
#define OPN	0x0000
#elif defined(L_ldclr)
#define LDNM	ldclr
#define OP	bic
#define OPN	0x1000
#elif defined(L_ldeor)
#define LDNM	ldeor
#define OP	eor
#define OPN	0x2000
#elif defined(L_ldset)
#define LDNM	ldset
#define OP	orr
#define OPN	0x3000
#else
#error
#endif
#ifdef HAVE_AS_LSE
# define LDOP	glue4(LDNM, A, L, S)	s(0), s(0), [x1]
#else
# define LDOP	.inst 0x38200020 + OPN + B + N
#endif

STARTFN	NAME(LDNM)
	JUMP_IF_NOT_LSE	8f

	LDOP		/* s(0), s(0), [x1] */
	ret

8:	mov		s(tmp0), s(0)
0:	LDXR		s(0), [x1]
	OP		s(tmp1), s(0), s(tmp0)
	STXR		w(tmp2), s(tmp1), [x1]
	cbnz		w(tmp2), 0b
	BARRIER
	ret

ENDFN	NAME(LDNM)
#endif
