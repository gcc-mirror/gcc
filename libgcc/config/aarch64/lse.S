/* Out-of-line LSE atomics for AArch64 architecture.
   Copyright (C) 2019 Free Software Foundation, Inc.
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
 */

/* Tell the assembler to accept LSE instructions.  */
	.arch armv8-a+lse

/* Declare the symbol gating the LSE implementations.  */
	.hidden	__aarch64_have_lse_atomics

/* Turn size and memory model defines into mnemonic fragments.  */
#if SIZE == 1
# define S     b
# define UXT   uxtb
#elif SIZE == 2
# define S     h
# define UXT   uxth
#elif SIZE == 4 || SIZE == 8 || SIZE == 16
# define S
# define UXT   mov
#else
# error
#endif

#if MODEL == 1
# define SUFF  _relax
# define A
# define L
#elif MODEL == 2
# define SUFF  _acq
# define A     a
# define L
#elif MODEL == 3
# define SUFF  _rel
# define A
# define L     l
#elif MODEL == 4
# define SUFF  _acq_rel
# define A     a
# define L     l
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
#define LDXR			glue4(ld, A, xr, S)
#define STXR			glue4(st, L, xr, S)

/* Temporary registers used.  Other than these, only the return value
   register (x0) and the flags are modified.  */
#define tmp0	16
#define tmp1	17
#define tmp2	15

/* Start and end a function.  */
.macro	STARTFN name
	.text
	.balign	16
	.globl	\name
	.hidden	\name
	.type	\name, %function
	.cfi_startproc
\name:
.endm

.macro	ENDFN name
	.cfi_endproc
	.size	\name, . - \name
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
#define CAS	glue4(cas, A, L, S)

	CAS		s(0), s(1), [x2]
	ret

8:	UXT		s(tmp0), s(0)
0:	LDXR		s(0), [x2]
	cmp		s(0), s(tmp0)
	bne		1f
	STXR		w(tmp1), s(1), [x2]
	cbnz		w(tmp1), 0b
1:	ret

#else
#define LDXP	glue3(ld, A, xp)
#define STXP	glue3(st, L, xp)
#define CASP	glue3(casp, A, L)

	CASP		x0, x1, x2, x3, [x4]
	ret

8:	mov		x(tmp0), x0
	mov		x(tmp1), x1
0:	LDXP		x0, x1, [x4]
	cmp		x0, x(tmp0)
	ccmp		x1, x(tmp1), #0, eq
	bne		1f
	STXP		w(tmp2), x(tmp0), x(tmp1), [x4]
	cbnz		w(tmp2), 0b
1:	ret

#endif

ENDFN	NAME(cas)
#endif

#ifdef L_swp
#define SWP	glue4(swp, A, L, S)

STARTFN	NAME(swp)
	JUMP_IF_NOT_LSE	8f

	SWP		s(0), s(0), [x1]
	ret

8:	mov		s(tmp0), s(0)
0:	LDXR		s(0), [x1]
	STXR		w(tmp1), s(tmp0), [x1]
	cbnz		w(tmp1), 0b
	ret

ENDFN	NAME(swp)
#endif

#if defined(L_ldadd) || defined(L_ldclr) \
    || defined(L_ldeor) || defined(L_ldset)

#ifdef L_ldadd
#define LDNM	ldadd
#define OP	add
#elif defined(L_ldclr)
#define LDNM	ldclr
#define OP	bic
#elif defined(L_ldeor)
#define LDNM	ldeor
#define OP	eor
#elif defined(L_ldset)
#define LDNM	ldset
#define OP	orr
#else
#error
#endif
#define LDOP	glue4(LDNM, A, L, S)

STARTFN	NAME(LDNM)
	JUMP_IF_NOT_LSE	8f

	LDOP		s(0), s(0), [x1]
	ret

8:	mov		s(tmp0), s(0)
0:	LDXR		s(0), [x1]
	OP		s(tmp1), s(0), s(tmp0)
	STXR		w(tmp2), s(tmp1), [x1]
	cbnz		w(tmp2), 0b
	ret

ENDFN	NAME(LDNM)
#endif
