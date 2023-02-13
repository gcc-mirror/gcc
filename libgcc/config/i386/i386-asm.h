/* Defines common perprocessor and assembly macros for use by various stubs.
   Copyright (C) 2016-2023 Free Software Foundation, Inc.
   Contributed by Daniel Santos <daniel.santos@pobox.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef I386_ASM_H
#define I386_ASM_H

#include "auto-target.h"

#ifdef __GCC_HAVE_DWARF2_CFI_ASM
# define cfi_startproc()		.cfi_startproc
# define cfi_endproc()			.cfi_endproc
# define cfi_adjust_cfa_offset(X) 	.cfi_adjust_cfa_offset X
# define cfi_def_cfa_register(X)	.cfi_def_cfa_register X
# define cfi_def_cfa(R,O)		.cfi_def_cfa R, O
# define cfi_register(D,S)		.cfi_register D, S
# define cfi_offset(R,O)		.cfi_offset R, O
# ifdef __x86_64__
#  define cfi_push(X)		.cfi_adjust_cfa_offset 8; .cfi_rel_offset X, 0
#  define cfi_pop(X)		.cfi_adjust_cfa_offset -8; .cfi_restore X
# else
#  define cfi_push(X)		.cfi_adjust_cfa_offset 4; .cfi_rel_offset X, 0
#  define cfi_pop(X)		.cfi_adjust_cfa_offset -4; .cfi_restore X
# endif
#else
# define cfi_startproc()
# define cfi_endproc()
# define cfi_adjust_cfa_offset(X)
# define cfi_def_cfa_register(X)
# define cfi_def_cfa(R,O)
# define cfi_register(D,S)
# define cfi_offset(R,O)
# define cfi_push(X)
# define cfi_pop(X)
#endif

#define PASTE2(a, b) PASTE2a(a, b)
#define PASTE2a(a, b) a ## b

/* These macros currently support GNU/Linux, Solaris and Darwin.  */

#ifdef __ELF__
# define FN_TYPE(fn) .type fn,@function
# define FN_SIZE(fn) .size fn,.-fn
# ifdef AS_HIDDEN_DIRECTIVE
#  define FN_HIDDEN(fn) AS_HIDDEN_DIRECTIVE fn
# endif
#else
# define FN_TYPE(fn)
# define FN_SIZE(fn)
#endif

#ifndef FN_HIDDEN
# define FN_HIDDEN(fn)
#endif

#ifdef __USER_LABEL_PREFIX__
# define ASMNAME(name)		PASTE2(__USER_LABEL_PREFIX__, name)
#else
# define ASMNAME(name)		name
#endif

#define FUNC_BEGIN(fn)		\
	.globl ASMNAME(fn);	\
	FN_TYPE (ASMNAME(fn));	\
ASMNAME(fn):

#define HIDDEN_FUNC(fn)		\
	.globl ASMNAME(fn);	\
	FN_TYPE(ASMNAME(fn));	\
	FN_HIDDEN(ASMNAME(fn));	\
ASMNAME(fn):

#define FUNC_END(fn) FN_SIZE(ASMNAME(fn))

#ifdef MS2SYSV_STUB_AVX
# define MS2SYSV_STUB_PREFIX __avx_
# ifdef HAVE_AS_AVX
#  define MOVAPS vmovaps
# endif
#elif defined(MS2SYSV_STUB_SSE)
# define MS2SYSV_STUB_PREFIX __sse_
# define MOVAPS movaps
#endif

#ifdef MS2SYSV_STUB_PREFIX

# define MS2SYSV_STUB_BEGIN(base_name) \
	HIDDEN_FUNC(PASTE2(MS2SYSV_STUB_PREFIX, base_name))

# define MS2SYSV_STUB_END(base_name) \
	FUNC_END(PASTE2(MS2SYSV_STUB_PREFIX, base_name))

/* If expanding for sse or avx and we have assembler support.  */
# ifdef MOVAPS
/* Save SSE registers 6-15 using rax as the base address.  */
#  define SSE_SAVE		   \
	MOVAPS %xmm15,-0x30(%rax); \
	MOVAPS %xmm14,-0x20(%rax); \
	MOVAPS %xmm13,-0x10(%rax); \
	MOVAPS %xmm12,     (%rax); \
	MOVAPS %xmm11, 0x10(%rax); \
	MOVAPS %xmm10, 0x20(%rax); \
	MOVAPS %xmm9,  0x30(%rax); \
	MOVAPS %xmm8,  0x40(%rax); \
	MOVAPS %xmm7,  0x50(%rax); \
	MOVAPS %xmm6,  0x60(%rax)

/* Restore SSE registers 6-15 using rsi as the base address.  */
#  define SSE_RESTORE		    \
	MOVAPS -0x30(%rsi), %xmm15; \
	MOVAPS -0x20(%rsi), %xmm14; \
	MOVAPS -0x10(%rsi), %xmm13; \
	MOVAPS      (%rsi), %xmm12; \
	MOVAPS  0x10(%rsi), %xmm11; \
	MOVAPS  0x20(%rsi), %xmm10; \
	MOVAPS  0x30(%rsi), %xmm9 ; \
	MOVAPS  0x40(%rsi), %xmm8 ; \
	MOVAPS  0x50(%rsi), %xmm7 ; \
	MOVAPS  0x60(%rsi), %xmm6
# else /* MOVAPS */
/* If the assembler doesn't support AVX then directly emit machine code
   for the instructions above.  */
#  define SSE_SAVE							     \
	.byte 0xc5, 0x78, 0x29, 0x78, 0xd0; /* vmovaps %xmm15,-0x30(%rax) */ \
	.byte 0xc5, 0x78, 0x29, 0x70, 0xe0; /* vmovaps %xmm14,-0x20(%rax) */ \
	.byte 0xc5, 0x78, 0x29, 0x68, 0xf0; /* vmovaps %xmm13,-0x10(%rax) */ \
	.byte 0xc5, 0x78, 0x29, 0x20;       /* vmovaps %xmm12,     (%rax) */ \
	.byte 0xc5, 0x78, 0x29, 0x58, 0x10; /* vmovaps %xmm11, 0x10(%rax) */ \
	.byte 0xc5, 0x78, 0x29, 0x50, 0x20; /* vmovaps %xmm10, 0x20(%rax) */ \
	.byte 0xc5, 0x78, 0x29, 0x48, 0x30; /* vmovaps %xmm9,  0x30(%rax) */ \
	.byte 0xc5, 0x78, 0x29, 0x40, 0x40; /* vmovaps %xmm8,  0x40(%rax) */ \
	.byte 0xc5, 0xf8, 0x29, 0x78, 0x50; /* vmovaps %xmm7,  0x50(%rax) */ \
	.byte 0xc5, 0xf8, 0x29, 0x70, 0x60; /* vmovaps %xmm6,  0x60(%rax) */
#  define SSE_RESTORE							     \
	.byte 0xc5, 0x78, 0x28, 0x7e, 0xd0; /* vmovaps -0x30(%rsi),%xmm15 */ \
	.byte 0xc5, 0x78, 0x28, 0x76, 0xe0; /* vmovaps -0x20(%rsi),%xmm14 */ \
	.byte 0xc5, 0x78, 0x28, 0x6e, 0xf0; /* vmovaps -0x10(%rsi),%xmm13 */ \
	.byte 0xc5, 0x78, 0x28, 0x26;       /* vmovaps      (%rsi),%xmm12 */ \
	.byte 0xc5, 0x78, 0x28, 0x5e, 0x10; /* vmovaps  0x10(%rsi),%xmm11 */ \
	.byte 0xc5, 0x78, 0x28, 0x56, 0x20; /* vmovaps  0x20(%rsi),%xmm10 */ \
	.byte 0xc5, 0x78, 0x28, 0x4e, 0x30; /* vmovaps  0x30(%rsi),%xmm9  */ \
	.byte 0xc5, 0x78, 0x28, 0x46, 0x40; /* vmovaps  0x40(%rsi),%xmm8  */ \
	.byte 0xc5, 0xf8, 0x28, 0x7e, 0x50; /* vmovaps  0x50(%rsi),%xmm7  */ \
	.byte 0xc5, 0xf8, 0x28, 0x76, 0x60; /* vmovaps  0x60(%rsi),%xmm6  */
# endif /* MOVAPS */
#endif /* MS2SYSV_STUB_PREFIX */
#endif /* I386_ASM_H */
