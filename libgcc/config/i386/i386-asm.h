/* Defines common perprocessor and assembly macros for use by various stubs.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

#include "auto-host.h"

#define PASTE2(a, b) PASTE2a(a, b)
#define PASTE2a(a, b) a ## b

/* These macros currently support GNU/Linux, Solaris and Darwin.  */

#ifdef __ELF__
# define FN_TYPE(fn) .type fn,@function
# define FN_SIZE(fn) .size fn,.-fn
# ifdef HAVE_GAS_HIDDEN
#  define FN_HIDDEN(fn) .hidden fn
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
# define MOVAPS vmovaps
#elif defined(MS2SYSV_STUB_SSE)
# define MS2SYSV_STUB_PREFIX __sse_
# define MOVAPS movaps
#endif

#if defined (MS2SYSV_STUB_PREFIX) && defined (MOVAPS)

# define MS2SYSV_STUB_BEGIN(base_name) \
	HIDDEN_FUNC(PASTE2(MS2SYSV_STUB_PREFIX, base_name))

# define MS2SYSV_STUB_END(base_name) \
	FUNC_END(PASTE2(MS2SYSV_STUB_PREFIX, base_name))

/* Save SSE registers 6-15. off is the offset of rax to get to xmm6.  */
# define SSE_SAVE		   \
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

/* Restore SSE registers 6-15. off is the offset of rsi to get to xmm6.  */
# define SSE_RESTORE		    \
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

#endif /* defined (MS2SYSV_STUB_ISA) && defined (MOVAPS) */
#endif /* I386_ASM_H */
