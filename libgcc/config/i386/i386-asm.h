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

#ifdef __ELF__
# define ELFFN(fn) .type fn,@function
#else
# define ELFFN(fn)
#endif

#define FUNC_START(fn)	\
	.global fn;	\
	ELFFN (fn);	\
fn:

#define HIDDEN_FUNC(fn)\
	FUNC_START (fn)	\
	.hidden fn;	\

#define FUNC_END(fn) .size fn,.-fn

#ifdef __SSE2__
# ifdef __AVX__
#  define MOVAPS vmovaps
# else
#  define MOVAPS movaps
# endif

/* Save SSE registers 6-15. off is the offset of rax to get to xmm6.  */
.macro SSE_SAVE off=0
	MOVAPS %xmm15,(\off - 0x90)(%rax)
	MOVAPS %xmm14,(\off - 0x80)(%rax)
	MOVAPS %xmm13,(\off - 0x70)(%rax)
	MOVAPS %xmm12,(\off - 0x60)(%rax)
	MOVAPS %xmm11,(\off - 0x50)(%rax)
	MOVAPS %xmm10,(\off - 0x40)(%rax)
	MOVAPS %xmm9, (\off - 0x30)(%rax)
	MOVAPS %xmm8, (\off - 0x20)(%rax)
	MOVAPS %xmm7, (\off - 0x10)(%rax)
	MOVAPS %xmm6, \off(%rax)
.endm

/* Restore SSE registers 6-15. off is the offset of rsi to get to xmm6.  */
.macro SSE_RESTORE off=0
	MOVAPS (\off - 0x90)(%rsi), %xmm15
	MOVAPS (\off - 0x80)(%rsi), %xmm14
	MOVAPS (\off - 0x70)(%rsi), %xmm13
	MOVAPS (\off - 0x60)(%rsi), %xmm12
	MOVAPS (\off - 0x50)(%rsi), %xmm11
	MOVAPS (\off - 0x40)(%rsi), %xmm10
	MOVAPS (\off - 0x30)(%rsi), %xmm9
	MOVAPS (\off - 0x20)(%rsi), %xmm8
	MOVAPS (\off - 0x10)(%rsi), %xmm7
	MOVAPS \off(%rsi), %xmm6
.endm

#endif /* __SSE2__ */
#endif /* I386_ASM_H */
