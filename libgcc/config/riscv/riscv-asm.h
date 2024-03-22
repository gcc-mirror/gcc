/* Copyright (C) 2017-2025 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define FUNC_TYPE(X)	.type X,@function
#define FUNC_SIZE(X)	.size X,.-X

#define FUNC_BEGIN(X)		\
	.align 2;		\
	.globl X;		\
	FUNC_TYPE (X);		\
X:				\
	LPAD

#define FUNC_END(X)		\
	FUNC_SIZE(X)

#define FUNC_ALIAS(X,Y)		\
	.globl X;		\
	X = Y

#define CONCAT1(a, b)		CONCAT2(a, b)
#define CONCAT2(a, b)		a ## b
#define HIDDEN_JUMPTARGET(X)	CONCAT1(__hidden_, X)
#define HIDDEN_DEF(X)		FUNC_ALIAS(HIDDEN_JUMPTARGET(X), X);     \
				.hidden HIDDEN_JUMPTARGET(X)

/* GNU_PROPERTY_RISCV64_* macros from elf.h for use in asm code.  */
#define FEATURE_1_AND 0xc0000000
#define FEATURE_1_FCFI 1
#define FEATURE_1_BCFI 2

/* Add a NT_GNU_PROPERTY_TYPE_0 note.  */
#if __riscv_xlen == 32
#  define GNU_PROPERTY(type, value)	\
    .section .note.gnu.property, "a";	\
    .p2align 2;				\
    .word 4;				\
    .word 12;				\
    .word 5;				\
    .asciz "GNU";			\
    .word type;				\
    .word 4;				\
    .word value;			\
    .text
#else
#  define GNU_PROPERTY(type, value)	\
    .section .note.gnu.property, "a";	\
    .p2align 3;				\
    .word 4;				\
    .word 16;				\
    .word 5;				\
    .asciz "GNU";			\
    .word type;				\
    .word 4;				\
    .word value;			\
    .word 0;				\
    .text
#endif

/* Add GNU property note with the supported features to all asm code
   where sysdep.h is included.  */
#undef __VALUE_FOR_FEATURE_1_AND
#if defined (__riscv_zicfilp) || defined (__riscv_zicfiss)
#  if defined (__riscv_zicfilp)
#    if defined (__riscv_zicfiss)
#      define __VALUE_FOR_FEATURE_1_AND 0x3
#    else
#      define __VALUE_FOR_FEATURE_1_AND 0x1
#    endif
#  else
#    if defined (__riscv_zicfiss)
#      define __VALUE_FOR_FEATURE_1_AND 0x2
#    else
#      error "What?"
#    endif
#  endif
#endif

#if defined (__VALUE_FOR_FEATURE_1_AND)
GNU_PROPERTY (FEATURE_1_AND, __VALUE_FOR_FEATURE_1_AND)
#endif
#undef __VALUE_FOR_FEATURE_1_AND

#ifdef __riscv_zicfilp
# define SET_LPAD   lui  t2, 0
# define LPAD       lpad 0
#else
# define SET_LPAD
# define LPAD
#endif
