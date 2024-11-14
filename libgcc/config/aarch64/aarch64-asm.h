/* AArch64 asm definitions.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef AARCH64_ASM_H
#define AARCH64_ASM_H

#include "auto-target.h"

#define L(label) .L ## label

/* Marking variant PCS symbol references is important for PLT calls
   otherwise it is for documenting the PCS in the symbol table.  */
#ifdef HAVE_AS_VARIANT_PCS
# define variant_pcs(name) .variant_pcs name
#else
# define variant_pcs(name)
#endif

/* GNU_PROPERTY_AARCH64_* macros from elf.h for use in asm code.  */
#define FEATURE_1_AND 0xc0000000
#define FEATURE_1_BTI 1
#define FEATURE_1_PAC 2
#define FEATURE_1_GCS 4

/* Supported features based on the code generation options.  */
#if defined(__ARM_FEATURE_BTI_DEFAULT)
# define BTI_FLAG FEATURE_1_BTI
# define BTI_C hint	34
#else
# define BTI_FLAG 0
# define BTI_C
#endif

#if __ARM_FEATURE_PAC_DEFAULT & 3
# define PAC_FLAG FEATURE_1_PAC
# define PACIASP hint	25; .cfi_negate_ra_state
# define AUTIASP hint	29; .cfi_negate_ra_state
#else
# define PAC_FLAG 0
# define PACIASP
# define AUTIASP
#endif

#if __ARM_FEATURE_GCS_DEFAULT
# define GCS_FLAG FEATURE_1_GCS
#else
# define GCS_FLAG 0
#endif

#ifdef __ELF__
#define HIDDEN(name) .hidden name
#define SYMBOL_SIZE(name) .size name, .-name
#define SYMBOL_TYPE(name, _type) .type name, _type
#else
#define HIDDEN(name)
#define SYMBOL_SIZE(name)
#define SYMBOL_TYPE(name, _type)
#endif

/* Add a NT_GNU_PROPERTY_TYPE_0 note.  */
#define GNU_PROPERTY(type, value)	\
  .section .note.gnu.property, "a";	\
  .p2align 3;				\
  .word 4;				\
  .word 16;				\
  .word 5;				\
  .asciz "GNU";				\
  .word type;				\
  .word 4;				\
  .word value;				\
  .word 0;				\
  .previous

#if defined(__linux__) || defined(__FreeBSD__)
/* Do not require executable stack.  */
.section .note.GNU-stack, "", %progbits
.previous

/* Add GNU property note if built with branch protection.  */
# if (BTI_FLAG|PAC_FLAG|GCS_FLAG) != 0
GNU_PROPERTY (FEATURE_1_AND, BTI_FLAG|PAC_FLAG|GCS_FLAG)
# endif
#endif

#define ENTRY_ALIGN(name, align) \
  .global name;		\
  SYMBOL_TYPE(name, %function);		\
  .balign align;	\
  name:			\
  .cfi_startproc;	\
  BTI_C

#define ENTRY(name) ENTRY_ALIGN(name, 16)

#define END(name) \
  .cfi_endproc;		\
  SYMBOL_SIZE(name)

#endif
