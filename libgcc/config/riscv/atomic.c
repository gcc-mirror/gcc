/* Legacy sub-word atomics for RISC-V.

   Copyright (C) 2016-2024 Free Software Foundation, Inc.

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

#ifdef __riscv_atomic

#include <stdbool.h>

#define INVERT		"not %[tmp1], %[tmp1]\n\t"
#define DONT_INVERT	""

/* Logic duplicated in gcc/gcc/config/riscv/sync.md for use when inlining is enabled */

#define GENERATE_FETCH_AND_OP(type, size, opname, insn, invert, cop)	\
  type __sync_fetch_and_ ## opname ## _ ## size (type *p, type v)	\
  {									\
    unsigned long aligned_addr = ((unsigned long) p) & ~3UL;		\
    int shift = (((unsigned long) p) & 3) * 8;				\
    unsigned mask = ((1U << ((sizeof v) * 8)) - 1) << shift;		\
    unsigned old, tmp1, tmp2;						\
									\
    asm volatile ("1:\n\t"						\
		  "lr.w.aqrl %[old], %[mem]\n\t"			\
		  #insn " %[tmp1], %[old], %[value]\n\t"		\
		  invert						\
		  "and %[tmp1], %[tmp1], %[mask]\n\t"			\
		  "and %[tmp2], %[old], %[not_mask]\n\t"		\
		  "or %[tmp2], %[tmp2], %[tmp1]\n\t"			\
		  "sc.w.rl %[tmp1], %[tmp2], %[mem]\n\t"		\
		  "bnez %[tmp1], 1b"					\
		  : [old] "=&r" (old),					\
		    [mem] "+A" (*(volatile unsigned*) aligned_addr),	\
		    [tmp1] "=&r" (tmp1),				\
		    [tmp2] "=&r" (tmp2)					\
		  : [value] "r" (((unsigned) v) << shift),		\
		    [mask] "r" (mask),					\
		    [not_mask] "r" (~mask));				\
									\
    return (type) (old >> shift);					\
  }									\
									\
  type __sync_ ## opname ## _and_fetch_ ## size (type *p, type v)	\
  {									\
    type o = __sync_fetch_and_ ## opname ## _ ## size (p, v);		\
    return cop;								\
  }

#define GENERATE_COMPARE_AND_SWAP(type, size)				\
  type __sync_val_compare_and_swap_ ## size (type *p, type o, type n)	\
  {									\
    unsigned long aligned_addr = ((unsigned long) p) & ~3UL;		\
    int shift = (((unsigned long) p) & 3) * 8;				\
    unsigned mask = ((1U << ((sizeof o) * 8)) - 1) << shift;		\
    unsigned old, tmp1;							\
									\
    asm volatile ("1:\n\t"						\
		  "lr.w.aqrl %[old], %[mem]\n\t"			\
		  "and %[tmp1], %[old], %[mask]\n\t"			\
		  "bne %[tmp1], %[o], 1f\n\t"				\
		  "and %[tmp1], %[old], %[not_mask]\n\t"		\
		  "or %[tmp1], %[tmp1], %[n]\n\t"			\
		  "sc.w.rl %[tmp1], %[tmp1], %[mem]\n\t"		\
		  "bnez %[tmp1], 1b\n\t"				\
		  "1:"							\
		  : [old] "=&r" (old),					\
		    [mem] "+A" (*(volatile unsigned*) aligned_addr),	\
		    [tmp1] "=&r" (tmp1)					\
		  : [o] "r" ((((unsigned) o) << shift) & mask),		\
		    [n] "r" ((((unsigned) n) << shift) & mask),		\
		    [mask] "r" (mask),					\
		    [not_mask] "r" (~mask));				\
									\
    return (type) (old >> shift);					\
  }									\
  bool __sync_bool_compare_and_swap_ ## size (type *p, type o, type n)	\
  {									\
    return __sync_val_compare_and_swap(p, o, n) == o;			\
  }

#define GENERATE_ALL(type, size)					\
  GENERATE_FETCH_AND_OP(type, size, add, add, DONT_INVERT, o + v)	\
  GENERATE_FETCH_AND_OP(type, size, sub, sub, DONT_INVERT, o - v)	\
  GENERATE_FETCH_AND_OP(type, size, and, and, DONT_INVERT, o & v)	\
  GENERATE_FETCH_AND_OP(type, size, xor, xor, DONT_INVERT, o ^ v)	\
  GENERATE_FETCH_AND_OP(type, size, or, or, DONT_INVERT, o | v)		\
  GENERATE_FETCH_AND_OP(type, size, nand, and, INVERT, ~(o & v))	\
  GENERATE_COMPARE_AND_SWAP(type, size)

GENERATE_ALL(unsigned char, 1)
GENERATE_ALL(unsigned short, 2)

#endif
