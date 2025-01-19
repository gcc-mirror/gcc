/* Linux-specific atomic operations for m68k Linux.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
   Based on code contributed by CodeSourcery for ARM EABI Linux.

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

/* Coldfire dropped the CAS instruction from the base M68K ISA.

   GCC automatically issues a asm memory barrier when it encounters
   a __sync_synchronize builtin.  Thus, we do not need to define this
   builtin.

   We implement byte, short and int versions of each atomic operation
   using the kernel helper defined below.  There is no support for
   64-bit operations yet.  */

#include <stdbool.h>

#ifndef __NR_atomic_cmpxchg_32
#define __NR_atomic_cmpxchg_32  335
#endif

/* Kernel helper for compare-and-exchange a 32-bit value.  */
static inline unsigned
__kernel_cmpxchg (unsigned *mem, unsigned oldval, unsigned newval)
{
  register unsigned *a0 asm("a0") = mem;
  register unsigned d2 asm("d2") = oldval;
  register unsigned d1 asm("d1") = newval;
  register unsigned d0 asm("d0") = __NR_atomic_cmpxchg_32;

  asm volatile ("trap #0"
		: "=r"(d0), "=r"(d1), "=r"(a0)
		: "r"(d0), "r"(d1), "r"(d2), "r"(a0)
		: "memory", "a1");

  return d0;
}

#define HIDDEN __attribute__ ((visibility ("hidden")))

/* Big endian masks  */
#define INVERT_MASK_1 24
#define INVERT_MASK_2 16

#define MASK_1 0xffu
#define MASK_2 0xffffu

#define NAME_oldval(OP, WIDTH) __sync_fetch_and_##OP##_##WIDTH
#define NAME_newval(OP, WIDTH) __sync_##OP##_and_fetch_##WIDTH

#define WORD_SYNC_OP(OP, PFX_OP, INF_OP, RETURN)			\
  unsigned HIDDEN							\
  NAME##_##RETURN (OP, 4) (unsigned *ptr, unsigned val)			\
  {									\
    unsigned oldval, newval, cmpval = *ptr;				\
									\
    do {								\
      oldval = cmpval;							\
      newval = PFX_OP (oldval INF_OP val);				\
      cmpval = __kernel_cmpxchg (ptr, oldval, newval);			\
    } while (__builtin_expect (oldval != cmpval, 0));			\
									\
    return RETURN;							\
  }

#define SUBWORD_SYNC_OP(OP, PFX_OP, INF_OP, TYPE, WIDTH, RETURN)	\
  TYPE HIDDEN								\
  NAME##_##RETURN (OP, WIDTH) (TYPE *ptr, TYPE sval)			\
  {									\
    unsigned *wordptr = (unsigned *) ((unsigned long) ptr & ~3);	\
    unsigned int mask, shift, oldval, newval, cmpval, wval;		\
									\
    shift = (((unsigned long) ptr & 3) << 3) ^ INVERT_MASK_##WIDTH;	\
    mask = MASK_##WIDTH << shift;					\
    wval = (sval & MASK_##WIDTH) << shift;				\
									\
    cmpval = *wordptr;							\
    do {								\
      oldval = cmpval;							\
      newval = PFX_OP (oldval INF_OP wval);				\
      newval = (newval & mask) | (oldval & ~mask);			\
      cmpval = __kernel_cmpxchg (wordptr, oldval, newval);		\
    } while (__builtin_expect (oldval != cmpval, 0));			\
									\
    return (RETURN >> shift) & MASK_##WIDTH;				\
  }

WORD_SYNC_OP (add,   , +, oldval)
WORD_SYNC_OP (sub,   , -, oldval)
WORD_SYNC_OP (or,    , |, oldval)
WORD_SYNC_OP (and,   , &, oldval)
WORD_SYNC_OP (xor,   , ^, oldval)
WORD_SYNC_OP (nand, ~, &, oldval)

SUBWORD_SYNC_OP (add,   , +, unsigned short, 2, oldval)
SUBWORD_SYNC_OP (sub,   , -, unsigned short, 2, oldval)
SUBWORD_SYNC_OP (or,    , |, unsigned short, 2, oldval)
SUBWORD_SYNC_OP (and,   , &, unsigned short, 2, oldval)
SUBWORD_SYNC_OP (xor,   , ^, unsigned short, 2, oldval)
SUBWORD_SYNC_OP (nand, ~, &, unsigned short, 2, oldval)

SUBWORD_SYNC_OP (add,   , +, unsigned char, 1, oldval)
SUBWORD_SYNC_OP (sub,   , -, unsigned char, 1, oldval)
SUBWORD_SYNC_OP (or,    , |, unsigned char, 1, oldval)
SUBWORD_SYNC_OP (and,   , &, unsigned char, 1, oldval)
SUBWORD_SYNC_OP (xor,   , ^, unsigned char, 1, oldval)
SUBWORD_SYNC_OP (nand, ~, &, unsigned char, 1, oldval)

WORD_SYNC_OP (add,   , +, newval)
WORD_SYNC_OP (sub,   , -, newval)
WORD_SYNC_OP (or,    , |, newval)
WORD_SYNC_OP (and,   , &, newval)
WORD_SYNC_OP (xor,   , ^, newval)
WORD_SYNC_OP (nand, ~, &, newval)

SUBWORD_SYNC_OP (add,   , +, unsigned short, 2, newval)
SUBWORD_SYNC_OP (sub,   , -, unsigned short, 2, newval)
SUBWORD_SYNC_OP (or,    , |, unsigned short, 2, newval)
SUBWORD_SYNC_OP (and,   , &, unsigned short, 2, newval)
SUBWORD_SYNC_OP (xor,   , ^, unsigned short, 2, newval)
SUBWORD_SYNC_OP (nand, ~, &, unsigned short, 2, newval)

SUBWORD_SYNC_OP (add,   , +, unsigned char, 1, newval)
SUBWORD_SYNC_OP (sub,   , -, unsigned char, 1, newval)
SUBWORD_SYNC_OP (or,    , |, unsigned char, 1, newval)
SUBWORD_SYNC_OP (and,   , &, unsigned char, 1, newval)
SUBWORD_SYNC_OP (xor,   , ^, unsigned char, 1, newval)
SUBWORD_SYNC_OP (nand, ~, &, unsigned char, 1, newval)

unsigned HIDDEN
__sync_val_compare_and_swap_4 (unsigned *ptr, unsigned oldval, unsigned newval)
{
  return __kernel_cmpxchg (ptr, oldval, newval);
}

bool HIDDEN
__sync_bool_compare_and_swap_4 (unsigned *ptr, unsigned oldval,
				unsigned newval)
{
  return __kernel_cmpxchg (ptr, oldval, newval) == oldval;
}

#define SUBWORD_VAL_CAS(TYPE, WIDTH)					\
  TYPE HIDDEN								\
  __sync_val_compare_and_swap_##WIDTH (TYPE *ptr, TYPE soldval,		\
				       TYPE snewval)			\
  {									\
    unsigned *wordptr = (unsigned *)((unsigned long) ptr & ~3);		\
    unsigned int mask, shift, woldval, wnewval;				\
    unsigned oldval, newval, cmpval;					\
									\
    shift = (((unsigned long) ptr & 3) << 3) ^ INVERT_MASK_##WIDTH;	\
    mask = MASK_##WIDTH << shift;					\
    woldval = (soldval & MASK_##WIDTH) << shift;			\
    wnewval = (snewval & MASK_##WIDTH) << shift;			\
    cmpval = *wordptr;							\
									\
    do {								\
      oldval = cmpval;							\
      if ((oldval & mask) != woldval)					\
	break;								\
      newval = (oldval & ~mask) | wnewval;				\
      cmpval = __kernel_cmpxchg (wordptr, oldval, newval);		\
    } while (__builtin_expect (oldval != cmpval, 0));			\
									\
    return (oldval >> shift) & MASK_##WIDTH;				\
  }

SUBWORD_VAL_CAS (unsigned short, 2)
SUBWORD_VAL_CAS (unsigned char,  1)

#define SUBWORD_BOOL_CAS(TYPE, WIDTH)					\
  bool HIDDEN								\
  __sync_bool_compare_and_swap_##WIDTH (TYPE *ptr, TYPE oldval,		\
					TYPE newval)			\
  {									\
    return (__sync_val_compare_and_swap_##WIDTH (ptr, oldval, newval)	\
	    == oldval);							\
  }

SUBWORD_BOOL_CAS (unsigned short, 2)
SUBWORD_BOOL_CAS (unsigned char,  1)

#undef NAME_oldval
#define NAME_oldval(OP, WIDTH) __sync_lock_##OP##_##WIDTH
#define COMMA ,

WORD_SYNC_OP (test_and_set, , COMMA, oldval)
SUBWORD_SYNC_OP (test_and_set, , COMMA, unsigned char, 1, oldval)
SUBWORD_SYNC_OP (test_and_set, , COMMA, unsigned short, 2, oldval)
