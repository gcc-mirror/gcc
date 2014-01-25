/* TILE atomics.
   Copyright (C) 2011-2014 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "atomic.h"

/* This code should be inlined by the compiler, but for now support
   it as out-of-line methods in libgcc.  */

static void
pre_atomic_barrier (int model)
{
  switch ((enum memmodel) model)
    {
    case MEMMODEL_RELEASE:
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      __atomic_thread_fence (model);
      break;
    default:
      break;
    }
  return;
}

static void
post_atomic_barrier (int model)
{
  switch ((enum memmodel) model)
    {
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      __atomic_thread_fence (model);
      break;
    default:
      break;
    }
  return;
}

#define __unused __attribute__((unused))

#define __atomic_fetch_and_do(type, size, opname)		\
type								\
__atomic_fetch_##opname##_##size(type* p, type i, int model)	\
{								\
  pre_atomic_barrier(model);					\
  type rv = arch_atomic_##opname(p, i);				\
  post_atomic_barrier(model);					\
  return rv;							\
}

__atomic_fetch_and_do (int, 4, add)
__atomic_fetch_and_do (int, 4, sub)
__atomic_fetch_and_do (int, 4, or)
__atomic_fetch_and_do (int, 4, and)
__atomic_fetch_and_do (int, 4, xor)
__atomic_fetch_and_do (int, 4, nand)
__atomic_fetch_and_do (long long, 8, add)
__atomic_fetch_and_do (long long, 8, sub)
__atomic_fetch_and_do (long long, 8, or)
__atomic_fetch_and_do (long long, 8, and)
__atomic_fetch_and_do (long long, 8, xor)
__atomic_fetch_and_do (long long, 8, nand)

#define __atomic_do_and_fetch(type, size, opname, op, op2)	\
type								\
__atomic_##opname##_fetch_##size(type* p, type i, int model)	\
{								\
  pre_atomic_barrier(model);					\
  type rv = op2 (arch_atomic_##opname(p, i) op i);		\
  post_atomic_barrier(model);					\
  return rv;							\
}
__atomic_do_and_fetch (int, 4, add, +, )
__atomic_do_and_fetch (int, 4, sub, -, )
__atomic_do_and_fetch (int, 4, or, |, )
__atomic_do_and_fetch (int, 4, and, &, )
__atomic_do_and_fetch (int, 4, xor, |, )
__atomic_do_and_fetch (int, 4, nand, &, ~)
__atomic_do_and_fetch (long long, 8, add, +, )
__atomic_do_and_fetch (long long, 8, sub, -, )
__atomic_do_and_fetch (long long, 8, or, |, )
__atomic_do_and_fetch (long long, 8, and, &, )
__atomic_do_and_fetch (long long, 8, xor, |, )
__atomic_do_and_fetch (long long, 8, nand, &, ~)

#define __atomic_exchange_methods(type, size)				\
bool									\
__atomic_compare_exchange_##size(volatile type* ptr, type* oldvalp,	\
				 type newval, bool weak __unused,	\
				 int models, int modelf __unused)	\
{									\
  type oldval = *oldvalp;						\
  pre_atomic_barrier(models);						\
  type retval = arch_atomic_val_compare_and_exchange(ptr, oldval, newval); \
  post_atomic_barrier(models);						\
  bool success = (retval == oldval);					\
  *oldvalp = retval;							\
  return success;							\
}									\
									\
type									\
__atomic_exchange_##size(volatile type* ptr, type val, int model)	\
{									\
  pre_atomic_barrier(model);						\
  type retval = arch_atomic_exchange(ptr, val);				\
  post_atomic_barrier(model);						\
  return retval;							\
}

__atomic_exchange_methods (int, 4)
__atomic_exchange_methods (long long, 8)

/* Subword methods require the same approach for both TILEPro and
   TILE-Gx.  We load the background data for the word, insert the
   desired subword piece, then compare-and-exchange it into place.  */
#define u8 unsigned char
#define u16 unsigned short

#define __atomic_subword_cmpxchg(type, size)				\
  									\
bool									\
__atomic_compare_exchange_##size(volatile type* ptr, type* guess,	\
				 type val, bool weak __unused, int models, \
				 int modelf __unused)			\
{									\
  pre_atomic_barrier(models);						\
  unsigned int *p = (unsigned int *)((unsigned long)ptr & ~3UL);	\
  const int shift = ((unsigned long)ptr & 3UL) * 8;			\
  const unsigned int valmask = (1 << (sizeof(type) * 8)) - 1;		\
  const unsigned int bgmask = ~(valmask << shift);			\
  unsigned int oldword = *p;						\
  type oldval = (oldword >> shift) & valmask;				\
  if (__builtin_expect((oldval == *guess), 1)) {			\
    unsigned int word = (oldword & bgmask) | ((val & valmask) << shift); \
    oldword = arch_atomic_val_compare_and_exchange(p, oldword, word);	\
    oldval = (oldword >> shift) & valmask;				\
  }									\
  post_atomic_barrier(models);						\
  bool success = (oldval == *guess);					\
  *guess = oldval;							\
  return success;							\
}

__atomic_subword_cmpxchg (u8, 1)
__atomic_subword_cmpxchg (u16, 2)

/* For the atomic-update subword methods, we use the same approach as
   above, but we retry until we succeed if the compare-and-exchange
   fails.  */
#define __atomic_subword(type, proto, top, expr, bottom)		\
proto									\
{									\
  top									\
  unsigned int *p = (unsigned int *)((unsigned long)ptr & ~3UL);	\
  const int shift = ((unsigned long)ptr & 3UL) * 8;			\
  const unsigned int valmask = (1 << (sizeof(type) * 8)) - 1;		\
  const unsigned int bgmask = ~(valmask << shift);			\
  unsigned int oldword, xword = *p;					\
  type val, oldval;							\
  do {									\
    oldword = xword;							\
    oldval = (oldword >> shift) & valmask;				\
    val = expr;								\
    unsigned int word = (oldword & bgmask) | ((val & valmask) << shift); \
    xword = arch_atomic_val_compare_and_exchange(p, oldword, word);	\
  } while (__builtin_expect(xword != oldword, 0));			\
  bottom								\
}

#define __atomic_subword_fetch(type, funcname, expr, retval)		\
  __atomic_subword(type,						\
		   type __atomic_ ## funcname(volatile type *ptr, type i, int model), \
		   pre_atomic_barrier(model);,				\
		   expr,						\
		   post_atomic_barrier(model); return retval;)

__atomic_subword_fetch (u8, fetch_add_1, oldval + i, oldval)
__atomic_subword_fetch (u8, fetch_sub_1, oldval - i, oldval)
__atomic_subword_fetch (u8, fetch_or_1, oldval | i, oldval)
__atomic_subword_fetch (u8, fetch_and_1, oldval & i, oldval)
__atomic_subword_fetch (u8, fetch_xor_1, oldval ^ i, oldval)
__atomic_subword_fetch (u8, fetch_nand_1, ~(oldval & i), oldval)

__atomic_subword_fetch (u16, fetch_add_2, oldval + i, oldval)
__atomic_subword_fetch (u16, fetch_sub_2, oldval - i, oldval)
__atomic_subword_fetch (u16, fetch_or_2, oldval | i, oldval)
__atomic_subword_fetch (u16, fetch_and_2, oldval & i, oldval)
__atomic_subword_fetch (u16, fetch_xor_2, oldval ^ i, oldval)
__atomic_subword_fetch (u16, fetch_nand_2, ~(oldval & i), oldval)

__atomic_subword_fetch (u8, add_fetch_1, oldval + i, val)
__atomic_subword_fetch (u8, sub_fetch_1, oldval - i, val)
__atomic_subword_fetch (u8, or_fetch_1, oldval | i, val)
__atomic_subword_fetch (u8, and_fetch_1, oldval & i, val)
__atomic_subword_fetch (u8, xor_fetch_1, oldval ^ i, val)
__atomic_subword_fetch (u8, nand_fetch_1, ~(oldval & i), val)

__atomic_subword_fetch (u16, add_fetch_2, oldval + i, val)
__atomic_subword_fetch (u16, sub_fetch_2, oldval - i, val)
__atomic_subword_fetch (u16, or_fetch_2, oldval | i, val)
__atomic_subword_fetch (u16, and_fetch_2, oldval & i, val)
__atomic_subword_fetch (u16, xor_fetch_2, oldval ^ i, val)
__atomic_subword_fetch (u16, nand_fetch_2, ~(oldval & i), val)

#define __atomic_subword_lock(type, size)				\
									\
__atomic_subword(type,							\
		 type __atomic_exchange_##size(volatile type* ptr, type nval, int model), \
	         pre_atomic_barrier(model);,				\
	         nval,							\
	         post_atomic_barrier(model); return oldval;)

__atomic_subword_lock (u8, 1)
__atomic_subword_lock (u16, 2)
