/* TILE atomics.
   Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

#include "tconfig.h"
#include "coretypes.h"
#include "atomic.h"

#define bool unsigned char

/* This code should be inlined by the compiler, but for now support
   it as out-of-line methods in libgcc.  */

static inline void
pre_atomic_barrier (int model)
{
  switch (model)
    {
    case __ATOMIC_RELEASE:
    case __ATOMIC_ACQ_REL:
    case __ATOMIC_SEQ_CST:
      __atomic_thread_fence (model);
      break;
    default:
      break;
    }
  return;
}

static inline void
post_atomic_barrier (int model)
{
  switch (model)
    {
    case __ATOMIC_ACQUIRE:
    case __ATOMIC_ACQ_REL:
    case __ATOMIC_SEQ_CST:
      __atomic_thread_fence (model);
      break;
    default:
      break;
    }
  return;
}

#define __unused __attribute__((unused))

#define __fetch_and_do(proto, type, size, opname, top, bottom)	\
proto								\
{								\
  top;								\
  type rv = arch_atomic_##opname(p, i);				\
  bottom;							\
  return rv;							\
}

#define __atomic_fetch_and_do(type, size, opname)			\
  __fetch_and_do(type __atomic_fetch_##opname##_##size(type* p, type i, int model), \
		 type, size, opname,					\
		 pre_atomic_barrier(model),				\
		 post_atomic_barrier(model))				\

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

#define __sync_fetch_and_do(type, size, opname)				\
  __fetch_and_do(type __sync_fetch_and_##opname##_##size(type* p, type i), \
		 type, size, opname,					\
		 arch_atomic_write_barrier(),				\
		 arch_atomic_read_barrier())

__sync_fetch_and_do (int, 4, add)
__sync_fetch_and_do (int, 4, sub)
__sync_fetch_and_do (int, 4, or)
__sync_fetch_and_do (int, 4, and)
__sync_fetch_and_do (int, 4, xor)
__sync_fetch_and_do (int, 4, nand)
__sync_fetch_and_do (long long, 8, add)
__sync_fetch_and_do (long long, 8, sub)
__sync_fetch_and_do (long long, 8, or)
__sync_fetch_and_do (long long, 8, and)
__sync_fetch_and_do (long long, 8, xor)
__sync_fetch_and_do (long long, 8, nand)

#define __do_and_fetch(proto, type, size, opname, op, op2, top, bottom)	\
proto									\
{									\
  top;									\
  type rv = op2 (arch_atomic_##opname(p, i) op i);			\
  bottom;								\
  return rv;								\
}

#define __atomic_do_and_fetch(type, size, opname, op, op2)		\
  __do_and_fetch(type __atomic_##opname##_fetch_##size(type* p, type i, int model), \
		 type, size, opname, op, op2,				\
		 pre_atomic_barrier(model),				\
		 post_atomic_barrier(model))				\

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

#define __sync_do_and_fetch(type, size, opname, op, op2)		\
  __do_and_fetch(type __sync_##opname##_and_fetch_##size(type* p, type i), \
		 type, size, opname, op, op2,				\
		 arch_atomic_write_barrier(),				\
		 arch_atomic_read_barrier())				\

__sync_do_and_fetch (int, 4, add, +, )
__sync_do_and_fetch (int, 4, sub, -, )
__sync_do_and_fetch (int, 4, or, |, )
__sync_do_and_fetch (int, 4, and, &, )
__sync_do_and_fetch (int, 4, xor, |, )
__sync_do_and_fetch (int, 4, nand, &, ~)
__sync_do_and_fetch (long long, 8, add, +, )
__sync_do_and_fetch (long long, 8, sub, -, )
__sync_do_and_fetch (long long, 8, or, |, )
__sync_do_and_fetch (long long, 8, and, &, )
__sync_do_and_fetch (long long, 8, xor, |, )
__sync_do_and_fetch (long long, 8, nand, &, ~)

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

#define __sync_exchange_methods(type, size)				\
type									\
__sync_val_compare_and_swap_##size(type* ptr, type oldval, type newval)	\
{									\
  arch_atomic_write_barrier();						\
  type retval = arch_atomic_val_compare_and_exchange(ptr, oldval, newval); \
  arch_atomic_read_barrier();						\
  return retval;							\
}									\
									\
bool									\
__sync_bool_compare_and_swap_##size(type* ptr, type oldval, type newval) \
{									\
  arch_atomic_write_barrier();						\
  bool retval = arch_atomic_bool_compare_and_exchange(ptr, oldval, newval); \
  arch_atomic_read_barrier();						\
  return retval;							\
}									\
									\
type									\
__sync_lock_test_and_set_##size(type* ptr, type val)			\
{									\
  type retval = arch_atomic_exchange(ptr, val);				\
  arch_atomic_acquire_barrier_value(retval);				\
  return retval;							\
}

__sync_exchange_methods (int, 4)
__sync_exchange_methods (long long, 8)

#ifdef __LITTLE_ENDIAN__
#define BIT_OFFSET(n, type) ((n) * 8)
#else
#define BIT_OFFSET(n, type) ((4 - sizeof(type) - (n)) * 8)
#endif

/* Subword methods require the same approach for both TILEPro and
   TILE-Gx.  We load the background data for the word, insert the
   desired subword piece, then compare-and-exchange it into place.  */
#define u8 unsigned char
#define u16 unsigned short

#define __subword_cmpxchg_body(type, size, ptr, guess, val)		\
  ({									\
    unsigned int *p = (unsigned int *)((unsigned long)ptr & ~3UL);	\
    const int shift = BIT_OFFSET((unsigned long)ptr & 3UL, type);	\
    const unsigned int valmask = (1 << (sizeof(type) * 8)) - 1;		\
    const unsigned int bgmask = ~(valmask << shift);			\
    unsigned int oldword = *p;						\
    type oldval = (oldword >> shift) & valmask;				\
    if (__builtin_expect((oldval == guess), 1)) {			\
      unsigned int word = (oldword & bgmask) | ((val & valmask) << shift); \
      oldword = arch_atomic_val_compare_and_exchange(p, oldword, word);	\
      oldval = (oldword >> shift) & valmask;				\
    }									\
    oldval;								\
  })									\

#define __atomic_subword_cmpxchg(type, size)				\
  									\
bool									\
__atomic_compare_exchange_##size(volatile type* ptr, type* guess_ptr,	\
				 type val, bool weak __unused, int models, \
				 int modelf __unused)			\
{									\
  pre_atomic_barrier(models);						\
  type guess = *guess_ptr;						\
  type oldval = __subword_cmpxchg_body(type, size, ptr, guess, val);	\
  post_atomic_barrier(models);						\
  bool success = (oldval == guess);					\
  *guess_ptr = oldval;							\
  return success;							\
}

__atomic_subword_cmpxchg (u8, 1)
__atomic_subword_cmpxchg (u16, 2)

#define __sync_subword_cmpxchg(type, size)				\
  									\
type									\
__sync_val_compare_and_swap_##size(type* ptr, type guess, type val)	\
{									\
  arch_atomic_write_barrier();						\
  type oldval = __subword_cmpxchg_body(type, size, ptr, guess, val);	\
  arch_atomic_read_barrier();						\
  return oldval;							\
}									\
									\
bool									\
__sync_bool_compare_and_swap_##size(type* ptr, type guess, type val)	\
{									\
  type oldval = __sync_val_compare_and_swap_##size(ptr, guess, val);	\
  return oldval == guess;						\
}

__sync_subword_cmpxchg (u8, 1)
__sync_subword_cmpxchg (u16, 2)

/* For the atomic-update subword methods, we use the same approach as
   above, but we retry until we succeed if the compare-and-exchange
   fails.  */
#define __subword(type, proto, top, expr, bottom)			\
proto									\
{									\
  top									\
  unsigned int *p = (unsigned int *)((unsigned long)ptr & ~3UL);	\
  const int shift = BIT_OFFSET((unsigned long)ptr & 3UL, type);		\
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
  __subword(type,							\
	    type __atomic_ ## funcname(volatile type *ptr, type i, int model), \
	    pre_atomic_barrier(model);,					\
	    expr,							\
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

#define __sync_subword_fetch(type, funcname, expr, retval)	\
  __subword(type,						\
	    type __sync_ ## funcname(type *ptr, type i),	\
	    arch_atomic_read_barrier();,			\
	    expr,						\
	    arch_atomic_write_barrier(); return retval;)

__sync_subword_fetch (u8, fetch_and_add_1, oldval + i, oldval)
__sync_subword_fetch (u8, fetch_and_sub_1, oldval - i, oldval)
__sync_subword_fetch (u8, fetch_and_or_1, oldval | i, oldval)
__sync_subword_fetch (u8, fetch_and_and_1, oldval & i, oldval)
__sync_subword_fetch (u8, fetch_and_xor_1, oldval ^ i, oldval)
__sync_subword_fetch (u8, fetch_and_nand_1, ~(oldval & i), oldval)

__sync_subword_fetch (u16, fetch_and_add_2, oldval + i, oldval)
__sync_subword_fetch (u16, fetch_and_sub_2, oldval - i, oldval)
__sync_subword_fetch (u16, fetch_and_or_2, oldval | i, oldval)
__sync_subword_fetch (u16, fetch_and_and_2, oldval & i, oldval)
__sync_subword_fetch (u16, fetch_and_xor_2, oldval ^ i, oldval)
__sync_subword_fetch (u16, fetch_and_nand_2, ~(oldval & i), oldval)

__sync_subword_fetch (u8, add_and_fetch_1, oldval + i, val)
__sync_subword_fetch (u8, sub_and_fetch_1, oldval - i, val)
__sync_subword_fetch (u8, or_and_fetch_1, oldval | i, val)
__sync_subword_fetch (u8, and_and_fetch_1, oldval & i, val)
__sync_subword_fetch (u8, xor_and_fetch_1, oldval ^ i, val)
__sync_subword_fetch (u8, nand_and_fetch_1, ~(oldval & i), val)

__sync_subword_fetch (u16, add_and_fetch_2, oldval + i, val)
__sync_subword_fetch (u16, sub_and_fetch_2, oldval - i, val)
__sync_subword_fetch (u16, or_and_fetch_2, oldval | i, val)
__sync_subword_fetch (u16, and_and_fetch_2, oldval & i, val)
__sync_subword_fetch (u16, xor_and_fetch_2, oldval ^ i, val)
__sync_subword_fetch (u16, nand_and_fetch_2, ~(oldval & i), val)

#define __atomic_subword_lock(type, size)				\
  __subword(type,							\
	    type __atomic_exchange_##size(volatile type* ptr, type nval, int model), \
	    pre_atomic_barrier(model);,					\
	    nval,							\
	    post_atomic_barrier(model); return oldval;)

__atomic_subword_lock (u8, 1)
__atomic_subword_lock (u16, 2)

#define __sync_subword_lock(type, size)					\
  __subword(type,							\
	    type __sync_lock_test_and_set_##size(type* ptr, type nval), \
	    ,								\
	    nval,							\
	    arch_atomic_acquire_barrier_value(oldval); return oldval;)

__sync_subword_lock (u8, 1)
__sync_subword_lock (u16, 2)
