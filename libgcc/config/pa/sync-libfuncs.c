/* PA-RISC sync libfunc support.
   Copyright (C) 2008-2023 Free Software Foundation, Inc.
   Based on code contributed by CodeSourcery for ARM EABI Linux.
   Modifications for PA Linux by Helge Deller <deller@gmx.de>
   Revised for general use by John David Anglin <danglin@gcc.gnu.org>

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

typedef unsigned char u8;
typedef short unsigned int u16;
typedef unsigned int u32;
#ifdef __LP64__
typedef long unsigned int u64;
#else
typedef long long unsigned int u64;
#endif

/* PA-RISC 2.0 supports out-of-order execution for loads and stores.
   Thus, we need to synchonize memory accesses.  For more info, see:
   "Advanced Performance Features of the 64-bit PA-8000" by Doug Hunt.  */

typedef volatile int __attribute__((aligned (16))) ldcw_t;
static ldcw_t __atomicity_lock = 1;

/* We want default visibility for the sync routines.  */
#undef VISIBILITY
#if defined(__hpux__) && !defined(__LP64__)
#define VISIBILITY
#else
#define VISIBILITY __attribute__ ((visibility ("default")))
#endif

/* Perform ldcw operation in cache when possible.  The ldcw instruction
   is a full barrier.  */
#ifndef _PA_LDCW_INSN
# ifdef _PA_RISC2_0
# define _PA_LDCW_INSN "ldcw,co"
# else
# define _PA_LDCW_INSN "ldcw"
# endif
#endif

static inline void
__sync_spin_lock (void)
{
  ldcw_t *lock = &__atomicity_lock;
  int tmp;

  __asm__ __volatile__ (_PA_LDCW_INSN " 0(%1),%0\n\t"
			"cmpib,<>,n 0,%0,.+20\n\t"
			"ldw,ma 0(%1),%0\n\t"
			"cmpib,<> 0,%0,.-12\n\t"
			"nop\n\t"
			"b,n .-12"
			: "=&r" (tmp)
			: "r" (lock)
			: "memory");
}

static inline void
__sync_spin_unlock (void)
{
  ldcw_t *lock = &__atomicity_lock;
  int tmp = 1;

  /* Use ordered store for release.  */
  __asm__ __volatile__ ("stw,ma %1,0(%0)"
			: : "r" (lock), "r" (tmp) : "memory");
}

/* Load value with an atomic processor load if possible.  */
#define ATOMIC_LOAD(TYPE, WIDTH)					\
  static inline TYPE							\
  atomic_load_##WIDTH (volatile void *ptr)				\
  {									\
    return *(volatile TYPE *)ptr;					\
  }

#if defined(__LP64__) || defined(__SOFTFP__)
ATOMIC_LOAD (u64, 8)
#else
static inline u64
atomic_load_8 (volatile void *ptr)
{
  u64 result;
  double tmp;

  asm volatile ("{fldds|fldd} 0(%2),%1\n\t"
		"{fstds|fstd} %1,-16(%%sp)\n\t"
		"{ldws|ldw} -16(%%sp),%0\n\t"
		"{ldws|ldw} -12(%%sp),%R0"
		: "=r" (result), "=f" (tmp) : "r" (ptr): "memory");
  return result;
}
#endif

ATOMIC_LOAD (u32, 4)
ATOMIC_LOAD (u16, 2)
ATOMIC_LOAD (u8, 1)

/* Store value with an atomic processor store if possible.  */
#define ATOMIC_STORE(TYPE, WIDTH)					\
  static inline void							\
  atomic_store_##WIDTH (volatile void *ptr, TYPE value)			\
  {									\
    *(volatile TYPE *)ptr = value;					\
  }

#if defined(__LP64__) || defined(__SOFTFP__)
ATOMIC_STORE (u64, 8)
#else
static inline void
atomic_store_8 (volatile void *ptr, u64 value)
{
  double tmp;

  asm volatile ("stws|stw} %2,-16(%%sp)\n\t"
		"{stws|stw} %R2,-12(%%sp)\n\t"
		"{fldds|fldd} -16(%%sp),%1\n\t"
		"{fstds|fstd} %1,0(%0)"
		: "=m" (ptr), "=&f" (tmp) : "r" (value): "memory");
}
#endif

ATOMIC_STORE (u32, 4)
ATOMIC_STORE (u16, 2)
ATOMIC_STORE (u8, 1)

#define FETCH_AND_OP(OP, PFX_OP, INF_OP, TYPE, WIDTH)			\
  TYPE VISIBILITY							\
  __sync_fetch_and_##OP##_##WIDTH (volatile void *ptr, TYPE val)	\
  {									\
    TYPE tmp, newval;							\
									\
    __sync_spin_lock();							\
    tmp = atomic_load_##WIDTH (ptr);					\
    newval = PFX_OP (tmp INF_OP val);					\
    atomic_store_##WIDTH (ptr, newval);					\
    __sync_spin_unlock();						\
									\
    return tmp;								\
  }

FETCH_AND_OP (add,   , +, u64, 8)
FETCH_AND_OP (sub,   , -, u64, 8)
FETCH_AND_OP (or,    , |, u64, 8)
FETCH_AND_OP (and,   , &, u64, 8)
FETCH_AND_OP (xor,   , ^, u64, 8)
FETCH_AND_OP (nand, ~, &, u64, 8)

FETCH_AND_OP (add,   , +, u32, 4)
FETCH_AND_OP (sub,   , -, u32, 4)
FETCH_AND_OP (or,    , |, u32, 4)
FETCH_AND_OP (and,   , &, u32, 4)
FETCH_AND_OP (xor,   , ^, u32, 4)
FETCH_AND_OP (nand, ~, &, u32, 4)

FETCH_AND_OP (add,   , +, u16, 2)
FETCH_AND_OP (sub,   , -, u16, 2)
FETCH_AND_OP (or,    , |, u16, 2)
FETCH_AND_OP (and,   , &, u16, 2)
FETCH_AND_OP (xor,   , ^, u16, 2)
FETCH_AND_OP (nand, ~, &, u16, 2)

FETCH_AND_OP (add,   , +, u8, 1)
FETCH_AND_OP (sub,   , -, u8, 1)
FETCH_AND_OP (or,    , |, u8, 1)
FETCH_AND_OP (and,   , &, u8, 1)
FETCH_AND_OP (xor,   , ^, u8, 1)
FETCH_AND_OP (nand, ~, &, u8, 1)

#define OP_AND_FETCH(OP, PFX_OP, INF_OP, TYPE, WIDTH)			\
  TYPE VISIBILITY 							\
  __sync_##OP##_and_fetch_##WIDTH (volatile void *ptr, TYPE val)	\
  {									\
    TYPE tmp, newval;							\
									\
    __sync_spin_lock();							\
    tmp = atomic_load_##WIDTH (ptr);					\
    newval = PFX_OP (tmp INF_OP val);					\
    atomic_store_##WIDTH (ptr, newval);					\
    __sync_spin_unlock();						\
									\
    return newval;							\
  }

OP_AND_FETCH (add,   , +, u64, 8)
OP_AND_FETCH (sub,   , -, u64, 8)
OP_AND_FETCH (or,    , |, u64, 8)
OP_AND_FETCH (and,   , &, u64, 8)
OP_AND_FETCH (xor,   , ^, u64, 8)
OP_AND_FETCH (nand, ~, &, u64, 8)

OP_AND_FETCH (add,   , +, u32, 4)
OP_AND_FETCH (sub,   , -, u32, 4)
OP_AND_FETCH (or,    , |, u32, 4)
OP_AND_FETCH (and,   , &, u32, 4)
OP_AND_FETCH (xor,   , ^, u32, 4)
OP_AND_FETCH (nand, ~, &, u32, 4)

OP_AND_FETCH (add,   , +, u16, 2)
OP_AND_FETCH (sub,   , -, u16, 2)
OP_AND_FETCH (or,    , |, u16, 2)
OP_AND_FETCH (and,   , &, u16, 2)
OP_AND_FETCH (xor,   , ^, u16, 2)
OP_AND_FETCH (nand, ~, &, u16, 2)

OP_AND_FETCH (add,   , +, u8, 1)
OP_AND_FETCH (sub,   , -, u8, 1)
OP_AND_FETCH (or,    , |, u8, 1)
OP_AND_FETCH (and,   , &, u8, 1)
OP_AND_FETCH (xor,   , ^, u8, 1)
OP_AND_FETCH (nand, ~, &, u8, 1)

#define COMPARE_AND_SWAP(TYPE, WIDTH)					\
  TYPE VISIBILITY 							\
  __sync_val_compare_and_swap_##WIDTH (volatile void *ptr, TYPE oldval,	\
				       TYPE newval)			\
  {									\
    TYPE actual_oldval;							\
									\
    __sync_spin_lock();							\
    actual_oldval = atomic_load_##WIDTH (ptr);				\
    if (actual_oldval == oldval)					\
      atomic_store_##WIDTH (ptr, newval);				\
    __sync_spin_unlock();						\
									\
    return actual_oldval;						\
  }									\
									\
  _Bool VISIBILITY							\
  __sync_bool_compare_and_swap_##WIDTH (volatile void *ptr,		\
					TYPE oldval, TYPE newval)	\
  {									\
    TYPE actual_oldval;							\
    _Bool result;							\
									\
    __sync_spin_lock();							\
    actual_oldval = atomic_load_##WIDTH (ptr);				\
    result = (actual_oldval == oldval);					\
    if (result)								\
      atomic_store_##WIDTH (ptr, newval);				\
    __sync_spin_unlock();						\
									\
    return result;							\
  }

COMPARE_AND_SWAP (u64, 8)
COMPARE_AND_SWAP (u32, 4)
COMPARE_AND_SWAP (u16, 2)
COMPARE_AND_SWAP (u8, 1)

#define SYNC_LOCK_TEST_AND_SET(TYPE, WIDTH)				\
TYPE VISIBILITY 							\
  __sync_lock_test_and_set_##WIDTH (volatile void *ptr, TYPE val)	\
  {									\
    TYPE oldval;							\
									\
    __sync_spin_lock();							\
    oldval = atomic_load_##WIDTH (ptr);					\
    atomic_store_##WIDTH (ptr, val);					\
    __sync_spin_unlock();						\
									\
    return oldval;							\
  }

SYNC_LOCK_TEST_AND_SET (u64, 8)
SYNC_LOCK_TEST_AND_SET (u32, 4)
SYNC_LOCK_TEST_AND_SET (u16, 2)
SYNC_LOCK_TEST_AND_SET (u8, 1)

#define SYNC_LOCK_RELEASE(TYPE, WIDTH)				\
  void VISIBILITY						\
  __sync_lock_release_##WIDTH (volatile void *ptr)		\
  {								\
    TYPE val = 0;						\
								\
    __sync_spin_lock();						\
    atomic_store_##WIDTH (ptr, val);				\
    __sync_spin_unlock();					\
  }

SYNC_LOCK_RELEASE (u64, 8)
SYNC_LOCK_RELEASE (u32, 4)
SYNC_LOCK_RELEASE (u16, 2)
SYNC_LOCK_RELEASE (u8, 1)

#define SYNC_LOCK_LOAD(TYPE, WIDTH)					\
TYPE VISIBILITY __sync_lock_load_##WIDTH (volatile void *); 		\
TYPE VISIBILITY 							\
  __sync_lock_load_##WIDTH (volatile void *ptr)				\
  {									\
    TYPE oldval;							\
									\
    __sync_spin_lock();							\
    oldval = atomic_load_##WIDTH (ptr);					\
    __sync_spin_unlock();						\
									\
    return oldval;							\
  }

SYNC_LOCK_LOAD (u64, 8)
SYNC_LOCK_LOAD (u32, 4)
SYNC_LOCK_LOAD (u16, 2)
SYNC_LOCK_LOAD (u8, 1)
