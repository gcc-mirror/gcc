/* Linux-specific atomic operations for PA Linux.
   Copyright (C) 2008-2018 Free Software Foundation, Inc.
   Based on code contributed by CodeSourcery for ARM EABI Linux.
   Modifications for PA Linux by Helge Deller <deller@gmx.de>

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

#define EFAULT  14 
#define EBUSY   16
#define ENOSYS 251 

/* PA-RISC 2.0 supports out-of-order execution for loads and stores.
   Thus, we need to synchonize memory accesses.  For more info, see:
   "Advanced Performance Features of the 64-bit PA-8000" by Doug Hunt.

   We implement byte, short and int versions of each atomic operation
   using the kernel helper defined below.  There is no support for
   64-bit operations yet.  */

/* Determine kernel LWS function call (0=32-bit, 1=64-bit userspace).  */
#define LWS_CAS (sizeof(long) == 4 ? 0 : 1)

/* Kernel helper for compare-and-exchange a 32-bit value.  */
static inline long
__kernel_cmpxchg (int *mem, int oldval, int newval)
{
  register unsigned long lws_mem asm("r26") = (unsigned long) (mem);
  register int lws_old asm("r25") = oldval;
  register int lws_new asm("r24") = newval;
  register long lws_ret   asm("r28");
  register long lws_errno asm("r21");
  asm volatile (	"ble	0xb0(%%sr2, %%r0)	\n\t"
			"ldi	%2, %%r20		\n\t"
	: "=r" (lws_ret), "=r" (lws_errno)
	: "i" (LWS_CAS), "r" (lws_mem), "r" (lws_old), "r" (lws_new)
	: "r1", "r20", "r22", "r23", "r29", "r31", "memory"
  );
  if (__builtin_expect (lws_errno == -EFAULT || lws_errno == -ENOSYS, 0))
    __builtin_trap ();

  /* If the kernel LWS call succeeded (lws_errno == 0), lws_ret contains
     the old value from memory.  If this value is equal to OLDVAL, the
     new value was written to memory.  If not, return -EBUSY.  */
  if (!lws_errno && lws_ret != oldval)
    lws_errno = -EBUSY;

  return lws_errno;
}

static inline long
__kernel_cmpxchg2 (void *mem, const void *oldval, const void *newval,
		   int val_size)
{
  register unsigned long lws_mem asm("r26") = (unsigned long) (mem);
  register unsigned long lws_old asm("r25") = (unsigned long) oldval;
  register unsigned long lws_new asm("r24") = (unsigned long) newval;
  register int lws_size asm("r23") = val_size;
  register long lws_ret   asm("r28");
  register long lws_errno asm("r21");
  asm volatile (	"ble	0xb0(%%sr2, %%r0)	\n\t"
			"ldi	%6, %%r20		\n\t"
	: "=r" (lws_ret), "=r" (lws_errno), "+r" (lws_mem),
	  "+r" (lws_old), "+r" (lws_new), "+r" (lws_size)
	: "i" (2)
	: "r1", "r20", "r22", "r29", "r31", "fr4", "memory"
  );

  /* If the kernel LWS call is successful, lws_ret contains 0.  */
  if (__builtin_expect (lws_ret == 0, 1))
    return 0;

  if (__builtin_expect (lws_errno == -EFAULT || lws_errno == -ENOSYS, 0))
    __builtin_trap ();

  /* If the kernel LWS call fails with no error, return -EBUSY */
  if (__builtin_expect (!lws_errno, 0))
    return -EBUSY;

  return lws_errno;
}
#define HIDDEN __attribute__ ((visibility ("hidden")))

/* Big endian masks  */
#define INVERT_MASK_1 24
#define INVERT_MASK_2 16

#define MASK_1 0xffu
#define MASK_2 0xffffu

#define FETCH_AND_OP_2(OP, PFX_OP, INF_OP, TYPE, WIDTH, INDEX)		\
  TYPE HIDDEN								\
  __sync_fetch_and_##OP##_##WIDTH (TYPE *ptr, TYPE val)			\
  {									\
    TYPE tmp, newval;							\
    long failure;							\
									\
    do {								\
      tmp = __atomic_load_n (ptr, __ATOMIC_RELAXED);			\
      newval = PFX_OP (tmp INF_OP val);					\
      failure = __kernel_cmpxchg2 (ptr, &tmp, &newval, INDEX);		\
    } while (failure != 0);						\
									\
    return tmp;								\
  }

FETCH_AND_OP_2 (add,   , +, long long, 8, 3)
FETCH_AND_OP_2 (sub,   , -, long long, 8, 3)
FETCH_AND_OP_2 (or,    , |, long long, 8, 3)
FETCH_AND_OP_2 (and,   , &, long long, 8, 3)
FETCH_AND_OP_2 (xor,   , ^, long long, 8, 3)
FETCH_AND_OP_2 (nand, ~, &, long long, 8, 3)

FETCH_AND_OP_2 (add,   , +, short, 2, 1)
FETCH_AND_OP_2 (sub,   , -, short, 2, 1)
FETCH_AND_OP_2 (or,    , |, short, 2, 1)
FETCH_AND_OP_2 (and,   , &, short, 2, 1)
FETCH_AND_OP_2 (xor,   , ^, short, 2, 1)
FETCH_AND_OP_2 (nand, ~, &, short, 2, 1)

FETCH_AND_OP_2 (add,   , +, signed char, 1, 0)
FETCH_AND_OP_2 (sub,   , -, signed char, 1, 0)
FETCH_AND_OP_2 (or,    , |, signed char, 1, 0)
FETCH_AND_OP_2 (and,   , &, signed char, 1, 0)
FETCH_AND_OP_2 (xor,   , ^, signed char, 1, 0)
FETCH_AND_OP_2 (nand, ~, &, signed char, 1, 0)

#define OP_AND_FETCH_2(OP, PFX_OP, INF_OP, TYPE, WIDTH, INDEX)		\
  TYPE HIDDEN								\
  __sync_##OP##_and_fetch_##WIDTH (TYPE *ptr, TYPE val)			\
  {									\
    TYPE tmp, newval;							\
    long failure;							\
									\
    do {								\
      tmp = __atomic_load_n (ptr, __ATOMIC_RELAXED);			\
      newval = PFX_OP (tmp INF_OP val);					\
      failure = __kernel_cmpxchg2 (ptr, &tmp, &newval, INDEX);		\
    } while (failure != 0);						\
									\
    return PFX_OP (tmp INF_OP val);					\
  }

OP_AND_FETCH_2 (add,   , +, long long, 8, 3)
OP_AND_FETCH_2 (sub,   , -, long long, 8, 3)
OP_AND_FETCH_2 (or,    , |, long long, 8, 3)
OP_AND_FETCH_2 (and,   , &, long long, 8, 3)
OP_AND_FETCH_2 (xor,   , ^, long long, 8, 3)
OP_AND_FETCH_2 (nand, ~, &, long long, 8, 3)

OP_AND_FETCH_2 (add,   , +, short, 2, 1)
OP_AND_FETCH_2 (sub,   , -, short, 2, 1)
OP_AND_FETCH_2 (or,    , |, short, 2, 1)
OP_AND_FETCH_2 (and,   , &, short, 2, 1)
OP_AND_FETCH_2 (xor,   , ^, short, 2, 1)
OP_AND_FETCH_2 (nand, ~, &, short, 2, 1)

OP_AND_FETCH_2 (add,   , +, signed char, 1, 0)
OP_AND_FETCH_2 (sub,   , -, signed char, 1, 0)
OP_AND_FETCH_2 (or,    , |, signed char, 1, 0)
OP_AND_FETCH_2 (and,   , &, signed char, 1, 0)
OP_AND_FETCH_2 (xor,   , ^, signed char, 1, 0)
OP_AND_FETCH_2 (nand, ~, &, signed char, 1, 0)

#define FETCH_AND_OP_WORD(OP, PFX_OP, INF_OP)				\
  int HIDDEN								\
  __sync_fetch_and_##OP##_4 (int *ptr, int val)				\
  {									\
    int tmp;								\
    long failure;							\
									\
    do {								\
      tmp = __atomic_load_n (ptr, __ATOMIC_RELAXED);			\
      failure = __kernel_cmpxchg (ptr, tmp, PFX_OP (tmp INF_OP val));	\
    } while (failure != 0);						\
									\
    return tmp;								\
  }

FETCH_AND_OP_WORD (add,   , +)
FETCH_AND_OP_WORD (sub,   , -)
FETCH_AND_OP_WORD (or,    , |)
FETCH_AND_OP_WORD (and,   , &)
FETCH_AND_OP_WORD (xor,   , ^)
FETCH_AND_OP_WORD (nand, ~, &)

#define OP_AND_FETCH_WORD(OP, PFX_OP, INF_OP)				\
  int HIDDEN								\
  __sync_##OP##_and_fetch_4 (int *ptr, int val)				\
  {									\
    int tmp;								\
    long failure;							\
									\
    do {								\
      tmp = __atomic_load_n (ptr, __ATOMIC_RELAXED);			\
      failure = __kernel_cmpxchg (ptr, tmp, PFX_OP (tmp INF_OP val));	\
    } while (failure != 0);						\
									\
    return PFX_OP (tmp INF_OP val);					\
  }

OP_AND_FETCH_WORD (add,   , +)
OP_AND_FETCH_WORD (sub,   , -)
OP_AND_FETCH_WORD (or,    , |)
OP_AND_FETCH_WORD (and,   , &)
OP_AND_FETCH_WORD (xor,   , ^)
OP_AND_FETCH_WORD (nand, ~, &)

typedef unsigned char bool;

#define COMPARE_AND_SWAP_2(TYPE, WIDTH, INDEX)				\
  TYPE HIDDEN								\
  __sync_val_compare_and_swap_##WIDTH (TYPE *ptr, TYPE oldval,		\
				       TYPE newval)			\
  {									\
    TYPE actual_oldval;							\
    long fail;								\
									\
    while (1)								\
      {									\
	actual_oldval = __atomic_load_n (ptr, __ATOMIC_RELAXED);	\
									\
	if (__builtin_expect (oldval != actual_oldval, 0))		\
	  return actual_oldval;						\
									\
	fail = __kernel_cmpxchg2 (ptr, &actual_oldval, &newval, INDEX);	\
									\
	if (__builtin_expect (!fail, 1))				\
	  return actual_oldval;						\
      }									\
  }									\
									\
  bool HIDDEN								\
  __sync_bool_compare_and_swap_##WIDTH (TYPE *ptr, TYPE oldval,		\
					TYPE newval)			\
  {									\
    long failure = __kernel_cmpxchg2 (ptr, &oldval, &newval, INDEX);	\
    return (failure == 0);						\
  }

COMPARE_AND_SWAP_2 (long long, 8, 3)
COMPARE_AND_SWAP_2 (short, 2, 1)
COMPARE_AND_SWAP_2 (char, 1, 0)

int HIDDEN
__sync_val_compare_and_swap_4 (int *ptr, int oldval, int newval)
{
  long fail;
  int actual_oldval;
    
  while (1)
    {
      actual_oldval = __atomic_load_n (ptr, __ATOMIC_RELAXED);

      if (__builtin_expect (oldval != actual_oldval, 0))
	return actual_oldval;

      fail = __kernel_cmpxchg (ptr, actual_oldval, newval);
  
      if (__builtin_expect (!fail, 1))
	return actual_oldval;
    }
}

bool HIDDEN
__sync_bool_compare_and_swap_4 (int *ptr, int oldval, int newval)
{
  long failure = __kernel_cmpxchg (ptr, oldval, newval);
  return (failure == 0);
}

#define SYNC_LOCK_TEST_AND_SET_2(TYPE, WIDTH, INDEX)			\
TYPE HIDDEN								\
  __sync_lock_test_and_set_##WIDTH (TYPE *ptr, TYPE val)		\
  {									\
    TYPE oldval;							\
    long failure;							\
									\
    do {								\
      oldval = __atomic_load_n (ptr, __ATOMIC_RELAXED);			\
      failure = __kernel_cmpxchg2 (ptr, &oldval, &val, INDEX);		\
    } while (failure != 0);						\
									\
    return oldval;							\
  }

SYNC_LOCK_TEST_AND_SET_2 (long long, 8, 3)
SYNC_LOCK_TEST_AND_SET_2 (short, 2, 1)
SYNC_LOCK_TEST_AND_SET_2 (signed char, 1, 0)

int HIDDEN
__sync_lock_test_and_set_4 (int *ptr, int val)
{
  long failure;
  int oldval;

  do {
    oldval = __atomic_load_n (ptr, __ATOMIC_RELAXED);
    failure = __kernel_cmpxchg (ptr, oldval, val);
  } while (failure != 0);

  return oldval;
}

void HIDDEN
__sync_lock_release_8 (long long *ptr)
{
  /* All accesses must be complete before we release the lock.  */
  __sync_synchronize ();
  *(double *)ptr = 0;
}

#define SYNC_LOCK_RELEASE_1(TYPE, WIDTH)			\
  void HIDDEN							\
  __sync_lock_release_##WIDTH (TYPE *ptr)			\
  {								\
    /* All accesses must be complete before we release		\
       the lock.  */						\
    __sync_synchronize ();					\
    *ptr = 0;							\
  }

SYNC_LOCK_RELEASE_1 (int, 4)
SYNC_LOCK_RELEASE_1 (short, 2)
SYNC_LOCK_RELEASE_1 (signed char, 1)
