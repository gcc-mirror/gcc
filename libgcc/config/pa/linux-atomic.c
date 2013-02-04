/* Linux-specific atomic operations for PA Linux.
   Copyright (C) 2008-2013 Free Software Foundation, Inc.
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

/* All PA-RISC implementations supported by linux have strongly
   ordered loads and stores.  Only cache flushes and purges can be
   delayed.  The data cache implementations are all globally
   coherent.  Thus, there is no need to synchonize memory accesses.

   GCC automatically issues a asm memory barrier when it encounters
   a __sync_synchronize builtin.  Thus, we do not need to define this
   builtin.

   We implement byte, short and int versions of each atomic operation
   using the kernel helper defined below.  There is no support for
   64-bit operations yet.  */

/* A privileged instruction to crash a userspace program with SIGILL.  */
#define ABORT_INSTRUCTION asm ("iitlbp %r0,(%sr0, %r0)")

/* Determine kernel LWS function call (0=32-bit, 1=64-bit userspace).  */
#define LWS_CAS (sizeof(unsigned long) == 4 ? 0 : 1)

/* Kernel helper for compare-and-exchange a 32-bit value.  */
static inline long
__kernel_cmpxchg (int oldval, int newval, int *mem)
{
  register unsigned long lws_mem asm("r26") = (unsigned long) (mem);
  register long lws_ret   asm("r28");
  register long lws_errno asm("r21");
  register int lws_old asm("r25") = oldval;
  register int lws_new asm("r24") = newval;
  asm volatile (	"ble	0xb0(%%sr2, %%r0)	\n\t"
			"ldi	%5, %%r20		\n\t"
	: "=r" (lws_ret), "=r" (lws_errno), "=r" (lws_mem),
	  "=r" (lws_old), "=r" (lws_new)
	: "i" (LWS_CAS), "2" (lws_mem), "3" (lws_old), "4" (lws_new)
	: "r1", "r20", "r22", "r23", "r29", "r31", "memory"
  );
  if (__builtin_expect (lws_errno == -EFAULT || lws_errno == -ENOSYS, 0))
    ABORT_INSTRUCTION;

  /* If the kernel LWS call succeeded (lws_errno == 0), lws_ret contains
     the old value from memory.  If this value is equal to OLDVAL, the
     new value was written to memory.  If not, return -EBUSY.  */
  if (!lws_errno && lws_ret != oldval)
    lws_errno = -EBUSY;

  return lws_errno;
}

#define HIDDEN __attribute__ ((visibility ("hidden")))

/* Big endian masks  */
#define INVERT_MASK_1 24
#define INVERT_MASK_2 16

#define MASK_1 0xffu
#define MASK_2 0xffffu

#define FETCH_AND_OP_WORD(OP, PFX_OP, INF_OP)				\
  int HIDDEN								\
  __sync_fetch_and_##OP##_4 (int *ptr, int val)				\
  {									\
    int failure, tmp;							\
									\
    do {								\
      tmp = *ptr;							\
      failure = __kernel_cmpxchg (tmp, PFX_OP (tmp INF_OP val), ptr);	\
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

#define NAME_oldval(OP, WIDTH) __sync_fetch_and_##OP##_##WIDTH
#define NAME_newval(OP, WIDTH) __sync_##OP##_and_fetch_##WIDTH

/* Implement both __sync_<op>_and_fetch and __sync_fetch_and_<op> for
   subword-sized quantities.  */

#define SUBWORD_SYNC_OP(OP, PFX_OP, INF_OP, TYPE, WIDTH, RETURN)	\
  TYPE HIDDEN								\
  NAME##_##RETURN (OP, WIDTH) (TYPE *ptr, TYPE val)			\
  {									\
    int *wordptr = (int *) ((unsigned long) ptr & ~3);			\
    unsigned int mask, shift, oldval, newval;				\
    int failure;							\
									\
    shift = (((unsigned long) ptr & 3) << 3) ^ INVERT_MASK_##WIDTH;	\
    mask = MASK_##WIDTH << shift;					\
									\
    do {								\
      oldval = *wordptr;						\
      newval = ((PFX_OP (((oldval & mask) >> shift)			\
                         INF_OP (unsigned int) val)) << shift) & mask;	\
      newval |= oldval & ~mask;						\
      failure = __kernel_cmpxchg (oldval, newval, wordptr);		\
    } while (failure != 0);						\
									\
    return (RETURN & mask) >> shift;					\
  }

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

#define OP_AND_FETCH_WORD(OP, PFX_OP, INF_OP)				\
  int HIDDEN								\
  __sync_##OP##_and_fetch_4 (int *ptr, int val)				\
  {									\
    int tmp, failure;							\
									\
    do {								\
      tmp = *ptr;							\
      failure = __kernel_cmpxchg (tmp, PFX_OP (tmp INF_OP val), ptr);	\
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

int HIDDEN
__sync_val_compare_and_swap_4 (int *ptr, int oldval, int newval)
{
  int actual_oldval, fail;
    
  while (1)
    {
      actual_oldval = *ptr;

      if (__builtin_expect (oldval != actual_oldval, 0))
	return actual_oldval;

      fail = __kernel_cmpxchg (actual_oldval, newval, ptr);
  
      if (__builtin_expect (!fail, 1))
	return actual_oldval;
    }
}

#define SUBWORD_VAL_CAS(TYPE, WIDTH)					\
  TYPE HIDDEN								\
  __sync_val_compare_and_swap_##WIDTH (TYPE *ptr, TYPE oldval,		\
				       TYPE newval)			\
  {									\
    int *wordptr = (int *)((unsigned long) ptr & ~3), fail;		\
    unsigned int mask, shift, actual_oldval, actual_newval;		\
									\
    shift = (((unsigned long) ptr & 3) << 3) ^ INVERT_MASK_##WIDTH;	\
    mask = MASK_##WIDTH << shift;					\
									\
    while (1)								\
      {									\
	actual_oldval = *wordptr;					\
									\
	if (__builtin_expect (((actual_oldval & mask) >> shift)		\
			      != (unsigned int) oldval, 0))		\
	  return (actual_oldval & mask) >> shift;			\
									\
	actual_newval = (actual_oldval & ~mask)				\
			| (((unsigned int) newval << shift) & mask);	\
									\
	fail = __kernel_cmpxchg (actual_oldval, actual_newval,		\
				 wordptr);				\
									\
	if (__builtin_expect (!fail, 1))				\
	  return (actual_oldval & mask) >> shift;			\
      }									\
  }

SUBWORD_VAL_CAS (unsigned short, 2)
SUBWORD_VAL_CAS (unsigned char,  1)

typedef unsigned char bool;

bool HIDDEN
__sync_bool_compare_and_swap_4 (int *ptr, int oldval, int newval)
{
  int failure = __kernel_cmpxchg (oldval, newval, ptr);
  return (failure == 0);
}

#define SUBWORD_BOOL_CAS(TYPE, WIDTH)					\
  bool HIDDEN								\
  __sync_bool_compare_and_swap_##WIDTH (TYPE *ptr, TYPE oldval,		\
					TYPE newval)			\
  {									\
    TYPE actual_oldval							\
      = __sync_val_compare_and_swap_##WIDTH (ptr, oldval, newval);	\
    return (oldval == actual_oldval);					\
  }

SUBWORD_BOOL_CAS (unsigned short, 2)
SUBWORD_BOOL_CAS (unsigned char,  1)

int HIDDEN
__sync_lock_test_and_set_4 (int *ptr, int val)
{
  int failure, oldval;

  do {
    oldval = *ptr;
    failure = __kernel_cmpxchg (oldval, val, ptr);
  } while (failure != 0);

  return oldval;
}

#define SUBWORD_TEST_AND_SET(TYPE, WIDTH)				\
  TYPE HIDDEN								\
  __sync_lock_test_and_set_##WIDTH (TYPE *ptr, TYPE val)		\
  {									\
    int failure;							\
    unsigned int oldval, newval, shift, mask;				\
    int *wordptr = (int *) ((unsigned long) ptr & ~3);			\
									\
    shift = (((unsigned long) ptr & 3) << 3) ^ INVERT_MASK_##WIDTH;	\
    mask = MASK_##WIDTH << shift;					\
									\
    do {								\
      oldval = *wordptr;						\
      newval = (oldval & ~mask)						\
	       | (((unsigned int) val << shift) & mask);		\
      failure = __kernel_cmpxchg (oldval, newval, wordptr);		\
    } while (failure != 0);						\
									\
    return (oldval & mask) >> shift;					\
  }

SUBWORD_TEST_AND_SET (unsigned short, 2)
SUBWORD_TEST_AND_SET (unsigned char,  1)

#define SYNC_LOCK_RELEASE(TYPE, WIDTH)					\
  void HIDDEN								\
  __sync_lock_release_##WIDTH (TYPE *ptr)				\
  {									\
    *ptr = 0;								\
  }

SYNC_LOCK_RELEASE (int,   4)
SYNC_LOCK_RELEASE (short, 2)
SYNC_LOCK_RELEASE (char,  1)
