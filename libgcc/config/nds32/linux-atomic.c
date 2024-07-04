/* Linux-specific atomic operations for NDS32 Linux.
   Copyright (C) 2012-2024 Free Software Foundation, Inc.

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

/* We implement byte, short and int versions of each atomic operation
   using the kernel helper defined below.  There is no support for
   64-bit operations yet.  */

/* This function copy form NDS32 Linux-kernal. */
static inline int
__kernel_cmpxchg (int oldval, int newval, int *mem)
{
  int temp1, temp2, temp3, offset;

  asm volatile ("msync\tall\n"
		"movi\t%0, #0\n"
		"1:\n"
		"\tllw\t%1, [%4+%0]\n"
		"\tsub\t%3, %1, %6\n"
		"\tcmovz\t%2, %5, %3\n"
		"\tcmovn\t%2, %1, %3\n"
		"\tscw\t%2, [%4+%0]\n"
		"\tbeqz\t%2, 1b\n"
		: "=&r" (offset), "=&r" (temp3), "=&r" (temp2), "=&r" (temp1)
		: "r" (mem), "r" (newval), "r" (oldval) : "memory");

  return temp1;
}

#define HIDDEN __attribute__ ((visibility ("hidden")))

#ifdef __NDS32_EL__
#define INVERT_MASK_1 0
#define INVERT_MASK_2 0
#else
#define INVERT_MASK_1 24
#define INVERT_MASK_2 16
#endif

#define MASK_1 0xffu
#define MASK_2 0xffffu

#define FETCH_AND_OP_WORD(OP, PFX_OP, INF_OP)				\
  int HIDDEN								\
  __sync_fetch_and_##OP##_4 (int *ptr, int val)				\
  {									\
    int failure, tmp;							\
									\
    do {								\
      tmp = __atomic_load_n (ptr, __ATOMIC_SEQ_CST);			\
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
      oldval = __atomic_load_n (wordptr, __ATOMIC_SEQ_CST);		\
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
      tmp = __atomic_load_n (ptr, __ATOMIC_SEQ_CST);			\
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
      actual_oldval = __atomic_load_n (ptr, __ATOMIC_SEQ_CST);

      if (oldval != actual_oldval)
	return actual_oldval;

      fail = __kernel_cmpxchg (actual_oldval, newval, ptr);

      if (!fail)
	return oldval;
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
	actual_oldval = __atomic_load_n (wordptr, __ATOMIC_SEQ_CST); 	\
									\
	if (((actual_oldval & mask) >> shift) != (unsigned int) oldval)	\
	  return (actual_oldval & mask) >> shift;			\
									\
	actual_newval = (actual_oldval & ~mask)				\
			| (((unsigned int) newval << shift) & mask);	\
									\
	fail = __kernel_cmpxchg (actual_oldval, actual_newval,		\
				 wordptr);				\
									\
	if (!fail)							\
	  return oldval;						\
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
    oldval = __atomic_load_n (ptr, __ATOMIC_SEQ_CST);
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
      oldval = __atomic_load_n (wordptr, __ATOMIC_SEQ_CST);		\
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
    /* All writes before this point must be seen before we release	\
       the lock itself.  */						\
    __builtin_nds32_msync_all ();					\
    *ptr = 0;								\
  }

SYNC_LOCK_RELEASE (int,   4)
SYNC_LOCK_RELEASE (short, 2)
SYNC_LOCK_RELEASE (char,  1)
