/* 64bit Linux-specific atomic operations for ARM EABI.
   Copyright (C) 2008-2020 Free Software Foundation, Inc.
   Based on linux-atomic.c

   64 bit additions david.gilbert@linaro.org

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

/* 64bit helper functions for atomic operations; the compiler will
   call these when the code is compiled for a CPU without ldrexd/strexd.
   (If the CPU had those then the compiler inlines the operation).

   These helpers require a kernel helper that's only present on newer
   kernels; we check for that in an init section and bail out rather
   unceremoneously.  */

extern int write (int fd, const void *buf, unsigned int count);
extern void abort (void);

/* Kernel helper for compare-and-exchange.  */
typedef int (__kernel_cmpxchg64_t) (const long long* oldval,
					const long long* newval,
					long long *ptr);
#define __kernel_cmpxchg64 (*(__kernel_cmpxchg64_t *) 0xffff0f60)

/* Kernel helper page version number.  */
#define __kernel_helper_version (*(unsigned int *)0xffff0ffc)

/* Check that the kernel has a new enough version at load.  */
static void __check_for_sync8_kernelhelper (void)
{
  if (__kernel_helper_version < 5)
    {
      const char err[] = "A newer kernel is required to run this binary. "
				"(__kernel_cmpxchg64 helper)\n";
      /* At this point we need a way to crash with some information
	 for the user - I'm not sure I can rely on much else being
	 available at this point, so do the same as generic-morestack.c
	 write () and abort ().  */
      write (2 /* stderr.  */, err, sizeof (err));
      abort ();
    }
};

static void (*__sync8_kernelhelper_inithook[]) (void)
		__attribute__ ((used, section (".init_array"))) = {
  &__check_for_sync8_kernelhelper
};

#define HIDDEN __attribute__ ((visibility ("hidden")))

#define FETCH_AND_OP_WORD64(OP, PFX_OP, INF_OP)			\
  long long HIDDEN						\
  __sync_fetch_and_##OP##_8 (long long *ptr, long long val)	\
  {								\
    int failure;						\
    long long tmp,tmp2;						\
								\
    do {							\
      tmp = *ptr;						\
      tmp2 = PFX_OP (tmp INF_OP val);				\
      failure = __kernel_cmpxchg64 (&tmp, &tmp2, ptr);		\
    } while (failure != 0);					\
								\
    return tmp;							\
  }

FETCH_AND_OP_WORD64 (add,   , +)
FETCH_AND_OP_WORD64 (sub,   , -)
FETCH_AND_OP_WORD64 (or,    , |)
FETCH_AND_OP_WORD64 (and,   , &)
FETCH_AND_OP_WORD64 (xor,   , ^)
FETCH_AND_OP_WORD64 (nand, ~, &)

#define NAME_oldval(OP, WIDTH) __sync_fetch_and_##OP##_##WIDTH
#define NAME_newval(OP, WIDTH) __sync_##OP##_and_fetch_##WIDTH

/* Implement both __sync_<op>_and_fetch and __sync_fetch_and_<op> for
   subword-sized quantities.  */

#define OP_AND_FETCH_WORD64(OP, PFX_OP, INF_OP)			\
  long long HIDDEN						\
  __sync_##OP##_and_fetch_8 (long long *ptr, long long val)	\
  {								\
    int failure;						\
    long long tmp,tmp2;						\
								\
    do {							\
      tmp = *ptr;						\
      tmp2 = PFX_OP (tmp INF_OP val);				\
      failure = __kernel_cmpxchg64 (&tmp, &tmp2, ptr);		\
    } while (failure != 0);					\
								\
    return tmp2;						\
  }

OP_AND_FETCH_WORD64 (add,   , +)
OP_AND_FETCH_WORD64 (sub,   , -)
OP_AND_FETCH_WORD64 (or,    , |)
OP_AND_FETCH_WORD64 (and,   , &)
OP_AND_FETCH_WORD64 (xor,   , ^)
OP_AND_FETCH_WORD64 (nand, ~, &)

long long HIDDEN
__sync_val_compare_and_swap_8 (long long *ptr, long long oldval,
				long long newval)
{
  int failure;
  long long actual_oldval;

  while (1)
    {
      actual_oldval = *ptr;

      if (__builtin_expect (oldval != actual_oldval, 0))
	return actual_oldval;

      failure = __kernel_cmpxchg64 (&actual_oldval, &newval, ptr);

      if (__builtin_expect (!failure, 1))
	return oldval;
    }
}

typedef unsigned char bool;

bool HIDDEN
__sync_bool_compare_and_swap_8 (long long *ptr, long long oldval,
				 long long newval)
{
  int failure = __kernel_cmpxchg64 (&oldval, &newval, ptr);
  return (failure == 0);
}

long long HIDDEN
__sync_lock_test_and_set_8 (long long *ptr, long long val)
{
  int failure;
  long long oldval;

  do {
    oldval = *ptr;
    failure = __kernel_cmpxchg64 (&oldval, &val, ptr);
  } while (failure != 0);

  return oldval;
}
