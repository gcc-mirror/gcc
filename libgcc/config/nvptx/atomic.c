/* NVPTX atomic operations
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
   Contributed by Mentor Graphics.

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

#include <stdbool.h>

/* Implement __sync_val_compare_and_swap and __sync_bool_compare_and_swap
   for 1 and 2-byte values (which are not natively supported) in terms of
   __sync_val_compare_and_swap for 4-byte values (which is supported).
   This assumes that the contents of the word surrounding the subword
   value that we are interested in are accessible as well (which should
   normally be the case).  Note that if the contents of the word surrounding
   the subword changes between the __sync_val_compare_and_swap_4 and the
   preceeding load of oldword, while the subword does not, the implementation
   loops, which may manifest worst-case as a hang.  */

#define __SYNC_SUBWORD_COMPARE_AND_SWAP(TYPE, SIZE)			     \
									     \
TYPE									     \
__sync_val_compare_and_swap_##SIZE (volatile void *vptr, TYPE oldval,	     \
				    TYPE newval)			     \
{									     \
  volatile TYPE *ptr = vptr;						     \
  volatile unsigned int *wordptr					     \
    = (volatile unsigned int *)((__UINTPTR_TYPE__) ptr & ~3UL);	     \
  int shift = ((__UINTPTR_TYPE__) ptr & 3UL) * 8;			     \
  unsigned int valmask = (1 << (SIZE * 8)) - 1;				     \
  unsigned int wordmask = ~(valmask << shift);				     \
  unsigned int oldword = *wordptr;					     \
  for (;;)								     \
    {									     \
      TYPE prevval = (oldword >> shift) & valmask;			     \
      /* Exit if the subword value previously read from memory is not */     \
      /* equal to the expected value OLDVAL.  */			     \
      if (__builtin_expect (prevval != oldval, 0))			     \
	return prevval;							     \
      unsigned int newword = oldword & wordmask;			     \
      newword |= ((unsigned int) newval) << shift;			     \
      unsigned int prevword						     \
	  = __sync_val_compare_and_swap_4 (wordptr, oldword, newword);	     \
      /* Exit only if the compare-and-swap succeeds on the whole word */     \
      /* (i.e. the contents of *WORDPTR have not changed since the last */   \
      /* memory read).  */						     \
      if (__builtin_expect (prevword == oldword, 1))			     \
	return oldval;							     \
      oldword = prevword;						     \
    }									     \
}									     \
									     \
bool									     \
__sync_bool_compare_and_swap_##SIZE (volatile void *ptr, TYPE oldval,	     \
				     TYPE newval)			     \
{									     \
  return __sync_val_compare_and_swap_##SIZE (ptr, oldval, newval) == oldval; \
}

__SYNC_SUBWORD_COMPARE_AND_SWAP (unsigned char, 1)
__SYNC_SUBWORD_COMPARE_AND_SWAP (unsigned short, 2)
