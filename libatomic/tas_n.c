/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libatomic_i.h"


/* If we support the builtin, just use it.  */
#if !DONE && SIZE(HAVE_ATOMIC_TAS)
bool
SIZE(libat_test_and_set) (UTYPE *mptr, int smodel)
{
  if (maybe_specialcase_relaxed(smodel))
    return __atomic_test_and_set (mptr, __ATOMIC_RELAXED);
  else if (maybe_specialcase_acqrel(smodel))
    return __atomic_test_and_set (mptr, __ATOMIC_ACQ_REL);
  else
    return __atomic_test_and_set (mptr, __ATOMIC_SEQ_CST);
}

#define DONE 1
#endif /* HAVE_ATOMIC_TAS */


/* If this type is smaller than word-sized, fall back to a word-sized
   compare-and-swap loop.  */
#if !DONE && N <= WORDSIZE && defined(atomic_compare_exchange_w)
bool
SIZE(libat_test_and_set) (UTYPE *mptr, int smodel)
{
  UWORD wval, woldval, shift, *wptr, t;

  pre_barrier (smodel);

  if (N < WORDSIZE)
    {
      wptr = (UWORD *)((uintptr_t)mptr & -WORDSIZE);
      shift = SIZE(INVERT_MASK);
    }
  else
    {
      wptr = (UWORD *)mptr;
      shift = 0;
    }

  wval = (UWORD)__GCC_ATOMIC_TEST_AND_SET_TRUEVAL << shift;
  woldval = __atomic_load_n (wptr, __ATOMIC_RELAXED);
  do
    {
      t = woldval | wval;
    }
  while (!atomic_compare_exchange_w (wptr, &woldval, t, true,
				     __ATOMIC_RELAXED, __ATOMIC_RELAXED));

  post_barrier (smodel);
  return woldval != 0;
}

#define DONE 1
#endif /* HAVE_ATOMIC_CAS && N < WORDSIZE */


/* Otherwise, fall back to some sort of protection mechanism.  */
#if !DONE && N == 1
bool
SIZE(libat_test_and_set) (UTYPE *mptr, int smodel)
{
  UTYPE oldval;
  UWORD magic;

  pre_seq_barrier (smodel);
  magic = protect_start (mptr);

  oldval = *mptr;
  *mptr = __GCC_ATOMIC_TEST_AND_SET_TRUEVAL;

  protect_end (mptr, magic);
  post_seq_barrier (smodel);

  return oldval != 0;
}

#define DONE 1
#endif /* N == 1 */


#if !DONE
bool
SIZE(libat_test_and_set) (UTYPE *mptr, int smodel UNUSED)
{
  return libat_test_and_set_1 ((U_1 *)mptr, smodel);
}
#endif

EXPORT_ALIAS (SIZE(test_and_set));
