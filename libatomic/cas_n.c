/* Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

#define LAT_CAS_N
#include "libatomic_i.h"


/* If we support the builtin, just use it.  */
#if !DONE && defined(atomic_compare_exchange_n)
bool
SIZE(libat_compare_exchange) (UTYPE *mptr, UTYPE *eptr, UTYPE newval,
			      int smodel, int fmodel UNUSED)
{
  if (maybe_specialcase_relaxed(smodel))
    return atomic_compare_exchange_n (mptr, eptr, newval, false,
				      __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  else if (maybe_specialcase_acqrel(smodel))
    return atomic_compare_exchange_n (mptr, eptr, newval, false,
				      __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);
  else
    return atomic_compare_exchange_n (mptr, eptr, newval, false,
				      __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
}

#define DONE 1
#endif /* HAVE_ATOMIC_CAS */


/* If this type is not larger than word-sized, fall back to a word-sized
   compare-and-swap loop, possibly assisted by the OS.  */
#if !DONE && N <= WORDSIZE && defined(atomic_compare_exchange_w)
bool
SIZE(libat_compare_exchange) (UTYPE *mptr, UTYPE *eptr, UTYPE newval,
			      int smodel, int fmodel)
{
  UWORD mask, shift, weval, woldval, wnewval, t, *wptr;

  pre_barrier (smodel);

  if (N < WORDSIZE)
    {
      wptr = (UWORD *)((uintptr_t)mptr & -WORDSIZE);
      shift = (((uintptr_t)mptr % WORDSIZE) * CHAR_BIT) ^ SIZE(INVERT_MASK);
      mask = SIZE(MASK) << shift;
    }
  else
    {
      wptr = (UWORD *)mptr;
      shift = 0;
      mask = -1;
    }

  weval = (UWORD)*eptr << shift;
  wnewval = (UWORD)newval << shift;
  woldval = __atomic_load_n (wptr, __ATOMIC_RELAXED);
  do
    {
      if ((woldval & mask) != weval)
	goto failure;
      t = (woldval & ~mask) | wnewval;
    }
  while (!atomic_compare_exchange_w (wptr, &woldval, t, true,
				     __ATOMIC_RELAXED, __ATOMIC_RELAXED));
  post_barrier (smodel);
  return true;

 failure:
  *eptr = woldval >> shift;
  post_barrier (fmodel);
  return false;
}

#define DONE 1
#endif /* HAVE_ATOMIC_CAS && N <= WORDSIZE */


/* Otherwise, fall back to some sort of protection mechanism.  */
#if !DONE
bool
SIZE(libat_compare_exchange) (UTYPE *mptr, UTYPE *eptr, UTYPE newval,
			      int smodel, int fmodel UNUSED)
{
  UTYPE oldval;
  UWORD magic;
  bool ret;

  pre_seq_barrier (smodel);
  magic = protect_start (mptr);

  oldval = *mptr;
  ret = (oldval == *eptr);
  if (ret)
    *mptr = newval;
  else
    *eptr = oldval;

  protect_end (mptr, magic);
  post_seq_barrier (smodel);

  return ret;
}
#endif

EXPORT_ALIAS (SIZE(compare_exchange));
#undef LAT_CAS_N
