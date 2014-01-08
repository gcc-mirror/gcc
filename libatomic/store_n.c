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
#if !DONE && SIZE(HAVE_ATOMIC_LDST)
void
SIZE(libat_store) (UTYPE *mptr, UTYPE newval, int smodel)
{
  if (maybe_specialcase_relaxed(smodel))
    __atomic_store_n (mptr, newval, __ATOMIC_RELAXED);
  else if (maybe_specialcase_acqrel(smodel))
    /* Note that ACQ and ACQ_REL are not valid for store.  */
    __atomic_store_n (mptr, newval, __ATOMIC_RELEASE);
  else
    __atomic_store_n (mptr, newval, __ATOMIC_SEQ_CST);
}

#define DONE 1
#endif /* HAVE_ATOMIC_STORE */

/* If we have compare-and-swap, use it perform the store.  */
#if !DONE && defined(atomic_compare_exchange_n)
void
SIZE(libat_store) (UTYPE *mptr, UTYPE newval, int smodel)
{
  UTYPE oldval;

  pre_barrier (smodel);

  oldval = *mptr;
  while (!atomic_compare_exchange_n (mptr, &oldval, newval, true,
				     __ATOMIC_RELAXED, __ATOMIC_RELAXED))
    continue;

  post_barrier (smodel);
}

#define DONE 1
#endif /* atomic_compare_exchange_n */


/* If this type is smaller than word-sized, fall back to a word-sized
   compare-and-swap loop.  */
#if !DONE && N < WORDSIZE && defined(atomic_compare_exchange_w)
void
SIZE(libat_store) (UTYPE *mptr, UTYPE newval, int smodel)
{
  UWORD mask, shift, woldval, wnewval, t, *wptr;

  pre_barrier (smodel);

  wptr = (UWORD *)((uintptr_t)mptr & -WORDSIZE);
  shift = (((uintptr_t)mptr % WORDSIZE) * CHAR_BIT) ^ SIZE(INVERT_MASK);
  mask = SIZE(MASK) << shift;

  wnewval = (UWORD)newval << shift;
  woldval = __atomic_load_n (wptr, __ATOMIC_RELAXED);
  do
    {
      t = (woldval & ~mask) | wnewval;
    }
  while (!atomic_compare_exchange_w (wptr, &woldval, t));

  post_barrier (smodel);
}

#define DONE 1
#endif /* N < WORDSIZE && atomic_compare_exchange_w */


/* Otherwise, fall back to some sort of protection mechanism.  */
#if !DONE
void
SIZE(libat_store) (UTYPE *mptr, UTYPE newval, int smodel)
{
  UWORD magic;

  pre_seq_barrier (smodel);
  magic = protect_start (mptr);

  *mptr = newval;

  protect_end (mptr, magic);
  post_seq_barrier (smodel);
}
#endif

EXPORT_ALIAS (SIZE(store));
