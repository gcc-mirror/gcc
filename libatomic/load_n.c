/* Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
UTYPE
SIZE(libat_load) (UTYPE *mptr, int smodel)
{
  if (maybe_specialcase_relaxed(smodel))
    return __atomic_load_n (mptr, __ATOMIC_RELAXED);
  else if (maybe_specialcase_acqrel(smodel))
    /* Note that REL and ACQ_REL are not valid for loads.  */
    return __atomic_load_n (mptr, __ATOMIC_ACQUIRE);
  else
    return __atomic_load_n (mptr, __ATOMIC_SEQ_CST);
}

#define DONE 1
#endif /* HAVE_ATOMIC_LOAD */


/* If we have compare-and-swap, use it to swap 0 with 0 and as a side
   effect load the original value.  */
#if !DONE && defined(atomic_compare_exchange_n)
UTYPE
SIZE(libat_load) (UTYPE *mptr, int smodel)
{
  UTYPE t = 0;

  if (maybe_specialcase_relaxed(smodel))
    atomic_compare_exchange_n (mptr, &t, 0, true,
			       __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  else if (maybe_specialcase_acqrel(smodel))
    atomic_compare_exchange_n (mptr, &t, 0, true,
			       __ATOMIC_ACQ_REL, __ATOMIC_ACQ_REL);
  else
    atomic_compare_exchange_n (mptr, &t, 0, true,
                               __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);

  return t;
}

#define DONE 1
#endif /* atomic_compare_exchange_n */


/* Similar, but only assume a word-sized compare-and-swap.  */
#if !DONE && N < WORDSIZE && defined(atomic_compare_exchange_w)
UTYPE
SIZE(libat_load) (UTYPE *mptr, int smodel)
{
  UWORD shift, t, *wptr;

  pre_barrier (smodel);

  wptr = (UWORD *)((uintptr_t)mptr & -WORDSIZE);
  shift = (((uintptr_t)mptr % WORDSIZE) * CHAR_BIT) ^ SIZE(INVERT_MASK);

  /* Exchange 0 with 0, placing the old value of *WPTR in T.  */
  t = 0;
  atomic_compare_exchange_w (wptr, &t, 0);

  post_barrier (smodel);
  return t >> shift;
}

#define DONE 1
#endif /* HAVE_ATOMIC_CAS && N < WORDSIZE */


/* Otherwise, fall back to some sort of protection mechanism.  */
#if !DONE
UTYPE
SIZE(libat_load) (UTYPE *mptr, int smodel)
{
  UTYPE ret;
  UWORD magic;

  pre_seq_barrier (smodel);
  magic = protect_start (mptr);

  ret = *mptr;

  protect_end (mptr, magic);
  post_seq_barrier (smodel);

  return ret;
}
#endif

EXPORT_ALIAS (SIZE(load));
