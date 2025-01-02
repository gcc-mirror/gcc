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

#define LAT_FOP_N
#include <libatomic_i.h>


/* This file is included multiple times with required defines:
     NAME	the name of the operation that we're implementing; 
     OP		a two-operand functional macro the implements the operation.
*/


/* If we support the builtin, just use it.  */
#if !DONE && SIZE(HAVE_ATOMIC_FETCH_OP)
UTYPE
SIZE(C2(libat_fetch_,NAME)) (UTYPE *mptr, UTYPE opval, int smodel)
{
  if (maybe_specialcase_relaxed(smodel))
    return C2(__atomic_fetch_,NAME) (mptr, opval, __ATOMIC_RELAXED);
  else if (maybe_specialcase_acqrel(smodel))
    return C2(__atomic_fetch_,NAME) (mptr, opval, __ATOMIC_ACQ_REL);
  else
    return C2(__atomic_fetch_,NAME) (mptr, opval, __ATOMIC_SEQ_CST);
}

UTYPE
SIZE(C3(libat_,NAME,_fetch)) (UTYPE *mptr, UTYPE opval, int smodel)
{
  if (maybe_specialcase_relaxed(smodel))
    return C3(__atomic_,NAME,_fetch) (mptr, opval, __ATOMIC_RELAXED);
  else if (maybe_specialcase_acqrel(smodel))
    return C3(__atomic_,NAME,_fetch) (mptr, opval, __ATOMIC_ACQ_REL);
  else
    return C3(__atomic_,NAME,_fetch) (mptr, opval, __ATOMIC_SEQ_CST);
}

#define DONE 1
#endif /* HAVE_ATOMIC_FETCH_OP */


#if !DONE && defined(atomic_compare_exchange_n)
UTYPE
SIZE(C2(libat_fetch_,NAME)) (UTYPE *mptr, UTYPE opval, int smodel)
{
  UTYPE oldval, t;

  pre_barrier (smodel);

  oldval = *mptr;
  do
    {
      t = OP(oldval, opval);
    }
  while (!atomic_compare_exchange_n (mptr, &oldval, t, true,
				     __ATOMIC_RELAXED, __ATOMIC_RELAXED));

  post_barrier (smodel);
  return oldval;
}

UTYPE
SIZE(C3(libat_,NAME,_fetch)) (UTYPE *mptr, UTYPE opval, int smodel)
{
  UTYPE oldval, t;

  pre_barrier (smodel);

  oldval = *mptr;
  do
    {
      t = OP(oldval, opval);
    }
  while (!atomic_compare_exchange_n (mptr, &oldval, t, true,
				     __ATOMIC_RELAXED, __ATOMIC_RELAXED));

  post_barrier (smodel);
  return t;
}

#define DONE 1
#endif /* atomic_compare_exchange_n */


/* If this type is no larger than word-sized, fall back to a word-sized
   compare-and-swap loop.  */
#if !DONE && N < WORDSIZE && defined(atomic_compare_exchange_w)
UTYPE
SIZE(C2(libat_fetch_,NAME)) (UTYPE *mptr, UTYPE opval, int smodel)
{
  UWORD mask, shift, woldval, wopval, t, *wptr;

  pre_barrier (smodel);

  wptr = (UWORD *)((uintptr_t)mptr & -WORDSIZE);
  shift = (((uintptr_t)mptr % WORDSIZE) * CHAR_BIT) ^ SIZE(INVERT_MASK);
  mask = SIZE(MASK) << shift;

  wopval = (UWORD)opval << shift;
  woldval = __atomic_load_n (wptr, __ATOMIC_RELAXED);
  do
    {
      t = (woldval & ~mask) | (OP(woldval, wopval) & mask);
    }
  while (!atomic_compare_exchange_w (wptr, &woldval, t, true,
				     __ATOMIC_RELAXED, __ATOMIC_RELAXED));

  post_barrier (smodel);
  return woldval >> shift;
}

UTYPE
SIZE(C3(libat_,NAME,_fetch)) (UTYPE *mptr, UTYPE opval, int smodel)
{
  UWORD mask, shift, woldval, wopval, t, *wptr;

  pre_barrier (smodel);

  wptr = (UWORD *)((uintptr_t)mptr & -WORDSIZE);
  shift = (((uintptr_t)mptr % WORDSIZE) * CHAR_BIT) ^ SIZE(INVERT_MASK);
  mask = SIZE(MASK) << shift;

  wopval = (UWORD)opval << shift;
  woldval = __atomic_load_n (wptr, __ATOMIC_RELAXED);
  do
    {
      t = (woldval & ~mask) | (OP(woldval, wopval) & mask);
    }
  while (!atomic_compare_exchange_w (wptr, &woldval, t, true,
				     __ATOMIC_RELAXED, __ATOMIC_RELAXED));

  post_barrier (smodel);
  return t >> shift;
}

#define DONE 1
#endif /* atomic_compare_exchange_w */


/* Otherwise, fall back to some sort of protection mechanism.  */
#if !DONE
UTYPE
SIZE(C2(libat_fetch_,NAME)) (UTYPE *mptr, UTYPE opval, int smodel UNUSED)
{
  UTYPE ret;
  UWORD magic;

  pre_seq_barrier (smodel);
  magic = protect_start (mptr);

  ret = *mptr;
  *mptr = OP(ret, opval);

  protect_end (mptr, magic);
  post_seq_barrier (smodel);

  return ret;
}

UTYPE
SIZE(C3(libat_,NAME,_fetch)) (UTYPE *mptr, UTYPE opval, int smodel UNUSED)
{
  UTYPE ret;
  UWORD magic;

  pre_seq_barrier (smodel);
  magic = protect_start (mptr);

  ret = OP (*mptr, opval);
  *mptr = ret;

  protect_end (mptr, magic);
  post_seq_barrier (smodel);

  return ret;
}
#endif

EXPORT_ALIAS (SIZE(C2(fetch_,NAME)));
EXPORT_ALIAS (SIZE(C2(NAME,_fetch)));
#undef LAT_FOP_N
