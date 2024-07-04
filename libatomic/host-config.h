/* Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

/* Included after all more target-specific host-config.h.  */


/* The target may have some OS specific way to implement compare-and-swap.  */
#if !defined(atomic_compare_exchange_n) && SIZE(HAVE_ATOMIC_CAS)
# define atomic_compare_exchange_n  __atomic_compare_exchange_n
#endif
#if !defined(atomic_compare_exchange_w) && WSIZE(HAVE_ATOMIC_CAS)
# define atomic_compare_exchange_w  __atomic_compare_exchange_n
#endif

/* For some targets, it may be significantly faster to avoid all barriers
   if the user only wants relaxed memory order.  Sometimes we don't want
   the extra code bloat.  In all cases, use the input to avoid warnings.  */
#if defined(WANT_SPECIALCASE_RELAXED) && !defined(__OPTIMIZE_SIZE__)
# define maybe_specialcase_relaxed(x)	((x) == __ATOMIC_RELAXED)
#else
# define maybe_specialcase_relaxed(x)	((x) & 0)
#endif

/* Similar, but for targets for which the seq_cst model is sufficiently
   more expensive than the acq_rel model.  */
#if defined(WANT_SPECIALCASE_ACQREL) && !defined(__OPTIMIZE_SIZE__)
# define maybe_specialcase_acqrel(x)	((x) != __ATOMIC_SEQ_CST)
#else
# define maybe_specialcase_acqrel(x)	((x) & 0)
#endif


/* The target may have some OS specific way to emit barriers.  */
#ifndef pre_post_barrier
static inline void __attribute__((always_inline, artificial))
pre_barrier(int model)
{
  if (!maybe_specialcase_relaxed(model))
    {
      if (maybe_specialcase_acqrel(model))
        __atomic_thread_fence (__ATOMIC_ACQ_REL);
      else
        __atomic_thread_fence (__ATOMIC_SEQ_CST);
    }
}
static inline void __attribute__((always_inline, artificial))
post_barrier(int model)
{
  pre_barrier(model);
}
#define pre_post_barrier 1
#endif /* pre_post_barrier */

/* Similar, but assume that acq_rel is already handled via locks.  */
#ifndef pre_post_seq_barrier
static inline void __attribute__((always_inline, artificial))
pre_seq_barrier(int model)
{
}
static inline void __attribute__((always_inline, artificial))
post_seq_barrier(int model)
{
}
#define pre_post_seq_barrier 1
#endif
