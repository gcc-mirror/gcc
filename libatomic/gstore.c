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

#define LAT_GSTORE
#include "libatomic_i.h"


/* If we natively support the store, and if we're unconcerned with extra
   barriers (e.g. fully in-order cpu for which barriers are a nop), then
   go ahead and expand the operation inline.  */
#if !defined(WANT_SPECIALCASE_RELAXED) && !defined(__OPTIMIZE_SIZE__)
# define EXACT_INLINE(N)					\
  if (C2(HAVE_ATOMIC_LDST_,N))					\
    {								\
      __atomic_store_n (PTR(N,mptr), *PTR(N,vptr), __ATOMIC_SEQ_CST);	\
      return;							\
    }
#else
# define EXACT_INLINE(N)
#endif


#define EXACT(N)						\
  do {								\
    if (!C2(HAVE_INT,N)) break;					\
    if ((uintptr_t)mptr & (N - 1)) break;			\
    EXACT_INLINE (N);						\
    C3(local_,store_,N) (PTR(N,mptr), *PTR(N,vptr), smodel);	\
    return;							\
  } while (0)


#define LARGER(N)						\
  do {								\
    union max_size_u u, v;					\
    uintptr_t r, a;						\
    if (!C2(HAVE_INT,N)) break;					\
    if (!C2(MAYBE_HAVE_ATOMIC_CAS_,N)) break;			\
    r = (uintptr_t)mptr & (N - 1);				\
    a = (uintptr_t)mptr & -N;					\
    if (r + n <= N)						\
      {								\
	pre_barrier (smodel);					\
	/* This load need not be atomic, as the CAS		\
	   below will validate it.  */				\
	u.C2(i,N) = *PTR(N,a);					\
	do {							\
	  v = u; memcpy (v.b + r, vptr, n);			\
	} while (!(C2(HAVE_ATOMIC_CAS_,N)			\
		   ? __atomic_compare_exchange_n (PTR(N,a),	\
			&u.C2(i,N), v.C2(i,N), true,		\
			__ATOMIC_RELAXED, __ATOMIC_RELAXED)	\
		   : C3(local_,compare_exchange_,N) (PTR(N,a),	\
			&u.C2(i,N), v.C2(i,N),			\
			__ATOMIC_RELAXED, __ATOMIC_RELAXED)));	\
	post_barrier (smodel);					\
	return;							\
      }								\
  } while (0)


void
libat_store (size_t n, void *mptr, void *vptr, int smodel)
{
  switch (n)
    {
    case 0:				return;
    case 1:		EXACT(1);	goto L4;
    case 2:		EXACT(2);	goto L4;
    case 4:		EXACT(4);	goto L8;
    case 8:		EXACT(8);	goto L16;
    case 16:		EXACT(16);	break;

    case 3: L4:		LARGER(4);	/* FALLTHRU */
    case 5 ... 7: L8:	LARGER(8);	/* FALLTHRU */
    case 9 ... 15: L16:	LARGER(16);	break;
    }

  pre_seq_barrier (smodel);
  libat_lock_n (mptr, n);

  memcpy (mptr, vptr, n);

  libat_unlock_n (mptr, n);
  post_seq_barrier (smodel);
}

EXPORT_ALIAS (store);
#undef LAT_GSTORE
