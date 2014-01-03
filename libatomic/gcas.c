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


/* If we natively support the cas, and if we're unconcerned with extra
   barriers (e.g. fully in-order cpu for which barriers are a nop), then
   go ahead and expand the operation inline.  */
#if !defined(WANT_SPECIALCASE_RELAXED) && !defined(__OPTIMIZE_SIZE__)
# define EXACT_INLINE(N)					\
  if (C2(HAVE_ATOMIC_CAS_,N))					\
    return __atomic_compare_exchange_n				\
      (PTR(N,mptr), PTR(N,eptr), *PTR(N,dptr), false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)
#else
# define EXACT_INLINE(N)
#endif

/* ... and if all that fails, invoke the function we generated elsewhere.
   Worst case, this will *also* use locks.  */
#define EXACT(N)						\
  do {								\
    if (!C2(HAVE_INT,N)) break;					\
    if ((uintptr_t)mptr & (N - 1)) break;			\
    EXACT_INLINE (N);						\
    return C3(local_,compare_exchange_,N)			\
      (PTR(N,mptr), PTR(N,eptr), *PTR(N,dptr), smodel, fmodel);	\
  } while (0)

#define LARGER(N)						\
  do {								\
    if (!C2(HAVE_INT,N)) break;					\
    if (!C2(HAVE_ATOMIC_LDST_,N)) break;			\
    if (!C2(MAYBE_HAVE_ATOMIC_CAS_,N)) break;			\
    r = (uintptr_t)mptr & (N - 1);				\
    a = (uintptr_t)mptr & -N;					\
    if (r + n <= N)						\
      {								\
	pre_barrier (smodel);					\
	u.C2(i,N) = __atomic_load_n (PTR(N,a), __ATOMIC_RELAXED); \
	do {							\
	  if (memcmp (u.b + r, eptr, n) != 0) goto Lfail;	\
	  v = u; memcpy (v.b + r, dptr, n);			\
	} while (!(C2(HAVE_ATOMIC_CAS_,N)			\
		   ? __atomic_compare_exchange_n (PTR(N,a),	\
			&u.C2(i,N), v.C2(i,N), true,		\
			__ATOMIC_RELAXED, __ATOMIC_RELAXED)	\
		   : C3(local_,compare_exchange_,N) (PTR(N,a),	\
			&u.C2(i,N), v.C2(i,N),			\
			__ATOMIC_RELAXED, __ATOMIC_RELAXED)));	\
	goto Lsucc;						\
      }								\
  } while (0)



bool
libat_compare_exchange (size_t n, void *mptr, void *eptr, void *dptr,
			int smodel, int fmodel)
{
  union max_size_u u, v;
  uintptr_t r, a;
  bool ret;

  switch (n)
    {
    case 0:				return true;
    case 1:		EXACT(1);	goto L4;
    case 2:		EXACT(2);	goto L4;
    case 4:		EXACT(4);	goto L8;
    case 8:		EXACT(8);	goto L16;
    case 16:		EXACT(16);	break;

    case 3: L4:		LARGER(4);	/* FALLTHRU */
    case 5 ... 7: L8:	LARGER(8);	/* FALLTHRU */
    case 9 ... 15: L16:	LARGER(16);	break;

    Lsucc:
      post_barrier (smodel);
      return true;
    Lfail:
      post_barrier (fmodel);
      memcpy (eptr, u.b + r, n);
      return false;
    }

  pre_seq_barrier (smodel);
  libat_lock_n (mptr, n);

  ret = memcmp (mptr, eptr, n) == 0;
  memcpy ((ret ? mptr : eptr), (ret ? dptr : mptr), n);

  libat_unlock_n (mptr, n);
  post_seq_barrier (ret ? smodel : fmodel);

  return ret;
}

EXPORT_ALIAS (compare_exchange);
