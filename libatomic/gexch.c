/* Copyright (C) 2012-2023 Free Software Foundation, Inc.
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


/* If we natively support the exchange, and if we're unconcerned with extra
   barriers (e.g. fully in-order cpu for which barriers are a nop), then
   go ahead and expand the operation inline.  */
#if !defined(WANT_SPECIALCASE_RELAXED) && !defined(__OPTIMIZE_SIZE__)
# define EXACT_INLINE(N)					\
  if (C2(HAVE_ATOMIC_EXCHANGE_,N))				\
    {								\
      *PTR(N,rptr) = __atomic_exchange_n			\
	(PTR(N,mptr), *PTR(N,vptr), __ATOMIC_SEQ_CST);		\
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
    *PTR(N,rptr) = C3(local_,exchange_,N)			\
      (PTR(N,mptr), *PTR(N,vptr), smodel);			\
    return;							\
  } while (0)


#define LARGER(N)						\
  do {								\
    if (!C2(HAVE_INT,N)) break;					\
    if (!C2(MAYBE_HAVE_ATOMIC_CAS_,N)) break;			\
    r = (uintptr_t)mptr & (N - 1);				\
    a = (uintptr_t)mptr & -N;					\
    if (r + n <= N)						\
      {								\
	pre_barrier (smodel);					\
	u.C2(i,N) = *PTR(N,a);					\
	do {							\
	  v = u;						\
	  memcpy (v.b + r, vptr, n);				\
	} while (!(C2(HAVE_ATOMIC_CAS_,N)			\
		   ? __atomic_compare_exchange_n (PTR(N,a),	\
			&u.C2(i,N), v.C2(i,N), true,		\
			__ATOMIC_RELAXED, __ATOMIC_RELAXED)	\
		   : C3(local_,compare_exchange_,N) (PTR(N,a),	\
			&u.C2(i,N), v.C2(i,N),			\
			__ATOMIC_RELAXED, __ATOMIC_RELAXED)));	\
	goto Lfinish;						\
      }								\
  } while (0)


static void __attribute__((noinline))
libat_exchange_large_inplace (size_t n, void *mptr, void *vptr)
{
#define BUF	1024

  char temp[BUF];
  size_t i = 0;

  for (i = 0; n >= BUF; i += BUF, n -= BUF)
    {
      memcpy (temp, mptr + i, BUF);
      memcpy (mptr + i, vptr + i, BUF);
      memcpy (vptr + i, temp, BUF);
    }
  if (n > 0)
    {
      memcpy (temp, mptr + i, n);
      memcpy (mptr + i, vptr + i, n);
      memcpy (vptr + i, temp, n);
    }

#undef BUF
}

void
libat_exchange (size_t n, void *mptr, void *vptr, void *rptr, int smodel)
{
  union max_size_u u, v;
  uintptr_t r, a;

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

    Lfinish:
      post_barrier (smodel);
      memcpy (rptr, u.b + r, n);
      return;
    }

  pre_seq_barrier (smodel);
  libat_lock_n (mptr, n);

  if (vptr != rptr)
    {
      memcpy (rptr, mptr, n);
      memcpy (mptr, vptr, n);
    }
  else
    libat_exchange_large_inplace (n, mptr, vptr);

  libat_unlock_n (mptr, n);
  post_seq_barrier (smodel);
}

EXPORT_ALIAS (exchange);
