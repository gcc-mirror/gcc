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

#define LAT_GLFREE
#include "libatomic_i.h"

/* Accesses with a power-of-two size are not lock-free if we don't have an
   integer type of this size or if they are not naturally aligned.  They
   are lock-free if such a naturally aligned access is always lock-free
   according to the compiler, which requires that both atomic loads and CAS
   are available.
   In all other cases, we fall through to LARGER (see below).  */
#define EXACT(N)						\
  do {								\
    if (!C2(HAVE_INT,N)) break;					\
    if ((uintptr_t)ptr & (N - 1)) break;			\
    if (__atomic_always_lock_free(N, 0)) return true;		\
    if (!C2(MAYBE_HAVE_ATOMIC_CAS_,N)) break;			\
    if (C2(FAST_ATOMIC_LDST_,N)) return true;			\
  } while (0)


/* We next check to see if an access of a larger size is lock-free.  We use
   a similar check as in EXACT, except that we also check that the alignment
   of the access is so that the data to be accessed is completely covered
   by the larger access.  */
#define LARGER(N)						\
  do {								\
    uintptr_t r = (uintptr_t)ptr & (N - 1);			\
    if (!C2(HAVE_INT,N)) break;					\
    if (!C2(FAST_ATOMIC_LDST_,N)) break;			\
    if (!C2(MAYBE_HAVE_ATOMIC_CAS_,N)) break;			\
    if (r + n <= N) return true;				\
  } while (0)


/* Note that this can return that a size/alignment is not lock-free even if
   all the operations that we use to implement the respective accesses provide
   lock-free forward progress as specified in C++14:  Users likely expect
   "lock-free" to also mean "fast", which is why we do not return true if, for
   example, we implement loads with this size/alignment using a CAS.  */
bool
libat_is_lock_free (size_t n, void *ptr)
{
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
    }

  return false;
}

EXPORT_ALIAS (is_lock_free);
#undef LAT_GLFREE
