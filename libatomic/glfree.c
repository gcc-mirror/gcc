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


#define EXACT(N)						\
  do {								\
    if (!C2(HAVE_INT,N)) break;					\
    if ((uintptr_t)ptr & (N - 1)) break;			\
    if (__atomic_always_lock_free(N, 0)) return true;		\
    if (C2(MAYBE_HAVE_ATOMIC_CAS_,N)) return true;		\
  } while (0)


#define LARGER(N)						\
  do {								\
    uintptr_t r = (uintptr_t)ptr & (N - 1);			\
    if (!C2(HAVE_INT,N)) break;					\
    if (!C2(HAVE_ATOMIC_LDST_,N)) break;			\
    if (!C2(MAYBE_HAVE_ATOMIC_CAS_,N)) break;			\
    if (r + n <= N) return true;				\
  } while (0)


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
