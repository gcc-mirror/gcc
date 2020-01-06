/* Copyright (C) 2005-2020 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
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

/* This file contains helpers for the ATOMIC construct.  */

#include "libgomp.h"

/* This mutex is used when atomic operations don't exist for the target
   in the mode requested.  The result is not globally atomic, but works so
   long as all parallel references are within #pragma omp atomic directives.
   According to responses received from omp@openmp.org, appears to be within
   spec.  Which makes sense, since that's how several other compilers
   handle this situation as well.  */

static gomp_mutex_t atomic_lock;

void
GOMP_atomic_start (void)
{
  gomp_mutex_lock (&atomic_lock);
}

void
GOMP_atomic_end (void)
{
  gomp_mutex_unlock (&atomic_lock);
}

#if !GOMP_MUTEX_INIT_0
static void __attribute__((constructor))
initialize_atomic (void)
{
  gomp_mutex_init (&atomic_lock);
}
#endif
