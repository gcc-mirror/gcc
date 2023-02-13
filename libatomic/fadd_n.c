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

#include <libatomic_i.h>

#define NAME	add
#define OP(X,Y)	((X) + (Y))

/* Defer to HAVE_ATOMIC_FETCH_ADD, which some targets implement specially,
   even if HAVE_ATOMIC_FETCH_OP is not defined.  */
#if !SIZE(HAVE_ATOMIC_FETCH_OP)
# undef HAVE_ATOMIC_FETCH_OP_1
# undef HAVE_ATOMIC_FETCH_OP_2
# undef HAVE_ATOMIC_FETCH_OP_4
# undef HAVE_ATOMIC_FETCH_OP_8
# undef HAVE_ATOMIC_FETCH_OP_16
# define HAVE_ATOMIC_FETCH_OP_1   HAVE_ATOMIC_FETCH_ADD_1
# define HAVE_ATOMIC_FETCH_OP_2   HAVE_ATOMIC_FETCH_ADD_2
# define HAVE_ATOMIC_FETCH_OP_4   HAVE_ATOMIC_FETCH_ADD_4
# define HAVE_ATOMIC_FETCH_OP_8   HAVE_ATOMIC_FETCH_ADD_8
# define HAVE_ATOMIC_FETCH_OP_16  HAVE_ATOMIC_FETCH_ADD_16
#endif

#include "fop_n.c"
