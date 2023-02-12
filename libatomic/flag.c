/* Copyright (C) 2015-2023 Free Software Foundation, Inc.

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

#include <stdatomic.h>

/* Out-of-line versions of <stdatomic.h> flag functions.  */

/* Atomically set *OBJECT to true, returning the previous value.  */

_Bool
(atomic_flag_test_and_set) (volatile atomic_flag *object)
{
  return atomic_flag_test_and_set (object);
}

/* Atomically set *OBJECT to true, returning the previous value, with
   memory affected according to ORDER.  */

_Bool
(atomic_flag_test_and_set_explicit) (volatile atomic_flag *object,
				     memory_order order)
{
  return atomic_flag_test_and_set_explicit (object, order);
}

/* Atomically set *OBJECT to false.  */

void
(atomic_flag_clear) (volatile atomic_flag *object)
{
  atomic_flag_clear (object);
}

/* Atomically set *OBJECT to false, with memory affected according to
   ORDER.  */

void
(atomic_flag_clear_explicit) (volatile atomic_flag *object,
			      memory_order order)
{
  return atomic_flag_clear_explicit (object, order);
}
