/* Copyright (C) 2015-2021 Free Software Foundation, Inc.

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

/* Out-of-line versions of <stdatomic.h> fence functions.  */

/* Fence as specified by ORDER.  */

void
(atomic_thread_fence) (memory_order order)
{
  atomic_thread_fence (order);
}

/* Fence as specified by ORDER but only establishing ordering between
   a thread and a signal handler executed in that thread.  */

void
(atomic_signal_fence) (memory_order order)
{
  atomic_signal_fence (order);
}
