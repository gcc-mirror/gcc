/* Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

/* Avoiding the DMB (or kernel helper) can be a good thing.  */
#define WANT_SPECIALCASE_RELAXED

/* Glibc, at least, uses acq_rel in its pthread mutex
   implementation.  If the user is asking for seq_cst,
   this is insufficient.  */

static inline void __attribute__((always_inline, artificial))
pre_seq_barrier(int model)
{
  if (model == __ATOMIC_SEQ_CST)
    __atomic_thread_fence (__ATOMIC_SEQ_CST);
}

static inline void __attribute__((always_inline, artificial))
post_seq_barrier(int model)
{
  pre_seq_barrier(model);
}

#define pre_post_seq_barrier 1

#include_next <host-config.h>
