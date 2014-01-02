/* Copyright (C) 2009-2014 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
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

#ifndef LIBITM_STMLOCK_H
#define LIBITM_STMLOCK_H 1

namespace GTM HIDDEN {

/* A versioned write lock on a cacheline.  This must be wide enough to
   store a pointer, and preferably wide enough to avoid overflowing the
   version counter.  Thus we use a "word", which should be 64-bits on
   64-bit systems even when their pointer size is forced smaller.  */
typedef gtm_word gtm_stmlock;

/* This has to be the same size as gtm_stmlock, we just use this name
   for documentation purposes.  */
typedef gtm_word gtm_version;

/* The maximum value a version number can have.  This is a consequence
   of having the low bit of gtm_stmlock reserved for the owned bit.  */
#define GTM_VERSION_MAX		(~(gtm_version)0 >> 1)

/* A value that may be used to indicate "uninitialized" for a version.  */
#define GTM_VERSION_INVALID	(~(gtm_version)0)

/* This bit is set when the write lock is held.  When set, the balance of
   the bits in the lock is a pointer that references STM backend specific
   data; it is up to the STM backend to determine if this thread holds the
   lock.  If this bit is clear, the balance of the bits are the last
   version number committed to the cacheline.  */
static inline bool
gtm_stmlock_owned_p (gtm_stmlock lock)
{
  return lock & 1;
}

static inline gtm_stmlock
gtm_stmlock_set_owned (void *data)
{
  return (gtm_stmlock)(uintptr_t)data | 1;
}

static inline void *
gtm_stmlock_get_addr (gtm_stmlock lock)
{
  return (void *)((uintptr_t)lock & ~(uintptr_t)1);
}

static inline gtm_version
gtm_stmlock_get_version (gtm_stmlock lock)
{
  return lock >> 1;
}

static inline gtm_stmlock
gtm_stmlock_set_version (gtm_version ver)
{
  return ver << 1;
}

/* We use a fixed set of locks for all memory, hashed into the
   following table.  */
#define LOCK_ARRAY_SIZE  (1024 * 1024)
extern gtm_stmlock gtm_stmlock_array[LOCK_ARRAY_SIZE];

static inline gtm_stmlock *
gtm_get_stmlock (const gtm_cacheline *addr)
{
  size_t idx = ((uintptr_t) addr / CACHELINE_SIZE) % LOCK_ARRAY_SIZE;
  return gtm_stmlock_array + idx;
}

/* The current global version number.  */
extern atomic<gtm_version> gtm_clock;

static inline gtm_version
gtm_get_clock (void)
{
  atomic_thread_fence(memory_order_release);
  return gtm_clock.load(memory_order_acquire);
}

static inline gtm_version
gtm_inc_clock (void)
{
  /* ??? Here we have a choice, the pre-inc operator mapping to
     __atomic_add_fetch with memory_order_seq_cst, or fetch_add
     with memory_order_acq_rel plus another separate increment.
     We really ought to recognize and optimize fetch_op(x) op x... */
  gtm_version r = ++gtm_clock;

  /* ??? Ought to handle wraparound for 32-bit.  */
  if (sizeof(r) < 8 && r > GTM_VERSION_MAX)
    abort ();

  return r;
}

} // namespace GTM

#endif // LIBITM_STMLOCK_H
