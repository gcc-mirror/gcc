// locks.h - Thread synchronization primitives. PowerPC implementation.

/* Copyright (C) 2002,2008  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/

// Atomically replace *addr by new_val if it was initially equal to old.
// Return true if the comparison succeeded.
// Assumed to have acquire semantics, i.e. later memory operations
// cannot execute before the compare_and_swap finishes.

inline static bool
compare_and_swap (volatile obj_addr_t *addr,
		  obj_addr_t old,
		  obj_addr_t new_val) 
{
  return __atomic_compare_exchange_n (addr, &old, new_val, 0,
				      __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
}


// Set *addr to new_val with release semantics, i.e. making sure
// that prior loads and stores complete before this
// assignment.

inline static void
release_set (volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __atomic_store_n(addr, new_val, __ATOMIC_RELEASE);
}


// Compare_and_swap with release semantics instead of acquire semantics.

inline static bool
compare_and_swap_release (volatile obj_addr_t *addr, obj_addr_t old,
			  obj_addr_t new_val)
{
  return __atomic_compare_exchange_n (addr, &old, new_val, 0,
				      __ATOMIC_RELEASE, __ATOMIC_RELAXED);
}


// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.

inline static void
read_barrier ()
{
  __atomic_thread_fence (__ATOMIC_ACQUIRE);
}


// Ensure that prior stores to memory are completed with respect to other
// processors.

inline static void
write_barrier ()
{
  __atomic_thread_fence (__ATOMIC_RELEASE);
}

#endif
