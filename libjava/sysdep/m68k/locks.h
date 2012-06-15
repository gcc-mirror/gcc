// locks.h - Thread synchronization primitives. m68k implementation.

/* Copyright (C) 2006, 2012  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

/* Integer type big enough for object address.  */
typedef size_t obj_addr_t __attribute__ ((aligned (4)));

// Atomically replace *addr by new_val if it was initially equal to old.
// Return true if the comparison succeeded.
// Assumed to have acquire semantics, i.e. later memory operations
// cannot execute before the compare_and_swap finishes.
static inline bool
compare_and_swap(volatile obj_addr_t *addr,
		 obj_addr_t old, obj_addr_t new_val)
{
  return __sync_bool_compare_and_swap (addr, old, new_val);
}

// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.
// On m68k, the hardware ensures that reads are properly ordered.
static inline void
read_barrier(void)
{
}

// Ensure that prior stores to memory are completed with respect to other
// processors.
static inline void
write_barrier(void)
{
  // m68k does not reorder writes. We just need to ensure that gcc also doesn't.
  __asm__ __volatile__(" " : : : "memory");
}

// Set *addr to new_val with release semantics, i.e. making sure
// that prior loads and stores complete before this
// assignment.
// On m68k, the hardware shouldn't reorder reads and writes,
// so we just have to convince gcc not to do it either.
static inline void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  write_barrier ();
  *addr = new_val;
}

// Compare_and_swap with release semantics instead of acquire semantics.
// On many architecture, the operation makes both guarantees, so the
// implementation can be the same.
static inline bool
compare_and_swap_release(volatile obj_addr_t *addr,
			 obj_addr_t old,
			 obj_addr_t new_val)
{
  return compare_and_swap(addr, old, new_val);
}

#endif
