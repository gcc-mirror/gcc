/* locks.h - Thread synchronization primitives. x86-64 implementation.

   Copyright (C) 2002  Free Software Foundation

   Contributed by Bo Thorsen <bo@suse.de>.

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
compare_and_swap(volatile obj_addr_t *addr, obj_addr_t old, obj_addr_t new_val)
{
  char result;
#ifdef __x86_64__
  __asm__ __volatile__("lock; cmpxchgq %2, %0; setz %1"
	      : "+m"(*(addr)), "=q"(result)
	      : "r" (new_val), "a"(old)
	      : "memory");
#else
  __asm__ __volatile__("lock; cmpxchgl %2, %0; setz %1"
		       : "+m"(*(addr)), "=q"(result)
		       : "r" (new_val), "a"(old)
		       : "memory");
#endif
  return (bool) result;
}

// Set *addr to new_val with release semantics, i.e. making sure
// that prior loads and stores complete before this
// assignment.
// On x86-64, the hardware shouldn't reorder reads and writes,
// so we just have to convince gcc not to do it either.
inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __asm__ __volatile__(" " : : : "memory");
  *(addr) = new_val;
}

// Compare_and_swap with release semantics instead of acquire semantics.
// On many architecture, the operation makes both guarantees, so the
// implementation can be the same.
inline static bool
compare_and_swap_release(volatile obj_addr_t *addr,
			 obj_addr_t old,
			 obj_addr_t new_val)
{
  return compare_and_swap(addr, old, new_val);
}

// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.
// On x86-64, the hardware ensures that reads are properly ordered.
inline static void
read_barrier()
{
}

// Ensure that prior stores to memory are completed with respect to other
// processors.
inline static void
write_barrier()
{
  /* x86-64 does not reorder writes. We just need to ensure that gcc also
     doesn't.  */
  __asm__ __volatile__(" " : : : "memory");
}
#endif
