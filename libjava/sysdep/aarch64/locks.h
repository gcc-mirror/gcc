// locks.h - Thread synchronization primitives. AArch64 implementation.

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/

// Atomically replace *addr by new_val if it was initially equal to old.
// Return true if the comparison succeeded.
// Assumed to have acquire semantics, i.e. later memory operations
// cannot execute before the compare_and_swap finishes.
inline static bool
compare_and_swap(volatile obj_addr_t *addr,
                 obj_addr_t old,
                 obj_addr_t new_val)
{
  return __sync_bool_compare_and_swap(addr, old, new_val);
}

// Set *addr to new_val with release semantics, i.e. making sure
// that prior loads and stores complete before this
// assignment.
inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __sync_synchronize();
  *addr = new_val;
}

// Compare_and_swap with release semantics instead of acquire semantics.
// On many architecture, the operation makes both guarantees, so the
// implementation can be the same.
inline static bool
compare_and_swap_release(volatile obj_addr_t *addr,
			 obj_addr_t old,
			 obj_addr_t new_val)
{
  return __sync_bool_compare_and_swap(addr, old, new_val);
}

// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.
inline static void
read_barrier()
{
  __sync_synchronize();
}

// Ensure that prior stores to memory are completed with respect to other
// processors.
inline static void
write_barrier()
{
  __sync_synchronize();
}
#endif
