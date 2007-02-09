// locks.h - Thread synchronization primitives. Alpha implementation.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/

inline static bool
compare_and_swap(volatile obj_addr_t *addr,
		  			      obj_addr_t old,
					      obj_addr_t new_val) 
{
  unsigned long oldval;
  char result;
  __asm__ __volatile__(
      "1:ldq_l %0, %1\n\t" \
      "cmpeq %0, %5, %2\n\t" \
      "beq %2, 2f\n\t" \
      "mov %3, %0\n\t" \
      "stq_c %0, %1\n\t" \
      "bne %0, 2f\n\t" \
      "br 1b\n\t" \
      "2:mb"
	      : "=&r"(oldval), "=m"(*addr), "=&r"(result)
	      : "r" (new_val), "m"(*addr), "r"(old) : "memory");
  return (bool) result;
}

inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __asm__ __volatile__("mb" : : : "memory");
  *(addr) = new_val;
}

inline static bool
compare_and_swap_release(volatile obj_addr_t *addr,
		  				     obj_addr_t old,
						     obj_addr_t new_val)
{
  return compare_and_swap(addr, old, new_val);
}

// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.
inline static void
read_barrier()
{
  __asm__ __volatile__("mb" : : : "memory");
}

// Ensure that prior stores to memory are completed with respect to other
// processors.
inline static void
write_barrier()
{
  __asm__ __volatile__("wmb" : : : "memory");
}

#endif
