// locks.h - Thread synchronization primitives. IA64 implementation.

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
  __asm__ __volatile__("mov ar.ccv=%4 ;; cmpxchg8.acq %0=%1,%2,ar.ccv"
	      : "=r"(oldval), "=m"(*addr)
	      : "r"(new_val), "1"(*addr), "r"(old) : "memory");
  return (oldval == old);
}

// The fact that *addr is volatile should cause the compiler to
// automatically generate an st8.rel.
inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __asm__ __volatile__(" " : : : "memory");
  *(addr) = new_val;
}

inline static bool
compare_and_swap_release(volatile obj_addr_t *addr,
	 				             obj_addr_t old,
						     obj_addr_t new_val) 
{
  unsigned long oldval;
  __asm__ __volatile__("mov ar.ccv=%4 ;; cmpxchg8.rel %0=%1,%2,ar.ccv"
	      : "=r"(oldval), "=m"(*addr)
	      : "r"(new_val), "1"(*addr), "r"(old) : "memory");
  return (oldval == old);
}

#endif
