// locks.h - Thread synchronization primitives. IA64 implementation.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

#include <ia64intrin.h>

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/

inline static bool
compare_and_swap(volatile obj_addr_t *addr,
	 				      obj_addr_t old,
					      obj_addr_t new_val) 
{
  return __sync_bool_compare_and_swap (addr, old, new_val);
}

// The fact that *addr is volatile should cause the compiler to
// automatically generate an st8.rel.
inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __asm__ __volatile__("" : : : "memory");
  *(addr) = new_val;
}

inline static bool
compare_and_swap_release(volatile obj_addr_t *addr,
	 				             obj_addr_t old,
						     obj_addr_t new_val) 
{
  register unsigned long ar_ccv __asm__("ar.ccv") = old;
  unsigned long out;
  __asm__ __volatile__("cmpxchg8.rel %0=%1,%2,%4"
	      : "=r"(out), "=m"(*addr)
	      : "r"(new_val), "m"(*addr), "d"(ar_ccv) : "memory");
  return (out == old);
}

#endif
