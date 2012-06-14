// locks.h - Thread synchronization primitives. SuperH implementation.

/* Copyright (C) 2002, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/

inline static bool
compare_and_swap (volatile obj_addr_t *addr, obj_addr_t old,
		  obj_addr_t new_val)
{
  return __sync_bool_compare_and_swap (addr, old, new_val);
}

inline static void
release_set (volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __asm__ __volatile__ (" " : : : "memory");
  *(addr) = new_val;
}

inline static bool
compare_and_swap_release (volatile obj_addr_t *addr, obj_addr_t old,
			  obj_addr_t new_val)
{
  return compare_and_swap (addr, old, new_val);
}

inline static void
read_barrier()
{
  __asm__ __volatile__(" " : : : "memory");
}

inline static void
write_barrier()
{
  __asm__ __volatile__(" " : : : "memory");
}

#endif /* ! __SYSDEP_LOCKS_H__ */
