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

static unsigned char __cas_lock = 0;

inline static void
__cas_start_atomic (void)
{
  unsigned int val;

  do
    __asm__ __volatile__ ("tas.b @%1; movt %0"
			  : "=r" (val)
			  : "r" (&__cas_lock)
			  : "memory");
  while (val == 0);
}

inline static void
__cas_end_atomic (void)
{
  __asm__ __volatile__ (" " : : : "memory");
  __cas_lock = 0;
}

inline static bool
compare_and_swap (volatile obj_addr_t *addr, obj_addr_t old,
		  obj_addr_t new_val)
{
  bool ret;

  __cas_start_atomic ();
  if (*addr != old)
    ret = false;
  else
    {
      *addr = new_val;
      ret = true;
    }
  __cas_end_atomic ();

  return ret;
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
