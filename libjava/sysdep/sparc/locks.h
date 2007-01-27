// locks.h - Thread synchronization primitives. Sparc implementation.

/* Copyright (C) 2002, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/

#ifdef __arch64__
/* Sparc64 implementation, use cas instruction.  */
inline static bool
compare_and_swap(volatile obj_addr_t *addr,
		 obj_addr_t old,
		 obj_addr_t new_val)
{
  __asm__ __volatile__("casx [%2], %3, %0\n\t"
		       "membar #StoreLoad | #StoreStore"
		       : "=&r" (new_val)
		       : "0" (new_val), "r" (addr), "r" (old)
		       : "memory");

  return (new_val == old) ? true : false;
}

inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __asm__ __volatile__("membar #StoreStore | #LoadStore" : : : "memory");
  *(addr) = new_val;
}

inline static bool
compare_and_swap_release(volatile obj_addr_t *addr, obj_addr_t old,
			 obj_addr_t new_val)
{
  return compare_and_swap(addr, old, new_val);
}

inline static void
read_barrier()
{
  __asm__ __volatile__("membar #LoadLoad | #LoadStore" : : : "memory");
}

inline static void
write_barrier()
{
  __asm__ __volatile__("membar #StoreLoad | #StoreStore" : : : "memory");
}
#else
/* Sparc32 implementation, use a spinlock.  */
static unsigned char __cas_lock = 0;

inline static void
__cas_start_atomic(void)
{
  unsigned int tmp;
  __asm__ __volatile__(
"1:	ldstub	[%1], %0\n"
"	orcc	%0, 0x0, %%g0\n"
"	be	3f\n"
"	 nop\n"
"2:	ldub	[%1], %0\n"
"	orcc	%0, 0x0, %%g0\n"
"	bne	2b\n"
"	 nop\n"
"3:"	: "=&r" (tmp)
	: "r" (&__cas_lock)
	: "memory", "cc");
}

inline static void
__cas_end_atomic(void)
{
  __asm__ __volatile__(
  "stb %%g0, [%0]"
  : /* no outputs */
  : "r" (&__cas_lock)
  : "memory");
}

inline static bool
compare_and_swap(volatile obj_addr_t *addr,
		 obj_addr_t old,
		 obj_addr_t new_val)
{
  bool ret;

  __cas_start_atomic ();
  if (*addr != old)
    {
      ret = false;
    }
  else
    {
      *addr = new_val;
      ret = true;
    }
  __cas_end_atomic ();

  return ret;
}

inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  /* Technically stbar would be needed here but no sparc32
     system actually requires it.  Also the stbar would mean
     this code would not work on sparcv7 chips.  */
  __asm__ __volatile__("" : : : "memory");
  *(addr) = new_val;
}

inline static bool
compare_and_swap_release(volatile obj_addr_t *addr, obj_addr_t old,
			 obj_addr_t new_val)
{
  return compare_and_swap(addr, old, new_val);
}

inline static void
read_barrier()
{
  __asm__ __volatile__ ("" : : : "memory");
}

inline static void
write_barrier()
{
  __asm__ __volatile__ ("" : : : "memory");
}
#endif /* __arch64__ */

#endif /* ! __SYSDEP_LOCKS_H__ */
