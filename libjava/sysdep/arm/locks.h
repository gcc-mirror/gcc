// locks.h - Thread synchronization primitives. ARM implementation.

/* Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

typedef size_t obj_addr_t;	/* Integer type big enough for object	*/
				/* address.				*/
#if (__ARM_EABI__ && __linux)

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
  return __sync_bool_compare_and_swap(addr, old, new_val);
}

// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.
// On X86, the hardware ensures that reads are properly ordered.
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

#else

/* Atomic compare and exchange.  These sequences are not actually
   atomic; there is a race if *ADDR != OLD_VAL and we are preempted
   between the two swaps.  However, they are very close to atomic, and
   are the best that a pre-ARMv6 implementation can do without
   operating system support.  LinuxThreads has been using these
   sequences for many years.  */

inline static bool
compare_and_swap(volatile obj_addr_t *addr,
		 obj_addr_t old_val,
		 obj_addr_t new_val)
{
  volatile obj_addr_t result, tmp;
  __asm__ ("\n"
	   "0:	ldr	%[tmp],[%[addr]]\n"
	   "	cmp	%[tmp],%[old_val]\n"
	   "	movne	%[result],#0\n"
	   "	bne	1f\n"
	   "	swp	%[result],%[new_val],[%[addr]]\n"
	   "	cmp	%[tmp],%[result]\n"
	   "	swpne	%[tmp],%[result],[%[addr]]\n"
	   "	bne	0b\n"
	   "	mov	%[result],#1\n"
	   "1:"
	   : [result] "=&r" (result), [tmp] "=&r" (tmp)
	   : [addr] "r" (addr), [new_val] "r" (new_val), [old_val] "r" (old_val)
	   : "cc", "memory");

  return result;
}

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
  return compare_and_swap(addr, old, new_val);
}

// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.
inline static void
read_barrier()
{
  __asm__ __volatile__("" : : : "memory");
}

// Ensure that prior stores to memory are completed with respect to other
// processors.
inline static void
write_barrier()
{
  __asm__ __volatile__("" : : : "memory");
}

#endif
#endif
