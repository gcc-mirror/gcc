// locks.h - Thread synchronization primitives. PA-RISC implementation.

/* Copyright (C) 2002, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

// Integer type big enough for object address.
typedef size_t obj_addr_t;

template<int _Inst>
  struct _pa_jv_cas_lock
  {
    static volatile int _S_pa_jv_cas_lock;
  };

template<int _Inst>
volatile int
_pa_jv_cas_lock<_Inst>::_S_pa_jv_cas_lock __attribute__ ((aligned (16))) = 1;

// Because of the lack of weak support when using the hpux som
// linker, we explicitly instantiate the atomicity lock.
template volatile int _pa_jv_cas_lock<0>::_S_pa_jv_cas_lock;

// Atomically replace *addr by new_val if it was initially equal to old_val.
// Return true if the comparison is successful.
// Assumed to have acquire semantics, i.e. later memory operations
// cannot execute before the compare_and_swap finishes.
// The following implementation is atomic but it can deadlock
// (e.g., if a thread dies holding the lock).
inline static bool
__attribute__ ((__unused__))
compare_and_swap(volatile obj_addr_t *addr,
	 	 obj_addr_t old_val,
		 obj_addr_t new_val) 
{
  bool result;
  int tmp;
  volatile int& lock = _pa_jv_cas_lock<0>::_S_pa_jv_cas_lock;

  __asm__ __volatile__ ("ldcw 0(%1),%0\n\t"
			"cmpib,<>,n 0,%0,.+20\n\t"
			"ldw 0(%1),%0\n\t"
			"cmpib,= 0,%0,.-4\n\t"
			"nop\n\t"
			"b,n .-20"
			: "=&r" (tmp)
			: "r" (&lock)
			: "memory");

  if (*addr != old_val)
    result = false;
  else
    {
      *addr = new_val;
      result = true;
    }

  /* Reset lock with PA 2.0 "ordered" store.  */
  __asm__ __volatile__ ("stw,ma %1,0(%0)"
			: : "r" (&lock), "r" (tmp) : "memory");

  return result;
}

// Set *addr to new_val with release semantics, i.e. making sure
// that prior loads and stores complete before this
// assignment.
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
inline static void
read_barrier()
{
  __asm__ __volatile__(" " : : : "memory");
}

// Ensure that prior stores to memory are completed with respect to other
// processors.
inline static void
write_barrier()
{
  __asm__ __volatile__(" " : : : "memory");
}

#endif

