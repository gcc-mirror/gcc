// locks.h - Thread synchronization primitives. MIPS implementation.

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_LOCKS_H__
#define __SYSDEP_LOCKS_H__

/* Integer type big enough for object address.	*/
typedef unsigned obj_addr_t __attribute__((__mode__(__pointer__)));


// Atomically replace *addr by new_val if it was initially equal to old.
// Return true if the comparison succeeded.
// Assumed to have acquire semantics, i.e. later memory operations
// cannot execute before the compare_and_swap finishes.
inline static bool
compare_and_swap(volatile obj_addr_t *addr,
                 obj_addr_t old,
                 obj_addr_t new_val) 
{
  long result;
  __asm__ __volatile__(".set\tpush\n\t"
                       ".set\tnoreorder\n\t"
                       ".set\tnomacro\n\t"
                       "1:\n\t"
#if _MIPS_SIM == _ABIO32
                       ".set\tmips2\n\t"
#endif
                       "ll\t%[result],0(%[addr])\n\t"
                       "bne\t%[result],%[old],2f\n\t"
                       "move\t%[result],$0\n\t"        // delay slot
                       "move\t%[result],%[new_val]\n\t"
                       "sc\t%[result],0(%[addr])\n\t"
                       "beq\t%[result],$0,1b\n\t"
                       "nop\n\t"                       // delay slot
                       "2:\n\t"
                       ".set\tpop"
          : [result] "=&r" (result)
          : [addr] "r" (addr), [new_val] "r" (new_val), [old] "r"(old)
          : "memory");
  return (bool) result;
}

// Set *addr to new_val with release semantics, i.e. making sure
// that prior loads and stores complete before this
// assignment.
inline static void
release_set(volatile obj_addr_t *addr, obj_addr_t new_val)
{
  __asm__ __volatile__(".set\tpush\n\t"
#if _MIPS_SIM == _ABIO32
                       ".set\tmips2\n\t"
#endif
                       "sync\n\t"
                       ".set\tpop" : : : "memory");
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
  __asm__ __volatile__(".set\tpush\n\t"
#if _MIPS_SIM == _ABIO32
                       ".set\tmips2\n\t"
#endif
                       "sync\n\t"
                       ".set\tpop" : : : "memory");
  return compare_and_swap(addr, old, new_val);
}

// Ensure that subsequent instructions do not execute on stale
// data that was loaded from memory before the barrier.
// On X86, the hardware ensures that reads are properly ordered.
inline static void
read_barrier()
{
  __asm__ __volatile__(".set\tpush\n\t"
#if _MIPS_SIM == _ABIO32
                       ".set\tmips2\n\t"
#endif
                       "sync\n\t"
                       ".set\tpop" : : : "memory");
}

// Ensure that prior stores to memory are completed with respect to other
// processors.
inline static void
write_barrier()
{
  __asm__ __volatile__(".set\tpush\n\t"
#if _MIPS_SIM == _ABIO32
                       ".set\tmips2\n\t"
#endif
                       "sync\n\t"
                       ".set\tpop" : : : "memory");
}

#endif   // __SYSDEP_LOCKS_H__
