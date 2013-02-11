/* Macros for atomic functionality for tile.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */


/* Provides macros for common atomic functionality.  */

#ifndef _ATOMIC_H_
#define _ATOMIC_H_

#ifdef __tilegx__
/* Atomic instruction macros

   The macros provided by atomic.h simplify access to the TILE-Gx
   architecture's atomic instructions.  The architecture provides a
   variety of atomic instructions, including "exchange", "compare and
   exchange", "fetch and ADD", "fetch and AND", "fetch and OR", and
   "fetch and ADD if greater than or equal to zero".

   No barrier or fence semantics are implied by any of the atomic
   instructions for manipulating memory; you must specify the barriers
   that you wish explicitly, using the provided macros.

   Any integral 32- or 64-bit value can be used as the argument
   to these macros, such as "int", "long long", "unsigned long", etc.
   The pointers must be aligned to 4 or 8 bytes for 32- or 64-bit data.
   The "exchange" and "compare and exchange" macros may also take
   pointer values.  We use the pseudo-type "VAL" in the documentation
   to indicate the use of an appropriate type.  */
#else
/* Atomic instruction macros

   The macros provided by atomic.h simplify access to the Tile
   architecture's atomic instructions.  Since the architecture
   supports test-and-set as its only in-silicon atomic operation, many
   of the operations provided by this header are implemented as
   fast-path calls to Linux emulation routines.

   Using the kernel for atomic operations allows userspace to take
   advantage of the kernel's existing atomic-integer support (managed
   by a distributed array of locks).  The kernel provides proper
   ordering among simultaneous atomic operations on different cores,
   and guarantees a process can not be context-switched part way
   through an atomic operation.  By virtue of sharing the kernel
   atomic implementation, the userspace atomic operations
   are compatible with the atomic methods provided by the kernel's
   futex() syscall API.  Note that these operations never cause Linux
   kernel scheduling, and are in fact invisible to the kernel; they
   simply act as regular function calls but with an elevated privilege
   level.  Note that the kernel's distributed lock array is hashed by
   using only VA bits from the atomic value's address (to avoid the
   performance hit of page table locking and multiple page-table
   lookups to get the PA) and only the VA bits that are below page
   granularity (to properly lock simultaneous accesses to the same
   page mapped at different VAs).  As a result, simultaneous atomic
   operations on values whose addresses are at the same offset on a
   page will contend in the kernel for the same lock array element.

   No barrier or fence semantics are implied by any of the atomic
   instructions for manipulating memory; you must specify the barriers
   that you wish explicitly, using the provided macros.

   Any integral 32- or 64-bit value can be used as the argument
   to these macros, such as "int", "long long", "unsigned long", etc.
   The pointers must be aligned to 4 or 8 bytes for 32- or 64-bit data.
   The "exchange" and "compare and exchange" macros may also take
   pointer values.  We use the pseudo-type "VAL" in the documentation
   to indicate the use of an appropriate type.

   The 32-bit routines are implemented using a single kernel fast
   syscall, as is the 64-bit compare-and-exchange.  The other 64-bit
   routines are implemented by looping over the 64-bit
   compare-and-exchange routine, so may be potentially less efficient.  */
#endif

#include <stdint.h>
#include <features.h>
#ifdef __tilegx__
#include <arch/spr_def.h>
#else
#include <asm/unistd.h>
#endif


/* 32-bit integer compare-and-exchange.  */
static __inline __attribute__ ((always_inline))
     int arch_atomic_val_compare_and_exchange_4 (volatile int *mem,
						 int oldval, int newval)
{
#ifdef __tilegx__
  __insn_mtspr (SPR_CMPEXCH_VALUE, oldval);
  return __insn_cmpexch4 (mem, newval);
#else
  int result;
  __asm__ __volatile__ ("swint1":"=R00" (result),
			"=m" (*mem):"R10" (__NR_FAST_cmpxchg), "R00" (mem),
			"R01" (oldval), "R02" (newval), "m" (*mem):"r20",
			"r21", "r22", "r23", "r24", "r25", "r26", "r27",
			"r28", "r29", "memory");
  return result;
#endif
}

/* 64-bit integer compare-and-exchange.  */
static __inline __attribute__ ((always_inline))
     int64_t arch_atomic_val_compare_and_exchange_8 (volatile int64_t * mem,
						     int64_t oldval,
						     int64_t newval)
{
#ifdef __tilegx__
  __insn_mtspr (SPR_CMPEXCH_VALUE, oldval);
  return __insn_cmpexch (mem, newval);
#else
  unsigned int result_lo, result_hi;
  unsigned int oldval_lo = oldval & 0xffffffffu, oldval_hi = oldval >> 32;
  unsigned int newval_lo = newval & 0xffffffffu, newval_hi = newval >> 32;
  __asm__ __volatile__ ("swint1":"=R00" (result_lo), "=R01" (result_hi),
			"=m" (*mem):"R10" (__NR_FAST_cmpxchg64), "R00" (mem),
			"R02" (oldval_lo), "R03" (oldval_hi),
			"R04" (newval_lo), "R05" (newval_hi),
			"m" (*mem):"r20", "r21", "r22", "r23", "r24", "r25",
			"r26", "r27", "r28", "r29", "memory");
  return ((uint64_t) result_hi) << 32 | result_lo;
#endif
}

/* This non-existent symbol is called for sizes other than "4" and "8",
   indicating a bug in the caller.  */
extern int __arch_atomic_error_bad_argument_size (void)
  __attribute__ ((warning ("sizeof atomic argument not 4 or 8")));


#define arch_atomic_val_compare_and_exchange(mem, o, n)                 \
  ({                                                                    \
    (__typeof(*(mem)))(__typeof(*(mem)-*(mem)))                         \
      ((sizeof(*(mem)) == 8) ?                                          \
       arch_atomic_val_compare_and_exchange_8(                          \
         (volatile int64_t*)(mem), (__typeof((o)-(o)))(o),              \
         (__typeof((n)-(n)))(n)) :                                      \
       (sizeof(*(mem)) == 4) ?                                          \
       arch_atomic_val_compare_and_exchange_4(                          \
         (volatile int*)(mem), (__typeof((o)-(o)))(o),                  \
         (__typeof((n)-(n)))(n)) :                                      \
       __arch_atomic_error_bad_argument_size());                        \
  })

#define arch_atomic_bool_compare_and_exchange(mem, o, n)                \
  ({                                                                    \
    __typeof(o) __o = (o);                                              \
    __builtin_expect(                                                   \
      __o == arch_atomic_val_compare_and_exchange((mem), __o, (n)), 1); \
  })


/* Loop with compare_and_exchange until we guess the correct value.
   Normally "expr" will be an expression using __old and __value.  */
#define __arch_atomic_update_cmpxchg(mem, value, expr)                  \
  ({                                                                    \
    __typeof(value) __value = (value);                                  \
    __typeof(*(mem)) *__mem = (mem), __old = *__mem, __guess;           \
    do {                                                                \
      __guess = __old;                                                  \
      __old = arch_atomic_val_compare_and_exchange(__mem, __old, (expr));    \
    } while (__builtin_expect(__old != __guess, 0));                    \
    __old;                                                              \
  })

#ifdef __tilegx__

/* Generic atomic op with 8- or 4-byte variant.
   The _mask, _addend, and _expr arguments are ignored on tilegx.  */
#define __arch_atomic_update(mem, value, op, _mask, _addend, _expr)     \
  ({                                                                    \
    ((__typeof(*(mem)))                                                 \
     ((sizeof(*(mem)) == 8) ? (__typeof(*(mem)-*(mem)))__insn_##op(     \
        (void *)(mem), (int64_t)(__typeof((value)-(value)))(value)) :   \
      (sizeof(*(mem)) == 4) ? (int)__insn_##op##4(                      \
        (void *)(mem), (int32_t)(__typeof((value)-(value)))(value)) :   \
      __arch_atomic_error_bad_argument_size()));                        \
  })

#else

/* This uses TILEPro's fast syscall support to atomically compute:

   int old = *ptr;
   *ptr = (old & mask) + addend;
   return old;

   This primitive can be used for atomic exchange, add, or, and.
   Only 32-bit support is provided.  */
static __inline __attribute__ ((always_inline))
     int
     __arch_atomic_update_4 (volatile int *mem, int mask, int addend)
{
  int result;
  __asm__ __volatile__ ("swint1":"=R00" (result),
			"=m" (*mem):"R10" (__NR_FAST_atomic_update),
			"R00" (mem), "R01" (mask), "R02" (addend),
			"m" (*mem):"r20", "r21", "r22", "r23", "r24", "r25",
			"r26", "r27", "r28", "r29", "memory");
  return result;
}

/* Generic atomic op with 8- or 4-byte variant.
   The _op argument is ignored on tilepro.  */
#define __arch_atomic_update(mem, value, _op, mask, addend, expr)       \
  ({                                                                    \
    (__typeof(*(mem)))(__typeof(*(mem)-*(mem)))                         \
      ((sizeof(*(mem)) == 8) ?                                          \
       __arch_atomic_update_cmpxchg((mem), (value), (expr)) :           \
       (sizeof(*(mem)) == 4) ?                                          \
       __arch_atomic_update_4((volatile int*)(mem),                     \
                              (__typeof((mask)-(mask)))(mask),          \
                              (__typeof((addend)-(addend)))(addend)) :  \
       __arch_atomic_error_bad_argument_size());                        \
  })

#endif /* __tilegx__ */


#define arch_atomic_exchange(mem, newvalue) \
  __arch_atomic_update(mem, newvalue, exch, 0, newvalue, __value)

#define arch_atomic_add(mem, value) \
  __arch_atomic_update(mem, value, fetchadd, -1, value, __old + __value)

#define arch_atomic_sub(mem, value) arch_atomic_add((mem), -(value))

#define arch_atomic_increment(mem) arch_atomic_add((mem), 1)

#define arch_atomic_decrement(mem) arch_atomic_add((mem), -1)

#define arch_atomic_and(mem, mask) \
  __arch_atomic_update(mem, mask, fetchand, mask, 0, __old & __value)

#define arch_atomic_or(mem, mask) \
  __arch_atomic_update(mem, mask, fetchor, ~mask, mask, __old | __value)

#define arch_atomic_xor(mem, mask) \
  __arch_atomic_update_cmpxchg(mem, mask, __old ^ __value)

#define arch_atomic_nand(mem, mask) \
  __arch_atomic_update_cmpxchg(mem, mask, ~(__old & __value))

#define arch_atomic_bit_set(mem, bit)                                   \
  ({                                                                    \
    __typeof(*(mem)) __mask = (__typeof(*(mem)))1 << (bit);             \
    __mask & arch_atomic_or((mem), __mask);                             \
  })

#define arch_atomic_bit_clear(mem, bit)                                 \
  ({                                                                    \
    __typeof(*(mem)) __mask = (__typeof(*(mem)))1 << (bit);             \
    __mask & arch_atomic_and((mem), ~__mask);                           \
  })

#ifdef __tilegx__
/* Atomically store a new value to memory.
   Note that you can freely use types of any size here, unlike the
   other atomic routines, which require 32- or 64-bit types.
   This accessor is provided for compatibility with TILEPro, which
   required an explicit atomic operation for stores that needed
   to be atomic with respect to other atomic methods in this header.  */
#define arch_atomic_write(mem, value) ((void) (*(mem) = (value)))
#else
#define arch_atomic_write(mem, value)                                   \
  do {                                                                  \
    __typeof(mem) __aw_mem = (mem);                                     \
    __typeof(value) __aw_val = (value);                                 \
    unsigned int *__aw_mem32, __aw_intval, __aw_val32, __aw_off, __aw_mask; \
    __aw_intval = (__typeof((value) - (value)))__aw_val;                \
    switch (sizeof(*__aw_mem)) {                                        \
    case 8:                                                             \
      __arch_atomic_update_cmpxchg(__aw_mem, __aw_val, __value);        \
      break;                                                            \
    case 4:                                                             \
      __arch_atomic_update_4((int *)__aw_mem, 0, __aw_intval);          \
      break;                                                            \
    case 2:                                                             \
      __aw_off = 8 * ((long)__aw_mem & 0x2);                            \
      __aw_mask = 0xffffU << __aw_off;                                  \
      __aw_mem32 = (unsigned int *)((long)__aw_mem & ~0x2);             \
      __aw_val32 = (__aw_intval << __aw_off) & __aw_mask;               \
      __arch_atomic_update_cmpxchg(__aw_mem32, __aw_val32,              \
                                   (__old & ~__aw_mask) | __value);     \
      break;                                                            \
    case 1:                                                             \
      __aw_off = 8 * ((long)__aw_mem & 0x3);                            \
      __aw_mask = 0xffU << __aw_off;                                    \
      __aw_mem32 = (unsigned int *)((long)__aw_mem & ~0x3);             \
      __aw_val32 = (__aw_intval << __aw_off) & __aw_mask;               \
      __arch_atomic_update_cmpxchg(__aw_mem32, __aw_val32,              \
                                   (__old & ~__aw_mask) | __value);     \
      break;                                                            \
    }                                                                   \
  } while (0)
#endif

/* Compiler barrier.

   This macro prevents loads or stores from being moved by the compiler
   across the macro.  Any loaded value that was loaded before this
   macro must then be reloaded by the compiler.  */
#define arch_atomic_compiler_barrier() __asm__ __volatile__("" ::: "memory")

/* Full memory barrier.

   This macro has the semantics of arch_atomic_compiler_barrer(), but also
   ensures that previous stores are visible to other cores, and that
   all previous loaded values have been placed into their target
   register on this core.  */
#define arch_atomic_full_barrier() __insn_mf()

/* Read memory barrier.

   Ensure that all reads by this processor that occurred prior to the
   read memory barrier have completed, and that no reads that occur
   after the read memory barrier on this processor are initiated
   before the barrier.

   On current TILE chips a read barrier is implemented as a full barrier,
   but this may not be true in later versions of the architecture.

   See also arch_atomic_acquire_barrier() for the appropriate idiom to use
   to ensure no reads are lifted above an atomic lock instruction.  */
#define arch_atomic_read_barrier() arch_atomic_full_barrier()

/* Write memory barrier.

   Ensure that all writes by this processor that occurred prior to the
   write memory barrier have completed, and that no writes that occur
   after the write memory barrier on this processor are initiated
   before the barrier.

   On current TILE chips a write barrier is implemented as a full barrier,
   but this may not be true in later versions of the architecture.

   See also arch_atomic_release_barrier() for the appropriate idiom to use
   to ensure all writes are complete prior to an atomic unlock instruction.  */
#define arch_atomic_write_barrier() arch_atomic_full_barrier()

/* Lock acquisition barrier.

   Ensure that no load operations that follow this macro in the
   program can issue prior to the barrier.  Without such a barrier,
   the compiler can reorder them to issue earlier, or the hardware can
   issue them speculatively.  The latter is not currently done in the
   Tile microarchitecture, but using this operation improves
   portability to future implementations.

   This operation is intended to be used as part of the "acquire"
   path for locking, that is, when entering a critical section.
   This should be done after the atomic operation that actually
   acquires the lock, and in conjunction with a "control dependency"
   that checks the atomic operation result to see if the lock was
   in fact acquired.  See the arch_atomic_read_barrier() macro
   for a heavier-weight barrier to use in certain unusual constructs,
   or arch_atomic_acquire_barrier_value() if no control dependency exists.  */
#define arch_atomic_acquire_barrier() arch_atomic_compiler_barrier()

/* Lock release barrier.

   Ensure that no store operations that precede this macro in the
   program complete subsequent to the barrier.  Without such a
   barrier, the compiler can reorder stores to issue later, or stores
   can be still outstanding in the memory network.

   This operation is intended to be used as part of the "release" path
   for locking, that is, when leaving a critical section.  This should
   be done before the operation (such as a store of zero) that
   actually releases the lock.  */
#define arch_atomic_release_barrier() arch_atomic_write_barrier()

/* Barrier until the read of a particular value is complete.

   This is occasionally useful when constructing certain locking
   scenarios.  For example, you might write a routine that issues an
   atomic instruction to enter a critical section, then reads one or
   more values within the critical section without checking to see if
   the critical section was in fact acquired, and only later checks
   the atomic instruction result to see if the lock was acquired.  If
   so the routine could properly release the lock and know that the
   values that were read were valid.

   In this scenario, it is required to wait for the result of the
   atomic instruction, even if the value itself is not checked.  This
   guarantees that if the atomic instruction succeeded in taking the lock,
   the lock was held before any reads in the critical section issued.  */
#define arch_atomic_acquire_barrier_value(val) \
  __asm__ __volatile__("move %0, %0" :: "r"(val))

/* Access the given variable in memory exactly once.

   In some contexts, an algorithm may need to force access to memory,
   since otherwise the compiler may think it can optimize away a
   memory load or store; for example, in a loop when polling memory to
   see if another cpu has updated it yet.  Generally this is only
   required for certain very carefully hand-tuned algorithms; using it
   unnecessarily may result in performance losses.

   A related use of this macro is to ensure that the compiler does not
   rematerialize the value of "x" by reloading it from memory
   unexpectedly; the "volatile" marking will prevent the compiler from
   being able to rematerialize.  This is helpful if an algorithm needs
   to read a variable without locking, but needs it to have the same
   value if it ends up being used several times within the algorithm.

   Note that multiple uses of this macro are guaranteed to be ordered,
   i.e. the compiler will not reorder stores or loads that are wrapped
   in arch_atomic_access_once().  */
#define arch_atomic_access_once(x) (*(volatile __typeof(x) *)&(x))



#endif /* !_ATOMIC_H_ */
