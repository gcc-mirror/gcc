/* Copyright (C) 2012-2023 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#if HAVE_IFUNC
#include <cpuid.h>

#ifdef __x86_64__
# define FEAT1_REGISTER ecx
#else
# define FEAT1_REGISTER edx
#endif

/* Value of the CPUID feature register FEAT1_REGISTER for the cmpxchg
   bit for IFUNC_COND1 below.  */
extern unsigned int __libat_feat1 HIDDEN;

/* Initialize libat_feat1 and return its value.  */
unsigned int __libat_feat1_init (void) HIDDEN;

/* Return the value of the relevant feature register for the relevant
   cmpxchg bit, or 0 if there is no CPUID support.  */
static inline unsigned int
__attribute__ ((const))
load_feat1 (void)
{
  /* See the store in __libat_feat1_init.  */
  unsigned int feat1 = __atomic_load_n (&__libat_feat1, __ATOMIC_RELAXED);
  if (feat1 == 0)
    /* Assume that initialization has not happened yet.  This may get
       called repeatedly if the CPU does not have any feature bits at
       all.  */
    feat1 = __libat_feat1_init ();
  return feat1;
}

#ifdef __x86_64__
# define IFUNC_COND_1	((load_feat1 () & (bit_AVX | bit_CMPXCHG16B)) \
			 == (bit_AVX | bit_CMPXCHG16B))
# define IFUNC_COND_2	(load_feat1 () & bit_CMPXCHG16B)
#else
# define IFUNC_COND_1	(load_feat1 () & bit_CMPXCHG8B)
#endif

#ifdef __x86_64__
# define IFUNC_NCOND(N) (2 * (N == 16))
#else
# define IFUNC_NCOND(N) (N == 8)
#endif

#ifdef __x86_64__
# undef MAYBE_HAVE_ATOMIC_CAS_16
# define MAYBE_HAVE_ATOMIC_CAS_16	IFUNC_COND_2
# undef MAYBE_HAVE_ATOMIC_EXCHANGE_16
# define MAYBE_HAVE_ATOMIC_EXCHANGE_16	IFUNC_COND_2
# undef MAYBE_HAVE_ATOMIC_LDST_16
# define MAYBE_HAVE_ATOMIC_LDST_16	IFUNC_COND_2
/* Since load and store are implemented with CAS, they are not fast.  */
# undef FAST_ATOMIC_LDST_16
# define FAST_ATOMIC_LDST_16		0
# if IFUNC_ALT != 0
#  undef HAVE_ATOMIC_CAS_16
#  define HAVE_ATOMIC_CAS_16 1
# endif
# if IFUNC_ALT == 1
#  undef HAVE_ATOMIC_LDST_16
#  define HAVE_ATOMIC_LDST_16 1
# endif
#else
# undef MAYBE_HAVE_ATOMIC_CAS_8
# define MAYBE_HAVE_ATOMIC_CAS_8	IFUNC_COND_1
# undef MAYBE_HAVE_ATOMIC_EXCHANGE_8
# define MAYBE_HAVE_ATOMIC_EXCHANGE_8	IFUNC_COND_1
# undef MAYBE_HAVE_ATOMIC_LDST_8
# define MAYBE_HAVE_ATOMIC_LDST_8	IFUNC_COND_1
# if IFUNC_ALT == 1
#  undef HAVE_ATOMIC_CAS_8
#  define HAVE_ATOMIC_CAS_8 1
# endif
#endif

#if defined(__x86_64__) && N == 16 && IFUNC_ALT != 0
static inline bool
atomic_compare_exchange_n (UTYPE *mptr, UTYPE *eptr, UTYPE newval,
                           bool weak_p UNUSED, int sm UNUSED, int fm UNUSED)
{
  UTYPE cmpval = *eptr;
  UTYPE oldval = __sync_val_compare_and_swap_16 (mptr, cmpval, newval);
  if (oldval == cmpval)
    return true;
  *eptr = oldval;
  return false;
}
# define atomic_compare_exchange_n atomic_compare_exchange_n
#endif /* Have CAS 16 */

#if defined(__x86_64__) && N == 16 && IFUNC_ALT == 1
#define __atomic_load_n(ptr, model) \
  (sizeof (*ptr) == 16 ? atomic_load_n (ptr, model) \
		       : (__atomic_load_n) (ptr, model))
#define __atomic_store_n(ptr, val, model) \
  (sizeof (*ptr) == 16 ? atomic_store_n (ptr, val, model) \
		       : (__atomic_store_n) (ptr, val, model))

static inline UTYPE
atomic_load_n (UTYPE *ptr, int model UNUSED)
{
  UTYPE ret;
  __asm__ ("vmovdqa\t{%1, %0|%0, %1}" : "=x" (ret) : "m" (*ptr));
  return ret;
}

static inline void
atomic_store_n (UTYPE *ptr, UTYPE val, int model UNUSED)
{
  __asm__ ("vmovdqa\t{%1, %0|%0, %1}\n\tmfence" : "=m" (*ptr) : "x" (val));
}
#endif

#endif /* HAVE_IFUNC */

#include_next <host-config.h>
