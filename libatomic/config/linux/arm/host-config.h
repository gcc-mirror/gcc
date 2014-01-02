/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#include <config/arm/arm-config.h>


/* Kernel helper for 32-bit compare-and-exchange.  */
typedef int (__kernel_cmpxchg_t) (UWORD oldval, UWORD newval, UWORD *ptr);
#define __kernel_cmpxchg (*(__kernel_cmpxchg_t *) 0xffff0fc0)

/* Kernel helper for 64-bit compare-and-exchange.  */
typedef int (__kernel_cmpxchg64_t) (const U_8 * oldval, const U_8 * newval,
				    U_8 *ptr);
#define __kernel_cmpxchg64 (*(__kernel_cmpxchg64_t *) 0xffff0f60)

/* Kernel helper for memory barrier.  */
typedef void (__kernel_dmb_t) (void);
#define __kernel_dmb (*(__kernel_dmb_t *) 0xffff0fa0)

/* Kernel helper page version number.  */
#define __kernel_helper_version (*(unsigned int *)0xffff0ffc)


#ifndef HAVE_STREX
static inline bool
atomic_compare_exchange_w (UWORD *mptr, UWORD *eptr, UWORD newval,
			   bool weak_p UNUSED, int sm UNUSED, int fm UNUSED)
{
  bool ret = true;
  UWORD oldval;

  oldval = *eptr;
  if (__builtin_expect (__kernel_cmpxchg (oldval, newval, mptr) != 0, 0))
    {
      oldval = *mptr;
      ret = false;
    }
  *eptr = oldval;

  return ret;
}
# define atomic_compare_exchange_w atomic_compare_exchange_w
# if N == WORDSIZE
#  define atomic_compare_exchange_n atomic_compare_exchange_w
# endif
#endif /* HAVE_STREX */

#if !defined(HAVE_STREXBHD) && defined(HAVE_KERNEL64) && N == 8
static inline bool
atomic_compare_exchange_n (UTYPE *mptr, UTYPE *eptr, UTYPE newval,
			   bool weak_p UNUSED, int sm UNUSED, int fm UNUSED)
{
  if (__kernel_cmpxchg64 (eptr, &newval, mptr) == 0)
    return true;
  else
    {
      *eptr = *mptr;
      return false;
    }
}
#define atomic_compare_exchange_n atomic_compare_exchange_n
#endif

#if !defined(HAVE_DMB) && !defined(HAVE_DMB_MCR)
static inline void
pre_barrier(int model UNUSED)
{
  __kernel_dmb ();
}

static inline void
post_barrier(int model UNUSED)
{
  __kernel_dmb ();
}
# define pre_post_barrier 1
#endif /* !HAVE_DMB */

#if HAVE_IFUNC
extern bool libat_have_strexbhd HIDDEN;

# define IFUNC_COND_1	libat_have_strexbhd
# define IFUNC_COND_2	(__kernel_helper_version >= 5)

/* Alternative 1 is -march=armv7-a -- we have everything native.  */
# if IFUNC_ALT == 1
#  undef  HAVE_ATOMIC_CAS_1
#  undef  HAVE_ATOMIC_CAS_2
#  undef  HAVE_ATOMIC_CAS_4
#  undef  HAVE_ATOMIC_CAS_8
#  undef  HAVE_ATOMIC_EXCHANGE_1
#  undef  HAVE_ATOMIC_EXCHANGE_2
#  undef  HAVE_ATOMIC_EXCHANGE_4
#  undef  HAVE_ATOMIC_EXCHANGE_8
#  undef  HAVE_ATOMIC_LDST_1
#  undef  HAVE_ATOMIC_LDST_2
#  undef  HAVE_ATOMIC_LDST_4
#  undef  HAVE_ATOMIC_LDST_8
#  undef  HAVE_ATOMIC_FETCH_OP_1
#  undef  HAVE_ATOMIC_FETCH_OP_2
#  undef  HAVE_ATOMIC_FETCH_OP_4
#  undef  HAVE_ATOMIC_FETCH_OP_8
#  undef  HAVE_ATOMIC_TAS_1
#  undef  HAVE_ATOMIC_TAS_2
#  undef  HAVE_ATOMIC_TAS_4
#  undef  HAVE_ATOMIC_TAS_8
#  define HAVE_ATOMIC_CAS_1		1
#  define HAVE_ATOMIC_CAS_2		1
#  define HAVE_ATOMIC_CAS_4		1
#  define HAVE_ATOMIC_CAS_8		1
#  define HAVE_ATOMIC_EXCHANGE_1	1
#  define HAVE_ATOMIC_EXCHANGE_2	1
#  define HAVE_ATOMIC_EXCHANGE_4	1
#  define HAVE_ATOMIC_EXCHANGE_8	1
#  define HAVE_ATOMIC_LDST_1		1
#  define HAVE_ATOMIC_LDST_2		1
#  define HAVE_ATOMIC_LDST_4		1
#  define HAVE_ATOMIC_LDST_8		1
#  define HAVE_ATOMIC_FETCH_OP_1	1
#  define HAVE_ATOMIC_FETCH_OP_2	1
#  define HAVE_ATOMIC_FETCH_OP_4	1
#  define HAVE_ATOMIC_FETCH_OP_8	1
#  define HAVE_ATOMIC_TAS_1		1
#  define HAVE_ATOMIC_TAS_2		1
#  define HAVE_ATOMIC_TAS_4		1
#  define HAVE_ATOMIC_TAS_8		1
# endif /* IFUNC_ALT == 1 */

# undef  MAYBE_HAVE_ATOMIC_CAS_1
# define MAYBE_HAVE_ATOMIC_CAS_1	IFUNC_COND_1
# undef  MAYBE_HAVE_ATOMIC_EXCHANGE_1
# define MAYBE_HAVE_ATOMIC_EXCHANGE_1	MAYBE_HAVE_ATOMIC_CAS_1
# undef  MAYBE_HAVE_ATOMIC_LDST_1
# define MAYBE_HAVE_ATOMIC_LDST_1	MAYBE_HAVE_ATOMIC_CAS_1
# undef  MAYBE_HAVE_ATOMIC_CAS_2
# define MAYBE_HAVE_ATOMIC_CAS_2	IFUNC_COND_1
# undef  MAYBE_HAVE_ATOMIC_EXCHANGE_2
# define MAYBE_HAVE_ATOMIC_EXCHANGE_2	MAYBE_HAVE_ATOMIC_CAS_2
# undef  MAYBE_HAVE_ATOMIC_LDST_2
# define MAYBE_HAVE_ATOMIC_LDST_2	MAYBE_HAVE_ATOMIC_CAS_2
# undef  MAYBE_HAVE_ATOMIC_CAS_4
# define MAYBE_HAVE_ATOMIC_CAS_4	IFUNC_COND_1
# undef  MAYBE_HAVE_ATOMIC_EXCHANGE_4
# define MAYBE_HAVE_ATOMIC_EXCHANGE_4	MAYBE_HAVE_ATOMIC_CAS_4
# undef  MAYBE_HAVE_ATOMIC_LDST_4
# define MAYBE_HAVE_ATOMIC_LDST_4	MAYBE_HAVE_ATOMIC_CAS_4
# undef  MAYBE_HAVE_ATOMIC_CAS_8
# define MAYBE_HAVE_ATOMIC_CAS_8	(IFUNC_COND_1 | IFUNC_COND_2)
# undef  MAYBE_HAVE_ATOMIC_EXCHANGE_8
# define MAYBE_HAVE_ATOMIC_EXCHANGE_8	MAYBE_HAVE_ATOMIC_CAS_8
# undef  MAYBE_HAVE_ATOMIC_LDST_8
# define MAYBE_HAVE_ATOMIC_LDST_8	MAYBE_HAVE_ATOMIC_CAS_8

# define IFUNC_NCOND(N)			(N == 8 ? 2 : 1)

#endif /* HAVE_IFUNC */

#include_next <host-config.h>
