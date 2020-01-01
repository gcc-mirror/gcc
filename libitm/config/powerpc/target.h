/* Copyright (C) 2012-2020 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
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

#ifdef HAVE_SYS_AUXV_H
#include <sys/auxv.h>
#endif

namespace GTM HIDDEN {

typedef int v128 __attribute__((vector_size(16), may_alias, aligned(16)));
typedef struct gtm_jmpbuf
{
#if defined(__ALTIVEC__) || defined(__VSX__)
  v128 vr[12];			/* vr20-vr31 */
  unsigned long long vscr;	/* long long for padding only */
#endif
#ifndef _SOFT_FLOAT
  double fr[18];		/* f14-f31 */
  double fpscr;
#endif
  unsigned long gr[18];		/* r14-r31 */
  void *cfa;
  unsigned long pc;
  unsigned long toc;		/* r2 on aix, r13 on darwin */
  unsigned long cr;
} gtm_jmpbuf;

/* The size of one line in hardware caches (in bytes). */
#if defined (__powerpc64__) || defined (__ppc64__)
# define HW_CACHELINE_SIZE 128
#else
# define HW_CACHELINE_SIZE 32
#endif

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

// Use HTM if it is supported by the system.
// See gtm_thread::begin_transaction for how these functions are used.
#if defined (__linux__) \
    && defined (HAVE_AS_HTM) \
    && defined (HAVE_GETAUXVAL) \
    && defined (AT_HWCAP2) \
    && defined (PPC_FEATURE2_HAS_HTM)

#include <htmintrin.h>

#define USE_HTM_FASTPATH

#define _TBEGIN_STARTED       0
#define _TBEGIN_INDETERMINATE 1
#define _TBEGIN_PERSISTENT    2

/* Number of retries for transient failures.  */
#define _HTM_RETRIES 10

static inline bool
htm_available (void)
{
#ifdef __BUILTIN_CPU_SUPPORTS__
  if (__builtin_cpu_supports ("htm-no-suspend")
      || __builtin_cpu_supports ("htm"))
    return true;
#else
  unsigned long htm_flags = PPC_FEATURE2_HAS_HTM
#ifdef PPC_FEATURE2_HTM_NO_SUSPEND
			    | PPC_FEATURE2_HTM_NO_SUSPEND
#endif
			    | 0;
  if (getauxval (AT_HWCAP2) & htm_flags)
    return true;
#endif
  return false;
}

static inline uint32_t
htm_init (void)
{
  // Maximum number of times we try to execute a transaction
  // as a HW transaction.
  return htm_available () ? _HTM_RETRIES : 0;
}

static inline uint32_t
htm_begin (void)
{
  if (__builtin_expect (__builtin_tbegin (0), 1))
    return _TBEGIN_STARTED;

  if (_TEXASRU_FAILURE_PERSISTENT (__builtin_get_texasru ()))
    return _TBEGIN_PERSISTENT;

  return _TBEGIN_INDETERMINATE;
}

static inline bool
htm_begin_success (uint32_t begin_ret)
{
  return begin_ret == _TBEGIN_STARTED;
}

static inline void
htm_commit (void)
{
  __builtin_tend (0);
}

static inline void
htm_abort (void)
{
  __builtin_tabort (0);
}

static inline bool
htm_abort_should_retry (uint32_t begin_ret)
{
  return begin_ret != _TBEGIN_PERSISTENT;
}

/* Returns true iff a hardware transaction is currently being executed.  */
static inline bool
htm_transaction_active (void)
{
  return (_HTM_STATE (__builtin_ttest ()) == _HTM_TRANSACTIONAL);
}

#endif

} // namespace GTM
