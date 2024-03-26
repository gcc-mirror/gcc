/* Copyright (C) 2017-2024 Free Software Foundation, Inc.

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
#include <sys/auxv.h>

#ifndef HWCAP_ATOMICS
# define HWCAP_ATOMICS	(1 << 8)
#endif
#ifndef HWCAP_CPUID
# define HWCAP_CPUID	(1 << 11)
#endif
#ifndef HWCAP_USCAT
# define HWCAP_USCAT	(1 << 25)
#endif
#ifndef HWCAP2_LSE128
# define HWCAP2_LSE128	(1UL << 47)
#endif

#if __has_include(<sys/ifunc.h>)
# include <sys/ifunc.h>
#else
typedef struct __ifunc_arg_t {
  unsigned long _size;
  unsigned long _hwcap;
  unsigned long _hwcap2;
} __ifunc_arg_t;
# define _IFUNC_ARG_HWCAP (1ULL << 62)
#endif

#if N == 16
# define IFUNC_COND_1		(has_lse128 (hwcap, features))
# define IFUNC_COND_2		(has_lse2 (hwcap, features))
# define IFUNC_NCOND(N)	2
#else
# define IFUNC_COND_1		(hwcap & HWCAP_ATOMICS)
# define IFUNC_NCOND(N)	1
#endif

#define MIDR_IMPLEMENTOR(midr)	(((midr) >> 24) & 255)
#define MIDR_PARTNUM(midr)	(((midr) >> 4) & 0xfff)

static inline bool
has_lse2 (unsigned long hwcap, const __ifunc_arg_t *features)
{
  /* Check for LSE2.  */
  if (hwcap & HWCAP_USCAT)
    return true;
  /* No point checking further for atomic 128-bit load/store if LSE
     prerequisite not met.  */
  if (!(hwcap & HWCAP_ATOMICS))
    return false;
  if (!(hwcap & HWCAP_CPUID))
    return false;

  unsigned long midr;
  asm volatile ("mrs %0, midr_el1" : "=r" (midr));

  /* Neoverse N1 supports atomic 128-bit load/store.  */
  if (MIDR_IMPLEMENTOR (midr) == 'A' && MIDR_PARTNUM (midr) == 0xd0c)
    return true;

  return false;
}

/* LSE128 atomic support encoded in ID_AA64ISAR0_EL1.Atomic,
   bits[23:20].  The expected value is 0b0011.  Check that.  */

#define AT_FEAT_FIELD(isar0)	(((isar0) >> 20) & 15)

static inline bool
has_lse128 (unsigned long hwcap, const __ifunc_arg_t *features)
{
  if (hwcap & _IFUNC_ARG_HWCAP
      && features->_hwcap2 & HWCAP2_LSE128)
    return true;
  /* A 0 HWCAP2_LSE128 bit may be just as much a sign of missing HWCAP2 bit
     support in older kernels as it is of CPU feature absence.  Try fallback
     method to guarantee LSE128 is not implemented.

     In the absence of HWCAP_CPUID, we are unable to check for LSE128.
     If feature check available, check LSE2 prerequisite before proceeding.  */
  if (!(hwcap & HWCAP_CPUID) || !(hwcap & HWCAP_USCAT))
     return false;

  unsigned long isar0;
  asm volatile ("mrs %0, ID_AA64ISAR0_EL1" : "=r" (isar0));
  if (AT_FEAT_FIELD (isar0) >= 3)
    return true;
  return false;
}

#endif /* HAVE_IFUNC */

/* All 128-bit atomic functions are defined in aarch64/atomic_16.S.  */
#if N == 16
# define DONE 1
# if !HAVE_IFUNC
#  define IFUNC_ALT 1
# endif
#endif

#include_next <host-config.h>
