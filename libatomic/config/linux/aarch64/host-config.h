/* Copyright (C) 2017-2025 Free Software Foundation, Inc.

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
#ifndef HWCAP2_LRCPC3
# define HWCAP2_LRCPC3	(1UL << 46)
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

/* From the file which imported `host-config.h' we can ascertain which
   architectural extension provides relevant atomic support.  From this,
   we can proceed to tweak the ifunc selector behavior.  */
#if defined (LAT_CAS_N)
# define LSE_ATOP
#elif defined (LAT_LOAD_N) || defined (LAT_STORE_N)
# define LSE2_LRCPC3_ATOP
#elif defined (LAT_EXCH_N) || defined (LAT_FIOR_N) || defined (LAT_FAND_N)
# define LSE128_ATOP
#endif

# if N == 16
#  if defined (LSE_ATOP)
#   define IFUNC_NCOND(N)	1
#   define IFUNC_COND_1	(hwcap & HWCAP_ATOMICS)
#  elif defined (LSE2_LRCPC3_ATOP)
#   define IFUNC_NCOND(N)	2
#   define IFUNC_COND_1	(has_rcpc3 (hwcap, features))
#   define IFUNC_COND_2	(has_lse2 (hwcap, features))
#  elif defined (LSE128_ATOP)
#   define IFUNC_NCOND(N)	1
#   define IFUNC_COND_1	(has_lse128 (hwcap, features))
#  else
#   define IFUNC_NCOND(N)	0
#   define IFUNC_ALT		1
#  endif
# else
#  define IFUNC_COND_1		(hwcap & HWCAP_ATOMICS)
#  define IFUNC_NCOND(N)	1
# endif

#define MIDR_IMPLEMENTOR(midr)	(((midr) >> 24) & 255)
#define MIDR_PARTNUM(midr)	(((midr) >> 4) & 0xfff)

static inline bool
has_lse2 (unsigned long hwcap, const __ifunc_arg_t *features)
{
  /* Check for LSE2.  */
  if (hwcap & HWCAP_USCAT)
    return true;

  /* If LSE and CPUID are supported, check MIDR.  */
  if (hwcap & HWCAP_CPUID && hwcap & HWCAP_ATOMICS)
    {
      unsigned long midr;
      asm volatile ("mrs %0, midr_el1" : "=r" (midr));

      /* Neoverse N1 supports atomic 128-bit load/store.  */
      return MIDR_IMPLEMENTOR (midr) == 'A' && MIDR_PARTNUM (midr) == 0xd0c;
    }

  return false;
}

/* LSE128 atomic support encoded in ID_AA64ISAR0_EL1.Atomic, bits[23:20].
   The minimum value for LSE128 is 0b0011.  */

#define AT_FEAT_FIELD(isar0)	(((isar0) >> 20) & 15)

static inline bool
has_lse128 (unsigned long hwcap, const __ifunc_arg_t *features)
{
  if (hwcap & _IFUNC_ARG_HWCAP && features->_hwcap2 & HWCAP2_LSE128)
    return true;

  /* If LSE2 and CPUID are supported, check for LSE128.  */
  if (hwcap & HWCAP_CPUID && hwcap & HWCAP_USCAT)
    {
      unsigned long isar0;
      asm volatile ("mrs %0, ID_AA64ISAR0_EL1" : "=r" (isar0));
      return AT_FEAT_FIELD (isar0) >= 3;
    }

  return false;
}

/* LRCPC atomic support encoded in ID_AA64ISAR1_EL1.Atomic, bits[23:20].
   The minimum value for LRCPC3 is 0b0011.  */

static inline bool
has_rcpc3 (unsigned long hwcap, const __ifunc_arg_t *features)
{
  /* LSE2 is a prerequisite for atomic LDIAPP/STILP - check HWCAP_USCAT since
     has_lse2 is more expensive and Neoverse N1 does not have LRCPC3. */
  if (!(hwcap & HWCAP_USCAT))
    return false;

  if (hwcap & _IFUNC_ARG_HWCAP && features->_hwcap2 & HWCAP2_LRCPC3)
    return true;

  if (hwcap & HWCAP_CPUID)
    {
      unsigned long isar1;
      asm volatile ("mrs %0, ID_AA64ISAR1_EL1" : "=r" (isar1));
      return AT_FEAT_FIELD (isar1) >= 3;
    }

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
