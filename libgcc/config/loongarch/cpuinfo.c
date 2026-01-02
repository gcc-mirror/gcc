/* CPU feature detection for LoongArch architecture.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "common/config/loongarch/cpu-features.h"

#if __has_include(<sys/auxv.h>)
#include <sys/auxv.h>

#define CPUCFG1_LA64	    2ULL << 0
#define CPUCFG1_UAL	    1ULL << 20
#define CPUCFG2_LSX	    1ULL << 6
#define CPUCFG2_LASX	    1ULL << 7
#define CPUCFG2_FRECIPE	    1ULL << 25
#define CPUCFG2_DIV32	    1ULL << 26
#define CPUCFG2_LAM_BH	    1ULL << 27
#define CPUCFG2_LAMCAS	    1ULL << 28
#define CPUCFG2_SCQ	    1ULL << 30
#define CPUCFG3_LD_SEQ_SA   1ULL << 23

struct {
    loongarch_fmv_feature_mask features;
} __loongarch_feature_bits __attribute__ ((visibility ("hidden"), nocommon));

void __init_loongarch_features_resolver (void);
void
__init_loongarch_features_resolver (void)
{
  if (__atomic_load_n (&__loongarch_feature_bits.features, __ATOMIC_RELAXED))
    return;

  loongarch_fmv_feature_mask feat = 0ULL;
#define setCPUFeature(F) feat |= 1ULL << F;
  unsigned int CPUCFG1 = __builtin_loongarch_cpucfg (1);
  unsigned int CPUCFG2 = __builtin_loongarch_cpucfg (2);
  unsigned int CPUCFG3 = __builtin_loongarch_cpucfg (3);
  unsigned long hwcap = getauxval (AT_HWCAP);

  if (CPUCFG1 & CPUCFG1_LA64)
    setCPUFeature (FEAT_LA64);
  if (CPUCFG1 & CPUCFG1_UAL)
    setCPUFeature (FEAT_UAL);
  if (CPUCFG2 & CPUCFG2_FRECIPE)
    setCPUFeature (FEAT_FRECIPE);
  if (CPUCFG2 & CPUCFG2_DIV32)
    setCPUFeature (FEAT_DIV32);
  if (CPUCFG2 & CPUCFG2_LAM_BH)
    setCPUFeature (FEAT_LAM_BH);
  if (CPUCFG2 & CPUCFG2_LAMCAS)
    setCPUFeature (FEAT_LAMCAS);
  if (CPUCFG2 & CPUCFG2_SCQ)
    setCPUFeature (FEAT_SCQ);
  if (CPUCFG3 & CPUCFG3_LD_SEQ_SA)
    setCPUFeature (FEAT_LD_SEQ_SA);

/* The macros HWCAP_LOONGARCH_LSX and HWCAP_LOONGARCH_LASX are not defined
   in glibc versions earlier than 2.38.  If these two macros are not defined,
   define them with reference to asm/hwcap.h.  */
#ifndef HWCAP_LOONGARCH_LSX
#define HWCAP_LOONGARCH_LSX             (1 << 4)
#endif

#ifndef HWCAP_LOONGARCH_LASX
#define HWCAP_LOONGARCH_LASX            (1 << 5)
#endif

  /* LSX and LASX can be disabled/enabled by kernel: on some old kernel
     versions the vector context switch wasn't implemented and so they are
     always disabled, and on Linux >= 6.18-rc1 the user can pass simd=
     parameter via kernel cmdline to disable LSX or LASX for debug or
     powersave purpose:
     https://git.kernel.org/torvalds/c/5dcddd268a8d
     Thus for LSX and LASX HWCAP must be used.*/
  if (hwcap & HWCAP_LOONGARCH_LSX)
    setCPUFeature (FEAT_LSX);
  if (hwcap & HWCAP_LOONGARCH_LASX)
    setCPUFeature (FEAT_LASX);
#undef setCPUFeature
  __atomic_store_n (&__loongarch_feature_bits.features, feat, __ATOMIC_RELAXED);
}
#endif /* __has_include(<sys/auxv.h>)  */
