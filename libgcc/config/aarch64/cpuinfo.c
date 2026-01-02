/* CPU feature detection for AArch64 architecture.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

   This file is part of GCC.

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

#include "common/config/aarch64/cpuinfo.h"

#if __has_include(<sys/auxv.h>)
#include <sys/auxv.h>

/* The following struct is ABI-correct description of the 2nd argument for an
   ifunc resolver as per SYSVABI spec (see link below).  It is safe to extend
   it with new fields.  The ifunc resolver implementations must always check
   the runtime size of the buffer using the value in the _size field.
   https://github.com/ARM-software/abi-aa/blob/main/sysvabi64/sysvabi64.rst.  */
typedef struct __ifunc_arg_t {
  unsigned long _size;
  unsigned long _hwcap;
  unsigned long _hwcap2;
  unsigned long _hwcap3;
  unsigned long _hwcap4;
} __ifunc_arg_t;

/* Architecture features used in Function Multi Versioning.  */
struct {
  unsigned long long features;
  /* As features grows new fields could be added.  */
} __aarch64_cpu_features __attribute__((visibility("hidden"), nocommon));

#define _IFUNC_ARG_HWCAP (1ULL << 62)
#define AT_HWCAP 16
#define AT_HWCAP2 26
#define AT_HWCAP3 29
#define AT_HWCAP4 30

#define HWCAP_FP		(1 << 0)
#define HWCAP_ASIMD		(1 << 1)
#define HWCAP_PMULL		(1 << 4)
#define HWCAP_SHA2		(1 << 6)
#define HWCAP_CRC32		(1 << 7)
#define HWCAP_ATOMICS		(1 << 8)
#define HWCAP_FPHP		(1 << 9)
#define HWCAP_ASIMDHP		(1 << 10)
#define HWCAP_ASIMDRDM		(1 << 12)
#define HWCAP_JSCVT		(1 << 13)
#define HWCAP_FCMA		(1 << 14)
#define HWCAP_LRCPC		(1 << 15)
#define HWCAP_DCPOP		(1 << 16)
#define HWCAP_SHA3		(1 << 17)
#define HWCAP_SM3		(1 << 18)
#define HWCAP_SM4		(1 << 19)
#define HWCAP_ASIMDDP		(1 << 20)
#define HWCAP_SVE		(1 << 22)
#define HWCAP_ASIMDFHM		(1 << 23)
#define HWCAP_DIT		(1 << 24)
#define HWCAP_ILRCPC		(1 << 26)
#define HWCAP_FLAGM		(1 << 27)
#define HWCAP_SSBS		(1 << 28)
#define HWCAP_SB		(1 << 29)

#define HWCAP2_DCPODP		(1 << 0)
#define HWCAP2_SVE2		(1 << 1)
#define HWCAP2_SVEPMULL		(1 << 3)
#define HWCAP2_SVEBITPERM	(1 << 4)
#define HWCAP2_SVESHA3		(1 << 5)
#define HWCAP2_SVESM4		(1 << 6)
#define HWCAP2_FLAGM2		(1 << 7)
#define HWCAP2_FRINT		(1 << 8)
#define HWCAP2_SVEF32MM		(1 << 10)
#define HWCAP2_SVEF64MM		(1 << 11)
#define HWCAP2_I8MM		(1 << 13)
#define HWCAP2_BF16		(1 << 14)
#define HWCAP2_RNG		(1 << 16)
#define HWCAP2_BTI		(1 << 17)
#define HWCAP2_MTE		(1 << 18)
#define HWCAP2_SME		(1 << 23)
#define HWCAP2_SME_I16I64	(1 << 24)
#define HWCAP2_SME_F64F64	(1 << 25)
#define HWCAP2_WFXT		(1UL << 31)
#define HWCAP2_CSSC		(1UL << 34)
#define HWCAP2_SME2		(1UL << 37)
#define HWCAP2_MOPS		(1UL << 43)
#define HWCAP2_LRCPC3		(1UL << 46)

#define __IFUNC_ARG_SIZE_HWCAP2 (sizeof (unsigned long) * 3)
#define __IFUNC_ARG_SIZE_HWCAP3 (sizeof (unsigned long) * 4)
#define __IFUNC_ARG_SIZE_HWCAP4 (sizeof (unsigned long) * 5)

static void
__init_cpu_features_constructor (unsigned long hwcap,
				 const __ifunc_arg_t *arg)
{
  unsigned long feat = 0;
#define setCPUFeature(F) feat |= 1UL << F
#define getCPUFeature(id, ftr) __asm__("mrs %0, " #id : "=r"(ftr))
#define extractBits(val, start, number) \
  (val & ((1UL << number) - 1UL) << start) >> start
  unsigned long hwcap2 = 0;
  if ((hwcap & _IFUNC_ARG_HWCAP) && arg->_size >= __IFUNC_ARG_SIZE_HWCAP2)
    hwcap2 = arg->_hwcap2;
  unsigned long hwcap3 __attribute__ ((unused)) = 0;
  if ((hwcap & _IFUNC_ARG_HWCAP) && arg->_size >= __IFUNC_ARG_SIZE_HWCAP3)
    hwcap3 = arg->_hwcap3;
  unsigned long hwcap4 __attribute__ ((unused)) = 0;
  if ((hwcap & _IFUNC_ARG_HWCAP) && arg->_size >= __IFUNC_ARG_SIZE_HWCAP4)
    hwcap4 = arg->_hwcap4;
  if (hwcap & HWCAP_CRC32)
    setCPUFeature(FEAT_CRC);
  if (hwcap & HWCAP_PMULL)
    setCPUFeature(FEAT_PMULL);
  if (hwcap & HWCAP_FLAGM)
    setCPUFeature(FEAT_FLAGM);
  if (hwcap2 & HWCAP2_FLAGM2)
    setCPUFeature(FEAT_FLAGM2);
  if (hwcap & HWCAP_SM4)
    setCPUFeature(FEAT_SM4);
  if (hwcap & HWCAP_ASIMDDP)
    setCPUFeature(FEAT_DOTPROD);
  if (hwcap & HWCAP_ASIMDFHM)
    setCPUFeature(FEAT_FP16FML);
  if (hwcap & HWCAP_FPHP)
    setCPUFeature(FEAT_FP16);
  if (hwcap & HWCAP_DIT)
    setCPUFeature(FEAT_DIT);
  if (hwcap & HWCAP_ASIMDRDM)
    setCPUFeature(FEAT_RDM);
  if (hwcap & HWCAP_SHA2)
    setCPUFeature(FEAT_SHA2);
  if (hwcap & HWCAP_JSCVT)
    setCPUFeature(FEAT_JSCVT);
  if (hwcap & HWCAP_FCMA)
    setCPUFeature(FEAT_FCMA);
  if (hwcap & HWCAP_SB)
    setCPUFeature(FEAT_SB);
  if (hwcap & HWCAP_SSBS)
    setCPUFeature(FEAT_SSBS2);
  if (hwcap2 & HWCAP2_MTE)
    setCPUFeature(FEAT_MEMTAG2);
  if (hwcap2 & HWCAP2_SVEPMULL)
    setCPUFeature(FEAT_SVE_PMULL128);
  if (hwcap2 & HWCAP2_SVEBITPERM)
    setCPUFeature(FEAT_SVE_BITPERM);
  if (hwcap2 & HWCAP2_SVESHA3)
    setCPUFeature(FEAT_SVE_SHA3);
  if (hwcap2 & HWCAP2_SVESM4)
    setCPUFeature(FEAT_SVE_SM4);
  if (hwcap2 & HWCAP2_DCPODP)
    setCPUFeature(FEAT_DPB2);
  if (hwcap & HWCAP_ATOMICS)
    setCPUFeature(FEAT_LSE);
  if (hwcap2 & HWCAP2_RNG)
    setCPUFeature(FEAT_RNG);
  if (hwcap2 & HWCAP2_I8MM)
    setCPUFeature(FEAT_I8MM);
  if (hwcap2 & HWCAP2_FRINT)
    setCPUFeature(FEAT_FRINTTS);
  if (hwcap2 & HWCAP2_SVEF32MM)
    setCPUFeature(FEAT_SVE_F32MM);
  if (hwcap2 & HWCAP2_SVEF64MM)
    setCPUFeature(FEAT_SVE_F64MM);
  if (hwcap2 & HWCAP2_BTI)
    setCPUFeature(FEAT_BTI);
  if (hwcap2 & HWCAP2_WFXT)
    setCPUFeature(FEAT_WFXT);
  if (hwcap2 & HWCAP2_SME)
    setCPUFeature(FEAT_SME);
  if (hwcap2 & HWCAP2_SME2)
    setCPUFeature(FEAT_SME2);
  if (hwcap2 & HWCAP2_SME_I16I64)
    setCPUFeature(FEAT_SME_I64);
  if (hwcap2 & HWCAP2_SME_F64F64)
    setCPUFeature(FEAT_SME_F64);
  if (hwcap2 & HWCAP2_MOPS)
    setCPUFeature(FEAT_MOPS);
  if (hwcap2 & HWCAP2_CSSC)
    setCPUFeature(FEAT_CSSC);
  if (hwcap & HWCAP_FP)
    {
      setCPUFeature(FEAT_FP);
      /* FP and AdvSIMD fields have the same value.  */
      setCPUFeature(FEAT_SIMD);
    }
  if (hwcap & HWCAP_DCPOP)
    setCPUFeature(FEAT_DPB);
  if (hwcap & HWCAP_LRCPC)
    setCPUFeature(FEAT_RCPC);
  if (hwcap & HWCAP_ILRCPC)
    setCPUFeature(FEAT_RCPC2);
  if (hwcap2 & HWCAP2_LRCPC3)
    setCPUFeature(FEAT_RCPC3);
  if (hwcap2 & HWCAP2_BF16)
    setCPUFeature(FEAT_BF16);
  if (hwcap & HWCAP_SVE)
    setCPUFeature(FEAT_SVE);
  if (hwcap2 & HWCAP2_SVE2)
    setCPUFeature(FEAT_SVE2);
  if (hwcap & HWCAP_SHA3)
    setCPUFeature(FEAT_SHA3);
  setCPUFeature(FEAT_INIT);

  __atomic_store_n (&__aarch64_cpu_features.features, feat, __ATOMIC_RELAXED);
}

void __init_cpu_features_resolver(unsigned long, const __ifunc_arg_t *);
void
__init_cpu_features_resolver(unsigned long hwcap, const __ifunc_arg_t *arg)
{
  if (__atomic_load_n (&__aarch64_cpu_features.features, __ATOMIC_RELAXED))
    return;
  __init_cpu_features_constructor(hwcap, arg);
}

void __init_cpu_features(void);
void __attribute__ ((constructor))
__init_cpu_features(void)
{
  unsigned long hwcap;
  unsigned long hwcap2;
  unsigned long hwcap3;
  unsigned long hwcap4;

  /* CPU features already initialized.  */
  if (__atomic_load_n (&__aarch64_cpu_features.features, __ATOMIC_RELAXED))
    return;
  hwcap = getauxval (AT_HWCAP);
  hwcap2 = getauxval (AT_HWCAP2);
  hwcap3 = getauxval (AT_HWCAP3);
  hwcap4 = getauxval (AT_HWCAP4);

  __ifunc_arg_t arg;
  arg._size = sizeof (__ifunc_arg_t);
  arg._hwcap = hwcap;
  arg._hwcap2 = hwcap2;
  arg._hwcap3 = hwcap3;
  arg._hwcap4 = hwcap4;
  __init_cpu_features_constructor (hwcap | _IFUNC_ARG_HWCAP, &arg);
#undef extractBits
#undef getCPUFeature
#undef setCPUFeature
}
#endif /* __has_include(<sys/auxv.h>)  */
