/* CPU feature detection for AArch64 architecture.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

#if __has_include(<sys/ifunc.h>)
#include <sys/ifunc.h>
#else
typedef struct __ifunc_arg_t {
  unsigned long _size;
  unsigned long _hwcap;
  unsigned long _hwcap2;
} __ifunc_arg_t;
#endif

#if __has_include(<asm/hwcap.h>)
#include <asm/hwcap.h>

/* Architecture features used in Function Multi Versioning.  */
struct {
  unsigned long long features;
  /* As features grows new fields could be added.  */
} __aarch64_cpu_features __attribute__((visibility("hidden"), nocommon));

#ifndef _IFUNC_ARG_HWCAP
#define _IFUNC_ARG_HWCAP (1ULL << 62)
#endif
#ifndef AT_HWCAP
#define AT_HWCAP 16
#endif
#ifndef HWCAP_FP
#define HWCAP_FP (1 << 0)
#endif
#ifndef HWCAP_ASIMD
#define HWCAP_ASIMD (1 << 1)
#endif
#ifndef HWCAP_EVTSTRM
#define HWCAP_EVTSTRM (1 << 2)
#endif
#ifndef HWCAP_AES
#define HWCAP_AES (1 << 3)
#endif
#ifndef HWCAP_PMULL
#define HWCAP_PMULL (1 << 4)
#endif
#ifndef HWCAP_SHA1
#define HWCAP_SHA1 (1 << 5)
#endif
#ifndef HWCAP_SHA2
#define HWCAP_SHA2 (1 << 6)
#endif
#ifndef HWCAP_CRC32
#define HWCAP_CRC32 (1 << 7)
#endif
#ifndef HWCAP_ATOMICS
#define HWCAP_ATOMICS (1 << 8)
#endif
#ifndef HWCAP_FPHP
#define HWCAP_FPHP (1 << 9)
#endif
#ifndef HWCAP_ASIMDHP
#define HWCAP_ASIMDHP (1 << 10)
#endif
#ifndef HWCAP_CPUID
#define HWCAP_CPUID (1 << 11)
#endif
#ifndef HWCAP_ASIMDRDM
#define HWCAP_ASIMDRDM (1 << 12)
#endif
#ifndef HWCAP_JSCVT
#define HWCAP_JSCVT (1 << 13)
#endif
#ifndef HWCAP_FCMA
#define HWCAP_FCMA (1 << 14)
#endif
#ifndef HWCAP_LRCPC
#define HWCAP_LRCPC (1 << 15)
#endif
#ifndef HWCAP_DCPOP
#define HWCAP_DCPOP (1 << 16)
#endif
#ifndef HWCAP_SHA3
#define HWCAP_SHA3 (1 << 17)
#endif
#ifndef HWCAP_SM3
#define HWCAP_SM3 (1 << 18)
#endif
#ifndef HWCAP_SM4
#define HWCAP_SM4 (1 << 19)
#endif
#ifndef HWCAP_ASIMDDP
#define HWCAP_ASIMDDP (1 << 20)
#endif
#ifndef HWCAP_SHA512
#define HWCAP_SHA512 (1 << 21)
#endif
#ifndef HWCAP_SVE
#define HWCAP_SVE (1 << 22)
#endif
#ifndef HWCAP_ASIMDFHM
#define HWCAP_ASIMDFHM (1 << 23)
#endif
#ifndef HWCAP_DIT
#define HWCAP_DIT (1 << 24)
#endif
#ifndef HWCAP_ILRCPC
#define HWCAP_ILRCPC (1 << 26)
#endif
#ifndef HWCAP_FLAGM
#define HWCAP_FLAGM (1 << 27)
#endif
#ifndef HWCAP_SSBS
#define HWCAP_SSBS (1 << 28)
#endif
#ifndef HWCAP_SB
#define HWCAP_SB (1 << 29)
#endif
#ifndef HWCAP_PACA
#define HWCAP_PACA (1 << 30)
#endif
#ifndef HWCAP_PACG
#define HWCAP_PACG (1UL << 31)
#endif

#ifndef AT_HWCAP2
#define AT_HWCAP2 26
#endif
#ifndef HWCAP2_DCPODP
#define HWCAP2_DCPODP (1 << 0)
#endif
#ifndef HWCAP2_SVE2
#define HWCAP2_SVE2 (1 << 1)
#endif
#ifndef HWCAP2_SVEAES
#define HWCAP2_SVEAES (1 << 2)
#endif
#ifndef HWCAP2_SVEPMULL
#define HWCAP2_SVEPMULL (1 << 3)
#endif
#ifndef HWCAP2_SVEBITPERM
#define HWCAP2_SVEBITPERM (1 << 4)
#endif
#ifndef HWCAP2_SVESHA3
#define HWCAP2_SVESHA3 (1 << 5)
#endif
#ifndef HWCAP2_SVESM4
#define HWCAP2_SVESM4 (1 << 6)
#endif
#ifndef HWCAP2_FLAGM2
#define HWCAP2_FLAGM2 (1 << 7)
#endif
#ifndef HWCAP2_FRINT
#define HWCAP2_FRINT (1 << 8)
#endif
#ifndef HWCAP2_SVEI8MM
#define HWCAP2_SVEI8MM (1 << 9)
#endif
#ifndef HWCAP2_SVEF32MM
#define HWCAP2_SVEF32MM (1 << 10)
#endif
#ifndef HWCAP2_SVEF64MM
#define HWCAP2_SVEF64MM (1 << 11)
#endif
#ifndef HWCAP2_SVEBF16
#define HWCAP2_SVEBF16 (1 << 12)
#endif
#ifndef HWCAP2_I8MM
#define HWCAP2_I8MM (1 << 13)
#endif
#ifndef HWCAP2_BF16
#define HWCAP2_BF16 (1 << 14)
#endif
#ifndef HWCAP2_DGH
#define HWCAP2_DGH (1 << 15)
#endif
#ifndef HWCAP2_RNG
#define HWCAP2_RNG (1 << 16)
#endif
#ifndef HWCAP2_BTI
#define HWCAP2_BTI (1 << 17)
#endif
#ifndef HWCAP2_MTE
#define HWCAP2_MTE (1 << 18)
#endif
#ifndef HWCAP2_RPRES
#define HWCAP2_RPRES (1 << 21)
#endif
#ifndef HWCAP2_MTE3
#define HWCAP2_MTE3 (1 << 22)
#endif
#ifndef HWCAP2_SME
#define HWCAP2_SME (1 << 23)
#endif
#ifndef HWCAP2_SME_I16I64
#define HWCAP2_SME_I16I64 (1 << 24)
#endif
#ifndef HWCAP2_SME_F64F64
#define HWCAP2_SME_F64F64 (1 << 25)
#endif
#ifndef HWCAP2_WFXT
#define HWCAP2_WFXT (1UL << 31)
#endif
#ifndef HWCAP2_EBF16
#define HWCAP2_EBF16 (1UL << 32)
#endif
#ifndef HWCAP2_SVE_EBF16
#define HWCAP2_SVE_EBF16 (1UL << 33)
#endif
#ifndef HWCAP2_SME2
#define HWCAP2_SME2 (1UL << 37)
#endif
#ifndef HWCAP2_LRCPC3
#define HWCAP2_LRCPC3	(1UL << 46)
#endif

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
  if (hwcap & _IFUNC_ARG_HWCAP)
    hwcap2 = arg->_hwcap2;
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
  if (hwcap & HWCAP_AES)
    setCPUFeature(FEAT_AES);
  if (hwcap & HWCAP_SHA1)
    setCPUFeature(FEAT_SHA1);
  if (hwcap & HWCAP_SHA2)
    setCPUFeature(FEAT_SHA2);
  if (hwcap & HWCAP_JSCVT)
    setCPUFeature(FEAT_JSCVT);
  if (hwcap & HWCAP_FCMA)
    setCPUFeature(FEAT_FCMA);
  if (hwcap & HWCAP_SB)
    setCPUFeature(FEAT_SB);
  if (hwcap & HWCAP_SSBS)
    {
      setCPUFeature(FEAT_SSBS);
      setCPUFeature(FEAT_SSBS2);
    }
  if (hwcap2 & HWCAP2_MTE)
    {
      setCPUFeature(FEAT_MEMTAG);
      setCPUFeature(FEAT_MEMTAG2);
    }
  if (hwcap2 & HWCAP2_MTE3)
    setCPUFeature(FEAT_MEMTAG3);
  if (hwcap2 & HWCAP2_SVEAES)
    setCPUFeature(FEAT_SVE_AES);
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
  if (hwcap2 & HWCAP2_EBF16)
    setCPUFeature(FEAT_EBF16);
  if (hwcap2 & HWCAP2_SVE_EBF16)
    setCPUFeature(FEAT_SVE_EBF16);
  if (hwcap2 & HWCAP2_DGH)
    setCPUFeature(FEAT_DGH);
  if (hwcap2 & HWCAP2_FRINT)
    setCPUFeature(FEAT_FRINTTS);
  if (hwcap2 & HWCAP2_SVEI8MM)
    setCPUFeature(FEAT_SVE_I8MM);
  if (hwcap2 & HWCAP2_SVEF32MM)
    setCPUFeature(FEAT_SVE_F32MM);
  if (hwcap2 & HWCAP2_SVEF64MM)
    setCPUFeature(FEAT_SVE_F64MM);
  if (hwcap2 & HWCAP2_BTI)
    setCPUFeature(FEAT_BTI);
  if (hwcap2 & HWCAP2_RPRES)
    setCPUFeature(FEAT_RPRES);
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
  if (hwcap & HWCAP_CPUID)
    {
      unsigned long ftr;

      getCPUFeature(ID_AA64ISAR1_EL1, ftr);
      /* ID_AA64ISAR1_EL1.SPECRES >= 0b0001  */
      if (extractBits(ftr, 40, 4) >= 0x1)
	setCPUFeature(FEAT_PREDRES);
      /* ID_AA64ISAR1_EL1.LS64 >= 0b0001  */
      if (extractBits(ftr, 60, 4) >= 0x1)
	setCPUFeature(FEAT_LS64);
      /* ID_AA64ISAR1_EL1.LS64 >= 0b0010  */
      if (extractBits(ftr, 60, 4) >= 0x2)
	setCPUFeature(FEAT_LS64_V);
      /* ID_AA64ISAR1_EL1.LS64 >= 0b0011  */
      if (extractBits(ftr, 60, 4) >= 0x3)
	setCPUFeature(FEAT_LS64_ACCDATA);
    }

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
  if (hwcap2 & HWCAP2_SVEBF16)
    setCPUFeature(FEAT_SVE_BF16);
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

  /* CPU features already initialized.  */
  if (__atomic_load_n (&__aarch64_cpu_features.features, __ATOMIC_RELAXED))
    return;
  hwcap = getauxval(AT_HWCAP);
  hwcap2 = getauxval(AT_HWCAP2);
  __ifunc_arg_t arg;
  arg._size = sizeof(__ifunc_arg_t);
  arg._hwcap = hwcap;
  arg._hwcap2 = hwcap2;
  __init_cpu_features_constructor(hwcap | _IFUNC_ARG_HWCAP, &arg);
#undef extractBits
#undef getCPUFeature
#undef setCPUFeature
}
#endif /* __has_include(<asm/hwcap.h>)  */
#endif /* __has_include(<sys/auxv.h>)  */
