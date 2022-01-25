/* Copyright (C) 2022 Free Software Foundation, Inc.
   Contributed by Mentor, a Siemens Business.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
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

/* This file contains an implementation of GOMP_evaluate_current_device for
   an x86/x64-based Linux host.  */

#include <string.h>
#include "libgomp.h"

bool
GOMP_evaluate_current_device (const char *kind, const char *arch,
			      const char *isa)
{
  if (kind && strcmp (kind, "cpu") != 0)
    return false;

  if (arch
      && strcmp (arch, "x86") != 0
      && strcmp (arch, "ia32") != 0
#ifdef __x86_64__
      && strcmp (arch, "x86_64") != 0
#endif
#ifdef __ILP32__
      && strcmp (arch, "x32") != 0
#endif
      && strcmp (arch, "i386") != 0
      && strcmp (arch, "i486") != 0
#ifndef __i486__
      && strcmp (arch, "i586") != 0
#endif
#if !defined (__i486__) && !defined (__i586__)
      && strcmp (arch, "i686") != 0
#endif
      )
    return false;

  if (!isa)
    return true;

#ifdef __WBNOINVD__
  if (strcmp (isa, "wbnoinvd") == 0) return true;
#endif
#ifdef __AVX512VP2INTERSECT__
  if (strcmp (isa, "avx512vp2intersect") == 0) return true;
#endif
#ifdef __MMX__
  if (strcmp (isa, "mmx") == 0) return true;
#endif
#ifdef __3dNOW__
  if (strcmp (isa, "3dnow") == 0) return true;
#endif
#ifdef __3dNOW_A__
  if (strcmp (isa, "3dnowa") == 0) return true;
#endif
#ifdef __SSE__
  if (strcmp (isa, "sse") == 0) return true;
#endif
#ifdef __SSE2__
  if (strcmp (isa, "sse2") == 0) return true;
#endif
#ifdef __SSE3__
  if (strcmp (isa, "sse3") == 0) return true;
#endif
#ifdef __SSSE3__
  if (strcmp (isa, "ssse3") == 0) return true;
#endif
#ifdef __SSE4_1__
  if (strcmp (isa, "sse4.1") == 0) return true;
#endif
#ifdef __SSE4_2__
  if (strcmp (isa, "sse4") == 0 || strcmp (isa, "sse4.2") == 0) return true;
#endif
#ifdef __AES__
  if (strcmp (isa, "aes") == 0) return true;
#endif
#ifdef __SHA__
  if (strcmp (isa, "sha") == 0) return true;
#endif
#ifdef __PCLMUL__
  if (strcmp (isa, "pclmul") == 0) return true;
#endif
#ifdef __AVX__
  if (strcmp (isa, "avx") == 0) return true;
#endif
#ifdef __AVX2__
  if (strcmp (isa, "avx2") == 0) return true;
#endif
#ifdef __AVX512F__
  if (strcmp (isa, "avx512f") == 0) return true;
#endif
#ifdef __AVX512ER__
  if (strcmp (isa, "avx512er") == 0) return true;
#endif
#ifdef __AVX512CD__
  if (strcmp (isa, "avx512cd") == 0) return true;
#endif
#ifdef __AVX512PF__
  if (strcmp (isa, "avx512pf") == 0) return true;
#endif
#ifdef __AVX512DQ__
  if (strcmp (isa, "avx512dq") == 0) return true;
#endif
#ifdef __AVX512BW__
  if (strcmp (isa, "avx512bw") == 0) return true;
#endif
#ifdef __AVX512VL__
  if (strcmp (isa, "avx512vl") == 0) return true;
#endif
#ifdef __AVX512VBMI__
  if (strcmp (isa, "avx512vbmi") == 0) return true;
#endif
#ifdef __AVX512IFMA__
  if (strcmp (isa, "avx512ifma") == 0) return true;
#endif
#ifdef __AVX5124VNNIW__
  if (strcmp (isa, "avx5124vnniw") == 0) return true;
#endif
#ifdef __AVX512VBMI2__
  if (strcmp (isa, "avx512vbmi2") == 0) return true;
#endif
#ifdef __AVX512VNNI__
  if (strcmp (isa, "avx512vnni") == 0) return true;
#endif
#ifdef __PCONFIG__
  if (strcmp (isa, "pconfig") == 0) return true;
#endif
#ifdef __SGX__
  if (strcmp (isa, "sgx") == 0) return true;
#endif
#ifdef __AVX5124FMAPS__
  if (strcmp (isa, "avx5124fmaps") == 0) return true;
#endif
#ifdef __AVX512BITALG__
  if (strcmp (isa, "avx512bitalg") == 0) return true;
#endif
#ifdef __AVX512VPOPCNTDQ__
  if (strcmp (isa, "avx512vpopcntdq") == 0) return true;
#endif
#ifdef __FMA__
  if (strcmp (isa, "fma") == 0) return true;
#endif
#ifdef __RTM__
  if (strcmp (isa, "rtm") == 0) return true;
#endif
#ifdef __SSE4A__
  if (strcmp (isa, "sse4a") == 0) return true;
#endif
#ifdef __FMA4__
  if (strcmp (isa, "fma4") == 0) return true;
#endif
#ifdef __XOP__
  if (strcmp (isa, "xop") == 0) return true;
#endif
#ifdef __LWP__
  if (strcmp (isa, "lwp") == 0) return true;
#endif
#ifdef __ABM__
  if (strcmp (isa, "abm") == 0) return true;
#endif
#ifdef __BMI__
  if (strcmp (isa, "bmi") == 0) return true;
#endif
#ifdef __BMI2__
  if (strcmp (isa, "bmi2") == 0) return true;
#endif
#ifdef __LZCNT__
  if (strcmp (isa, "lzcnt") == 0) return true;
#endif
#ifdef __TBM__
  if (strcmp (isa, "tbm") == 0) return true;
#endif
#ifdef __CRC32__
  if (strcmp (isa, "crc32") == 0) return true;
#endif
#ifdef __POPCNT__
  if (strcmp (isa, "popcnt") == 0) return true;
#endif
#ifdef __FSGSBASE__
  if (strcmp (isa, "fsgsbase") == 0) return true;
#endif
#ifdef __RDRND__
  if (strcmp (isa, "rdrnd") == 0) return true;
#endif
#ifdef __F16C__
  if (strcmp (isa, "f16c") == 0) return true;
#endif
#ifdef __RDSEED__
  if (strcmp (isa, "rdseed") == 0) return true;
#endif
#ifdef __PRFCHW__
  if (strcmp (isa, "prfchw") == 0) return true;
#endif
#ifdef __ADX__
  if (strcmp (isa, "adx") == 0) return true;
#endif
#ifdef __FXSR__
  if (strcmp (isa, "fxsr") == 0) return true;
#endif
#ifdef __XSAVE__
  if (strcmp (isa, "xsave") == 0) return true;
#endif
#ifdef __XSAVEOPT__
  if (strcmp (isa, "xsaveopt") == 0) return true;
#endif
#ifdef __PREFETCHWT1__
  if (strcmp (isa, "prefetchwt1") == 0) return true;
#endif
#ifdef __CLFLUSHOPT__
  if (strcmp (isa, "clflushopt") == 0) return true;
#endif
#ifdef __CLZERO__
  if (strcmp (isa, "clzero") == 0) return true;
#endif
#ifdef __XSAVEC__
  if (strcmp (isa, "xsavec") == 0) return true;
#endif
#ifdef __XSAVES__
  if (strcmp (isa, "xsaves") == 0) return true;
#endif
#ifdef __CLWB__
  if (strcmp (isa, "clwb") == 0) return true;
#endif
#ifdef __MWAITX__
  if (strcmp (isa, "mwaitx") == 0) return true;
#endif
#ifdef __PKU__
  if (strcmp (isa, "pku") == 0) return true;
#endif
#ifdef __RDPID__
  if (strcmp (isa, "rdpid") == 0) return true;
#endif
#ifdef __GFNI__
  if (strcmp (isa, "gfni") == 0) return true;
#endif
#ifdef __SHSTK__
  if (strcmp (isa, "shstk") == 0) return true;
#endif
#ifdef __VAES__
  if (strcmp (isa, "vaes") == 0) return true;
#endif
#ifdef __VPCLMULQDQ__
  if (strcmp (isa, "vpclmulqdq") == 0) return true;
#endif
#ifdef __MOVDIRI__
  if (strcmp (isa, "movdiri") == 0) return true;
#endif
#ifdef __MOVDIR64B__
  if (strcmp (isa, "movdir64b") == 0) return true;
#endif
#ifdef __WAITPKG__
  if (strcmp (isa, "waitpkg") == 0) return true;
#endif
#ifdef __CLDEMOTE__
  if (strcmp (isa, "cldemote") == 0) return true;
#endif
#ifdef __SERIALIZE__
  if (strcmp (isa, "serialize") == 0) return true;
#endif
#ifdef __PTWRITE__
  if (strcmp (isa, "ptwrite") == 0) return true;
#endif
#ifdef __AVX512BF16__
  if (strcmp (isa, "avx512bf16") == 0) return true;
#endif
#ifdef __AVX512FP16__
  if (strcmp (isa, "avx512fp16") == 0) return true;
#endif
#ifdef __ENQCMD__
  if (strcmp (isa, "enqcmd") == 0) return true;
#endif
#ifdef __TSXLDTRK__
  if (strcmp (isa, "tsxldtrk") == 0) return true;
#endif
#ifdef __AMX_TILE__
  if (strcmp (isa, "amx-tile") == 0) return true;
#endif
#ifdef __AMX_INT8__
  if (strcmp (isa, "amx-int8") == 0) return true;
#endif
#ifdef __AMX_BF16__
  if (strcmp (isa, "amx-bf16") == 0) return true;
#endif
#ifdef __LAHF_SAHF__
  if (strcmp (isa, "sahf") == 0) return true;
#endif
#ifdef __MOVBE__
  if (strcmp (isa, "movbe") == 0) return true;
#endif
#ifdef __UINTR__
  if (strcmp (isa, "uintr") == 0) return true;
#endif
#ifdef __HRESET__
  if (strcmp (isa, "hreset") == 0) return true;
#endif
#ifdef __KL__
  if (strcmp (isa, "kl") == 0) return true;
#endif
#ifdef __WIDEKL__
  if (strcmp (isa, "widekl") == 0) return true;
#endif

  return false;
}
