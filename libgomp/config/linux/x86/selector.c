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

/* The selectors are passed as strings, but are actually sets of multiple
   trait property names, separated by '\0' and with an extra '\0' at
   the end.  Match such a string SELECTORS against an array of strings
   CHOICES, that is terminated by a null pointer.
   matches.  */
static bool
gomp_match_selectors (const char *selectors, const char **choices)
{
  while (*selectors != '\0')
    {
      bool match = false;
      for (int i = 0; !match && choices[i]; i++)
	match = !strcmp (selectors, choices[i]);
      if (!match)
	return false;
      selectors += strlen (selectors) + 1;
    }
  return true;
}

bool
GOMP_evaluate_current_device (const char *kind, const char *arch,
			      const char *isa)
{
  static const char *kind_choices[] = { "cpu", "host", NULL };

  static const char *arch_choices[]
    = { "x86",
	"ia32",
#ifdef __x86_64__
	"x86_64",
#endif
#ifdef __ILP32__
	"x32",
#endif
	"i386",
#ifdef __i486__
	"i486",
#endif
#ifdef __i586__
	"i586",
#endif
#ifdef __i686__
	"i686",
#endif
	NULL };

  static const char *isa_choices[]
    = {
#ifdef __WBNOINVD__
       "wbnoinvd",
#endif
#ifdef __AVX512VP2INTERSECT__
       "avx512vp2intersect",
#endif
#ifdef __MMX__
       "mmx",
#endif
#ifdef __3dNOW__
       "3dnow",
#endif
#ifdef __3dNOW_A__
       "3dnowa",
#endif
#ifdef __SSE__
       "sse",
#endif
#ifdef __SSE2__
       "sse2",
#endif
#ifdef __SSE3__
       "sse3",
#endif
#ifdef __SSSE3__
       "ssse3",
#endif
#ifdef __SSE4_1__
       "sse4.1",
#endif
#ifdef __SSE4_2__
       "sse4",
       "sse4.2",
#endif
#ifdef __AES__
       "aes",
#endif
#ifdef __SHA__
       "sha",
#endif
#ifdef __PCLMUL__
       "pclmul",
#endif
#ifdef __AVX__
       "avx",
#endif
#ifdef __AVX2__
       "avx2",
#endif
#ifdef __AVX512F__
  "avx512f",
#endif
#ifdef __AVX512ER__
       "avx512er",
#endif
#ifdef __AVX512CD__
       "avx512cd",
#endif
#ifdef __AVX512PF__
       "avx512pf",
#endif
#ifdef __AVX512DQ__
       "avx512dq",
#endif
#ifdef __AVX512BW__
       "avx512bw",
#endif
#ifdef __AVX512VL__
       "avx512vl",
#endif
#ifdef __AVX512VBMI__
       "avx512vbmi",
#endif
#ifdef __AVX512IFMA__
       "avx512ifma",
#endif
#ifdef __AVX5124VNNIW__
       "avx5124vnniw",
#endif
#ifdef __AVX512VBMI2__
       "avx512vbmi2",
#endif
#ifdef __AVX512VNNI__
       "avx512vnni",
#endif
#ifdef __PCONFIG__
       "pconfig",
#endif
#ifdef __SGX__
       "sgx",
#endif
#ifdef __AVX5124FMAPS__
       "avx5124fmaps",
#endif
#ifdef __AVX512BITALG__
       "avx512bitalg",
#endif
#ifdef __AVX512VPOPCNTDQ__
       "avx512vpopcntdq",
#endif
#ifdef __FMA__
       "fma",
#endif
#ifdef __RTM__
       "rtm",
#endif
#ifdef __SSE4A__
       "sse4a",
#endif
#ifdef __FMA4__
       "fma4",
#endif
#ifdef __XOP__
       "xop",
#endif
#ifdef __LWP__
       "lwp",
#endif
#ifdef __ABM__
       "abm",
#endif
#ifdef __BMI__
       "bmi",
#endif
#ifdef __BMI2__
       "bmi2",
#endif
#ifdef __LZCNT__
       "lzcnt",
#endif
#ifdef __TBM__
       "tbm",
#endif
#ifdef __CRC32__
       "crc32",
#endif
#ifdef __POPCNT__
       "popcnt",
#endif
#ifdef __FSGSBASE__
       "fsgsbase",
#endif
#ifdef __RDRND__
       "rdrnd",
#endif
#ifdef __F16C__
       "f16c",
#endif
#ifdef __RDSEED__
       "rdseed",
#endif
#ifdef __PRFCHW__
       "prfchw",
#endif
#ifdef __ADX__
       "adx",
#endif
#ifdef __FXSR__
       "fxsr",
#endif
#ifdef __XSAVE__
       "xsave",
#endif
#ifdef __XSAVEOPT__
       "xsaveopt",
#endif
#ifdef __PREFETCHWT1__
       "prefetchwt1",
#endif
#ifdef __CLFLUSHOPT__
       "clflushopt",
#endif
#ifdef __CLZERO__
       "clzero",
#endif
#ifdef __XSAVEC__
       "xsavec",
#endif
#ifdef __XSAVES__
       "xsaves",
#endif
#ifdef __CLWB__
       "clwb",
#endif
#ifdef __MWAITX__
       "mwaitx",
#endif
#ifdef __PKU__
       "pku",
#endif
#ifdef __RDPID__
       "rdpid",
#endif
#ifdef __GFNI__
       "gfni",
#endif
#ifdef __SHSTK__
       "shstk",
#endif
#ifdef __VAES__
       "vaes",
#endif
#ifdef __VPCLMULQDQ__
       "vpclmulqdq",
#endif
#ifdef __MOVDIRI__
       "movdiri",
#endif
#ifdef __MOVDIR64B__
       "movdir64b",
#endif
#ifdef __WAITPKG__
       "waitpkg",
#endif
#ifdef __CLDEMOTE__
       "cldemote",
#endif
#ifdef __SERIALIZE__
       "serialize",
#endif
#ifdef __PTWRITE__
       "ptwrite",
#endif
#ifdef __AVX512BF16__
       "avx512bf16",
#endif
#ifdef __AVX512FP16__
       "avx512fp16",
#endif
#ifdef __ENQCMD__
       "enqcmd",
#endif
#ifdef __TSXLDTRK__
       "tsxldtrk",
#endif
#ifdef __AMX_TILE__
       "amx-tile",
#endif
#ifdef __AMX_INT8__
       "amx-int8",
#endif
#ifdef __AMX_BF16__
       "amx-bf16",
#endif
#ifdef __LAHF_SAHF__
       "sahf",
#endif
#ifdef __MOVBE__
       "movbe",
#endif
#ifdef __UINTR__
       "uintr",
#endif
#ifdef __HRESET__
       "hreset",
#endif
#ifdef __KL__
       "kl",
#endif
#ifdef __WIDEKL__
       "widekl",
#endif
#ifdef __AVXVNNI__
       "avxvnni",
#endif
#ifdef __AVXIFMA_
       "avxifma",_
#endif
#ifdef __AVXVNNIINT8__
       "avxvnniint8",
#endif
#ifdef __AVXNECONVERT__
       "avxneconvert",
#endif
#ifdef __CMPCCXADD__
       "cmpccxadd",
#endif
#ifdef __AMX_FP16__
       "amx-fp16",
#endif
#ifdef __PREFETCHI__
       "prefetchi",
#endif
#ifdef __RAOINT__
       "raoint",
#endif
#ifdef __AMX_COMPLEX__
       "amx-complex",
#endif
#ifdef __AVXVNNIINT16__
       "amxvnniint16",
#endif
#ifdef __SM3__
       "sm3",
#endif
#ifdef __SHA512__
       "sha512",
#endif
#ifdef __SM4__
       "sm4",
#endif
#ifdef __EVEX512__
       "evex512",
#endif
#ifdef __USER_MSR__
       "usermsr",
#endif
#ifdef __AVX10_1_256__
       "avx10.1-256",
#endif
#ifdef __AVX10_1_512__
       "avx10.1-512",
#endif
#ifdef __APX_F__
       "apxf",
#endif
       NULL };

  if (kind && !gomp_match_selectors (kind, kind_choices))
    return false;
  if (arch && !gomp_match_selectors (arch, arch_choices))
    return false;
  if (isa && !gomp_match_selectors (isa, isa_choices))
    return false;
  return true;
}
