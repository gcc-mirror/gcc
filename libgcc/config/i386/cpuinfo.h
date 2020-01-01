/* Get CPU type and Features for x86 processors.
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
   Contributed by Sriraman Tallam (tmsriram@google.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Processor Vendor and Models. */

enum processor_vendor
{
  VENDOR_INTEL = 1,
  VENDOR_AMD,
  VENDOR_OTHER,
  VENDOR_MAX
};

/* Any new types or subtypes have to be inserted at the end. */

enum processor_types
{
  INTEL_BONNELL = 1,
  INTEL_CORE2,
  INTEL_COREI7,
  AMDFAM10H,
  AMDFAM15H,
  INTEL_SILVERMONT,
  INTEL_KNL,
  AMD_BTVER1,
  AMD_BTVER2,  
  AMDFAM17H,
  INTEL_KNM,
  INTEL_GOLDMONT,
  INTEL_GOLDMONT_PLUS,
  INTEL_TREMONT,
  CPU_TYPE_MAX
};

enum processor_subtypes
{
  INTEL_COREI7_NEHALEM = 1,
  INTEL_COREI7_WESTMERE,
  INTEL_COREI7_SANDYBRIDGE,
  AMDFAM10H_BARCELONA,
  AMDFAM10H_SHANGHAI,
  AMDFAM10H_ISTANBUL,
  AMDFAM15H_BDVER1,
  AMDFAM15H_BDVER2,
  AMDFAM15H_BDVER3,
  AMDFAM15H_BDVER4,
  AMDFAM17H_ZNVER1,
  INTEL_COREI7_IVYBRIDGE,
  INTEL_COREI7_HASWELL,
  INTEL_COREI7_BROADWELL,
  INTEL_COREI7_SKYLAKE,
  INTEL_COREI7_SKYLAKE_AVX512,
  INTEL_COREI7_CANNONLAKE,
  INTEL_COREI7_ICELAKE_CLIENT,
  INTEL_COREI7_ICELAKE_SERVER,
  AMDFAM17H_ZNVER2,
  INTEL_COREI7_CASCADELAKE,
  INTEL_COREI7_TIGERLAKE,
  INTEL_COREI7_COOPERLAKE,
  CPU_SUBTYPE_MAX
};

/* ISA Features supported. New features have to be inserted at the end.  */

enum processor_features
{
  FEATURE_CMOV = 0,
  FEATURE_MMX,
  FEATURE_POPCNT,
  FEATURE_SSE,
  FEATURE_SSE2,
  FEATURE_SSE3,
  FEATURE_SSSE3,
  FEATURE_SSE4_1,
  FEATURE_SSE4_2,
  FEATURE_AVX,
  FEATURE_AVX2,
  FEATURE_SSE4_A,
  FEATURE_FMA4,
  FEATURE_XOP,
  FEATURE_FMA,
  FEATURE_AVX512F,
  FEATURE_BMI,
  FEATURE_BMI2,
  FEATURE_AES,
  FEATURE_PCLMUL,
  FEATURE_AVX512VL,
  FEATURE_AVX512BW,
  FEATURE_AVX512DQ,
  FEATURE_AVX512CD,
  FEATURE_AVX512ER,
  FEATURE_AVX512PF,
  FEATURE_AVX512VBMI,
  FEATURE_AVX512IFMA,
  FEATURE_AVX5124VNNIW,
  FEATURE_AVX5124FMAPS,
  FEATURE_AVX512VPOPCNTDQ,
  FEATURE_AVX512VBMI2,
  FEATURE_GFNI,
  FEATURE_VPCLMULQDQ,
  FEATURE_AVX512VNNI,
  FEATURE_AVX512BITALG,
  FEATURE_AVX512BF16
};

extern struct __processor_model
{
  unsigned int __cpu_vendor;
  unsigned int __cpu_type;
  unsigned int __cpu_subtype;
  unsigned int __cpu_features[1];
} __cpu_model;
extern unsigned int __cpu_features2;
