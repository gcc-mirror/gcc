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

#include "cpuid.h"
#include "tsystem.h"
#include "auto-target.h"
#include "cpuinfo.h"

#ifdef HAVE_INIT_PRIORITY
#define CONSTRUCTOR_PRIORITY (101)
#else
#define CONSTRUCTOR_PRIORITY
#endif

int __cpu_indicator_init (void)
  __attribute__ ((constructor CONSTRUCTOR_PRIORITY));


struct __processor_model __cpu_model = { };
#ifndef SHARED
/* We want to move away from __cpu_model in libgcc_s.so.1 and the
   size of __cpu_model is part of ABI.  So, new features that don't
   fit into __cpu_model.__cpu_features[0] go into extra variables
   in libgcc.a only, preferrably hidden.  */
unsigned int __cpu_features2;
#endif


/* Get the specific type of AMD CPU.  */

static void
get_amd_cpu (unsigned int family, unsigned int model)
{
  switch (family)
    {
    /* AMD Family 10h.  */
    case 0x10:
      __cpu_model.__cpu_type = AMDFAM10H;
      switch (model)
	{
	case 0x2:
	  /* Barcelona.  */
	  __cpu_model.__cpu_subtype = AMDFAM10H_BARCELONA;
	  break;
	case 0x4:
	  /* Shanghai.  */
	  __cpu_model.__cpu_subtype = AMDFAM10H_SHANGHAI;
	  break;
	case 0x8:
	  /* Istanbul.  */
	  __cpu_model.__cpu_subtype = AMDFAM10H_ISTANBUL;
	  break;
	default:
	  break;
	}
      break;
    /* AMD Family 14h "btver1". */
    case 0x14:
      __cpu_model.__cpu_type = AMD_BTVER1;
      break;
    /* AMD Family 15h "Bulldozer".  */
    case 0x15:
      __cpu_model.__cpu_type = AMDFAM15H;

      if (model == 0x2)
	__cpu_model.__cpu_subtype = AMDFAM15H_BDVER2;      
      /* Bulldozer version 1.  */
      else if (model <= 0xf)
	__cpu_model.__cpu_subtype = AMDFAM15H_BDVER1;
      /* Bulldozer version 2 "Piledriver" */
      else if (model <= 0x2f)
	__cpu_model.__cpu_subtype = AMDFAM15H_BDVER2;      
      /* Bulldozer version 3 "Steamroller"  */
      else if (model <= 0x4f)
	__cpu_model.__cpu_subtype = AMDFAM15H_BDVER3;
      /* Bulldozer version 4 "Excavator"   */
      else if (model <= 0x7f)
	__cpu_model.__cpu_subtype = AMDFAM15H_BDVER4;
      break;
    /* AMD Family 16h "btver2" */
    case 0x16:
      __cpu_model.__cpu_type = AMD_BTVER2;
      break;
    case 0x17:
      __cpu_model.__cpu_type = AMDFAM17H;
      /* AMD family 17h version 1.  */
      if (model <= 0x1f)
	__cpu_model.__cpu_subtype = AMDFAM17H_ZNVER1;
      if (model >= 0x30)
	 __cpu_model.__cpu_subtype = AMDFAM17H_ZNVER2;
      break;
    case 0x19:
      __cpu_model.__cpu_type = AMDFAM19H;
      /* AMD family 19h version 1.  */
      if (model <= 0x0f)
	__cpu_model.__cpu_subtype = AMDFAM19H_ZNVER3;
      break;
    default:
      break;
    }
}

/* Get the specific type of Intel CPU.  */

static void
get_intel_cpu (unsigned int family, unsigned int model, unsigned int brand_id)
{
  /* Parse family and model only if brand ID is 0. */
  if (brand_id == 0)
    {
      switch (family)
	{
	case 0x5:
	  /* Pentium.  */
	  break;
	case 0x6:
	  switch (model)
	    {
	    case 0x1c:
	    case 0x26:
	      /* Bonnell.  */
	      __cpu_model.__cpu_type = INTEL_BONNELL;
	      break;
	    case 0x37:
	    case 0x4a:
	    case 0x4d:
	    case 0x5a:
	    case 0x5d:
	      /* Silvermont.  */
	      __cpu_model.__cpu_type = INTEL_SILVERMONT;
	      break;
	    case 0x5c:
	    case 0x5f:
	      /* Goldmont.  */
	      __cpu_model.__cpu_type = INTEL_GOLDMONT;
	      break;
	    case 0x7a:
	      /* Goldmont Plus.  */
	      __cpu_model.__cpu_type = INTEL_GOLDMONT_PLUS;
	      break;
	    case 0x57:
	      /* Knights Landing.  */
	      __cpu_model.__cpu_type = INTEL_KNL;
	      break;
	    case 0x85:
	      /* Knights Mill. */
	      __cpu_model.__cpu_type = INTEL_KNM;
	      break;
	    case 0x1a:
	    case 0x1e:
	    case 0x1f:
	    case 0x2e:
	      /* Nehalem.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_NEHALEM;
	      break;
	    case 0x25:
	    case 0x2c:
	    case 0x2f:
	      /* Westmere.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_WESTMERE;
	      break;
	    case 0x2a:
	    case 0x2d:
	      /* Sandy Bridge.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_SANDYBRIDGE;
	      break;
	    case 0x3a:
	    case 0x3e:
	      /* Ivy Bridge.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_IVYBRIDGE;
	      break;
	    case 0x3c:
	    case 0x3f:
	    case 0x45:
	    case 0x46:
	      /* Haswell.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_HASWELL;
	      break;
	    case 0x3d:
	    case 0x47:
	    case 0x4f:
	    case 0x56:
	      /* Broadwell.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_BROADWELL;
	      break;
	    case 0x4e:
	    case 0x5e:
	      /* Skylake.  */
	    case 0x8e:
	    case 0x9e:
	      /* Kaby Lake.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_SKYLAKE;
	      break;
	    case 0x55:
	      {
	        unsigned int eax, ebx, ecx, edx;
	        __cpu_model.__cpu_type = INTEL_COREI7;
	        __cpuid_count (7, 0, eax, ebx, ecx, edx);
	        if (ecx & bit_AVX512VNNI)
	          /* Cascade Lake.  */
	          __cpu_model.__cpu_subtype = INTEL_COREI7_CASCADELAKE;
	        else
	          /* Skylake with AVX-512 support.  */
	          __cpu_model.__cpu_subtype = INTEL_COREI7_SKYLAKE_AVX512;
	      }
	      break;
	    case 0x66:
	      /* Cannon Lake.  */
	      __cpu_model.__cpu_type = INTEL_COREI7;
	      __cpu_model.__cpu_subtype = INTEL_COREI7_CANNONLAKE;
	      break;
	    case 0x17:
	    case 0x1d:
	      /* Penryn.  */
	    case 0x0f:
	      /* Merom.  */
	      __cpu_model.__cpu_type = INTEL_CORE2;
	      break;
	    default:
	      break;
	    }
	  break;
	default:
	  /* We have no idea.  */
	  break;
	}
    }
}	             	

/* ECX and EDX are output of CPUID at level one.  MAX_CPUID_LEVEL is
   the max possible level of CPUID insn.  */
static void
get_available_features (unsigned int ecx, unsigned int edx,
			int max_cpuid_level)
{
  unsigned int eax, ebx;
  unsigned int ext_level;

  unsigned int features = 0;
  unsigned int features2 = 0;

  /* Get XCR_XFEATURE_ENABLED_MASK register with xgetbv.  */
#define XCR_XFEATURE_ENABLED_MASK	0x0
#define XSTATE_FP			0x1
#define XSTATE_SSE			0x2
#define XSTATE_YMM			0x4
#define XSTATE_OPMASK			0x20
#define XSTATE_ZMM			0x40
#define XSTATE_HI_ZMM			0x80

#define XCR_AVX_ENABLED_MASK \
  (XSTATE_SSE | XSTATE_YMM)
#define XCR_AVX512F_ENABLED_MASK \
  (XSTATE_SSE | XSTATE_YMM | XSTATE_OPMASK | XSTATE_ZMM | XSTATE_HI_ZMM)

  /* Check if AVX and AVX512 are usable.  */
  int avx_usable = 0;
  int avx512_usable = 0;
  if ((ecx & bit_OSXSAVE))
    {
      /* Check if XMM, YMM, OPMASK, upper 256 bits of ZMM0-ZMM15 and
         ZMM16-ZMM31 states are supported by OSXSAVE.  */
      unsigned int xcrlow;
      unsigned int xcrhigh;
      asm (".byte 0x0f, 0x01, 0xd0"
	   : "=a" (xcrlow), "=d" (xcrhigh)
	   : "c" (XCR_XFEATURE_ENABLED_MASK));
      if ((xcrlow & XCR_AVX_ENABLED_MASK) == XCR_AVX_ENABLED_MASK)
	{
	  avx_usable = 1;
	  avx512_usable = ((xcrlow & XCR_AVX512F_ENABLED_MASK)
			   == XCR_AVX512F_ENABLED_MASK);
	}
    }

#define set_feature(f) \
  do						\
    {						\
      if (f < 32)				\
	features |= (1U << (f & 31));		\
      else					\
	features2 |= (1U << ((f - 32) & 31));	\
    }						\
  while (0)

  if (edx & bit_CMOV)
    set_feature (FEATURE_CMOV);
  if (edx & bit_MMX)
    set_feature (FEATURE_MMX);
  if (edx & bit_SSE)
    set_feature (FEATURE_SSE);
  if (edx & bit_SSE2)
    set_feature (FEATURE_SSE2);
  if (ecx & bit_POPCNT)
    set_feature (FEATURE_POPCNT);
  if (ecx & bit_AES)
    set_feature (FEATURE_AES);
  if (ecx & bit_PCLMUL)
    set_feature (FEATURE_PCLMUL);
  if (ecx & bit_SSE3)
    set_feature (FEATURE_SSE3);
  if (ecx & bit_SSSE3)
    set_feature (FEATURE_SSSE3);
  if (ecx & bit_SSE4_1)
    set_feature (FEATURE_SSE4_1);
  if (ecx & bit_SSE4_2)
    set_feature (FEATURE_SSE4_2);
  if (avx_usable)
    {
      if (ecx & bit_AVX)
	set_feature (FEATURE_AVX);
      if (ecx & bit_FMA)
	set_feature (FEATURE_FMA);
    }

  /* Get Advanced Features at level 7 (eax = 7, ecx = 0/1). */
  if (max_cpuid_level >= 7)
    {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_BMI)
	set_feature (FEATURE_BMI);
      if (avx_usable)
	{
	  if (ebx & bit_AVX2)
	    set_feature (FEATURE_AVX2);
	  if (ecx & bit_VPCLMULQDQ)
	    set_feature (FEATURE_VPCLMULQDQ);
	}
      if (ebx & bit_BMI2)
	set_feature (FEATURE_BMI2);
      if (ecx & bit_GFNI)
	set_feature (FEATURE_GFNI);
      if (avx512_usable)
	{
	  if (ebx & bit_AVX512F)
	    set_feature (FEATURE_AVX512F);
	  if (ebx & bit_AVX512VL)
	    set_feature (FEATURE_AVX512VL);
	  if (ebx & bit_AVX512BW)
	    set_feature (FEATURE_AVX512BW);
	  if (ebx & bit_AVX512DQ)
	    set_feature (FEATURE_AVX512DQ);
	  if (ebx & bit_AVX512CD)
	    set_feature (FEATURE_AVX512CD);
	  if (ebx & bit_AVX512PF)
	    set_feature (FEATURE_AVX512PF);
	  if (ebx & bit_AVX512ER)
	    set_feature (FEATURE_AVX512ER);
	  if (ebx & bit_AVX512IFMA)
	    set_feature (FEATURE_AVX512IFMA);
	  if (ecx & bit_AVX512VBMI)
	    set_feature (FEATURE_AVX512VBMI);
	  if (ecx & bit_AVX512VBMI2)
	    set_feature (FEATURE_AVX512VBMI2);
	  if (ecx & bit_AVX512VNNI)
	    set_feature (FEATURE_AVX512VNNI);
	  if (ecx & bit_AVX512BITALG)
	    set_feature (FEATURE_AVX512BITALG);
	  if (ecx & bit_AVX512VPOPCNTDQ)
	    set_feature (FEATURE_AVX512VPOPCNTDQ);
	  if (edx & bit_AVX5124VNNIW)
	    set_feature (FEATURE_AVX5124VNNIW);
	  if (edx & bit_AVX5124FMAPS)
	    set_feature (FEATURE_AVX5124FMAPS);
	  if (edx & bit_AVX512VP2INTERSECT)
	    set_feature (FEATURE_AVX512VP2INTERSECT);

	  __cpuid_count (7, 1, eax, ebx, ecx, edx);
	  if (eax & bit_AVX512BF16)
	    set_feature (FEATURE_AVX512BF16);
	}
    }

  /* Check cpuid level of extended features.  */
  __cpuid (0x80000000, ext_level, ebx, ecx, edx);

  if (ext_level >= 0x80000001)
    {
      __cpuid (0x80000001, eax, ebx, ecx, edx);

      if (ecx & bit_SSE4a)
	set_feature (FEATURE_SSE4_A);
      if (avx_usable)
	{
	  if (ecx & bit_FMA4)
	    set_feature (FEATURE_FMA4);
	  if (ecx & bit_XOP)
	    set_feature (FEATURE_XOP);
	}
    }
    
  __cpu_model.__cpu_features[0] = features;
#ifndef SHARED
  __cpu_features2 = features2;
#else
  (void) features2;
#endif
}

/* A constructor function that is sets __cpu_model and __cpu_features with
   the right values.  This needs to run only once.  This constructor is
   given the highest priority and it should run before constructors without
   the priority set.  However, it still runs after ifunc initializers and
   needs to be called explicitly there.  */

int __attribute__ ((constructor CONSTRUCTOR_PRIORITY))
__cpu_indicator_init (void)
{
  unsigned int eax, ebx, ecx, edx;

  int max_level;
  unsigned int vendor;
  unsigned int model, family, brand_id;
  unsigned int extended_model, extended_family;

  /* This function needs to run just once.  */
  if (__cpu_model.__cpu_vendor)
    return 0;

  /* Assume cpuid insn present. Run in level 0 to get vendor id. */
  if (!__get_cpuid (0, &eax, &ebx, &ecx, &edx))
    {
      __cpu_model.__cpu_vendor = VENDOR_OTHER;
      return -1;
    }

  vendor = ebx;
  max_level = eax;

  if (max_level < 1)
    {
      __cpu_model.__cpu_vendor = VENDOR_OTHER;
      return -1;
    }

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    {
      __cpu_model.__cpu_vendor = VENDOR_OTHER;
      return -1;
    }

  model = (eax >> 4) & 0x0f;
  family = (eax >> 8) & 0x0f;
  brand_id = ebx & 0xff;
  extended_model = (eax >> 12) & 0xf0;
  extended_family = (eax >> 20) & 0xff;

  if (vendor == signature_INTEL_ebx)
    {
      /* Adjust model and family for Intel CPUS. */
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += extended_model;
	}
      else if (family == 0x06)
	model += extended_model;

      /* Get CPU type.  */
      get_intel_cpu (family, model, brand_id);
      /* Find available features. */
      get_available_features (ecx, edx, max_level);
      __cpu_model.__cpu_vendor = VENDOR_INTEL;
    }
  else if (vendor == signature_AMD_ebx)
    {
      /* Adjust model and family for AMD CPUS. */
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += extended_model;
	}

      /* Get CPU type.  */
      get_amd_cpu (family, model);
      /* Find available features. */
      get_available_features (ecx, edx, max_level);
      __cpu_model.__cpu_vendor = VENDOR_AMD;
    }
  else
    __cpu_model.__cpu_vendor = VENDOR_OTHER;

  gcc_assert (__cpu_model.__cpu_vendor < VENDOR_MAX);
  gcc_assert (__cpu_model.__cpu_type < CPU_TYPE_MAX);
  gcc_assert (__cpu_model.__cpu_subtype < CPU_SUBTYPE_MAX);

  return 0;
}

#if defined SHARED && defined USE_ELF_SYMVER
__asm__ (".symver __cpu_indicator_init, __cpu_indicator_init@GCC_4.8.0");
__asm__ (".symver __cpu_model, __cpu_model@GCC_4.8.0");
#endif
