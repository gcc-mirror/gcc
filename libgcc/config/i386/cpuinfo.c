/* Get CPU type and Features for x86 processors.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#ifdef HAVE_INIT_PRIORITY
#define CONSTRUCTOR_PRIORITY (101)
#else
#define CONSTRUCTOR_PRIORITY
#endif

int __cpu_indicator_init (void)
  __attribute__ ((constructor CONSTRUCTOR_PRIORITY));

enum vendor_signatures
{
  SIG_INTEL =	0x756e6547 /* Genu */,
  SIG_AMD =	0x68747541 /* Auth */
};

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
  INTEL_ATOM = 1,
  INTEL_CORE2,
  INTEL_COREI7,
  AMDFAM10H,
  AMDFAM15H,
  INTEL_SLM,
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
  CPU_SUBTYPE_MAX
};

/* ISA Features supported. */

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
  FEATURE_AVX2
};

struct __processor_model
{
  unsigned int __cpu_vendor;
  unsigned int __cpu_type;
  unsigned int __cpu_subtype;
  unsigned int __cpu_features[1];
} __cpu_model;


/* Get the specific type of AMD CPU.  */

static void
get_amd_cpu (unsigned int family, unsigned int model)
{
  switch (family)
    {
    /* AMD Family 10h.  */
    case 0x10:
      switch (model)
	{
	case 0x2:
	  /* Barcelona.  */
	  __cpu_model.__cpu_type = AMDFAM10H;
	  __cpu_model.__cpu_subtype = AMDFAM10H_BARCELONA;
	  break;
	case 0x4:
	  /* Shanghai.  */
	  __cpu_model.__cpu_type = AMDFAM10H;
	  __cpu_model.__cpu_subtype = AMDFAM10H_SHANGHAI;
	  break;
	case 0x8:
	  /* Istanbul.  */
	  __cpu_model.__cpu_type = AMDFAM10H;
	  __cpu_model.__cpu_subtype = AMDFAM10H_ISTANBUL;
	  break;
	default:
	  break;
	}
      break;
    /* AMD Family 15h.  */
    case 0x15:
      __cpu_model.__cpu_type = AMDFAM15H;
      /* Bulldozer version 1.  */
      if ( model <= 0xf)
	__cpu_model.__cpu_subtype = AMDFAM15H_BDVER1;
      /* Bulldozer version 2.  */
      if (model >= 0x10 && model <= 0x1f)
	__cpu_model.__cpu_subtype = AMDFAM15H_BDVER2;
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
	      /* Atom.  */
	      __cpu_model.__cpu_type = INTEL_ATOM;
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
  unsigned int features = 0;

  if (edx & bit_CMOV)
    features |= (1 << FEATURE_CMOV);
  if (edx & bit_MMX)
    features |= (1 << FEATURE_MMX);
  if (edx & bit_SSE)
    features |= (1 << FEATURE_SSE);
  if (edx & bit_SSE2)
    features |= (1 << FEATURE_SSE2);
  if (ecx & bit_POPCNT)
    features |= (1 << FEATURE_POPCNT);
  if (ecx & bit_SSE3)
    features |= (1 << FEATURE_SSE3);
  if (ecx & bit_SSSE3)
    features |= (1 << FEATURE_SSSE3);
  if (ecx & bit_SSE4_1)
    features |= (1 << FEATURE_SSE4_1);
  if (ecx & bit_SSE4_2)
    features |= (1 << FEATURE_SSE4_2);
  if (ecx & bit_AVX)
    features |= (1 << FEATURE_AVX);

  /* Get Advanced Features at level 7 (eax = 7, ecx = 0). */
  if (max_cpuid_level >= 7)
    {
      unsigned int eax, ebx, ecx, edx;
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_AVX2)
	features |= (1 << FEATURE_AVX2);
    }

  __cpu_model.__cpu_features[0] = features;
}

/* A noinline function calling __get_cpuid. Having many calls to
   cpuid in one function in 32-bit mode causes GCC to complain:
   "can't find a register in class CLOBBERED_REGS".  This is
   related to PR rtl-optimization 44174. */

static int __attribute__ ((noinline))
__get_cpuid_output (unsigned int __level,
		    unsigned int *__eax, unsigned int *__ebx,
		    unsigned int *__ecx, unsigned int *__edx)
{
  return __get_cpuid (__level, __eax, __ebx, __ecx, __edx);
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

  int max_level = 5;
  unsigned int vendor;
  unsigned int model, family, brand_id;
  unsigned int extended_model, extended_family;

  /* This function needs to run just once.  */
  if (__cpu_model.__cpu_vendor)
    return 0;

  /* Assume cpuid insn present. Run in level 0 to get vendor id. */
  if (!__get_cpuid_output (0, &eax, &ebx, &ecx, &edx))
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

  if (!__get_cpuid_output (1, &eax, &ebx, &ecx, &edx))
    {
      __cpu_model.__cpu_vendor = VENDOR_OTHER;
      return -1;
    }

  model = (eax >> 4) & 0x0f;
  family = (eax >> 8) & 0x0f;
  brand_id = ebx & 0xff;
  extended_model = (eax >> 12) & 0xf0;
  extended_family = (eax >> 20) & 0xff;

  if (vendor == SIG_INTEL)
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
  else if (vendor == SIG_AMD)
    {
      /* Adjust model and family for AMD CPUS. */
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += (extended_model << 4);
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
