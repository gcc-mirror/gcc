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
#include "common/config/i386/i386-cpuinfo.h"
#include "common/config/i386/cpuinfo.h"

#ifdef HAVE_INIT_PRIORITY
#define CONSTRUCTOR_PRIORITY (101)
#else
#define CONSTRUCTOR_PRIORITY
#endif

int __cpu_indicator_init (void)
  __attribute__ ((constructor CONSTRUCTOR_PRIORITY));


struct __processor_model __cpu_model = { };
/* We want to move away from __cpu_model in libgcc_s.so.1 and the
   size of __cpu_model is part of ABI.  So, new features that don't
   fit into __cpu_model.__cpu_features[0] go into extra variables
   in libgcc.a only, preferably hidden.

   NB: Since older 386-builtins.c accesses __cpu_features2 as scalar or
   smaller array, it can only access the first few elements.  */
unsigned int __cpu_features2[SIZE_OF_CPU_FEATURES];

/* A constructor function that is sets __cpu_model and __cpu_features with
   the right values.  This needs to run only once.  This constructor is
   given the highest priority and it should run before constructors without
   the priority set.  However, it still runs after ifunc initializers and
   needs to be called explicitly there.  */

int __attribute__ ((constructor CONSTRUCTOR_PRIORITY))
__cpu_indicator_init (void)
{
  struct __processor_model2 cpu_model2;
  return cpu_indicator_init (&__cpu_model, &cpu_model2,
			     __cpu_features2);
}

#if defined SHARED && defined USE_ELF_SYMVER
__asm__ (".symver __cpu_indicator_init, __cpu_indicator_init@GCC_4.8.0");
__asm__ (".symver __cpu_model, __cpu_model@GCC_4.8.0");
#endif
