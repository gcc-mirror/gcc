// Copyright (C) 2012-2017 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#ifndef _VTV_MALLOC_H
#define _VTV_MALLOC_H 1

#include <stdlib.h>

/* Alignment mask for any object returned by the VTV memory pool */
#ifdef __LP64__
#define VTV_ALIGNMENT_MASK (0x7)
#else
#define VTV_ALIGNMENT_MASK (0x3)
#endif

/* The following function is used to instrument the compiler and find
   out how many cycles are being spent in various vtable verification
   runtime library functions.  */

#ifdef __x86_64__
static inline unsigned long
rdtsc ()
{
  unsigned long long hi, lo;

  asm volatile ("rdtsc" : "=a" (lo), "=d" (hi));
  return hi << 32 | lo;
}
#elif defined (__i386__)
static inline unsigned long long
rdtsc ()
{
  unsigned long long var;

  asm volatile ("rdtsc" : "=A" (var));

  return var;
}
#else 
static inline unsigned long long
rdtsc ()
{
  /* Create an empty function for unknown architectures, so that the
     calls to this function in vtv_malloc.cc and vtv_rts.cc do not cause
     compilation errors.  */
  return ((unsigned long long) 0);
}
#endif


/* The following variables are used only for debugging and performance tuning
   purposes. Therefore they do not need to be "protected".  They cannot be used
   to attack the vtable verification system and if they become corrupted it will
   not affect the correctness or security of any of the rest of the vtable
   verification feature.  */

extern unsigned int num_calls_to_mprotect;
extern unsigned int num_pages_protected;
extern unsigned int num_calls_to_regset;
extern unsigned int num_calls_to_regpair;
extern unsigned long long mprotect_cycles;
extern unsigned long long regset_cycles;
extern unsigned long long regpair_cycles;


/* Function declarations.  */

extern void __vtv_malloc_init (void);
extern void *__vtv_malloc (size_t size) __attribute__ ((malloc));
extern void __vtv_free (void * ptr);
extern void __vtv_malloc_protect (void);
extern void __vtv_malloc_unprotect (void);
extern void __vtv_malloc_stats (void);
extern void __vtv_malloc_dump_stats (void);
extern int __vtv_count_mmapped_pages (void);

#if defined (__CYGWIN__) || defined (__MINGW32__)
extern "C" int mprotect (void *addr, int len, int prot);

  #define PROT_READ 0x1
  #define PROT_WRITE 0x2
#endif

#endif /* vtv_malloc.h */
