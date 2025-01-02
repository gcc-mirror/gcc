/* Stack scrubbing infrastructure
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>

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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "libgcc2.h"

#if ! STACK_GROWS_DOWNWARD
# define TOPS >
#else
# define TOPS <
#endif

/* Make sure these builtins won't be inlined, even with LTO.  */
#define ATTRIBUTE_NOINLINE \
  __attribute__ ((__noinline__, __noclone__, __noipa__))

#define ATTRIBUTE_STRUB_CALLABLE \
  __attribute__ ((__strub__ ("callable"))) ATTRIBUTE_NOINLINE

/* Enter a stack scrubbing context, initializing the watermark to the caller's
   stack address.  */
void ATTRIBUTE_STRUB_CALLABLE
__strub_enter (void **watermark)
{
  *watermark = __builtin_frame_address (0);
}

/* Update the watermark within a stack scrubbing context with the current stack
   pointer.  */
void ATTRIBUTE_STRUB_CALLABLE
__strub_update (void **watermark)
{
  void *sp = __builtin_frame_address (0);

  if (sp TOPS *watermark)
    *watermark = sp;
}

#if TARGET_STRUB_USE_DYNAMIC_ARRAY && ! defined TARGET_STRUB_MAY_USE_MEMSET
# define TARGET_STRUB_MAY_USE_MEMSET 1
#endif

#if defined __x86_64__ && __OPTIMIZE__
# define TARGET_STRUB_DISABLE_RED_ZONE \
  /* __attribute__ ((__target__ ("no-red-zone"))) // not needed when optimizing */
#elif !defined RED_ZONE_SIZE || defined __i386__
# define TARGET_STRUB_DISABLE_RED_ZONE
#endif

#ifndef TARGET_STRUB_DISABLE_RED_ZONE
/* Dummy function, called to force the caller to not be a leaf function, so
   that it can't use the red zone.  */
static void ATTRIBUTE_STRUB_CALLABLE
__strub_dummy_force_no_leaf (void)
{
}
#endif

/* Leave a stack scrubbing context, clearing the stack between its top and
   *MARK.  */
void ATTRIBUTE_STRUB_CALLABLE
#if ! TARGET_STRUB_MAY_USE_MEMSET
__attribute__ ((__optimize__ ("-fno-tree-loop-distribute-patterns")))
#endif
#ifdef TARGET_STRUB_DISABLE_RED_ZONE
TARGET_STRUB_DISABLE_RED_ZONE
#endif
__strub_leave (void **mark)
{
  void *sp = __builtin_stack_address ();

  void **base, **end;
#if ! STACK_GROWS_DOWNWARD
  base = sp; /* ??? Do we need an offset here?  */
  end = *mark;
#else
  base = *mark;
  end = sp; /* ??? Does any platform require an offset here?  */
#endif

  if (! (base < end))
    return;

#if TARGET_STRUB_USE_DYNAMIC_ARRAY
  /* Compute the length without assuming the pointers are both sufficiently
     aligned.  They should be, but pointer differences expected to be exact may
     yield unexpected results when the assumption doesn't hold.  Given the
     potential security implications, compute the length without that
     expectation.  If the pointers are misaligned, we may leave a partial
     unscrubbed word behind.  */
  ptrdiff_t len = ((char *)end - (char *)base) / sizeof (void *);
  /* Allocate a dynamically-sized array covering the desired range, so that we
     can safely call memset on it.  */
  void *ptr[len];
  base = &ptr[0];
  end = &ptr[len];
#elifndef TARGET_STRUB_DISABLE_RED_ZONE
  /* Prevent the use of the red zone, by making this function non-leaf through
     an unreachable call that, because of the asm stmt, the compiler will
     consider reachable.  */
  asm goto ("" : : : : no_leaf);
  if (0)
    {
    no_leaf:
      __strub_dummy_force_no_leaf ();
      return;
    }
#endif

  /* ldist may turn these loops into a memset (thus the conditional
     -fno-tree-loop-distribute-patterns above).  Without the dynamic array
     above, that call would likely be unsafe: possibly tail-called, and likely
     scribbling over its own stack frame.  */
#if ! STACK_GROWS_DOWNWARD
  do
    *base++ = 0;
  while (base < end);
  /* Make sure the stack overwrites are not optimized away.  */
  asm ("" : : "m" (end[0]));
#else
  do
    *--end = 0;
  while (base < end);
  /* Make sure the stack overwrites are not optimized away.  */
  asm ("" : : "m" (base[0]));
#endif
}
