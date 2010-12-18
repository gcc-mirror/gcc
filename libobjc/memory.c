/* GNU Objective C Runtime Memory allocation functions
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 2002, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This file includes the standard functions for memory allocation and
   disposal.  Users should use these functions in their ObjC programs
   so that they work properly with garbage collectors.  */

/* TODO: Turn these into macros or inline functions.  */

#include "objc-private/common.h"
#include "objc-private/error.h"

/* __USE_FIXED_PROTOTYPES__ used to be required to get prototypes for
   malloc, free, etc. on some platforms.  It is unclear if we still
   need it, but it can't hurt.  */
#define __USE_FIXED_PROTOTYPES__
#include <stdlib.h>

#include "objc/runtime.h"

#if OBJC_WITH_GC
#include <gc.h>

void *
objc_malloc (size_t size)
{
  void *res = (void *)(GC_malloc (size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_atomic_malloc (size_t size)
{
  void *res = (void *)(GC_malloc_atomic (size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_realloc (void *mem, size_t size)
{
  void *res = (void *)(GC_realloc (mem, size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_calloc (size_t nelem, size_t size)
{
  /* Note that GC_malloc returns cleared memory (see documentation) so
     there is no need to clear it.  */
  void *res = (void *)(GC_malloc (nelem * size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void
objc_free (void *mem __attribute__ ((__unused__)))
{
  return;
}

#else

void *
objc_malloc (size_t size)
{
  void *res = (void *)(malloc (size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_atomic_malloc (size_t size)
{
  void *res = (void *)(malloc (size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_realloc (void *mem, size_t size)
{
  void *res = (void *)(realloc (mem, size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_calloc (size_t nelem, size_t size)
{
  void *res = (void *)(calloc (nelem, size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void
objc_free (void *mem)
{
  free (mem);
}

#endif	/* !OBJC_WITH_GC */

/* The rest of the file contains deprecated code.  */

#if OBJC_WITH_GC

void *
objc_valloc (size_t size)
{
  void *res = (void *)(GC_malloc (size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

#else

void *
objc_valloc (size_t size)
{
  void *res = (void *)(malloc (size));
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

#endif	/* !OBJC_WITH_GC */

/* Hook functions for memory allocation and disposal.  Deprecated and
   currently unused.  */
void *(*_objc_malloc) (size_t) = malloc;
void *(*_objc_atomic_malloc) (size_t) = malloc;
void *(*_objc_valloc) (size_t) = malloc;
void *(*_objc_realloc) (void *, size_t) = realloc;
void *(*_objc_calloc) (size_t, size_t) = calloc;
void (*_objc_free) (void *) = free;
