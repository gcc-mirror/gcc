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

#include "objc-private/common.h"
#include "objc-private/error.h"

/* __USE_FIXED_PROTOTYPES__ used to be required to get prototypes for
   malloc, free, etc. on some platforms.  It is unclear if we still
   need it, but it can't hurt.
*/
#define __USE_FIXED_PROTOTYPES__
#include <stdlib.h>

#include "objc/objc.h"
#include "objc/objc-api.h"
#include "objc-private/runtime.h"

/*
  Standard functions for memory allocation and disposal.  Users should
  use these functions in their ObjC programs so that they work
  properly with garbage collectors as well as can take advantage of
  the exception/error handling available.
*/

void *
objc_malloc (size_t size)
{
  void *res = (void *) (*_objc_malloc) (size);
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_atomic_malloc (size_t size)
{
  void *res = (void *) (*_objc_atomic_malloc) (size);
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_valloc (size_t size)
{
  void *res = (void *) (*_objc_valloc) (size);
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_realloc (void *mem, size_t size)
{
  void *res = (void *) (*_objc_realloc) (mem, size);
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void *
objc_calloc (size_t nelem, size_t size)
{
  void *res = (void *) (*_objc_calloc) (nelem, size);
  if (! res)
    _objc_abort ("Virtual memory exhausted\n");
  return res;
}

void
objc_free (void *mem)
{
  (*_objc_free) (mem);
}

/*
  Hook functions for memory allocation and disposal.  This makes it
  easy to substitute garbage collection systems such as Boehm's GC by
  assigning these function pointers to the GC's allocation routines.
  By default these point to the ANSI standard malloc, realloc, free,
  etc.

  Users should call the normal objc routines above for memory
  allocation and disposal within their programs.
*/

#if OBJC_WITH_GC
#include <gc.h>

/* FIXME: The following sounds pointless because the GC_malloc
   documentation says that it returns memory that is already zeroed!
*/
static void *
GC_calloc (size_t nelem, size_t size)
{
  void *p = GC_malloc (nelem * size);
  if (! p)
    _objc_abort ("Virtual memory exhausted!\n");

  memset (p, 0, nelem * size);
  return p;
}

static void
noFree (void *p)
{
}

void *(*_objc_malloc) (size_t) = GC_malloc;
void *(*_objc_atomic_malloc) (size_t) = GC_malloc_atomic;
void *(*_objc_valloc) (size_t) = GC_malloc;
void *(*_objc_realloc) (void *, size_t) = GC_realloc;
void *(*_objc_calloc) (size_t, size_t) = GC_calloc;
void (*_objc_free) (void *) = noFree;

#else	/* !OBJC_WITH_GC */

void *(*_objc_malloc) (size_t) = malloc;
void *(*_objc_atomic_malloc) (size_t) = malloc;
void *(*_objc_valloc) (size_t) = malloc;
void *(*_objc_realloc) (void *, size_t) = realloc;
void *(*_objc_calloc) (size_t, size_t) = calloc;
void (*_objc_free) (void *) = free;


#endif	/* !OBJC_WITH_GC */
