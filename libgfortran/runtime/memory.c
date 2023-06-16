/* Memory management routines.
   Copyright (C) 2002-2023 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"
#include <errno.h>

#ifndef LIBGFOR_MINIMAL
static void * (*gfortran_malloc)(size_t) = malloc;
static void * (*gfortran_calloc)(size_t, size_t) = calloc;
static void * (*gfortran_realloc)(void *, size_t) = realloc;
static void (*gfortran_free)(void *) = free;

void
mem_allocators_init (void *malloc_ptr, void *calloc_ptr,
		     void *realloc_ptr, void *free_ptr)
{
  gfortran_malloc = malloc_ptr;
  gfortran_calloc = calloc_ptr;
  gfortran_realloc = realloc_ptr;
  gfortran_free = free_ptr;
}

#else
#define gfortran_malloc malloc
#define gfortran_calloc calloc
#define gfortran_realloc realloc
#define gfortran_free free
#endif

void *
xmalloc (size_t n)
{
  void *p;

  if (n == 0)
    n = 1;

  p = gfortran_malloc (n);

  if (p == NULL)
    os_error ("Memory allocation failed");

  return p;
}

void *
xmallocarray (size_t nmemb, size_t size)
{
  void *p;
  size_t prod;

  if (!nmemb || !size)
    prod = 1;
  else if (__builtin_mul_overflow (nmemb, size, &prod))
    {
      errno = ENOMEM;
      os_error ("Integer overflow in xmallocarray");
    }

  p = gfortran_malloc (prod);

  if (!p)
    os_error ("Memory allocation failed in xmallocarray");

  return p;
}

/* calloc wrapper that aborts on error.  */

void *
xcalloc (size_t nmemb, size_t size)
{
  if (!nmemb || !size)
    nmemb = size = 1;

  void *p = gfortran_calloc (nmemb, size);
  if (!p)
    os_error ("Allocating cleared memory failed");

  return p;
}

void *
xrealloc (void *ptr, size_t size)
{
  if (size == 0)
    size = 1;

  void *newp = gfortran_realloc (ptr, size);
  if (!newp)
    os_error ("Memory allocation failure in xrealloc");

  return newp;
}

void
xfree (void *ptr)
{
  gfortran_free (ptr);
}
