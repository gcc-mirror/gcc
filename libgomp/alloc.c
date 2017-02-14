/* Copyright (C) 2005-2017 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* This file contains wrappers for the system allocation routines.  Most
   places in the OpenMP API do not make any provision for failure, so in
   general we cannot allow memory allocation to fail.  */

#include "libgomp.h"
#include <stdlib.h>


void *
gomp_malloc (size_t size)
{
  void *ret = malloc (size);
  if (ret == NULL)
    gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return ret;
}

void *
gomp_malloc_cleared (size_t size)
{
  void *ret = calloc (1, size);
  if (ret == NULL)
    gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return ret;
}

void *
gomp_realloc (void *old, size_t size)
{
  void *ret = realloc (old, size);
  if (ret == NULL)
    gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return ret;
}
