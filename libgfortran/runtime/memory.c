/* Memory management routines.
   Copyright 2002, 2005, 2006, 2007, 2009, 2010 Free Software Foundation, Inc.
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
#include <stdlib.h>

/* If GFC_CLEAR_MEMORY is defined, the memory allocation routines will
   return memory that is guaranteed to be set to zero.  This can have
   a severe efficiency penalty, so it should never be set if good
   performance is desired, but it can help when you're debugging code.  */
/* #define GFC_CLEAR_MEMORY */

void *
get_mem (size_t n)
{
  void *p;

#ifdef GFC_CLEAR_MEMORY
  p = (void *) calloc (1, n);
#else
  p = (void *) malloc (n);
#endif
  if (p == NULL)
    os_error ("Memory allocation failed");

  return p;
}


/* Allocate memory for internal (compiler generated) use.  */

void *
internal_malloc_size (size_t size)
{
  if (size == 0)
    return NULL;

  return get_mem (size);
}
