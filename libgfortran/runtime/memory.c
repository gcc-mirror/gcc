/* Memory management routines.
   Copyright 2002, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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


void
free_mem (void *p)
{
  free (p);
}


/* Allocate memory for internal (compiler generated) use.  */

void *
internal_malloc_size (size_t size)
{
  if (size == 0)
    return NULL;

  return get_mem (size);
}
