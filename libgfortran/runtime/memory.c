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

#include "config.h"
#include <stdlib.h>
#include "libgfortran.h"

/* If GFC_CLEAR_MEMORY is defined, the memory allocation routines will
   return memory that is guaranteed to be set to zero.  This can have
   a severe efficiency penalty, so it should never be set if good
   performance is desired, but it can help when you're debugging code.  */
/* #define GFC_CLEAR_MEMORY */

/* If GFC_CHECK_MEMORY is defined, we do some sanity checks at runtime.
   This causes small overhead, but again, it also helps debugging.  */
#define GFC_CHECK_MEMORY

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


/* Reallocate internal memory MEM so it has SIZE bytes of data.
   Allocate a new block if MEM is zero, and free the block if
   SIZE is 0.  */

extern void *internal_realloc (void *, index_type);
export_proto(internal_realloc);

void *
internal_realloc (void *mem, index_type size)
{
#ifdef GFC_CHECK_MEMORY
  /* Under normal circumstances, this is _never_ going to happen!  */
  if (size < 0)
    runtime_error ("Attempt to allocate a negative amount of memory.");
#endif
  mem = realloc (mem, size);
  if (!mem && size != 0)
    os_error ("Out of memory.");
  
  if (size == 0)
      return NULL;

  return mem;
}


/* User-allocate, one call for each member of the alloc-list of an
   ALLOCATE statement. */

extern void *allocate (index_type, GFC_INTEGER_4 *) __attribute__ ((malloc));
export_proto(allocate);

void *
allocate (index_type size, GFC_INTEGER_4 * stat)
{
  void *newmem;

#ifdef GFC_CHECK_MEMORY
  /* The only time this can happen is the size computed by the
     frontend wraps around.  */
  if (size < 0)
    {
      if (stat)
	{
	  *stat = ERROR_ALLOCATION;
	  return NULL;
	}
      else
	runtime_error ("Attempt to allocate negative amount of memory. "
		       "Possible integer overflow");
    }
#endif
  newmem = malloc (size ? size : 1);
  if (!newmem)
    {
      if (stat)
	{
	  *stat = ERROR_ALLOCATION;
	  return newmem;
	}
      else
	runtime_error ("ALLOCATE: Out of memory.");
    }

  if (stat)
    *stat = 0;

  return newmem;
}

/* Function to call in an ALLOCATE statement when the argument is an
   allocatable array.  If the array is currently allocated, it is
   an error to allocate it again.  */

extern void *allocate_array (void *, index_type, GFC_INTEGER_4 *);
export_proto(allocate_array);

void *
allocate_array (void *mem, index_type size, GFC_INTEGER_4 * stat)
{
  if (mem == NULL)
    return allocate (size, stat);
  if (stat)
    {
      free (mem);
      mem = allocate (size, stat);
      *stat = ERROR_ALLOCATION;
      return mem;
    }

  runtime_error ("Attempting to allocate already allocated array.");
}


/* User-deallocate; pointer is then NULLified by the front-end. */

extern void deallocate (void *, GFC_INTEGER_4 *);
export_proto(deallocate);

void
deallocate (void *mem, GFC_INTEGER_4 * stat)
{
  if (!mem)
    {
      if (stat)
	{
	  *stat = 1;
	  return;
	}
      else
	runtime_error ("Internal: Attempt to DEALLOCATE unallocated memory.");
    }

  free (mem);

  if (stat)
    *stat = 0;
}
