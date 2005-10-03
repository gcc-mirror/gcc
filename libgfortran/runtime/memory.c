/* Memory mamagement routines.
   Copyright 2002, 2005 Free Software Foundation, Inc.
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

extern void *internal_malloc (GFC_INTEGER_4);
export_proto(internal_malloc);

void *
internal_malloc (GFC_INTEGER_4 size)
{
#ifdef GFC_CHECK_MEMORY
  /* Under normal circumstances, this is _never_ going to happen!  */
  if (size < 0)
    runtime_error ("Attempt to allocate a negative amount of memory.");

#endif
  return internal_malloc_size ((size_t) size);
}

extern void *internal_malloc64 (GFC_INTEGER_8);
export_proto(internal_malloc64);

void *
internal_malloc64 (GFC_INTEGER_8 size)
{
#ifdef GFC_CHECK_MEMORY
  /* Under normal circumstances, this is _never_ going to happen!  */
  if (size < 0)
    runtime_error ("Attempt to allocate a negative amount of memory.");
#endif
  return internal_malloc_size ((size_t) size);
}


/* Free internally allocated memory.  Pointer is NULLified.  Also used to
   free user allocated memory.  */

void
internal_free (void *mem)
{
  if (mem != NULL)
    free (mem);
}
iexport(internal_free);

/* Reallocate internal memory MEM so it has SIZE bytes of data.
   Allocate a new block if MEM is zero, and free the block if
   SIZE is 0.  */

static void *
internal_realloc_size (void *mem, size_t size)
{
  if (size == 0)
    {
      if (mem)
	free (mem);
      return NULL;
    }

  if (mem == 0)
    return get_mem (size);

  mem = realloc (mem, size);
  if (!mem)
    os_error ("Out of memory.");

  return mem;
}

extern void *internal_realloc (void *, GFC_INTEGER_4);
export_proto(internal_realloc);

void *
internal_realloc (void *mem, GFC_INTEGER_4 size)
{
#ifdef GFC_CHECK_MEMORY
  /* Under normal circumstances, this is _never_ going to happen!  */
  if (size < 0)
    runtime_error ("Attempt to allocate a negative amount of memory.");
#endif
  return internal_realloc_size (mem, (size_t) size);
}

extern void *internal_realloc64 (void *, GFC_INTEGER_8);
export_proto(internal_realloc64);

void *
internal_realloc64 (void *mem, GFC_INTEGER_8 size)
{
#ifdef GFC_CHECK_MEMORY
  /* Under normal circumstances, this is _never_ going to happen!  */
  if (size < 0)
    runtime_error ("Attempt to allocate a negative amount of memory.");
#endif
  return internal_realloc_size (mem, (size_t) size);
}


/* User-allocate, one call for each member of the alloc-list of an
   ALLOCATE statement. */

static void
allocate_size (void **mem, size_t size, GFC_INTEGER_4 * stat)
{
  void *newmem;

  if (!mem)
    runtime_error ("Internal: NULL mem pointer in ALLOCATE.");

  newmem = malloc (size ? size : 1);
  if (!newmem)
    {
      if (stat)
	{
	  *stat = 1;
	  return;
	}
      else
	runtime_error ("ALLOCATE: Out of memory.");
    }

  (*mem) = newmem;

  if (stat)
    *stat = 0;
}

extern void allocate (void **, GFC_INTEGER_4, GFC_INTEGER_4 *);
export_proto(allocate);

void
allocate (void **mem, GFC_INTEGER_4 size, GFC_INTEGER_4 * stat)
{
  if (size < 0)
    {
      runtime_error ("Attempt to allocate negative amount of memory.  "
		     "Possible integer overflow");
      abort ();
    }

  allocate_size (mem, (size_t) size, stat);
}

extern void allocate64 (void **, GFC_INTEGER_8, GFC_INTEGER_4 *);
export_proto(allocate64);

void
allocate64 (void **mem, GFC_INTEGER_8 size, GFC_INTEGER_4 * stat)
{
  if (size < 0)
    {
      runtime_error
	("ALLOCATE64: Attempt to allocate negative amount of memory. "
	 "Possible integer overflow");
      abort ();
    }

  allocate_size (mem, (size_t) size, stat);
}


/* User-deallocate; pointer is NULLified. */

extern void deallocate (void **, GFC_INTEGER_4 *);
export_proto(deallocate);

void
deallocate (void **mem, GFC_INTEGER_4 * stat)
{
  if (!mem)
    runtime_error ("Internal: NULL mem pointer in DEALLOCATE.");

  if (!*mem)
    {
      if (stat)
	{
	  *stat = 1;
	  return;
	}
      else
	{
	  runtime_error
	    ("Internal: Attempt to DEALLOCATE unallocated memory.");
	  abort ();
	}
    }

  free (*mem);
  *mem = NULL;

  if (stat)
    *stat = 0;
}
