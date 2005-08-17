/* Memory mamagement routines.
   Copyright 2002 Free Software Foundation, Inc.
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

/* We use a double linked list of these structures to keep track of
   the memory we allocate internally.  We could also use this for user
   allocated memory (ALLOCATE/DEALLOCATE).  This should be stored in a
   seperate list.  */
typedef struct malloc_t
{
  int magic;
  int marker;
  struct malloc_t *prev, *next;

  /* The start of the block.  */
  void *data;
}
malloc_t;

/* We try to make sure we don't get memory corruption by checking for
   a magic number.  */
#define GFC_MALLOC_MAGIC 0x4d353941	/* "G95M" */

#define HEADER_SIZE offsetof (malloc_t, data)
#define DATA_POINTER(pheader) (&((pheader)->data))
#define DATA_HEADER(pdata) ((malloc_t *)((char *) (pdata) - HEADER_SIZE))

/* The root of the circular double linked list for compiler generated
   malloc calls.  */
static malloc_t mem_root = {
	.next = &mem_root,
	.prev = &mem_root
};

#if 0
/* ??? Disabled because, well, it wasn't being called before transforming
   it to a destructor, and turning it on causes testsuite failures.  */
/* Doesn't actually do any cleaning up, just throws an error if something
   has got out of sync somewhere.  */

static void __attribute__((destructor))
runtime_cleanup (void)
{
  /* Make sure all memory we've allocated is freed on exit.  */
  if (mem_root.next != &mem_root)
    runtime_error ("Unfreed memory on program termination");
}
#endif


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


/* Allocates a block of memory with a size of N bytes.  N does not
   include the size of the header.  */

static malloc_t *
malloc_with_header (size_t n)
{
  malloc_t *newmem;

  n = n + HEADER_SIZE;

  newmem = (malloc_t *) get_mem (n);

  if (newmem)
    {
      newmem->magic = GFC_MALLOC_MAGIC;
      newmem->marker = 0;
    }

  return newmem;
}


/* Allocate memory for internal (compiler generated) use.  */

void *
internal_malloc_size (size_t size)
{
  malloc_t *newmem;

  newmem = malloc_with_header (size);

  if (!newmem)
    os_error ("Out of memory.");

  /* Add to end of list.  */
  newmem->next = &mem_root;
  newmem->prev = mem_root.prev;
  mem_root.prev->next = newmem;
  mem_root.prev = newmem;

  return DATA_POINTER (newmem);
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
/* TODO: keep a list of previously allocated blocks and reuse them.  */

void
internal_free (void *mem)
{
  malloc_t *m;

  if (!mem)
    runtime_error ("Internal: Possible double free of temporary.");

  m = DATA_HEADER (mem);

  if (m->magic != GFC_MALLOC_MAGIC)
    runtime_error ("Internal: No magic memblock marker.  "
		   "Possible memory corruption");

  /* Move markers up the chain, so they don't get lost.  */
  m->prev->marker += m->marker;
  /* Remove from list.  */
  m->prev->next = m->next;
  m->next->prev = m->prev;

  free (m);
}
iexport(internal_free);


/* User-allocate, one call for each member of the alloc-list of an
   ALLOCATE statement. */

static void
allocate_size (void **mem, size_t size, GFC_INTEGER_4 * stat)
{
  malloc_t *newmem;

  if (!mem)
    runtime_error ("Internal: NULL mem pointer in ALLOCATE.");

  newmem = malloc_with_header (size);
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

  /* We don't keep a list of these at the moment, so just link to itself. */
  newmem->next = newmem;
  newmem->prev = newmem;

  (*mem) = DATA_POINTER (newmem);

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
    runtime_error ("Internal: NULL mem pointer in ALLOCATE.");

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

  /* Just use the internal routine.  */
  internal_free (*mem);
  *mem = NULL;

  if (stat)
    *stat = 0;
}
