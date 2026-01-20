/* Run-time routines for memory allocation.

   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   Under Section 7 of GPL version 3, you are granted additional permissions
   described in the GCC Runtime Library Exception, version 3.1, as published by
   the Free Software Foundation.

   You should have received a copy of the GNU General Public License and a copy
   of the GCC Runtime Library Exception along with this program; see the files
   COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "ga68.h"

#include <stdlib.h>


/* Heap allocation routines.  */

void
_libga68_free_internal (void *pt)
{
  free (pt);
}

void *
_libga68_malloc_internal (size_t size)
{
  void *res = (void *) malloc (size);
  if (!res)
    _libga68_abort ("Virtual memory exhausted\n");
  return res;
}

#if LIBGA68_WITH_GC
#include <gc/gc.h>

void
_libga68_init_heap (void)
{
  if (!GC_is_init_called ())
    {
      GC_INIT ();
      /*      GC_allow_register_threads (); */
    }
}

void *
_libga68_realloc (void *ptr, size_t size)
{
  void *res = (void *) GC_REALLOC (ptr, size);
  if (!res)
    _libga68_abort ("Virtual memory exhausted\n");
  return res;
}

void *
_libga68_realloc_unchecked (void *ptr, size_t size)
{
  void *res = (void *) GC_REALLOC (ptr, size);
  return res;
}

void *
_libga68_malloc (size_t size)
{
  void *res = (void *) GC_MALLOC (size);
  if (!res)
    _libga68_abort ("Virtual memory exhausted\n");
  return res;
}

void *
_libga68_malloc_leaf (size_t size)
{
  void *res = (void *) GC_MALLOC_ATOMIC (size);
  if (!res)
    _libga68_abort ("Virtual memory exhausted\n");
  return res;
}

#else

void
_libga68_init_heap (void)
{
}

void *
_libga68_realloc (void *ptr, size_t size)
{
  void *res = (void *) realloc (ptr, size);
  if (!res)
    _libga68_abort ("Virtual memory exhausted\n");
  return res;
}

void *
_libga68_realloc_unchecked (void *ptr, size_t size)
{
  void *res = (void *) realloc (ptr, size);
  return res;
}

void *
_libga68_malloc (size_t size)
{
  void *res = (void *) malloc (size);
  if (!res)
    _libga68_abort ("Virtual memory exhausted\n");
  return res;
}

void *
_libga68_malloc_leaf (size_t size)
{
  return _libga68_malloc (size);
}

#endif /* !LIBGA68_WITH_GC */
