/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
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

#ifndef HASHMAP_H
#define HASHMAP_H

#include "allocator.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* Data structures and variables:

   memid is a unique identifier for the coarray.  */

typedef uint64_t memid;

typedef struct {
  shared_mem_ptr data;
  size_t size;
  int bitnum;
} hashmap_shared;

typedef struct hashmap
{
  hashmap_shared *s;
  shared_memory sm;
  allocator *a;
} hashmap;

typedef struct {
  shared_mem_ptr p;
  size_t size;
  ssize_t res_offset;
} hashmap_search_result;

/* Initialize the hashmap on a worker image.  */

void hashmap_init (hashmap *, hashmap_shared *, allocator *a);

/* Initialize the hashmap on the supervisor.  This routine must be called only
   on the supervisor.  */

void hashmap_init_supervisor (hashmap *, hashmap_shared *, allocator *);

/* Look up memid in the hashmap.  The result can be inspected via the
   hm_search_result_* functions.  */

hashmap_search_result hashmap_get (hashmap *, memid);

/* Given a search result, returns the size.  */
size_t hm_search_result_size (hashmap_search_result *);

/* Given a search result, returns the pointer.  */
shared_mem_ptr hm_search_result_ptr (hashmap_search_result *);

/* Given a search result, returns whether something was found.  */
bool hm_search_result_contains (hashmap_search_result *);

/* Sets the hashmap entry for memid to shared_mem_ptr and
   size_t.  Optionally, if a hashmap_search_result is supplied, it is
   used to make the lookup faster.  */

void hashmap_set (hashmap *, memid, hashmap_search_result *, shared_mem_ptr p,
		  size_t);

/* Increments the hashmap entry for memid.  Optionally, if a
   hashmap_search_result is supplied, it is used to make the lookup
   faster.  */

void hashmap_inc (hashmap *, memid, hashmap_search_result *);

/* Same, but decrement.  */
int hashmap_dec (hashmap *, memid, hashmap_search_result *);

#endif
