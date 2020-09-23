/* Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

This file is part of the GNU Fortran Native Coarray Library (libnca).

Libnca is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libnca is distributed in the hope that it will be useful,
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

#ifndef ALLOC_H
#define ALLOC_H

#include "allocator.h"
#include "hashmap.h"

/* High-level interface for shared memory allocation.  */

/* This part of the alloc interface goes into shared memory.  */

typedef struct alloc_iface_shared
{
  allocator_shared allocator_s;
  hashmap_shared hms;
  pthread_mutex_t lock;
} alloc_iface_shared;

/* This is the local part.  */

typedef struct alloc_iface
{
  alloc_iface_shared *as;
  shared_memory *mem;
  allocator alloc;
  hashmap hm;
} alloc_iface;
  
void *get_memory_by_id (alloc_iface *, size_t, memid);
internal_proto (get_memory_by_id);

void *get_memory_by_id_zero (alloc_iface *, size_t, memid);
internal_proto (get_memory_by_id_zero);

void free_memory_with_id (alloc_iface *, memid);
internal_proto (free_memory_with_id);

void alloc_iface_init (alloc_iface *, shared_memory *);
internal_proto (alloc_iface_init);

allocator *get_allocator (alloc_iface *);
internal_proto (get_allocator);

#endif
