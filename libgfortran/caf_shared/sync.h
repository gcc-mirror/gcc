/* Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#ifndef IPSYNC_HDR
#define IPSYNC_HDR

#include "shared_memory.h"
#include "alloc.h"
#include "counter_barrier.h"
#include <pthread.h>

typedef struct {
  counter_barrier sync_all;
  pthread_mutex_t table_lock;
  shared_mem_ptr table;
  shared_mem_ptr triggers;
} sync_iface_shared;

typedef struct {
  sync_iface_shared *cis;
  shared_memory *sm;
  allocator *a;
  int *table; // we can cache the table and the trigger pointers here
  pthread_cond_t *triggers;
} sync_iface;

void sync_iface_init (sync_iface *, alloc_iface *, shared_memory *);
internal_proto (sync_iface_init);

void sync_all (sync_iface *);
internal_proto (sync_all);

void sync_table (sync_iface *, int *, int);
internal_proto (sync_table);

#endif
