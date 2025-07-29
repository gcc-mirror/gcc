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

#ifndef SYNC_H
#define SYNC_H

#include "alloc.h"
#include "counter_barrier.h"

#include <pthread.h>

typedef struct {
  /* Mutex and condition variable needed for signaling events.  */
  pthread_mutex_t event_lock;
  pthread_cond_t event_cond;
  pthread_mutex_t sync_images_table_lock;
  shared_mem_ptr sync_images_table;
  shared_mem_ptr sync_images_cond_vars;
} sync_shared;

typedef struct {
  sync_shared *cis;
  int *table; // we can cache the table and the trigger pointers here
  pthread_cond_t *triggers;
} sync_t;

typedef pthread_mutex_t lock_t;

typedef int event_t;

void sync_init (sync_t *, shared_memory);

void sync_init_supervisor (sync_t *, alloc *);

void sync_all (void);

/* Prototype for circular dependency break.  */

struct caf_shmem_team;
typedef struct caf_shmem_team *caf_shmem_team_t;

void sync_team (caf_shmem_team_t team);

void sync_table (sync_t *, int *, int);

void lock_alloc_lock (sync_t *);

void unlock_alloc_lock (sync_t *);

void lock_event (sync_t *);

void unlock_event (sync_t *);

void event_post (sync_t *);

void event_wait (sync_t *);

#endif
