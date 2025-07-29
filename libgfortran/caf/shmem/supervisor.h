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

#ifndef SUPERVISOR_H
#define SUPERVISOR_H

#include "caf/libcaf.h"
#include "alloc.h"
#include "collective_subroutine.h"
#include "sync.h"

#include <stdatomic.h>

typedef enum
{
  IMAGE_UNKNOWN = 0,
  IMAGE_OK,
  IMAGE_FAILED,
  IMAGE_SUCCESS
} image_status;

typedef struct
{
  pid_t pid;
  image_status status;
} image_tracker;

typedef struct supervisor
{
  ptrdiff_t magic_number;
  alloc_shared alloc_shared;
  hashmap_shared hms;
  collsub_shared collsub_shared;
  sync_shared sync_shared;
  atomic_int failed_images;
  atomic_int finished_images;
  counter_barrier num_active_images;
  pthread_mutex_t image_tracker_lock;
  image_tracker images[];
} supervisor;

typedef struct
{
  int image_num;
  supervisor *supervisor;
} image;

extern image this_image;

typedef struct
{
  int total_num_images;
  struct shared_memory_act sm;
  alloc ai;
  sync_t si;
} image_local;

extern image_local *local;

struct caf_shmem_token
{
  /* The pointer to the memory registered for the current image.  For arrays
     this is the data member in the descriptor.  For components it's the pure
     data pointer.  */
  void *memptr;
  /* The descriptor when this token is associated to an allocatable array.  */
  gfc_descriptor_t *desc;
  /* The base address this coarray's memory in the shared memory space.  The
     base address of image I is computed by base + I * image_size.  */
  void *base;
  /* The size of memory in each image aligned on pointer borders, i.e. each
     images memory starts on an address that is aligned to enable maximum speed
     for the processor architecure used.  */
  size_t image_size;
  /* The id of this token.  */
  memid token_id;
  /* Set when the caf lib has allocated the memory in memptr and is responsible
     for freeing it on deregister.  */
  bool owning_memory;
};
typedef struct caf_shmem_token *caf_shmem_token_t;


/* Ensure the shared memory environment is up and all support structures are
   initialized and linked correctly.  */

void ensure_shmem_initialization (void);

int supervisor_main_loop (int *argc, char ***argv, int *exit_code);

#endif
