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

#ifndef COLLECTIVE_SUBROUTINE_HDR
#define COLLECTIVE_SUBROUTINE_HDR

#include "alloc.h"
#include "counter_barrier.h"
#include "shared_memory.h"

#include "caf/libcaf.h"

typedef struct collsub_shared
{
  size_t curr_size;
  shared_mem_ptr collsub_buf;
  counter_barrier barrier;
  pthread_mutex_t mutex;
} collsub_shared;

void collsub_init_supervisor (collsub_shared *, allocator *,
			      const int init_num_images);

void collsub_broadcast_array (gfc_descriptor_t *, int);

void collsub_reduce_array (gfc_descriptor_t *, int, void *(*) (void *, void *),
			   int opr_flags, int str_len);

#endif
