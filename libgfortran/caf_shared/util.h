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

#ifndef UTIL_HDR
#define UTIL_HDR

#include <pthread.h>
#include <limits.h>
#include <assert.h>
#include <sys/types.h>
#include <unistd.h>

#define CAS_DEBUG_PR(str, ...) dprintf(2, "Image %d (pid %ld):\t" str "\n", this_image.image_num, (unsigned long) getpid(), ##__VA_ARGS__)

#define PTR_BITS (CHAR_BIT*sizeof(void *))

size_t alignto (size_t, size_t);
internal_proto (alignto);

size_t round_to_pagesize (size_t);
internal_proto (round_to_pagesize);

size_t next_power_of_two (size_t);
internal_proto (next_power_of_two);

int get_shmem_fd (void);
internal_proto (get_shmem_fd);

void initialize_shared_mutex (pthread_mutex_t *);
internal_proto (initialize_shared_mutex);

void initialize_shared_condition (pthread_cond_t *);
internal_proto (initialize_shared_condition);

extern size_t pagesize;
internal_proto (pagesize);

/* Usage: 
     pack_info pi;
     packed = pack_array_prepare (&pi, source);
 
     // Awesome allocation of destptr using pi.num_elem
     if (packed)
       memcpy (...);
     else
       pack_array_finish (&pi, source, destptr);
 
   This could also be used in in_pack_generic.c. Additionally, since
   pack_array_prepare is the same for all type sizes, we would only have to
   specialize pack_array_finish, saving on code size.  */

typedef struct
{
  index_type num_elem;
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];  /* Stride is byte-based.  */
} pack_info;

bool pack_array_prepare (pack_info *restrict, const gfc_array_char * restrict);
internal_proto (pack_array_prepare);

void pack_array_finish (pack_info * const restrict, const gfc_array_char * const restrict,
			char * restrict);

internal_proto (pack_array_finish);

void unpack_array_finish (pack_info * const restrict, const gfc_array_char * const,
			  const char * restrict);
internal_proto (unpack_array_finish);

#endif
