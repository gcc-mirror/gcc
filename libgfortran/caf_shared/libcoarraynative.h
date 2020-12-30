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

#ifndef COARRAY_NATIVE_HDR
#define COARRAY_NATIVE_HDR

#define DEBUG_NATIVE_COARRAY 0

#if defined(DEBUG_NATIVE_COARRAY) && DEBUG_NATIVE_COARRAY
#define DEBUG_PRINTF(...) dprintf (2,__VA_ARGS__)
#else
#define DEBUG_PRINTF(...) do {} while(0)
#endif

#include "shared_memory.h"
#include "alloc.h"
#include "sync.h"
#include "util.h"
#include "collective_subroutine.h"
#include "counter_barrier.h"

/* Defnitions of the Fortran 2008 standard; need to be kept in sync
   with ISO_FORTRAN_ENV, cf. gcc/fortran/libgfortran.h.  */
typedef enum
{
  CAS_STAT_UNLOCKED = 0,
  CAS_STAT_LOCKED,
  CAS_STAT_LOCKED_OTHER_IMAGE,
  CAS_STAT_STOPPED_IMAGE = 6000,
  CAS_STAT_FAILED_IMAGE  = 6001
} stat_constants;

typedef enum {
  IMAGE_UNKNOWN = 0,
  IMAGE_OK,
  IMAGE_FAILED,
  IMAGE_SUCCESS
} image_status;

typedef struct {
  pid_t pid;
  image_status status;
  int active_image_index;
} image_tracker;

typedef struct {
  volatile int has_failed_image;
  volatile int finished_images;
  waitable_counter num_active_images;
  pthread_mutex_t image_tracker_lock;
  volatile image_tracker images[];
} master;

typedef struct {
  int image_num;
  master *m;
} image;

extern image this_image;

typedef struct {
  int total_num_images;
  shared_memory sm;
  alloc_iface ai;
  collsub_iface ci;
  sync_iface si;
} nca_local_data;

extern nca_local_data *local;
internal_proto (local);
void ensure_initialization(void);
internal_proto(ensure_initialization);

int test_for_cas_errors(int *, char *, size_t);
internal_proto(test_for_cas_errors);

int master_get_num_active_images(master *m);
internal_proto(master_get_num_active_images);

void master_bind_active_image_barrier (master *m, counter_barrier *);
internal_proto(master_bind_active_image_barrier);

int master_is_image_active(master *m, int image_num);
internal_proto(master_is_image_active);

void error_on_missing_images(void);
internal_proto(error_on_missing_images);

#define STAT_ERRMSG_ENTRY_CHECK(stat, errmsg, errmsg_len) \
	do { \
	  if (unlikely (test_for_cas_errors(stat, errmsg, errmsg_len)))	\
	    return;\
  	} while(0)

#define STAT_ERRMSG_ENTRY_CHECK_RET(stat, errmsg, errmsg_len, retval) \
	do { \
	  if (unlikely(test_for_cas_errors(stat, errmsg, errmsg_len)))	\
	    return retval;\
  	} while(0)

void cas_master(void (*)(void));
export_proto (cas_master);

#endif
