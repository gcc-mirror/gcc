/* Common declarations for all of GNU Fortran libcaf implementations.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.
   Contributed by Tobias Burnus <burnus@net-b.de>

This file is part of the GNU Fortran Coarray Runtime Library (libcaf).

Libcaf is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libcaf is distributed in the hope that it will be useful,
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

#ifndef LIBCAF_H
#define LIBCAF_H

#include <stdbool.h>
#include <stddef.h>	/* For size_t.  */
#include <stdint.h>	/* For int32_t.  */

#include "libgfortran.h"

#if 0
#ifndef __GNUC__
#define __attribute__(x)
#define likely(x)       (x)
#define unlikely(x)     (x)
#else
#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
#endif

/* Definitions of the Fortran 2008 standard; need to kept in sync with
   ISO_FORTRAN_ENV, cf. libgfortran.h.  */
#define STAT_UNLOCKED		0
#define STAT_LOCKED		1
#define STAT_LOCKED_OTHER_IMAGE	2
#define STAT_STOPPED_IMAGE 	6000
#endif

/* Describes what type of array we are registerring. Keep in sync with
   gcc/fortran/trans.h.  */
typedef enum caf_register_t {
  CAF_REGTYPE_COARRAY_STATIC,
  CAF_REGTYPE_COARRAY_ALLOC,
  CAF_REGTYPE_LOCK_STATIC,
  CAF_REGTYPE_LOCK_ALLOC,
  CAF_REGTYPE_CRITICAL,
  CAF_REGTYPE_EVENT_STATIC,
  CAF_REGTYPE_EVENT_ALLOC
}
caf_register_t;

typedef void* caf_token_t;
typedef gfc_array_void gfc_descriptor_t;

/* Linked list of static coarrays registered.  */
typedef struct caf_static_t {
  caf_token_t token;
  struct caf_static_t *prev;
}
caf_static_t;

/* When there is a vector subscript in this dimension, nvec == 0, otherwise,
   lower_bound, upper_bound, stride contains the bounds relative to the declared
   bounds; kind denotes the integer kind of the elements of vector[].  */
typedef struct caf_vector_t {
  size_t nvec;
  union {
    struct {
      void *vector;
      int kind;
    } v;
    struct {
      ptrdiff_t lower_bound, upper_bound, stride;
    } triplet;
  } u;
}
caf_vector_t;


void _gfortran_caf_init (int *, char ***);
void _gfortran_caf_finalize (void);

int _gfortran_caf_this_image (int);
int _gfortran_caf_num_images (int, int);

void *_gfortran_caf_register (size_t, caf_register_t, caf_token_t *, int *,
			      char *, int);
void _gfortran_caf_deregister (caf_token_t *, int *, char *, int);

void _gfortran_caf_sync_all (int *, char *, int);
void _gfortran_caf_sync_memory (int *, char *, int);
void _gfortran_caf_sync_images (int, int[], int *, char *, int);

void _gfortran_caf_stop_numeric (int32_t)
     __attribute__ ((noreturn));
void _gfortran_caf_stop_str (const char *, int32_t)
     __attribute__ ((noreturn));
void _gfortran_caf_error_stop_str (const char *, int32_t)
     __attribute__ ((noreturn));
void _gfortran_caf_error_stop (int32_t) __attribute__ ((noreturn));

void _gfortran_caf_co_broadcast (gfc_descriptor_t *, int, int *, char *, int);
void _gfortran_caf_co_sum (gfc_descriptor_t *, int, int *, char *, int);
void _gfortran_caf_co_min (gfc_descriptor_t *, int, int *, char *, int, int);
void _gfortran_caf_co_max (gfc_descriptor_t *, int, int *, char *, int, int);
void _gfortran_caf_co_reduce (gfc_descriptor_t *, void* (*) (void *, void*),
			      int, int, int *, char *, int, int);

void _gfortran_caf_get (caf_token_t, size_t, int, gfc_descriptor_t *,
			caf_vector_t *, gfc_descriptor_t *, int, int, bool,
			int *);
void _gfortran_caf_send (caf_token_t, size_t, int, gfc_descriptor_t *,
			 caf_vector_t *, gfc_descriptor_t *, int, int, bool,
			 int *);
void _gfortran_caf_sendget (caf_token_t, size_t, int, gfc_descriptor_t *,
			    caf_vector_t *, caf_token_t, size_t, int,
			    gfc_descriptor_t *, caf_vector_t *, int, int, bool);

void _gfortran_caf_atomic_define (caf_token_t, size_t, int, void *, int *,
				  int, int);
void _gfortran_caf_atomic_ref (caf_token_t, size_t, int, void *, int *,
			       int, int);
void _gfortran_caf_atomic_cas (caf_token_t, size_t, int, void *, void *,
			       void *, int *, int, int);
void _gfortran_caf_atomic_op (int, caf_token_t, size_t, int, void *, void *,
			      int *, int, int);

void _gfortran_caf_lock (caf_token_t, size_t, int, int *, int *, char *, int);
void _gfortran_caf_unlock (caf_token_t, size_t, int, int *, char *, int);
void _gfortran_caf_event_post (caf_token_t, size_t, int, int *, char *, int);
void _gfortran_caf_event_wait (caf_token_t, size_t, int, int *, char *, int);
void _gfortran_caf_event_query (caf_token_t, size_t, int, int *, int *);

#endif  /* LIBCAF_H  */
