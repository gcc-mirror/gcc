/* Common declarations for all of GNU Fortran libcaf implementations.
   Copyright (C) 2011-2019 Free Software Foundation, Inc.
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
#endif

/* Definitions of the Fortran 2008 standard; need to kept in sync with
   ISO_FORTRAN_ENV, cf. gcc/fortran/libgfortran.h.  */
typedef enum
{
  CAF_STAT_UNLOCKED = 0,
  CAF_STAT_LOCKED,
  CAF_STAT_LOCKED_OTHER_IMAGE,
  CAF_STAT_STOPPED_IMAGE = 6000,
  CAF_STAT_FAILED_IMAGE  = 6001
}
caf_stat_codes_t;


/* Describes what type of array we are registerring.  Keep in sync with
   gcc/fortran/trans.h.  */
typedef enum caf_register_t {
  CAF_REGTYPE_COARRAY_STATIC,
  CAF_REGTYPE_COARRAY_ALLOC,
  CAF_REGTYPE_LOCK_STATIC,
  CAF_REGTYPE_LOCK_ALLOC,
  CAF_REGTYPE_CRITICAL,
  CAF_REGTYPE_EVENT_STATIC,
  CAF_REGTYPE_EVENT_ALLOC,
  CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY,
  CAF_REGTYPE_COARRAY_ALLOC_ALLOCATE_ONLY
}
caf_register_t;

/* Describes the action to take on _caf_deregister.  Keep in sync with
   gcc/fortran/trans.h.  */
typedef enum caf_deregister_t {
  CAF_DEREGTYPE_COARRAY_DEREGISTER,
  CAF_DEREGTYPE_COARRAY_DEALLOCATE_ONLY
}
caf_deregister_t;

typedef void* caf_token_t;
typedef void * caf_team_t;
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

typedef enum caf_ref_type_t {
  /* Reference a component of a derived type, either regular one or an
     allocatable or pointer type.  For regular ones idx in caf_reference_t is
     set to -1.  */
  CAF_REF_COMPONENT,
  /* Reference an allocatable array.  */
  CAF_REF_ARRAY,
  /* Reference a non-allocatable/non-pointer array.  */
  CAF_REF_STATIC_ARRAY
} caf_ref_type_t;

typedef enum caf_array_ref_t {
  /* No array ref.  This terminates the array ref.  */
  CAF_ARR_REF_NONE = 0,
  /* Reference array elements given by a vector.  Only for this mode
     caf_reference_t.u.a.dim[i].v is valid.  */
  CAF_ARR_REF_VECTOR,
  /* A full array ref (:).  */
  CAF_ARR_REF_FULL,
  /* Reference a range on elements given by start, end and stride.  */
  CAF_ARR_REF_RANGE,
  /* Only a single item is referenced given in the start member.  */
  CAF_ARR_REF_SINGLE,
  /* An array ref of the kind (i:), where i is an arbitrary valid index in the
     array.  The index i is given in the start member.  */
  CAF_ARR_REF_OPEN_END,
  /* An array ref of the kind (:i), where the lower bound of the array ref
     is given by the remote side.  The index i is given in the end member.  */
  CAF_ARR_REF_OPEN_START
} caf_array_ref_t;

/* References to remote components of a derived type.  */
typedef struct caf_reference_t {
  /* A pointer to the next ref or NULL.  */
  struct caf_reference_t *next;
  /* The type of the reference.  */
  /* caf_ref_type_t, replaced by int to allow specification in fortran FE.  */
  int type;
  /* The size of an item referenced in bytes.  I.e. in an array ref this is
     the factor to advance the array pointer with to get to the next item.
     For component refs this gives just the size of the element referenced.  */
  size_t item_size;
  union {
    struct {
      /* The offset (in bytes) of the component in the derived type.  */
      ptrdiff_t offset;
      /* The offset (in bytes) to the caf_token associated with this
	 component.  NULL, when not allocatable/pointer ref.  */
      ptrdiff_t caf_token_offset;
    } c;
    struct {
      /* The mode of the array ref.  See CAF_ARR_REF_*.  */
      /* caf_array_ref_t, replaced by unsigend char to allow specification in
	 fortran FE.  */
      unsigned char mode[GFC_MAX_DIMENSIONS];
      /* The type of a static array.  Unset for array's with descriptors.  */
      int static_array_type;
      /* Subscript refs (s) or vector refs (v).  */
      union {
	struct {
	  /* The start and end boundary of the ref and the stride.  */
	  index_type start, end, stride;
	} s;
	struct {
	  /* nvec entries of kind giving the elements to reference.  */
	  void *vector;
	  /* The number of entries in vector.  */
	  size_t nvec;
	  /* The integer kind used for the elements in vector.  */
	  int kind;
	} v;
      } dim[GFC_MAX_DIMENSIONS];
    } a;
  } u;
} caf_reference_t;

void _gfortran_caf_init (int *, char ***);
void _gfortran_caf_finalize (void);

int _gfortran_caf_this_image (int);
int _gfortran_caf_num_images (int, int);

void _gfortran_caf_register (size_t, caf_register_t, caf_token_t *,
			     gfc_descriptor_t *, int *, char *, size_t);
void _gfortran_caf_deregister (caf_token_t *, caf_deregister_t, int *, char *,
			       size_t);

void _gfortran_caf_sync_all (int *, char *, size_t);
void _gfortran_caf_sync_memory (int *, char *, size_t);
void _gfortran_caf_sync_images (int, int[], int *, char *, size_t);

void _gfortran_caf_stop_numeric (int, bool)
     __attribute__ ((noreturn));
void _gfortran_caf_stop_str (const char *, size_t, bool)
     __attribute__ ((noreturn));
void _gfortran_caf_error_stop_str (const char *, size_t, bool)
     __attribute__ ((noreturn));
void _gfortran_caf_error_stop (int, bool) __attribute__ ((noreturn));
void _gfortran_caf_fail_image (void) __attribute__ ((noreturn));

void _gfortran_caf_co_broadcast (gfc_descriptor_t *, int, int *, char *, size_t);
void _gfortran_caf_co_sum (gfc_descriptor_t *, int, int *, char *, size_t);
void _gfortran_caf_co_min (gfc_descriptor_t *, int, int *, char *, int, size_t);
void _gfortran_caf_co_max (gfc_descriptor_t *, int, int *, char *, int, size_t);
void _gfortran_caf_co_reduce (gfc_descriptor_t *, void* (*) (void *, void*),
			      int, int, int *, char *, int, size_t);

void _gfortran_caf_get (caf_token_t, size_t, int, gfc_descriptor_t *,
			caf_vector_t *, gfc_descriptor_t *, int, int, bool,
			int *);
void _gfortran_caf_send (caf_token_t, size_t, int, gfc_descriptor_t *,
			 caf_vector_t *, gfc_descriptor_t *, int, int, bool,
			 int *);
void _gfortran_caf_sendget (caf_token_t, size_t, int, gfc_descriptor_t *,
			    caf_vector_t *, caf_token_t, size_t, int,
			    gfc_descriptor_t *, caf_vector_t *, int, int, bool);

void _gfortran_caf_get_by_ref (caf_token_t token, int image_idx,
	gfc_descriptor_t *dst, caf_reference_t *refs, int dst_kind,
	int src_kind, bool may_require_tmp, bool dst_reallocatable, int *stat,
	int src_type);
void _gfortran_caf_send_by_ref (caf_token_t token, int image_index,
	gfc_descriptor_t *src, caf_reference_t *refs, int dst_kind,
	int src_kind, bool may_require_tmp, bool dst_reallocatable, int *stat,
	int dst_type);
void _gfortran_caf_sendget_by_ref (
	caf_token_t dst_token, int dst_image_index, caf_reference_t *dst_refs,
	caf_token_t src_token, int src_image_index, caf_reference_t *src_refs,
	int dst_kind, int src_kind, bool may_require_tmp, int *dst_stat,
	int *src_stat, int dst_type, int src_type);

void _gfortran_caf_atomic_define (caf_token_t, size_t, int, void *, int *,
				  int, int);
void _gfortran_caf_atomic_ref (caf_token_t, size_t, int, void *, int *,
			       int, int);
void _gfortran_caf_atomic_cas (caf_token_t, size_t, int, void *, void *,
			       void *, int *, int, int);
void _gfortran_caf_atomic_op (int, caf_token_t, size_t, int, void *, void *,
			      int *, int, int);

void _gfortran_caf_lock (caf_token_t, size_t, int, int *, int *, char *, size_t);
void _gfortran_caf_unlock (caf_token_t, size_t, int, int *, char *, size_t);
void _gfortran_caf_event_post (caf_token_t, size_t, int, int *, char *, size_t);
void _gfortran_caf_event_wait (caf_token_t, size_t, int, int *, char *, size_t);
void _gfortran_caf_event_query (caf_token_t, size_t, int, int *, int *);

void _gfortran_caf_failed_images (gfc_descriptor_t *,
				  caf_team_t * __attribute__ ((unused)), int *);
int _gfortran_caf_image_status (int, caf_team_t * __attribute__ ((unused)));
void _gfortran_caf_stopped_images (gfc_descriptor_t *,
				   caf_team_t * __attribute__ ((unused)),
				   int *);

int _gfortran_caf_is_present (caf_token_t, int, caf_reference_t *);

#endif  /* LIBCAF_H  */
