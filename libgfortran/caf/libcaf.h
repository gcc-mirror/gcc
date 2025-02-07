/* Common declarations for all of GNU Fortran libcaf implementations.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

void _gfortran_caf_register_accessor (
  const int hash,
  void (*accessor) (void *, const int *, void **, int32_t *, void *,
		    caf_token_t, const size_t, size_t *, const size_t *));

void _gfortran_caf_register_accessors_finish (void);

int _gfortran_caf_get_remote_function_index (const int hash);

void _gfortran_caf_get_from_remote (
  caf_token_t token, const gfc_descriptor_t *opt_src_desc,
  const size_t *opt_src_charlen, const int image_index, const size_t dst_size,
  void **dst_data, size_t *opt_dst_charlen, gfc_descriptor_t *opt_dst_desc,
  const bool may_realloc_dst, const int accessor_index, void *add_data,
  const size_t add_data_size, int *stat, caf_team_t *team, int *team_number);

int32_t _gfortran_caf_is_present_on_remote (caf_token_t token, int, int,
					    void *add_data,
					    const size_t add_data_size);

void _gfortran_caf_send_to_remote (
  caf_token_t token, gfc_descriptor_t *opt_dst_desc,
  const size_t *opt_dst_charlen, const int image_index, const size_t src_size,
  const void *src_data, const size_t *opt_src_charlen,
  const gfc_descriptor_t *opt_src_desc, const int accessor_index,
  void *add_data, const size_t add_data_size, int *stat, caf_team_t *team,
  int *team_number);

void _gfortran_caf_transfer_between_remotes (
  caf_token_t dst_token, gfc_descriptor_t *opt_dst_desc,
  size_t *opt_dst_charlen, const int dst_image_index,
  const int dst_access_index, void *dst_add_data,
  const size_t dst_add_data_size, caf_token_t src_token,
  const gfc_descriptor_t *opt_src_desc, const size_t *opt_src_charlen,
  const int src_image_index, const int src_access_index, void *src_add_data,
  const size_t src_add_data_size, const size_t src_size,
  const bool scalar_transfer, int *dst_stat, int *src_stat,
  caf_team_t *dst_team, int *dst_team_number, caf_team_t *src_team,
  int *src_team_number);

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

void _gfortran_caf_random_init (bool, bool);

#endif  /* LIBCAF_H  */
