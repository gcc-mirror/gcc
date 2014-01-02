/* Common declarations for all of GNU Fortran libcaf implementations.
   Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

#include <stdint.h>	/* For int32_t.  */
#include <stddef.h>	/* For ptrdiff_t.  */

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

/* Describes what type of array we are registerring. Keep in sync with
   gcc/fortran/trans.h.  */
typedef enum caf_register_t {
  CAF_REGTYPE_COARRAY_STATIC,
  CAF_REGTYPE_COARRAY_ALLOC,
  CAF_REGTYPE_LOCK,
  CAF_REGTYPE_LOCK_COMP
}
caf_register_t;

/* Linked list of static coarrays registered.  */
typedef struct caf_static_t {
  void **token;
  struct caf_static_t *prev;
}
caf_static_t;


void _gfortran_caf_init (int *, char ***, int *, int *);
void _gfortran_caf_finalize (void);

void * _gfortran_caf_register (ptrdiff_t, caf_register_t, void ***, int *,
			       char *, int);
void _gfortran_caf_deregister (void ***, int *, char *, int);


void _gfortran_caf_sync_all (int *, char *, int);
void _gfortran_caf_sync_images (int, int[], int *, char *, int);

/* FIXME: The CRITICAL functions should be removed;
   the functionality is better represented using Coarray's lock feature.  */
void _gfortran_caf_critical (void);
void _gfortran_caf_critical (void)  { }

void _gfortran_caf_end_critical (void);
void _gfortran_caf_end_critical (void)  { }


void _gfortran_caf_error_stop_str (const char *, int32_t)
     __attribute__ ((noreturn));
void _gfortran_caf_error_stop (int32_t) __attribute__ ((noreturn));

#endif  /* LIBCAF_H  */
