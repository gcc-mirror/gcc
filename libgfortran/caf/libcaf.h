/* Common declarations for all of GNU Fortran libcaf implementations.
   Copyright (C) 2011
   Free Software Foundation, Inc.
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

#include <stdint.h>
#include <string.h>

/* Definitions of the Fortran 2008 standard; need to kept in sync with
   ISO_FORTRAN_ENV, cf. libgfortran.h.  */
#define STAT_UNLOCKED		0
#define STAT_LOCKED		1
#define STAT_LOCKED_OTHER_IMAGE	2
#define STAT_STOPPED_IMAGE 	3


void _gfortran_caf_init (int *, char ***, int *, int *);
void _gfortran_caf_finalize (void);

int _gfortran_caf_sync_all (char *, int);
int _gfortran_caf_sync_images (int count, int images[], char *, int);

void _gfortran_caf_critical (void);
void _gfortran_caf_end_critical (void);

void _gfortran_caf_error_stop_str (const char *, int32_t) __attribute__ ((noreturn));
void _gfortran_caf_error_stop (int32_t) __attribute__ ((noreturn));

#endif  /* LIBCAF_H  */
