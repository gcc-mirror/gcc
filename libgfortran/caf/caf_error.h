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

#ifndef CAF_ERROR_H
#define CAF_ERROR_H

#include <stddef.h>

/* Emit a printf style error message and exit with EXIT_FAILURE.  */

void caf_runtime_error (const char *format, ...);

/* If `stat` is given, it will be set to 1 and procedure returns to the caller.
   If additionally `errmsg` is non-NULL, then printf-style `format` will by
   printed to `errmsg`.  If the resulting message is longer then `errmsg_len`,
   it will be truncated, else filled with spaces.
   If `stat` is not given, then the printf-formated message will be emited to
   stderr and the program terminates with EXIT_FAILURE.  */

void caf_internal_error (const char *format, int *stat, char *errmsg,
			 size_t errmsg_len, ...);

#endif
