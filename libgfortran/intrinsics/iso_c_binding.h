/* Copyright (C) 2007-2014 Free Software Foundation, Inc.
   Contributed by Christopher Rickett.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
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


/* Declarations for ISO_C_BINDING library helper functions.  */

#ifndef GFOR_ISO_C_BINDING_H
#define GFOR_ISO_C_BINDING_H

#include "libgfortran.h"

typedef struct c_ptr
{
  void *c_address;
}
c_ptr_t;

typedef struct c_funptr
{
  void *c_address;
}
c_funptr_t;

#define ISO_C_BINDING_PREFIX(a) __iso_c_binding_##a

void ISO_C_BINDING_PREFIX(c_f_pointer)(void *, gfc_array_void *,
				       const array_t *, int, int);

void ISO_C_BINDING_PREFIX(c_f_pointer_u0) (void *, gfc_array_void *,
					   const array_t *);
void ISO_C_BINDING_PREFIX(c_f_pointer_d0) (void *, gfc_array_void *,
					   const array_t *);

#endif
