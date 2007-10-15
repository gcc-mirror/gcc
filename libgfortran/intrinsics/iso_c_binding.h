/* Copyright (C) 2007 Free Software Foundation, Inc.
   Contributed by Christopher Rickett.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


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

/* The second param here may change, once procedure pointers are
   implemented.  */
void ISO_C_BINDING_PREFIX(c_f_procpointer) (void *, gfc_array_void *);

void ISO_C_BINDING_PREFIX(c_f_pointer_u0) (void *, gfc_array_void *,
					   const array_t *);
void ISO_C_BINDING_PREFIX(c_f_pointer_d0) (void *, gfc_array_void *,
					   const array_t *);

#endif
