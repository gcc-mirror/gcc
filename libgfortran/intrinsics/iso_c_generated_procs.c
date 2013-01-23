/* Implementation of the ISO_C_BINDING library helper generated functions.
   Copyright (C) 2007-2013 Free Software Foundation, Inc.
   Contributed by Christopher Rickett.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

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


#include "libgfortran.h"
#include "iso_c_binding.h"


/* TODO: This file needs to be finished so that a function is provided
   for all possible type/kind combinations!  */

#ifdef HAVE_GFC_INTEGER_1
void ISO_C_BINDING_PREFIX (c_f_pointer_i1) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_INTEGER_2
void ISO_C_BINDING_PREFIX (c_f_pointer_i2) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_INTEGER_4
void ISO_C_BINDING_PREFIX (c_f_pointer_i4) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_INTEGER_8
void ISO_C_BINDING_PREFIX (c_f_pointer_i8) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_INTEGER_16
void ISO_C_BINDING_PREFIX (c_f_pointer_i16) (void *, gfc_array_void *,
					     const array_t *);
#endif

#ifdef HAVE_GFC_REAL_4
void ISO_C_BINDING_PREFIX (c_f_pointer_r4) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_REAL_8
void ISO_C_BINDING_PREFIX (c_f_pointer_r8) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_REAL_10
void ISO_C_BINDING_PREFIX (c_f_pointer_r10) (void *, gfc_array_void *,
					     const array_t *);
#endif

#ifdef HAVE_GFC_REAL_16
void ISO_C_BINDING_PREFIX (c_f_pointer_r16) (void *, gfc_array_void *,
					     const array_t *);
#endif

#ifdef HAVE_GFC_COMPLEX_4
void ISO_C_BINDING_PREFIX (c_f_pointer_c4) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_COMPLEX_8
void ISO_C_BINDING_PREFIX (c_f_pointer_c8) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_COMPLEX_10
void ISO_C_BINDING_PREFIX (c_f_pointer_c10) (void *, gfc_array_void *,
					     const array_t *);
#endif

#ifdef HAVE_GFC_COMPLEX_16
void ISO_C_BINDING_PREFIX (c_f_pointer_c16) (void *, gfc_array_void *,
					     const array_t *);
#endif

#ifdef GFC_DEFAULT_CHAR
void ISO_C_BINDING_PREFIX (c_f_pointer_s0) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_LOGICAL_1
void ISO_C_BINDING_PREFIX (c_f_pointer_l1) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_LOGICAL_2
void ISO_C_BINDING_PREFIX (c_f_pointer_l2) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_LOGICAL_4
void ISO_C_BINDING_PREFIX (c_f_pointer_l4) (void *, gfc_array_void *,
					    const array_t *);
#endif

#ifdef HAVE_GFC_LOGICAL_8
void ISO_C_BINDING_PREFIX (c_f_pointer_l8) (void *, gfc_array_void *,
					    const array_t *);
#endif


#ifdef HAVE_GFC_INTEGER_1
/* Set the given Fortran pointer, 'f_ptr_out', to point to the given C
   address, 'c_ptr_in'.  The Fortran pointer is of type integer and
   kind=1.  The function c_f_pointer is used to set up the pointer
   descriptor.  shape is a one-dimensional array of integers
   specifying the upper bounds of the array pointed to by the given C
   address, if applicable.  'shape' is an optional parameter in
   Fortran, so if the user does not provide it, it will come in here
   as NULL.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_i1) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an integer(kind=1).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_INTEGER,
				      (int) sizeof (GFC_INTEGER_1));
}
#endif


#ifdef HAVE_GFC_INTEGER_2
/* Set the given Fortran pointer, 'f_ptr_out', to point to the given C
   address, 'c_ptr_in'.  The Fortran pointer is of type integer and
   kind=2.  The function c_f_pointer is used to set up the pointer
   descriptor.  shape is a one-dimensional array of integers
   specifying the upper bounds of the array pointed to by the given C
   address, if applicable.  'shape' is an optional parameter in
   Fortran, so if the user does not provide it, it will come in here
   as NULL.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_i2) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an integer(kind=2).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_INTEGER,
				      (int) sizeof (GFC_INTEGER_2));
}
#endif


#ifdef HAVE_GFC_INTEGER_4
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type integer and
   kind=4.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_i4) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an integer(kind=4).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_INTEGER,
				      (int) sizeof (GFC_INTEGER_4));
}
#endif


#ifdef HAVE_GFC_INTEGER_8
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type integer and
   kind=8.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_i8) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an integer(kind=8).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_INTEGER,
				      (int) sizeof (GFC_INTEGER_8));
}
#endif


#ifdef HAVE_GFC_INTEGER_16
/* Set the given Fortran pointer, 'f_ptr_out', to point to the given C
   address, 'c_ptr_in'.  The Fortran pointer is of type integer and
   kind=16.  The function c_f_pointer is used to set up the pointer
   descriptor.  shape is a one-dimensional array of integers
   specifying the upper bounds of the array pointed to by the given C
   address, if applicable.  'shape' is an optional parameter in
   Fortran, so if the user does not provide it, it will come in here
   as NULL.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_i16) (void *c_ptr_in,
					gfc_array_void *f_ptr_out,
					const array_t *shape)
{
  /* Here we have an integer(kind=16).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_INTEGER,
				      (int) sizeof (GFC_INTEGER_16));
}
#endif


#ifdef HAVE_GFC_REAL_4
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type real and
   kind=4.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_r4) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an real(kind=4).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_REAL,
				      (int) sizeof (GFC_REAL_4));
}
#endif


#ifdef HAVE_GFC_REAL_8
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type real and
   kind=8.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_r8) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an real(kind=8).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_REAL,
				      (int) sizeof (GFC_REAL_8));
}
#endif


#ifdef HAVE_GFC_REAL_10
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type real and
   kind=10.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_r10) (void *c_ptr_in,
					gfc_array_void *f_ptr_out,
					const array_t *shape)
{
  /* Here we have an real(kind=10).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_REAL,
				      (int) sizeof (GFC_REAL_10));
}
#endif


#ifdef HAVE_GFC_REAL_16
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type real and
   kind=16.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_r16) (void *c_ptr_in,
					gfc_array_void *f_ptr_out,
					const array_t *shape)
{
  /* Here we have an real(kind=16).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_REAL,
				      (int) sizeof (GFC_REAL_16));
}
#endif


#ifdef HAVE_GFC_COMPLEX_4
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type complex and
   kind=4.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_c4) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an complex(kind=4).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_COMPLEX,
				      (int) sizeof (GFC_COMPLEX_4));
}
#endif


#ifdef HAVE_GFC_COMPLEX_8
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type complex and
   kind=8.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_c8) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have an complex(kind=8).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_COMPLEX,
				      (int) sizeof (GFC_COMPLEX_8));
}
#endif


#ifdef HAVE_GFC_COMPLEX_10
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type complex and
   kind=10.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_c10) (void *c_ptr_in,
					gfc_array_void *f_ptr_out,
					const array_t *shape)
{
  /* Here we have an complex(kind=10).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_COMPLEX,
				      (int) sizeof (GFC_COMPLEX_10));
}
#endif


#ifdef HAVE_GFC_COMPLEX_16
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type complex and
   kind=16.  The function c_f_pointer is used to set up the pointer
   descriptor.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_c16) (void *c_ptr_in,
					gfc_array_void *f_ptr_out,
					const array_t *shape)
{
  /* Here we have an complex(kind=16).  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_COMPLEX,
				      (int) sizeof (GFC_COMPLEX_16));
}
#endif


#ifdef GFC_DEFAULT_CHAR
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type character.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_s0) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have a character string of len=1.  */
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_CHARACTER,
				      (int) sizeof (char));
}
#endif


#ifdef HAVE_GFC_LOGICAL_1
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type logical, kind=1.	*/

void
ISO_C_BINDING_PREFIX (c_f_pointer_l1) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have a logical of kind=1.	*/
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_LOGICAL,
				      (int) sizeof (GFC_LOGICAL_1));
}
#endif


#ifdef HAVE_GFC_LOGICAL_2
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type logical, kind=2.	*/

void
ISO_C_BINDING_PREFIX (c_f_pointer_l2) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have a logical of kind=2.	*/
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_LOGICAL,
				      (int) sizeof (GFC_LOGICAL_2));
}
#endif


#ifdef HAVE_GFC_LOGICAL_4
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type logical, kind=4.	*/

void
ISO_C_BINDING_PREFIX (c_f_pointer_l4) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have a logical of kind=4.	*/
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_LOGICAL,
				      (int) sizeof (GFC_LOGICAL_4));
}
#endif


#ifdef HAVE_GFC_LOGICAL_8
/* Set the given Fortran pointer, f_ptr_out, to point to the given C
   address, c_ptr_in.  The Fortran pointer is of type logical, kind=8.	*/

void
ISO_C_BINDING_PREFIX (c_f_pointer_l8) (void *c_ptr_in,
				       gfc_array_void *f_ptr_out,
				       const array_t *shape)
{
  /* Here we have a logical of kind=8.	*/
  ISO_C_BINDING_PREFIX (c_f_pointer) (c_ptr_in, f_ptr_out, shape,
				      (int) BT_LOGICAL,
				      (int) sizeof (GFC_LOGICAL_8));
}
#endif
