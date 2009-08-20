/* Implementation of the ISO_C_BINDING library helper functions.
   Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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


/* Implement the functions and subroutines provided by the intrinsic
   iso_c_binding module.  */

#include "libgfortran.h"
#include "iso_c_binding.h"

#include <stdlib.h>


/* Set the fields of a Fortran pointer descriptor to point to the
   given C address.  It uses c_f_pointer_u0 for the common
   fields, and will set up the information necessary if this C address
   is to an array (i.e., offset, type, element size).  The parameter
   c_ptr_in represents the C address to have Fortran point to.  The
   parameter f_ptr_out is the Fortran pointer to associate with the C
   address.  The parameter shape is a one-dimensional array of integers
   specifying the upper bound(s) of the array pointed to by the given C
   address, if applicable.  The shape parameter is optional in Fortran,
   which will cause it to come in here as NULL.  The parameter type is
   the type of the data being pointed to (i.e.,libgfortran.h). The
   elem_size parameter is the size, in bytes, of the data element being
   pointed to.  If the address is for an array, then the size needs to
   be the size of a single element (i.e., for an array of doubles, it
   needs to be the number of bytes for the size of one double).  */

void
ISO_C_BINDING_PREFIX (c_f_pointer) (void *c_ptr_in,
                                    gfc_array_void *f_ptr_out,
                                    const array_t *shape,
                                    int type, int elemSize)
{
  if (shape != NULL)
    {
      f_ptr_out->offset = 0;

      /* Set the necessary dtype field for all pointers.  */
      f_ptr_out->dtype = 0;

      /* Put in the element size.  */
      f_ptr_out->dtype = f_ptr_out->dtype | (elemSize << GFC_DTYPE_SIZE_SHIFT);

      /* Set the data type (e.g., GFC_DTYPE_INTEGER).  */
      f_ptr_out->dtype = f_ptr_out->dtype | (type << GFC_DTYPE_TYPE_SHIFT);
    }
  
  /* Use the generic version of c_f_pointer to set common fields.  */
  ISO_C_BINDING_PREFIX (c_f_pointer_u0) (c_ptr_in, f_ptr_out, shape);
}


/* A generic function to set the common fields of all descriptors, no
   matter whether it's to a scalar or an array.  Fields set are: data,
   and if appropriate, rank, offset, dim[*].lbound, dim[*].ubound, and
   dim[*].stride.  Parameter shape is a rank 1 array of integers
   containing the upper bound of each dimension of what f_ptr_out
   points to.  The length of this array must be EXACTLY the rank of
   what f_ptr_out points to, as required by the draft (J3/04-007).  If
   f_ptr_out points to a scalar, then this parameter will be NULL.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_u0) (void *c_ptr_in,
                                       gfc_array_void *f_ptr_out,
                                       const array_t *shape)
{
  int i = 0;
  int shapeSize = 0;

  GFC_DESCRIPTOR_DATA (f_ptr_out) = c_ptr_in;

  if (shape != NULL)
    {
      f_ptr_out->offset = 0;
      shapeSize = 0;
      
      /* shape's length (rank of the output array) */
      shapeSize = shape->dim[0].ubound + 1 - shape->dim[0].lbound;
      for (i = 0; i < shapeSize; i++)
        {
          /* Lower bound is 1, as specified by the draft.  */
          f_ptr_out->dim[i].lbound = 1;
          /* Have to allow for the SHAPE array to be any valid kind for
             an INTEGER type.  */
#ifdef HAVE_GFC_INTEGER_1
	  if (GFC_DESCRIPTOR_SIZE (shape) == 1)
	    f_ptr_out->dim[i].ubound = ((GFC_INTEGER_1 *) (shape->data))[i];
#endif
#ifdef HAVE_GFC_INTEGER_2
	  if (GFC_DESCRIPTOR_SIZE (shape) == 2)
	    f_ptr_out->dim[i].ubound = ((GFC_INTEGER_2 *) (shape->data))[i];
#endif
#ifdef HAVE_GFC_INTEGER_4
	  if (GFC_DESCRIPTOR_SIZE (shape) == 4)
	    f_ptr_out->dim[i].ubound = ((GFC_INTEGER_4 *) (shape->data))[i];
#endif
#ifdef HAVE_GFC_INTEGER_8
	  if (GFC_DESCRIPTOR_SIZE (shape) == 8)
	    f_ptr_out->dim[i].ubound = ((GFC_INTEGER_8 *) (shape->data))[i];
#endif
#ifdef HAVE_GFC_INTEGER_16
	  if (GFC_DESCRIPTOR_SIZE (shape) == 16)
	    f_ptr_out->dim[i].ubound = ((GFC_INTEGER_16 *) (shape->data))[i];
#endif		
        }

      /* Set the offset and strides.
         offset is (sum of (dim[i].lbound * dim[i].stride) for all
         dims) the -1 means we'll back the data pointer up that much
         perhaps we could just realign the data pointer and not change
         the offset?  */
      f_ptr_out->dim[0].stride = 1;
      f_ptr_out->offset = f_ptr_out->dim[0].lbound * f_ptr_out->dim[0].stride;
      for (i = 1; i < shapeSize; i++)
        {
          f_ptr_out->dim[i].stride = ((f_ptr_out->dim[i-1].ubound + 1)
            - f_ptr_out->dim[i-1].lbound) * f_ptr_out->dim[i-1].stride;
          f_ptr_out->offset += f_ptr_out->dim[i].lbound
            * f_ptr_out->dim[i].stride;
        }

      f_ptr_out->offset *= -1;

      /* All we know is the rank, so set it, leaving the rest alone.
         Make NO assumptions about the state of dtype coming in!  If we
         shift right by TYPE_SHIFT bits we'll throw away the existing
         rank.  Then, shift left by the same number to shift in zeros
         and or with the new rank.  */
      f_ptr_out->dtype = ((f_ptr_out->dtype >> GFC_DTYPE_TYPE_SHIFT)
                           << GFC_DTYPE_TYPE_SHIFT) | shapeSize;
    }
}


/* Sets the descriptor fields for a Fortran pointer to a derived type,
   using c_f_pointer_u0 for the majority of the work.  */

void
ISO_C_BINDING_PREFIX (c_f_pointer_d0) (void *c_ptr_in,
                                       gfc_array_void *f_ptr_out,
                                       const array_t *shape)
{
  /* Set the common fields.  */
  ISO_C_BINDING_PREFIX (c_f_pointer_u0) (c_ptr_in, f_ptr_out, shape);

  /* Preserve the size and rank bits, but reset the type.  */
  if (shape != NULL)
    {
      f_ptr_out->dtype = f_ptr_out->dtype & (~GFC_DTYPE_TYPE_MASK);
      f_ptr_out->dtype = f_ptr_out->dtype
			 | (GFC_DTYPE_DERIVED << GFC_DTYPE_TYPE_SHIFT);
    }
}
