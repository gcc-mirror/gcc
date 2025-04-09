/* Generic implementation of the reduce intrinsic
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Paul Thomas <pault@gcc.gnu.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Ligbfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WarrayANTY; without even the implied warrayanty of
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
#include <string.h>
#include <stdio.h>

typedef GFC_FULL_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, char) parray;

extern void reduce (parray *, parray *,
		    void (*operation) (void *, void *, void *),
		    GFC_INTEGER_4 *, gfc_array_l4 *, void *, void *);
export_proto (reduce);

void
reduce (parray *ret,
	parray *array,
	void (*operation) (void *, void *, void *),
	GFC_INTEGER_4 *dim,
	gfc_array_l4 *mask,
	void *identity,
	void *ordered __attribute__ ((unused)))
{
  GFC_LOGICAL_4 maskR = 0;
  void *array_ptr;
  void *buffer_ptr;
  void *zero;
  void *buffer;
  void *res;
  index_type ext0, ext1, ext2;
  index_type str0, str1, str2;
  index_type idx0, idx1, idx2;
  index_type dimen, dimen_m1, ldx, ext, str;
  bool started;
  bool masked = false;
  bool dim_present = dim != NULL;
  bool mask_present = mask != NULL;
  bool identity_present = identity != NULL;
  bool scalar_result;
  int i, j;
  int array_rank = (int)GFC_DESCRIPTOR_RANK (array);
  size_t elem_len = GFC_DESCRIPTOR_SIZE (array);

/* The standard mandates that OPERATION is a pure scalar function such that in
   the reduction below:

	*buffer_ptr = OPERATION (*buffer_ptr, array(idx1, idx2, idx3))

   To make this type agnostic, the front end builds a wrapper, that puts the
   assignment within a subroutine and transforms it into a pointer operation:

	operation (buffer_ptr, &array(idx1, idx2, idx3), buffer_ptr)

   The wrapper also detects the presence or not of the second argument. If it
   is not present, the wrapper effects *third_arg = *first_arg.

   The only information needed about the type of array is its element size. In
   both modes, the wrapper takes care of allocatable components correctly,
   which is why the second mode is used to fill the result elements.  */

  if (dim_present)
    {
      if ((*dim < 1) || (*dim > (GFC_INTEGER_4)array_rank))
	runtime_error ("Mismatch between DIM and the rank of ARRAY in the "
		       "REDUCE intrinsic (%d/%d)", (int)*dim, array_rank);
      dimen = (index_type) *dim;
    }
  else
    dimen = 1;
  dimen_m1 = dimen -1;

  /* Set up the shape and strides for the reduction. This is made relatively
     painless by the use of pointer arithmetic throughout (except for MASK,
     whose type is known.  */
  ext0 = ext1 = ext2 = 1;
  str0 = str1 = str2 = 1;

  scalar_result = (!dim_present && array_rank > 1) || array_rank == 1;

  j = 0;
  for (i = 0; i < array_rank; i++)
    {
      /* Obtain the shape of the reshaped ARRAY.  */
      ext = GFC_DESCRIPTOR_EXTENT (array,i);
      str = GFC_DESCRIPTOR_STRIDE (array,i);

      if (masked && (ext != GFC_DESCRIPTOR_EXTENT (mask, i)))
	{
	  int mext = (int)GFC_DESCRIPTOR_EXTENT (mask, i);
	  runtime_error ("shape mismatch between ARRAY and MASK in the REDUCE "
			 "intrinsic (%zd/%d)", ext, mext);
	}

      if (scalar_result)
	{
	  ext1 *= ext;
	  continue;
	}
      else if (i < (int)dimen_m1)
	ext0 *= ext;
      else if (i == (int)dimen_m1)
	ext1 = ext;
      else
	ext2 *= ext;

      /* The dimensions of the return array.  */
      if (i != (int)dimen_m1)
	{
	  str = GFC_DESCRIPTOR_STRIDE (array, j);
	  GFC_DIMENSION_SET (ret->dim[j], 0, ext - 1, str);
	  j++;
	}
    }

  if (!scalar_result)
    {
      str1 = GFC_DESCRIPTOR_STRIDE (array, dimen_m1);
      if (dimen < array_rank)
	str2 = GFC_DESCRIPTOR_STRIDE (array, dimen);
      else
	str2 = 1;
    }

  /* Allocate the result data, the result buffer and zero.  */
  if (ret->base_addr == NULL)
    ret->base_addr = calloc ((size_t)(ext0 * ext2), elem_len);
  buffer = calloc (1, elem_len);
  zero = calloc (1, elem_len);

  /* Now loop over the first and third dimensions of the reshaped ARRAY.  */
  for (idx0 = 0; idx0 < ext0; idx0++)
    {
      for (idx2 = 0; idx2 < ext2; idx2++)
	{
	  ldx = idx0 * str0  + idx2 * str2;
	  if (mask_present)
	    maskR = mask->base_addr[ldx];

	  started = (mask_present && maskR) || !mask_present;

	  buffer_ptr = array->base_addr
			+ (size_t)((idx0 * str0 + idx2 * str2) * elem_len);

	  /* Start the iteration over the second dimension of ARRAY.  */
	  for (idx1 = 1; idx1 < ext1; idx1++)
	    {
	      /* If masked, cycle until after first element that is not masked
		 out. Then set 'started' and cycle so that this becomes the
		 first element in the reduction.  */
	      ldx = idx0 * str0 + idx1 * str1 + idx2 * str2;
	      if (mask_present)
		maskR = mask->base_addr[ldx];

	      array_ptr = array->base_addr
			  + (size_t)((idx0 * str0 + idx1 * str1
				      + idx2 * str2) * elem_len);
	      if (!started)
		{
		  if (mask_present && maskR)
		    started = true;
		  buffer_ptr = array_ptr;
		  continue;
		}

	      /* Call the operation, if not masked out, with the previous
		 element or the buffer and current element as arguments. The
		 result is stored in the buffer and the buffer_ptr set to
		 point to buffer, instead of the previous array element.  */
	      if ((mask_present && maskR) || !mask_present)
		{
		  operation (buffer_ptr, array_ptr, buffer);
		  buffer_ptr = buffer;
		}
	    }

	  /* Now the result of the iteration is transferred to the returned
	     result. If this result element is empty emit an error or, if
	     available, set to identity. Note that str1 is paired with idx2
	     here because the result skips a dimension.  */
	  res = ret->base_addr + (size_t)((idx0 * str0 + idx2 * str1) * elem_len);
	  if (started)
	    {
	      operation (buffer_ptr, NULL, res);
	      operation (zero, NULL, buffer);
	    }
	  else
	    {
	      if (!identity_present)
		runtime_error ("Empty column and no IDENTITY in REDUCE "
			       "intrinsic");
	      else
		operation (identity, NULL, res);
	    }
	}
    }
  free (zero);
  free (buffer);
}


extern void * reduce_scalar (parray *,
			   void (*operation) (void *, void *, void *),
			   GFC_INTEGER_4 *, gfc_array_l4 *, void *, void *);
export_proto (reduce_scalar);

void *
reduce_scalar (parray *array,
	       void (*operation) (void *, void *, void *),
	       GFC_INTEGER_4 *dim,
	       gfc_array_l4 *mask,
	       void *identity,
	       void *ordered)
{
  parray ret;
  ret.base_addr = NULL;
  ret.dtype.rank = 0;
  reduce (&ret, array, operation, dim, mask, identity, ordered);
  return (void *)ret.base_addr;
}

extern void reduce_c (parray *, gfc_charlen_type, parray *,
		      void (*operation) (void *, void *, void *),
		      GFC_INTEGER_4 *, gfc_array_l4 *, void *, void *,
		      gfc_charlen_type, gfc_charlen_type);
export_proto (reduce_c);

void
reduce_c (parray *ret,
	  gfc_charlen_type ret_strlen __attribute__ ((unused)),
	  parray *array,
	  void (*operation) (void *, void *, void *),
	  GFC_INTEGER_4 *dim,
	  gfc_array_l4 *mask,
	  void *identity,
	  void *ordered,
	  gfc_charlen_type array_strlen __attribute__ ((unused)),
	  gfc_charlen_type identity_strlen __attribute__ ((unused)))
{
  /* The frontend constraints make string length checking redundant.  Also, the
     scalar symbol is flagged to be allocatable in trans-intrinsic.cc so that
     gfc_conv_procedure_call does the necessary allocation/deallocation.  */
  reduce (ret, array, operation, dim, mask, identity, ordered);
}


extern void reduce_scalar_c (void *, gfc_charlen_type, parray *,
		      void (*operation) (void *, void *, void *),
		      GFC_INTEGER_4 *, gfc_array_l4 *, void *, void *,
		      gfc_charlen_type, gfc_charlen_type);
export_proto (reduce_scalar_c);


void
reduce_scalar_c (void *res,
		 gfc_charlen_type res_strlen __attribute__ ((unused)),
		 parray *array,
		 void (*operation) (void *, void *, void *),
		 GFC_INTEGER_4 *dim,
		 gfc_array_l4 *mask,
		 void *identity,
		 void *ordered,
		 gfc_charlen_type array_strlen __attribute__ ((unused)),
		 gfc_charlen_type identity_strlen __attribute__ ((unused)))
{
  parray ret;
  ret.base_addr = NULL;
  ret.dtype.rank = 0;
  /* The frontend constraints make string length checking redundant.  */
  reduce (&ret, array, operation, dim, mask, identity, ordered);
  if (res)
    {
      memcpy (res, ret.base_addr, GFC_DESCRIPTOR_SIZE (array));
      if (ret.base_addr) free (ret.base_addr);
    }
  else
    res = ret.base_addr;
}
