/* Generic implementation of the UNPACK intrinsic
   Copyright 2002, 2003, 2004, 2005, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Ligbfortran is distributed in the hope that it will be useful,
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
#include <stdlib.h>
#include <assert.h>
#include <string.h>

/* All the bounds checking for unpack in one function.  If field is NULL,
   we don't check it, for the unpack0 functions.  */

static void
unpack_bounds (gfc_array_char *ret, const gfc_array_char *vector,
	 const gfc_array_l1 *mask, const gfc_array_char *field)
{
  index_type vec_size, mask_count;
  vec_size = size0 ((array_t *) vector);
  mask_count = count_0 (mask);
  if (vec_size < mask_count)
    runtime_error ("Incorrect size of return value in UNPACK"
		   " intrinsic: should be at least %ld, is"
		   " %ld", (long int) mask_count,
		   (long int) vec_size);

  if (field != NULL)
    bounds_equal_extents ((array_t *) field, (array_t *) mask,
			  "FIELD", "UNPACK");

  if (ret->data != NULL)
    bounds_equal_extents ((array_t *) ret, (array_t *) mask,
			  "return value", "UNPACK");

}

static void
unpack_internal (gfc_array_char *ret, const gfc_array_char *vector,
		 const gfc_array_l1 *mask, const gfc_array_char *field,
		 index_type size)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rs;
  char * restrict rptr;
  /* v.* indicates the vector array.  */
  index_type vstride0;
  char *vptr;
  /* f.* indicates the field array.  */
  index_type fstride[GFC_MAX_DIMENSIONS];
  index_type fstride0;
  const char *fptr;
  /* m.* indicates the mask array.  */
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type mstride0;
  const GFC_LOGICAL_1 *mptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type n;
  index_type dim;

  int empty;
  int mask_kind;

  empty = 0;

  mptr = mask->data;

  /* Use the same loop for all logical types, by using GFC_LOGICAL_1
     and using shifting to address size and endian issues.  */

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    {
      /*  Don't convert a NULL pointer as we use test for NULL below.  */
      if (mptr)
	mptr = GFOR_POINTER_TO_L1 (mptr, mask_kind);
    }
  else
    runtime_error ("Funny sized logical array");

  if (ret->data == NULL)
    {
      /* The front end has signalled that we need to populate the
	 return array descriptor.  */
      dim = GFC_DESCRIPTOR_RANK (mask);
      rs = 1;
      for (n = 0; n < dim; n++)
	{
	  count[n] = 0;
	  GFC_DIMENSION_SET(ret->dim[n], 0,
			    GFC_DESCRIPTOR_EXTENT(mask,n) - 1, rs);
	  extent[n] = GFC_DESCRIPTOR_EXTENT(ret,n);
	  empty = empty || extent[n] <= 0;
	  rstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(ret, n);
	  fstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(field, n);
	  mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask, n);
	  rs *= extent[n];
	}
      ret->offset = 0;
      ret->data = internal_malloc_size (rs * size);
    }
  else
    {
      dim = GFC_DESCRIPTOR_RANK (ret);
      for (n = 0; n < dim; n++)
	{
	  count[n] = 0;
	  extent[n] = GFC_DESCRIPTOR_EXTENT(ret,n);
	  empty = empty || extent[n] <= 0;
	  rstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(ret, n);
	  fstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(field, n);
	  mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask, n);
	}
    }

  if (empty)
    return;

  vstride0 = GFC_DESCRIPTOR_STRIDE_BYTES(vector,0);
  rstride0 = rstride[0];
  fstride0 = fstride[0];
  mstride0 = mstride[0];
  rptr = ret->data;
  fptr = field->data;
  vptr = vector->data;

  while (rptr)
    {
      if (*mptr)
        {
          /* From vector.  */
          memcpy (rptr, vptr, size);
          vptr += vstride0;
        }
      else
        {
          /* From field.  */
          memcpy (rptr, fptr, size);
        }
      /* Advance to the next element.  */
      rptr += rstride0;
      fptr += fstride0;
      mptr += mstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          rptr -= rstride[n] * extent[n];
          fptr -= fstride[n] * extent[n];
          mptr -= mstride[n] * extent[n];
          n++;
          if (n >= dim)
            {
              /* Break out of the loop.  */
              rptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              rptr += rstride[n];
              fptr += fstride[n];
              mptr += mstride[n];
            }
        }
    }
}

extern void unpack1 (gfc_array_char *, const gfc_array_char *,
		     const gfc_array_l1 *, const gfc_array_char *);
export_proto(unpack1);

void
unpack1 (gfc_array_char *ret, const gfc_array_char *vector,
	 const gfc_array_l1 *mask, const gfc_array_char *field)
{
  index_type type_size;
  index_type size;

  if (unlikely(compile_options.bounds_check))
    unpack_bounds (ret, vector, mask, field);

  type_size = GFC_DTYPE_TYPE_SIZE (vector);
  size = GFC_DESCRIPTOR_SIZE (vector);

  switch(type_size)
    {
    case GFC_DTYPE_LOGICAL_1:
    case GFC_DTYPE_INTEGER_1:
    case GFC_DTYPE_DERIVED_1:
      unpack1_i1 ((gfc_array_i1 *) ret, (gfc_array_i1 *) vector,
		  mask, (gfc_array_i1 *) field);
      return;

    case GFC_DTYPE_LOGICAL_2:
    case GFC_DTYPE_INTEGER_2:
      unpack1_i2 ((gfc_array_i2 *) ret, (gfc_array_i2 *) vector,
		  mask, (gfc_array_i2 *) field);
      return;

    case GFC_DTYPE_LOGICAL_4:
    case GFC_DTYPE_INTEGER_4:
      unpack1_i4 ((gfc_array_i4 *) ret, (gfc_array_i4 *) vector,
		  mask, (gfc_array_i4 *) field);
      return;

    case GFC_DTYPE_LOGICAL_8:
    case GFC_DTYPE_INTEGER_8:
      unpack1_i8 ((gfc_array_i8 *) ret, (gfc_array_i8 *) vector,
		  mask, (gfc_array_i8 *) field);
      return;

#ifdef HAVE_GFC_INTEGER_16
    case GFC_DTYPE_LOGICAL_16:
    case GFC_DTYPE_INTEGER_16:
      unpack1_i16 ((gfc_array_i16 *) ret, (gfc_array_i16 *) vector,
		   mask, (gfc_array_i16 *) field);
      return;
#endif
    case GFC_DTYPE_REAL_4:
      unpack1_r4 ((gfc_array_r4 *) ret, (gfc_array_r4 *) vector,
		  mask, (gfc_array_r4 *) field);
      return;

    case GFC_DTYPE_REAL_8:
      unpack1_r8 ((gfc_array_r8 *) ret, (gfc_array_r8 *) vector,
		  mask, (gfc_array_r8 *) field);
      return;

#ifdef HAVE_GFC_REAL_10
    case GFC_DTYPE_REAL_10:
      unpack1_r10 ((gfc_array_r10 *) ret, (gfc_array_r10 *) vector,
		   mask, (gfc_array_r10 *) field);
	  return;
#endif

#ifdef HAVE_GFC_REAL_16
    case GFC_DTYPE_REAL_16:
      unpack1_r16 ((gfc_array_r16 *) ret, (gfc_array_r16 *) vector,
		   mask, (gfc_array_r16 *) field);
      return;
#endif

    case GFC_DTYPE_COMPLEX_4:
      unpack1_c4 ((gfc_array_c4 *) ret, (gfc_array_c4 *) vector,
		  mask, (gfc_array_c4 *) field);
      return;

    case GFC_DTYPE_COMPLEX_8:
      unpack1_c8 ((gfc_array_c8 *) ret, (gfc_array_c8 *) vector,
		  mask, (gfc_array_c8 *) field);
      return;

#ifdef HAVE_GFC_COMPLEX_10
    case GFC_DTYPE_COMPLEX_10:
      unpack1_c10 ((gfc_array_c10 *) ret, (gfc_array_c10 *) vector,
		   mask, (gfc_array_c10 *) field);
      return;
#endif

#ifdef HAVE_GFC_COMPLEX_16
    case GFC_DTYPE_COMPLEX_16:
      unpack1_c16 ((gfc_array_c16 *) ret, (gfc_array_c16 *) vector,
		   mask, (gfc_array_c16 *) field);
      return;
#endif

    case GFC_DTYPE_DERIVED_2:
      if (GFC_UNALIGNED_2(ret->data) || GFC_UNALIGNED_2(vector->data)
	  || GFC_UNALIGNED_2(field->data))
	break;
      else
	{
	  unpack1_i2 ((gfc_array_i2 *) ret, (gfc_array_i2 *) vector,
		      mask, (gfc_array_i2 *) field);
	  return;
	}

    case GFC_DTYPE_DERIVED_4:
      if (GFC_UNALIGNED_4(ret->data) || GFC_UNALIGNED_4(vector->data)
	  || GFC_UNALIGNED_4(field->data))
	break;
      else
	{
	  unpack1_i4 ((gfc_array_i4 *) ret, (gfc_array_i4 *) vector,
		      mask, (gfc_array_i4 *) field);
	  return;
	}

    case GFC_DTYPE_DERIVED_8:
      if (GFC_UNALIGNED_8(ret->data) || GFC_UNALIGNED_8(vector->data)
	  || GFC_UNALIGNED_8(field->data))
	break;
      else
	{
	  unpack1_i8 ((gfc_array_i8 *) ret, (gfc_array_i8 *) vector,
		      mask, (gfc_array_i8 *) field);
	  return;
	}

#ifdef HAVE_GFC_INTEGER_16
    case GFC_DTYPE_DERIVED_16:
      if (GFC_UNALIGNED_16(ret->data) || GFC_UNALIGNED_16(vector->data)
	  || GFC_UNALIGNED_16(field->data))
	break;
      else
	{
	  unpack1_i16 ((gfc_array_i16 *) ret, (gfc_array_i16 *) vector,
		       mask, (gfc_array_i16 *) field);
	  return;
	}
#endif
    }

  unpack_internal (ret, vector, mask, field, size);
}


extern void unpack1_char (gfc_array_char *, GFC_INTEGER_4,
			  const gfc_array_char *, const gfc_array_l1 *,
			  const gfc_array_char *, GFC_INTEGER_4,
			  GFC_INTEGER_4);
export_proto(unpack1_char);

void
unpack1_char (gfc_array_char *ret,
	      GFC_INTEGER_4 ret_length __attribute__((unused)),
	      const gfc_array_char *vector, const gfc_array_l1 *mask,
	      const gfc_array_char *field, GFC_INTEGER_4 vector_length,
	      GFC_INTEGER_4 field_length __attribute__((unused)))
{

  if (unlikely(compile_options.bounds_check))
    unpack_bounds (ret, vector, mask, field);

  unpack_internal (ret, vector, mask, field, vector_length);
}


extern void unpack1_char4 (gfc_array_char *, GFC_INTEGER_4,
			   const gfc_array_char *, const gfc_array_l1 *,
			   const gfc_array_char *, GFC_INTEGER_4,
			   GFC_INTEGER_4);
export_proto(unpack1_char4);

void
unpack1_char4 (gfc_array_char *ret,
	       GFC_INTEGER_4 ret_length __attribute__((unused)),
	       const gfc_array_char *vector, const gfc_array_l1 *mask,
	       const gfc_array_char *field, GFC_INTEGER_4 vector_length,
	       GFC_INTEGER_4 field_length __attribute__((unused)))
{

  if (unlikely(compile_options.bounds_check))
    unpack_bounds (ret, vector, mask, field);

  unpack_internal (ret, vector, mask, field,
		   vector_length * sizeof (gfc_char4_t));
}


extern void unpack0 (gfc_array_char *, const gfc_array_char *,
		     const gfc_array_l1 *, char *);
export_proto(unpack0);

void
unpack0 (gfc_array_char *ret, const gfc_array_char *vector,
	 const gfc_array_l1 *mask, char *field)
{
  gfc_array_char tmp;

  index_type type_size;

  if (unlikely(compile_options.bounds_check))
    unpack_bounds (ret, vector, mask, NULL);

  type_size = GFC_DTYPE_TYPE_SIZE (vector);

  switch (type_size)
    {
    case GFC_DTYPE_LOGICAL_1:
    case GFC_DTYPE_INTEGER_1:
    case GFC_DTYPE_DERIVED_1:
      unpack0_i1 ((gfc_array_i1 *) ret, (gfc_array_i1 *) vector,
		  mask, (GFC_INTEGER_1 *) field);
      return;

    case GFC_DTYPE_LOGICAL_2:
    case GFC_DTYPE_INTEGER_2:
      unpack0_i2 ((gfc_array_i2 *) ret, (gfc_array_i2 *) vector,
		  mask, (GFC_INTEGER_2 *) field);
      return;

    case GFC_DTYPE_LOGICAL_4:
    case GFC_DTYPE_INTEGER_4:
      unpack0_i4 ((gfc_array_i4 *) ret, (gfc_array_i4 *) vector,
		  mask, (GFC_INTEGER_4 *) field);
      return;

    case GFC_DTYPE_LOGICAL_8:
    case GFC_DTYPE_INTEGER_8:
      unpack0_i8 ((gfc_array_i8 *) ret, (gfc_array_i8 *) vector,
		  mask, (GFC_INTEGER_8 *) field);
      return;

#ifdef HAVE_GFC_INTEGER_16
    case GFC_DTYPE_LOGICAL_16:
    case GFC_DTYPE_INTEGER_16:
      unpack0_i16 ((gfc_array_i16 *) ret, (gfc_array_i16 *) vector,
		   mask, (GFC_INTEGER_16 *) field);
      return;
#endif
    case GFC_DTYPE_REAL_4:
      unpack0_r4 ((gfc_array_r4 *) ret, (gfc_array_r4 *) vector,
		  mask, (GFC_REAL_4 *) field);
      return;

    case GFC_DTYPE_REAL_8:
      unpack0_r8 ((gfc_array_r8 *) ret, (gfc_array_r8*) vector,
		  mask, (GFC_REAL_8  *) field);
      return;

#ifdef HAVE_GFC_REAL_10
    case GFC_DTYPE_REAL_10:
      unpack0_r10 ((gfc_array_r10 *) ret, (gfc_array_r10 *) vector,
		   mask, (GFC_REAL_10 *) field);
      return;
#endif

#ifdef HAVE_GFC_REAL_16
    case GFC_DTYPE_REAL_16:
      unpack0_r16 ((gfc_array_r16 *) ret, (gfc_array_r16 *) vector,
		   mask, (GFC_REAL_16 *) field);
      return;
#endif

    case GFC_DTYPE_COMPLEX_4:
      unpack0_c4 ((gfc_array_c4 *) ret, (gfc_array_c4 *) vector,
		  mask, (GFC_COMPLEX_4 *) field);
      return;

    case GFC_DTYPE_COMPLEX_8:
      unpack0_c8 ((gfc_array_c8 *) ret, (gfc_array_c8 *) vector,
		  mask, (GFC_COMPLEX_8 *) field);
      return;

#ifdef HAVE_GFC_COMPLEX_10
    case GFC_DTYPE_COMPLEX_10:
      unpack0_c10 ((gfc_array_c10 *) ret, (gfc_array_c10 *) vector,
		   mask, (GFC_COMPLEX_10 *) field);
      return;
#endif

#ifdef HAVE_GFC_COMPLEX_16
    case GFC_DTYPE_COMPLEX_16:
      unpack0_c16 ((gfc_array_c16 *) ret, (gfc_array_c16 *) vector,
		   mask, (GFC_COMPLEX_16 *) field);
      return;
#endif
    case GFC_DTYPE_DERIVED_2:
      if (GFC_UNALIGNED_2(ret->data) || GFC_UNALIGNED_2(vector->data)
	  || GFC_UNALIGNED_2(field))
	break;
      else
	{
	  unpack0_i2 ((gfc_array_i2 *) ret, (gfc_array_i2 *) vector,
		      mask, (GFC_INTEGER_2 *) field);
	  return;
	}

    case GFC_DTYPE_DERIVED_4:
      if (GFC_UNALIGNED_4(ret->data) || GFC_UNALIGNED_4(vector->data)
	  || GFC_UNALIGNED_4(field))
	break;
      else
	{
	  unpack0_i4 ((gfc_array_i4 *) ret, (gfc_array_i4 *) vector,
		      mask, (GFC_INTEGER_4 *) field);
	  return;
	}

    case GFC_DTYPE_DERIVED_8:
      if (GFC_UNALIGNED_8(ret->data) || GFC_UNALIGNED_8(vector->data)
	  || GFC_UNALIGNED_8(field))
	break;
      else
	{
	  unpack0_i8 ((gfc_array_i8 *) ret, (gfc_array_i8 *) vector,
		      mask, (GFC_INTEGER_8 *) field);
	  return;
	}
#ifdef HAVE_GFC_INTEGER_16
    case GFC_DTYPE_DERIVED_16:
      if (GFC_UNALIGNED_16(ret->data) || GFC_UNALIGNED_16(vector->data)
	  || GFC_UNALIGNED_16(field))
	break;
      else
	{
	  unpack0_i16 ((gfc_array_i16 *) ret, (gfc_array_i16 *) vector,
		       mask, (GFC_INTEGER_16 *) field);
	  return;
	}
#endif
    }

  memset (&tmp, 0, sizeof (tmp));
  tmp.dtype = 0;
  tmp.data = field;
  unpack_internal (ret, vector, mask, &tmp, GFC_DESCRIPTOR_SIZE (vector));
}


extern void unpack0_char (gfc_array_char *, GFC_INTEGER_4,
			  const gfc_array_char *, const gfc_array_l1 *,
			  char *, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(unpack0_char);

void
unpack0_char (gfc_array_char *ret,
	      GFC_INTEGER_4 ret_length __attribute__((unused)),
	      const gfc_array_char *vector, const gfc_array_l1 *mask,
	      char *field, GFC_INTEGER_4 vector_length,
	      GFC_INTEGER_4 field_length __attribute__((unused)))
{
  gfc_array_char tmp;

  if (unlikely(compile_options.bounds_check))
    unpack_bounds (ret, vector, mask, NULL);

  memset (&tmp, 0, sizeof (tmp));
  tmp.dtype = 0;
  tmp.data = field;
  unpack_internal (ret, vector, mask, &tmp, vector_length);
}


extern void unpack0_char4 (gfc_array_char *, GFC_INTEGER_4,
			   const gfc_array_char *, const gfc_array_l1 *,
			   char *, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(unpack0_char4);

void
unpack0_char4 (gfc_array_char *ret,
	       GFC_INTEGER_4 ret_length __attribute__((unused)),
	       const gfc_array_char *vector, const gfc_array_l1 *mask,
	       char *field, GFC_INTEGER_4 vector_length,
	       GFC_INTEGER_4 field_length __attribute__((unused)))
{
  gfc_array_char tmp;

  if (unlikely(compile_options.bounds_check))
    unpack_bounds (ret, vector, mask, NULL);

  memset (&tmp, 0, sizeof (tmp));
  tmp.dtype = 0;
  tmp.data = field;
  unpack_internal (ret, vector, mask, &tmp,
		   vector_length * sizeof (gfc_char4_t));
}
