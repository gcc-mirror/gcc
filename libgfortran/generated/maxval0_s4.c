/* Implementation of the MAXLOC intrinsic
   Copyright (C) 2017-2023 Free Software Foundation, Inc.
   Contributed by Thomas Koenig

This file is part of the GNU Fortran runtime library (libgfortran).

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
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


#if defined (HAVE_GFC_UINTEGER_4) && defined (HAVE_GFC_UINTEGER_4)

static inline int
compare_fcn (const GFC_UINTEGER_4 *a, const GFC_UINTEGER_4 *b, gfc_charlen_type n)
{
  if (sizeof (GFC_UINTEGER_4) == 1)
    return memcmp (a, b, n);
  else
    return memcmp_char4 (a, b, n);

}

#define INITVAL 0

extern void maxval0_s4 (GFC_UINTEGER_4 * restrict,
        gfc_charlen_type,
	gfc_array_s4 * const restrict array, gfc_charlen_type);
export_proto(maxval0_s4);

void
maxval0_s4 (GFC_UINTEGER_4 * restrict ret,
        gfc_charlen_type xlen,
	gfc_array_s4 * const restrict array, gfc_charlen_type len)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  const GFC_UINTEGER_4 *base;
  index_type rank;
  index_type n;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  assert (xlen == len);

  /* Initialize return value.  */
  memset (ret, INITVAL, sizeof(*ret) * len);

  for (n = 0; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n) * len;
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);
      count[n] = 0;
      if (extent[n] <= 0)
        return;
    }

  base = array->base_addr;

  {

  const GFC_UINTEGER_4 *retval;
   retval = ret;

  while (base)
    {
      do
	{
	  /* Implementation start.  */

  if (compare_fcn (base, retval, len) > 0)
    {
      retval = base;
    }
	  /* Implementation end.  */
	  /* Advance to the next element.  */
	  base += sstride[0];
	}
      while (++count[0] != extent[0]);
      n = 0;
      do
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  base -= sstride[n] * extent[n];
	  n++;
	  if (n >= rank)
	    {
	      /* Break out of the loop.  */
	      base = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      base += sstride[n];
	    }
	}
      while (count[n] == extent[n]);
    }
   memcpy (ret, retval, len * sizeof (*ret));
  }
}


extern void mmaxval0_s4 (GFC_UINTEGER_4 * restrict,
       gfc_charlen_type, gfc_array_s4 * const restrict array,
       gfc_array_l1 * const restrict mask, gfc_charlen_type len);
export_proto(mmaxval0_s4);

void
mmaxval0_s4 (GFC_UINTEGER_4 * const restrict ret,
	gfc_charlen_type xlen, gfc_array_s4 * const restrict array,
	gfc_array_l1 * const restrict mask, gfc_charlen_type len)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  const GFC_UINTEGER_4 *base;
  GFC_LOGICAL_1 *mbase;
  int rank;
  index_type n;
  int mask_kind;

  if (mask == NULL)
    {
      maxval0_s4 (ret, xlen, array, len);
      return;
    }

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  assert (xlen == len);

/* Initialize return value.  */
  memset (ret, INITVAL, sizeof(*ret) * len);

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  mbase = mask->base_addr;

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    mbase = GFOR_POINTER_TO_L1 (mbase, mask_kind);
  else
    runtime_error ("Funny sized logical array");

  for (n = 0; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n) * len;
      mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);
      count[n] = 0;
      if (extent[n] <= 0)
	return;
    }

  base = array->base_addr;
  {

  const GFC_UINTEGER_4 *retval;

  retval = ret;

  while (base)
    {
      do
	{
	  /* Implementation start.  */

  if (*mbase && compare_fcn (base, retval, len) > 0)
    {
      retval = base;
    }
	  /* Implementation end.  */
	  /* Advance to the next element.  */
	  base += sstride[0];
	  mbase += mstride[0];
	}
      while (++count[0] != extent[0]);
      n = 0;
      do
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  base -= sstride[n] * extent[n];
	  mbase -= mstride[n] * extent[n];
	  n++;
	  if (n >= rank)
	    {
	      /* Break out of the loop.  */
	      base = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      base += sstride[n];
	      mbase += mstride[n];
	    }
	}
      while (count[n] == extent[n]);
    }
    memcpy (ret, retval, len * sizeof (*ret));
  }
}


extern void smaxval0_s4 (GFC_UINTEGER_4 * restrict,
        gfc_charlen_type,
	gfc_array_s4 * const restrict array, GFC_LOGICAL_4 *, gfc_charlen_type);
export_proto(smaxval0_s4);

void
smaxval0_s4 (GFC_UINTEGER_4 * restrict ret,
        gfc_charlen_type xlen, gfc_array_s4 * const restrict array,
	GFC_LOGICAL_4 *mask, gfc_charlen_type len)
	
{
  if (mask == NULL || *mask)
    {
      maxval0_s4 (ret, xlen, array, len);
      return;
    }
  memset (ret, INITVAL, sizeof (*ret) * len);
}

#endif
