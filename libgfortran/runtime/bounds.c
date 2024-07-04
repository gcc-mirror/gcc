/* Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by Thomas Koenig

This file is part of the GNU Fortran runtime library (libgfortran).

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

#include "libgfortran.h"
#include <assert.h>

/* Auxiliary functions for bounds checking, mostly to reduce library size.  */

/* Bounds checking for the return values of the iforeach functions (such
   as maxloc and minloc).  The extent of ret_array must
   must match the rank of array.  */

void
bounds_iforeach_return (array_t *retarray, array_t *array, const char *name)
{
  index_type rank;
  index_type ret_rank;
  index_type ret_extent;

  ret_rank = GFC_DESCRIPTOR_RANK (retarray);

  /* ret_rank should always be 1, otherwise there is an internal error */
  GFC_ASSERT(ret_rank == 1);

  rank = GFC_DESCRIPTOR_RANK (array);
  ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,0);
  if (ret_extent != rank)
    runtime_error ("Incorrect extent in return value of"
		   " %s intrinsic: is %ld, should be %ld",
		   name, (long int) ret_extent, (long int) rank);

}

/* Check the return of functions generated from ifunction.m4.
   We check the array descriptor "a" against the extents precomputed
   from ifunction.m4, and complain about the argument a_name in the
   intrinsic function. */

void
bounds_ifunction_return (array_t * a, const index_type * extent,
			 const char * a_name, const char * intrinsic)
{
  int empty;
  int rank;
  index_type a_size;

  rank = GFC_DESCRIPTOR_RANK (a);
  a_size = size0 (a);

  empty = 0;
  for (index_type n = 0; n < rank; n++)
    {
      if (extent[n] == 0)
	empty = 1;
    }
  if (empty)
    {
      if (a_size != 0)
	runtime_error ("Incorrect size in %s of %s"
		       " intrinsic: should be zero-sized",
		       a_name, intrinsic);
    }
  else
    {
      if (a_size == 0)
	runtime_error ("Incorrect size of %s in %s"
		       " intrinsic: should not be zero-sized",
		       a_name, intrinsic);

      for (index_type n = 0; n < rank; n++)
	{
	  index_type a_extent;
	  a_extent = GFC_DESCRIPTOR_EXTENT(a, n);
	  if (a_extent != extent[n])
	    runtime_error("Incorrect extent in %s of %s"
			  " intrinsic in dimension %ld: is %ld,"
			  " should be %ld", a_name, intrinsic, (long int) n + 1,
			  (long int) a_extent, (long int) extent[n]);

	}
    }
}

/* Check that two arrays have equal extents, or are both zero-sized.  Abort
   with a runtime error if this is not the case.  Complain that a has the
   wrong size.  */

void
bounds_equal_extents (array_t *a, array_t *b, const char *a_name,
		      const char *intrinsic)
{
  index_type a_size, b_size, n;

  assert (GFC_DESCRIPTOR_RANK(a) == GFC_DESCRIPTOR_RANK(b));

  a_size = size0 (a);
  b_size = size0 (b);

  if (b_size == 0)
    {
      if (a_size != 0)
	runtime_error ("Incorrect size of %s in %s"
		       " intrinsic: should be zero-sized",
		       a_name, intrinsic);
    }
  else
    {
      if (a_size == 0) 
	runtime_error ("Incorrect size of %s of %s"
		       " intrinsic: Should not be zero-sized",
		       a_name, intrinsic);

      for (n = 0; n < GFC_DESCRIPTOR_RANK (b); n++)
	{
	  index_type a_extent, b_extent;
	  
	  a_extent = GFC_DESCRIPTOR_EXTENT(a, n);
	  b_extent = GFC_DESCRIPTOR_EXTENT(b, n);
	  if (a_extent != b_extent)
	    runtime_error("Incorrect extent in %s of %s"
			  " intrinsic in dimension %ld: is %ld,"
			  " should be %ld", a_name, intrinsic, (long int) n + 1,
			  (long int) a_extent, (long int) b_extent);
	}
    }
}

/* Check that the extents of a and b agree, except that a has a missing
   dimension in argument which.  Complain about a if anything is wrong.  */

void
bounds_reduced_extents (array_t *a, array_t *b, int which, const char *a_name,
		      const char *intrinsic)
{

  index_type i, n, a_size, b_size;

  assert (GFC_DESCRIPTOR_RANK(a) == GFC_DESCRIPTOR_RANK(b) - 1);

  a_size = size0 (a);
  b_size = size0 (b);

  if (b_size == 0)
    {
      if (a_size != 0)
	runtime_error ("Incorrect size in %s of %s"
		       " intrinsic: should not be zero-sized",
		       a_name, intrinsic);
    }
  else
    {
      if (a_size == 0) 
	runtime_error ("Incorrect size of %s of %s"
		       " intrinsic: should be zero-sized",
		       a_name, intrinsic);

      i = 0;
      for (n = 0; n < GFC_DESCRIPTOR_RANK (b); n++)
	{
	  index_type a_extent, b_extent;

	  if (n != which)
	    {
	      a_extent = GFC_DESCRIPTOR_EXTENT(a, i);
	      b_extent = GFC_DESCRIPTOR_EXTENT(b, n);
	      if (a_extent != b_extent)
		runtime_error("Incorrect extent in %s of %s"
			      " intrinsic in dimension %ld: is %ld,"
			      " should be %ld", a_name, intrinsic, (long int) i + 1,
			      (long int) a_extent, (long int) b_extent);
	      i++;
	    }
	}
    }
}

/* count_0 - count all the true elements in an array.  The front
   end usually inlines this, we need this for bounds checking
   for unpack.  */

index_type count_0 (const gfc_array_l1 * array)
{
  const GFC_LOGICAL_1 * restrict base;
  index_type rank;
  int kind;
  int continue_loop;
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type result;
  index_type n;

  rank = GFC_DESCRIPTOR_RANK (array);
  kind = GFC_DESCRIPTOR_SIZE (array);

  base = array->base_addr;

  if (kind == 1 || kind == 2 || kind == 4 || kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || kind == 16
#endif
    )
    {
      if (base)
	base = GFOR_POINTER_TO_L1 (base, kind);
    }
  else
    internal_error (NULL, "Funny sized logical array in count_0");

  for (n = 0; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(array,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);
      count[n] = 0;

      if (extent[n] <= 0)
	return 0;
    }

  result = 0;
  continue_loop = 1;
  while (continue_loop)
    {
      if (*base)
	result ++;

      count[0]++;
      base += sstride[0];
      n = 0;
      while (count[n] == extent[n])
	{
	  count[n] = 0;
	  base -= sstride[n] * extent[n];
	  n++;
	  if (n == rank)
	    {
	      continue_loop = 0;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      base += sstride[n];
	    }
	}
    }
  return result;
}
