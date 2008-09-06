dnl Support macro file for intrinsic functions.
dnl Contains the generic sections of the array functions.
dnl This file is part of the GNU Fortran 95 Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
define(START_FOREACH_FUNCTION,
`
extern void name`'rtype_qual`_'atype_code (rtype * const restrict retarray, 
	atype * const restrict array);
export_proto(name`'rtype_qual`_'atype_code);

void
name`'rtype_qual`_'atype_code (rtype * const restrict retarray, 
	atype * const restrict array)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  const atype_name *base;
  rtype_name * restrict dest;
  index_type rank;
  index_type n;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->data == NULL)
    {
      retarray->dim[0].lbound = 0;
      retarray->dim[0].ubound = rank-1;
      retarray->dim[0].stride = 1;
      retarray->dtype = (retarray->dtype & ~GFC_DTYPE_RANK_MASK) | 1;
      retarray->offset = 0;
      retarray->data = internal_malloc_size (sizeof (rtype_name) * rank);
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{
	  int ret_rank;
	  index_type ret_extent;

	  ret_rank = GFC_DESCRIPTOR_RANK (retarray);
	  if (ret_rank != 1)
	    runtime_error ("rank of return array in u_name intrinsic"
			   " should be 1, is %ld", (long int) ret_rank);

	  ret_extent = retarray->dim[0].ubound + 1 - retarray->dim[0].lbound;
	  if (ret_extent != rank)
	    runtime_error ("Incorrect extent in return value of"
			   " u_name intrnisic: is %ld, should be %ld",
			   (long int) ret_extent, (long int) rank);
	}
    }

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = array->dim[n].stride;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
      count[n] = 0;
      if (extent[n] <= 0)
	{
	  /* Set the return value.  */
	  for (n = 0; n < rank; n++)
	    dest[n * dstride] = 0;
	  return;
	}
    }

  base = array->data;

  /* Initialize the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;
  {
')dnl
define(START_FOREACH_BLOCK,
`  while (base)
    {
      {
        /* Implementation start.  */
')dnl
define(FINISH_FOREACH_FUNCTION,
`        /* Implementation end.  */
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          base -= sstride[n] * extent[n];
          n++;
          if (n == rank)
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
    }
  }
}')dnl
define(START_MASKED_FOREACH_FUNCTION,
`
extern void `m'name`'rtype_qual`_'atype_code (rtype * const restrict, 
	atype * const restrict, gfc_array_l1 * const restrict);
export_proto(`m'name`'rtype_qual`_'atype_code);

void
`m'name`'rtype_qual`_'atype_code (rtype * const restrict retarray, 
	atype * const restrict array,
	gfc_array_l1 * const restrict mask)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  rtype_name *dest;
  const atype_name *base;
  GFC_LOGICAL_1 *mbase;
  int rank;
  index_type n;
  int mask_kind;

  rank = GFC_DESCRIPTOR_RANK (array);
  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->data == NULL)
    {
      retarray->dim[0].lbound = 0;
      retarray->dim[0].ubound = rank-1;
      retarray->dim[0].stride = 1;
      retarray->dtype = (retarray->dtype & ~GFC_DTYPE_RANK_MASK) | 1;
      retarray->offset = 0;
      retarray->data = internal_malloc_size (sizeof (rtype_name) * rank);
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{
	  int ret_rank, mask_rank;
	  index_type ret_extent;
	  int n;
	  index_type array_extent, mask_extent;

	  ret_rank = GFC_DESCRIPTOR_RANK (retarray);
	  if (ret_rank != 1)
	    runtime_error ("rank of return array in u_name intrinsic"
			   " should be 1, is %ld", (long int) ret_rank);

	  ret_extent = retarray->dim[0].ubound + 1 - retarray->dim[0].lbound;
	  if (ret_extent != rank)
	    runtime_error ("Incorrect extent in return value of"
			   " u_name intrnisic: is %ld, should be %ld",
			   (long int) ret_extent, (long int) rank);
	
	  mask_rank = GFC_DESCRIPTOR_RANK (mask);
	  if (rank != mask_rank)
	    runtime_error ("rank of MASK argument in u_name intrnisic"
	                   "should be %ld, is %ld", (long int) rank,
			   (long int) mask_rank);

	  for (n=0; n<rank; n++)
	    {
	      array_extent = array->dim[n].ubound + 1 - array->dim[n].lbound;
	      mask_extent = mask->dim[n].ubound + 1 - mask->dim[n].lbound;
	      if (array_extent != mask_extent)
		runtime_error ("Incorrect extent in MASK argument of"
			       " u_name intrinsic in dimension %ld:"
			       " is %ld, should be %ld", (long int) n + 1,
			       (long int) mask_extent, (long int) array_extent);
	    }
	}
    }

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  mbase = mask->data;

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    mbase = GFOR_POINTER_TO_L1 (mbase, mask_kind);
  else
    runtime_error ("Funny sized logical array");

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = array->dim[n].stride;
      mstride[n] = mask->dim[n].stride * mask_kind;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
      count[n] = 0;
      if (extent[n] <= 0)
	{
	  /* Set the return value.  */
	  for (n = 0; n < rank; n++)
	    dest[n * dstride] = 0;
	  return;
	}
    }

  base = array->data;

  /* Initialize the return value.  */
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;
  {
')dnl
define(START_MASKED_FOREACH_BLOCK, `START_FOREACH_BLOCK')dnl
define(FINISH_MASKED_FOREACH_FUNCTION,
`        /* Implementation end.  */
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      mbase += mstride[0];
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          base -= sstride[n] * extent[n];
          mbase -= mstride[n] * extent[n];
          n++;
          if (n == rank)
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
    }
  }
}')dnl
define(FOREACH_FUNCTION,
`START_FOREACH_FUNCTION
$1
START_FOREACH_BLOCK
$2
FINISH_FOREACH_FUNCTION')dnl
define(MASKED_FOREACH_FUNCTION,
`START_MASKED_FOREACH_FUNCTION
$1
START_MASKED_FOREACH_BLOCK
$2
FINISH_MASKED_FOREACH_FUNCTION')dnl
define(SCALAR_FOREACH_FUNCTION,
`
extern void `s'name`'rtype_qual`_'atype_code (rtype * const restrict, 
	atype * const restrict, GFC_LOGICAL_4 *);
export_proto(`s'name`'rtype_qual`_'atype_code);

void
`s'name`'rtype_qual`_'atype_code (rtype * const restrict retarray, 
	atype * const restrict array,
	GFC_LOGICAL_4 * mask)
{
  index_type rank;
  index_type dstride;
  index_type n;
  rtype_name *dest;

  if (*mask)
    {
      name`'rtype_qual`_'atype_code (retarray, array);
      return;
    }

  rank = GFC_DESCRIPTOR_RANK (array);

  if (rank <= 0)
    runtime_error ("Rank of array needs to be > 0");

  if (retarray->data == NULL)
    {
      retarray->dim[0].lbound = 0;
      retarray->dim[0].ubound = rank-1;
      retarray->dim[0].stride = 1;
      retarray->dtype = (retarray->dtype & ~GFC_DTYPE_RANK_MASK) | 1;
      retarray->offset = 0;
      retarray->data = internal_malloc_size (sizeof (rtype_name) * rank);
    }
  else
    {
      if (unlikely (compile_options.bounds_check))
	{
	  int ret_rank;
	  index_type ret_extent;

	  ret_rank = GFC_DESCRIPTOR_RANK (retarray);
	  if (ret_rank != 1)
	    runtime_error ("rank of return array in u_name intrinsic"
			   " should be 1, is %ld", (long int) ret_rank);

	  ret_extent = retarray->dim[0].ubound + 1 - retarray->dim[0].lbound;
	    if (ret_extent != rank)
	      runtime_error ("dimension of return array incorrect");
	}
    }

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n<rank; n++)
    dest[n * dstride] = $1 ;
}')dnl
