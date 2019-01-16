dnl Support macro file for intrinsic functions.
dnl Contains the generic sections of the array functions.
dnl This file is part of the GNU Fortran Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
dnl
dnl Pass the implementation for a single section as the parameter to
dnl {MASK_}ARRAY_FUNCTION.
dnl The variables base, delta, and len describe the input section.
dnl For masked section the mask is described by mbase and mdelta.
dnl These should not be modified. The result should be stored in *dest.
dnl The names count, extent, sstride, dstride, base, dest, rank, dim
dnl retarray, array, pdim and mstride should not be used.
dnl The variable n is declared as index_type and may be used.
dnl Other variable declarations may be placed at the start of the code,
dnl The types of the array parameter and the return value are
dnl atype_name and rtype_name respectively.
dnl Execution should be allowed to continue to the end of the block.
dnl You should not return or break from the inner loop of the implementation.
dnl Care should also be taken to avoid using the names defined in iparm.m4
define(START_ARRAY_FUNCTION,
`
extern void name`'rtype_qual`_'atype_code (rtype * const restrict, 
	atype` * const restrict, const 'index_type` * const restrict'back_arg`);
export_proto('name`'rtype_qual`_'atype_code);

void
name`'rtype_qual`_'atype_code` ('rtype` * const restrict retarray, 
	'atype` * const restrict array, 
	const index_type * const restrict pdim'back_arg`)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  const 'atype_name * restrict base;
  rtype_name * restrict dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type delta;
  index_type dim;
  int continue_loop;

  /* Make dim zero based to avoid confusion.  */
  rank = GFC_DESCRIPTOR_RANK (array) - 1;
  dim = (*pdim) - 1;

  if (unlikely (dim < 0 || dim > rank))
    {
      runtime_error ("Dim argument incorrect in u_name intrinsic: "
 		     "is %ld, should be between 1 and %ld",
		     (long int) dim + 1, (long int) rank + 1);
    }

  len = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (len < 0)
    len = 0;
  delta = GFC_DESCRIPTOR_STRIDE(array,dim);

  for (n = 0; n < dim; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] < 0)
	extent[n] = 0;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array, n + 1);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array, n + 1);

      if (extent[n] < 0)
	extent[n] = 0;
    }

  if (retarray->base_addr == NULL)
    {
      size_t alloc_size, str;

      for (n = 0; n < rank; n++)
	{
	  if (n == 0)
	    str = 1;
	  else
	    str = GFC_DESCRIPTOR_STRIDE(retarray,n-1) * extent[n-1];

	  GFC_DIMENSION_SET(retarray->dim[n], 0, extent[n] - 1, str);

	}

      retarray->offset = 0;
      retarray->dtype.rank = rank;

      alloc_size = GFC_DESCRIPTOR_STRIDE(retarray,rank-1) * extent[rank-1];

      retarray->base_addr = xmallocarray (alloc_size, sizeof (rtype_name));
      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;

	}
    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in"
		       " u_name intrinsic: is %ld, should be %ld",
		       (long int) (GFC_DESCRIPTOR_RANK (retarray)),
		       (long int) rank);

      if (unlikely (compile_options.bounds_check))
	bounds_ifunction_return ((array_t *) retarray, extent,
				 "return value", "u_name");
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
      if (extent[n] <= 0)
	return;
    }

  base = array->base_addr;
  dest = retarray->base_addr;

  continue_loop = 1;
  while (continue_loop)
    {
      const atype_name * restrict src;
      rtype_name result;
      src = base;
      {
')dnl
define(START_ARRAY_BLOCK,
`	if (len <= 0)
	  *dest = '$1`;
	else
	  {
#if ! defined HAVE_BACK_ARG
	    for (n = 0; n < len; n++, src += delta)
	      {
#endif
')dnl
define(FINISH_ARRAY_FUNCTION,
`	      }
	    '$1`
	    *dest = result;
	  }
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      dest += dstride[0];
      n = 0;
      while (count[n] == extent[n])
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  base -= sstride[n] * extent[n];
	  dest -= dstride[n] * extent[n];
	  n++;
	  if (n >= rank)
	    {
	      /* Break out of the loop.  */
	      continue_loop = 0;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      base += sstride[n];
	      dest += dstride[n];
	    }
	}
    }
}')dnl
define(START_MASKED_ARRAY_FUNCTION,
`
extern void `m'name`'rtype_qual`_'atype_code` ('rtype` * const restrict, 
	'atype` * const restrict, const 'index_type` * const restrict,
	gfc_array_l1 * const restrict'back_arg`);
export_proto(m'name`'rtype_qual`_'atype_code`);

void
m'name`'rtype_qual`_'atype_code` ('rtype` * const restrict retarray, 
	'atype` * const restrict array, 
	const index_type * const restrict pdim, 
	gfc_array_l1 * const restrict mask'back_arg`)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  'rtype_name * restrict dest;
  const atype_name * restrict base;
  const GFC_LOGICAL_1 * restrict mbase;
  index_type rank;
  index_type dim;
  index_type n;
  index_type len;
  index_type delta;
  index_type mdelta;
  int mask_kind;

  if (mask == NULL)
    {
#ifdef HAVE_BACK_ARG
      name`'rtype_qual`_'atype_code (retarray, array, pdim, back);
#else
      name`'rtype_qual`_'atype_code (retarray, array, pdim);
#endif
      return;
    }

  dim = (*pdim) - 1;
  rank = GFC_DESCRIPTOR_RANK (array) - 1;


  if (unlikely (dim < 0 || dim > rank))
    {
      runtime_error ("Dim argument incorrect in u_name intrinsic: "
 		     "is %ld, should be between 1 and %ld",
		     (long int) dim + 1, (long int) rank + 1);
    }

  len = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (len <= 0)
    return;

  mbase = mask->base_addr;

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    mbase = GFOR_POINTER_TO_L1 (mbase, mask_kind);
  else
    runtime_error ("Funny sized logical array");

  delta = GFC_DESCRIPTOR_STRIDE(array,dim);
  mdelta = GFC_DESCRIPTOR_STRIDE_BYTES(mask,dim);

  for (n = 0; n < dim; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n);
      mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] < 0)
	extent[n] = 0;

    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE(array,n + 1);
      mstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(mask, n + 1);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array, n + 1);

      if (extent[n] < 0)
	extent[n] = 0;
    }

  if (retarray->base_addr == NULL)
    {
      size_t alloc_size, str;

      for (n = 0; n < rank; n++)
	{
	  if (n == 0)
	    str = 1;
	  else
	    str= GFC_DESCRIPTOR_STRIDE(retarray,n-1) * extent[n-1];

	  GFC_DIMENSION_SET(retarray->dim[n], 0, extent[n] - 1, str);

	}

      alloc_size = GFC_DESCRIPTOR_STRIDE(retarray,rank-1) * extent[rank-1];

      retarray->offset = 0;
      retarray->dtype.rank = rank;

      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;
	}
      else
	retarray->base_addr = xmallocarray (alloc_size, sizeof (rtype_name));

    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in u_name intrinsic");

      if (unlikely (compile_options.bounds_check))
	{
	  bounds_ifunction_return ((array_t *) retarray, extent,
				   "return value", "u_name");
	  bounds_equal_extents ((array_t *) mask, (array_t *) array,
	  			"MASK argument", "u_name");
	}
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
      if (extent[n] <= 0)
	return;
    }

  dest = retarray->base_addr;
  base = array->base_addr;

  while (base)
    {
      const atype_name * restrict src;
      const GFC_LOGICAL_1 * restrict msrc;
      rtype_name result;
      src = base;
      msrc = mbase;
      {
')dnl
define(START_MASKED_ARRAY_BLOCK,
`	for (n = 0; n < len; n++, src += delta, msrc += mdelta)
	  {
')dnl
define(FINISH_MASKED_ARRAY_FUNCTION,
`	  }
	*dest = result;
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      mbase += mstride[0];
      dest += dstride[0];
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
	  dest -= dstride[n] * extent[n];
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
	      dest += dstride[n];
	    }
	}
    }
}')dnl
define(SCALAR_ARRAY_FUNCTION,
`
extern void `s'name`'rtype_qual`_'atype_code` ('rtype` * const restrict, 
	'atype` * const restrict, const index_type * const restrict,
	GFC_LOGICAL_4 *'back_arg`);
export_proto(s'name`'rtype_qual`_'atype_code);

void
`s'name`'rtype_qual`_'atype_code` ('rtype` * const restrict retarray, 
	'atype` * const restrict array, 
	const index_type * const restrict pdim, 
	GFC_LOGICAL_4 * mask'back_arg`)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  'rtype_name * restrict dest;
  index_type rank;
  index_type n;
  index_type dim;


  if (mask == NULL || *mask)
    {
#ifdef HAVE_BACK_ARG
      name`'rtype_qual`_'atype_code (retarray, array, pdim, back);
#else
      name`'rtype_qual`_'atype_code (retarray, array, pdim);
#endif
      return;
    }
  /* Make dim zero based to avoid confusion.  */
  dim = (*pdim) - 1;
  rank = GFC_DESCRIPTOR_RANK (array) - 1;

  if (unlikely (dim < 0 || dim > rank))
    {
      runtime_error ("Dim argument incorrect in u_name intrinsic: "
 		     "is %ld, should be between 1 and %ld",
		     (long int) dim + 1, (long int) rank + 1);
    }

  for (n = 0; n < dim; n++)
    {
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] <= 0)
	extent[n] = 0;
    }

  for (n = dim; n < rank; n++)
    {
      extent[n] =
	GFC_DESCRIPTOR_EXTENT(array,n + 1);

      if (extent[n] <= 0)
	extent[n] = 0;
    }

  if (retarray->base_addr == NULL)
    {
      size_t alloc_size, str;

      for (n = 0; n < rank; n++)
	{
	  if (n == 0)
	    str = 1;
	  else
	    str = GFC_DESCRIPTOR_STRIDE(retarray,n-1) * extent[n-1];

	  GFC_DIMENSION_SET(retarray->dim[n], 0, extent[n] - 1, str);

	}

      retarray->offset = 0;
      retarray->dtype.rank = rank;

      alloc_size = GFC_DESCRIPTOR_STRIDE(retarray,rank-1) * extent[rank-1];

      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;
	}
      else
	retarray->base_addr = xmallocarray (alloc_size, sizeof (rtype_name));
    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in"
		       " u_name intrinsic: is %ld, should be %ld",
		       (long int) (GFC_DESCRIPTOR_RANK (retarray)),
		       (long int) rank);

      if (unlikely (compile_options.bounds_check))
	{
	  for (n=0; n < rank; n++)
	    {
	      index_type ret_extent;

	      ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,n);
	      if (extent[n] != ret_extent)
		runtime_error ("Incorrect extent in return value of"
			       " u_name intrinsic in dimension %ld:"
			       " is %ld, should be %ld", (long int) n + 1,
			       (long int) ret_extent, (long int) extent[n]);
	    }
	}
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
    }

  dest = retarray->base_addr;

  while(1)
    {
      *dest = '$1`;
      count[0]++;
      dest += dstride[0];
      n = 0;
      while (count[n] == extent[n])
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  dest -= dstride[n] * extent[n];
	  n++;
	  if (n >= rank)
	    return;
	  else
	    {
	      count[n]++;
	      dest += dstride[n];
	    }
      	}
    }
}')dnl
define(ARRAY_FUNCTION,
`START_ARRAY_FUNCTION
$2
START_ARRAY_BLOCK($1)
$3
FINISH_ARRAY_FUNCTION($4)')dnl
define(MASKED_ARRAY_FUNCTION,
`START_MASKED_ARRAY_FUNCTION
$2
START_MASKED_ARRAY_BLOCK
$3
FINISH_MASKED_ARRAY_FUNCTION')dnl
