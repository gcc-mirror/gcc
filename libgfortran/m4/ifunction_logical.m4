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
	gfc_array_l1 * const restrict, const index_type * const restrict);
export_proto(name`'rtype_qual`_'atype_code);

void
name`'rtype_qual`_'atype_code (rtype * const restrict retarray, 
	gfc_array_l1 * const restrict array, 
	const index_type * const restrict pdim)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  const GFC_LOGICAL_1 * restrict base;
  rtype_name * restrict dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type delta;
  index_type dim;
  int src_kind;
  int continue_loop;

  /* Make dim zero based to avoid confusion.  */
  dim = (*pdim) - 1;
  rank = GFC_DESCRIPTOR_RANK (array) - 1;

  src_kind = GFC_DESCRIPTOR_SIZE (array);

  len = GFC_DESCRIPTOR_EXTENT(array,dim);
  if (len < 0)
    len = 0;

  delta = GFC_DESCRIPTOR_STRIDE_BYTES(array,dim);

  for (n = 0; n < dim; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(array,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n);

      if (extent[n] < 0)
	extent[n] = 0;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(array,n + 1);
      extent[n] = GFC_DESCRIPTOR_EXTENT(array,n + 1);

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
      retarray->dtype = (array->dtype & ~GFC_DTYPE_RANK_MASK) | rank;

      alloc_size = sizeof (rtype_name) * GFC_DESCRIPTOR_STRIDE(retarray,rank-1)
    		   * extent[rank-1];

      if (alloc_size == 0)
	{
	  /* Make sure we have a zero-sized array.  */
	  GFC_DIMENSION_SET(retarray->dim[0], 0, -1, 1);
	  return;
	}
      else
	retarray->base_addr = xmalloc (alloc_size);
    }
  else
    {
      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect in"
		       " u_name intrinsic: is %ld, should be %ld",
		       (long int) GFC_DESCRIPTOR_RANK (retarray),
		       (long int) rank);

      if (unlikely (compile_options.bounds_check))
	{
	  for (n=0; n < rank; n++)
	    {
	      index_type ret_extent;

	      ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,n);
	      if (extent[n] != ret_extent)
		runtime_error ("Incorrect extent in return value of"
			       " u_name intrinsic in dimension %d:"
			       " is %ld, should be %ld", (int) n + 1,
			       (long int) ret_extent, (long int) extent[n]);
	    }
	}
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = GFC_DESCRIPTOR_STRIDE(retarray,n);
      if (extent[n] <= 0)
	return;
    }

  base = array->base_addr;

  if (src_kind == 1 || src_kind == 2 || src_kind == 4 || src_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || src_kind == 16
#endif
    )
    {
      if (base)
	base = GFOR_POINTER_TO_L1 (base, src_kind);
    }
  else
    internal_error (NULL, "Funny sized logical array in u_name intrinsic");

  dest = retarray->base_addr;

  continue_loop = 1;
  while (continue_loop)
    {
      const GFC_LOGICAL_1 * restrict src;
      rtype_name result;
      src = base;
      {
')dnl
define(START_ARRAY_BLOCK,
`        if (len <= 0)
	  *dest = '$1`;
	else
	  {
	    for (n = 0; n < len; n++, src += delta)
	      {
')dnl
define(FINISH_ARRAY_FUNCTION,
    `          }
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
          if (n == rank)
            {
              /* Break out of the look.  */
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
define(ARRAY_FUNCTION,
`START_ARRAY_FUNCTION
$2
START_ARRAY_BLOCK($1)
$3
FINISH_ARRAY_FUNCTION')dnl
