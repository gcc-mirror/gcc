dnl Support macro file for intrinsic functions.
dnl Contains the generic sections of the array functions.
dnl This file is part of the GNU Fortran 95 Runtime Library (libgfortran)
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
extern void name`'rtype_qual`_'atype_code (rtype *, atype *, index_type *);
export_proto(name`'rtype_qual`_'atype_code);

void
name`'rtype_qual`_'atype_code (rtype *retarray, atype *array, index_type *pdim)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  atype_name *base;
  rtype_name *dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type delta;
  index_type dim;

  /* Make dim zero based to avoid confusion.  */
  dim = (*pdim) - 1;
  rank = GFC_DESCRIPTOR_RANK (array) - 1;

  /* TODO:  It should be a front end job to correctly set the strides.  */

  if (array->dim[0].stride == 0)
    array->dim[0].stride = 1;

  len = array->dim[dim].ubound + 1 - array->dim[dim].lbound;
  delta = array->dim[dim].stride;

  for (n = 0; n < dim; n++)
    {
      sstride[n] = array->dim[n].stride;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = array->dim[n + 1].stride;
      extent[n] =
        array->dim[n + 1].ubound + 1 - array->dim[n + 1].lbound;
    }

  if (retarray->data == NULL)
    {
      for (n = 0; n < rank; n++)
        {
          retarray->dim[n].lbound = 0;
          retarray->dim[n].ubound = extent[n]-1;
          if (n == 0)
            retarray->dim[n].stride = 1;
          else
            retarray->dim[n].stride = retarray->dim[n-1].stride * extent[n-1];
        }

      retarray->data
	 = internal_malloc_size (sizeof (rtype_name)
		 		 * retarray->dim[rank-1].stride
				 * extent[rank-1]);
      retarray->offset = 0;
      retarray->dtype = (array->dtype & ~GFC_DTYPE_RANK_MASK) | rank;
    }
  else
    {
      if (retarray->dim[0].stride == 0)
	retarray->dim[0].stride = 1;

      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect");
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = retarray->dim[n].stride;
      if (extent[n] <= 0)
        len = 0;
    }

  base = array->data;
  dest = retarray->data;

  while (base)
    {
      atype_name *src;
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
             frequently used path so proabably not worth it.  */
          base -= sstride[n] * extent[n];
          dest -= dstride[n] * extent[n];
          n++;
          if (n == rank)
            {
              /* Break out of the look.  */
              base = NULL;
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
extern void `m'name`'rtype_qual`_'atype_code (rtype *, atype *, index_type *,
					       gfc_array_l4 *);
export_proto(`m'name`'rtype_qual`_'atype_code);

void
`m'name`'rtype_qual`_'atype_code (rtype * retarray, atype * array,
				  index_type *pdim, gfc_array_l4 * mask)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  rtype_name *dest;
  atype_name *base;
  GFC_LOGICAL_4 *mbase;
  int rank;
  int dim;
  index_type n;
  index_type len;
  index_type delta;
  index_type mdelta;

  dim = (*pdim) - 1;
  rank = GFC_DESCRIPTOR_RANK (array) - 1;

  /* TODO:  It should be a front end job to correctly set the strides.  */

  if (array->dim[0].stride == 0)
    array->dim[0].stride = 1;

  if (mask->dim[0].stride == 0)
    mask->dim[0].stride = 1;

  len = array->dim[dim].ubound + 1 - array->dim[dim].lbound;
  if (len <= 0)
    return;
  delta = array->dim[dim].stride;
  mdelta = mask->dim[dim].stride;

  for (n = 0; n < dim; n++)
    {
      sstride[n] = array->dim[n].stride;
      mstride[n] = mask->dim[n].stride;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = array->dim[n + 1].stride;
      mstride[n] = mask->dim[n + 1].stride;
      extent[n] =
        array->dim[n + 1].ubound + 1 - array->dim[n + 1].lbound;
    }

  if (retarray->data == NULL)
    {
      for (n = 0; n < rank; n++)
        {
          retarray->dim[n].lbound = 0;
          retarray->dim[n].ubound = extent[n]-1;
          if (n == 0)
            retarray->dim[n].stride = 1;
          else
            retarray->dim[n].stride = retarray->dim[n-1].stride * extent[n-1];
        }

      retarray->data
	 = internal_malloc_size (sizeof (rtype_name)
		 		 * retarray->dim[rank-1].stride
				 * extent[rank-1]);
      retarray->offset = 0;
      retarray->dtype = (array->dtype & ~GFC_DTYPE_RANK_MASK) | rank;
    }
  else
    {
      if (retarray->dim[0].stride == 0)
	retarray->dim[0].stride = 1;

      if (rank != GFC_DESCRIPTOR_RANK (retarray))
	runtime_error ("rank of return array incorrect");
    }

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = retarray->dim[n].stride;
      if (extent[n] <= 0)
        return;
    }

  dest = retarray->data;
  base = array->data;
  mbase = mask->data;

  if (GFC_DESCRIPTOR_SIZE (mask) != 4)
    {
      /* This allows the same loop to be used for all logical types.  */
      assert (GFC_DESCRIPTOR_SIZE (mask) == 8);
      for (n = 0; n < rank; n++)
        mstride[n] <<= 1;
      mdelta <<= 1;
      mbase = (GFOR_POINTER_L8_TO_L4 (mbase));
    }

  while (base)
    {
      atype_name *src;
      GFC_LOGICAL_4 *msrc;
      rtype_name result;
      src = base;
      msrc = mbase;
      {
')dnl
define(START_MASKED_ARRAY_BLOCK,
`        if (len <= 0)
	  *dest = '$1`;
	else
	  {
	    for (n = 0; n < len; n++, src += delta, msrc += mdelta)
	      {
')dnl
define(FINISH_MASKED_ARRAY_FUNCTION,
`              }
	    *dest = result;
	  }
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
             frequently used path so proabably not worth it.  */
          base -= sstride[n] * extent[n];
          mbase -= mstride[n] * extent[n];
          dest -= dstride[n] * extent[n];
          n++;
          if (n == rank)
            {
              /* Break out of the look.  */
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
define(ARRAY_FUNCTION,
`START_ARRAY_FUNCTION
$2
START_ARRAY_BLOCK($1)
$3
FINISH_ARRAY_FUNCTION')dnl
define(MASKED_ARRAY_FUNCTION,
`START_MASKED_ARRAY_FUNCTION
$2
START_MASKED_ARRAY_BLOCK($1)
$3
FINISH_MASKED_ARRAY_FUNCTION')dnl
