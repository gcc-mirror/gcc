dnl Support macro file for intrinsic functions.
dnl Contains the generic sections of the array functions.
dnl This file is part of the GNU Fortran 95 Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
define(START_FOREACH_FUNCTION,
`
extern void name`'rtype_qual`_'atype_code (rtype * retarray, atype *array);
export_proto(name`'rtype_qual`_'atype_code);

void
name`'rtype_qual`_'atype_code (rtype * retarray, atype *array)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  atype_name *base;
  rtype_name *dest;
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
      if (GFC_DESCRIPTOR_RANK (retarray) != 1)
	runtime_error ("rank of return array does not equal 1");

      if (retarray->dim[0].ubound + 1 - retarray->dim[0].lbound != rank)
        runtime_error ("dimension of return array incorrect");

      if (retarray->dim[0].stride == 0)
	retarray->dim[0].stride = 1;
    }

  /* TODO:  It should be a front end job to correctly set the strides.  */

  if (array->dim[0].stride == 0)
    array->dim[0].stride = 1;

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
             frequently used path so proabably not worth it.  */
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
extern void `m'name`'rtype_qual`_'atype_code (rtype *, atype *, gfc_array_l4 *);
export_proto(`m'name`'rtype_qual`_'atype_code);

void
`m'name`'rtype_qual`_'atype_code (rtype * retarray, atype *array,
				  gfc_array_l4 * mask)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type dstride;
  rtype_name *dest;
  atype_name *base;
  GFC_LOGICAL_4 *mbase;
  int rank;
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
      if (GFC_DESCRIPTOR_RANK (retarray) != 1)
	runtime_error ("rank of return array does not equal 1");

      if (retarray->dim[0].ubound + 1 - retarray->dim[0].lbound != rank)
        runtime_error ("dimension of return array incorrect");

      if (retarray->dim[0].stride == 0)
	retarray->dim[0].stride = 1;
    }

  /* TODO:  It should be a front end job to correctly set the strides.  */

  if (array->dim[0].stride == 0)
    array->dim[0].stride = 1;

  if (mask->dim[0].stride == 0)
    mask->dim[0].stride = 1;

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n < rank; n++)
    {
      sstride[n] = array->dim[n].stride;
      mstride[n] = mask->dim[n].stride;
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
  mbase = mask->data;

  if (GFC_DESCRIPTOR_SIZE (mask) != 4)
    {
      /* This allows the same loop to be used for all logical types.  */
      assert (GFC_DESCRIPTOR_SIZE (mask) == 8);
      for (n = 0; n < rank; n++)
        mstride[n] <<= 1;
      mbase = (GFOR_POINTER_L8_TO_L4 (mbase));
    }


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
             frequently used path so proabably not worth it.  */
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
      if (GFC_DESCRIPTOR_RANK (retarray) != 1)
	runtime_error ("rank of return array does not equal 1");

      if (retarray->dim[0].ubound + 1 - retarray->dim[0].lbound != rank)
        runtime_error ("dimension of return array incorrect");

      if (retarray->dim[0].stride == 0)
	retarray->dim[0].stride = 1;
    }

  dstride = retarray->dim[0].stride;
  dest = retarray->data;
  for (n = 0; n<rank; n++)
    dest[n * dstride] = $1 ;
}')dnl
