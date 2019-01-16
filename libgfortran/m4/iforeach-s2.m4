dnl Support macro file for intrinsic functions.
dnl Contains the generic sections of the array functions.
dnl This file is part of the GNU Fortran Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
define(START_FOREACH_FUNCTION,
`static inline int
compare_fcn (const atype_name *a, const atype_name *b, gfc_charlen_type n)
{
  if (sizeof ('atype_name`) == 1)
    return memcmp (a, b, n);
  else
    return memcmp_char4 (a, b, n);

}

#define INITVAL 'initval`

extern void 'name`'rtype_qual`_'atype_code (atype_name * restrict,
        gfc_charlen_type,
	atype * const restrict array, gfc_charlen_type);
export_proto(name`'rtype_qual`_'atype_code);

void
name`'rtype_qual`_'atype_code` ('atype_name` * restrict ret,
        gfc_charlen_type xlen,
	'atype` * const restrict array, gfc_charlen_type len)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  const 'atype_name` *base;
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
')dnl
define(START_FOREACH_BLOCK,
`  while (base)
    {
      do
	{
	  /* Implementation start.  */
')dnl
define(FINISH_FOREACH_FUNCTION,
`	  /* Implementation end.  */
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
}')dnl
define(START_MASKED_FOREACH_FUNCTION,
`
extern void `m'name`'rtype_qual`_'atype_code (atype_name * restrict,
       gfc_charlen_type, atype * const restrict array,
       gfc_array_l1 * const restrict mask, gfc_charlen_type len);
export_proto(`m'name`'rtype_qual`_'atype_code);

void
`m'name`'rtype_qual`_'atype_code (atype_name * const restrict ret,
	gfc_charlen_type xlen, atype * const restrict array,
	gfc_array_l1 * const restrict mask, gfc_charlen_type len)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type mstride[GFC_MAX_DIMENSIONS];
  const atype_name *base;
  GFC_LOGICAL_1 *mbase;
  int rank;
  index_type n;
  int mask_kind;

  if (mask == NULL)
    {
      name`'rtype_qual`_'atype_code (ret, xlen, array, len);
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
')dnl
define(START_MASKED_FOREACH_BLOCK, `START_FOREACH_BLOCK')dnl
define(FINISH_MASKED_FOREACH_FUNCTION,
`	  /* Implementation end.  */
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
extern void `s'name`'rtype_qual`_'atype_code (atype_name * restrict,
        gfc_charlen_type,
	atype * const restrict array, GFC_LOGICAL_4 *, gfc_charlen_type);
export_proto(`s'name`'rtype_qual`_'atype_code);

void
`s'name`'rtype_qual`_'atype_code (atype_name * restrict ret,
        gfc_charlen_type xlen, atype * const restrict array,
	GFC_LOGICAL_4 *mask, gfc_charlen_type len)
	
{
  if (mask == NULL || *mask)
    {
      name`'rtype_qual`_'atype_code (ret, xlen, array, len);
      return;
    }
  memset (ret, INITVAL, sizeof (*ret) * len);
}')dnl
