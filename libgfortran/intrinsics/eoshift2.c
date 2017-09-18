/* Generic implementation of the EOSHIFT intrinsic
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran runtime library (libgfortran).

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
#include <string.h>

/* TODO: make this work for large shifts when
   sizeof(int) < sizeof (index_type).  */

static void
eoshift2 (gfc_array_char *ret, const gfc_array_char *array,
	  int shift, const gfc_array_char *bound, int which,
	  const char *filler, index_type filler_len)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type roffset;
  char * restrict rptr;
  char *dest;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type soffset;
  const char *sptr;
  const char *src;
  /* b.* indicates the bound array.  */
  index_type bstride[GFC_MAX_DIMENSIONS];
  index_type bstride0;
  const char *bptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type dim;
  index_type len;
  index_type n;
  index_type arraysize;
  index_type size;

  /* The compiler cannot figure out that these are set, initialize
     them to avoid warnings.  */
  len = 0;
  soffset = 0;
  roffset = 0;

  size = GFC_DESCRIPTOR_SIZE (array);

  arraysize = size0 ((array_t *) array);

  if (ret->base_addr == NULL)
    {
      int i;

      ret->offset = 0;
      ret->dtype = array->dtype;

      /* xmallocarray allocates a single byte for zero size.  */
      ret->base_addr = xmallocarray (arraysize, size);

      for (i = 0; i < GFC_DESCRIPTOR_RANK (array); i++)
        {
	  index_type ub, str;

          ub = GFC_DESCRIPTOR_EXTENT(array,i) - 1;

          if (i == 0)
	    str = 1;
          else
            str = GFC_DESCRIPTOR_EXTENT(ret,i-1)
	      * GFC_DESCRIPTOR_STRIDE(ret,i-1);

	  GFC_DIMENSION_SET(ret->dim[i], 0, ub, str);
        }
    }
  else if (unlikely (compile_options.bounds_check))
    {
      bounds_equal_extents ((array_t *) ret, (array_t *) array,
				 "return value", "EOSHIFT");
    }

  if (arraysize == 0)
    return;

  which = which - 1;

  extent[0] = 1;
  count[0] = 0;
  sstride[0] = -1;
  rstride[0] = -1;
  bstride[0] = -1;
  n = 0;
  for (dim = 0; dim < GFC_DESCRIPTOR_RANK (array); dim++)
    {
      if (dim == which)
        {
          roffset = GFC_DESCRIPTOR_STRIDE_BYTES(ret,dim);
          if (roffset == 0)
            roffset = size;
          soffset = GFC_DESCRIPTOR_STRIDE_BYTES(array,dim);
          if (soffset == 0)
            soffset = size;
          len = GFC_DESCRIPTOR_EXTENT(array,dim);
        }
      else
        {
          count[n] = 0;
          extent[n] = GFC_DESCRIPTOR_EXTENT(array,dim);
          rstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(ret,dim);
          sstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(array,dim);
          if (bound)
            bstride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(bound,n);
          else
            bstride[n] = 0;
          n++;
        }
    }
  if (sstride[0] == 0)
    sstride[0] = size;
  if (rstride[0] == 0)
    rstride[0] = size;
  if (bound && bstride[0] == 0)
    bstride[0] = size;

  dim = GFC_DESCRIPTOR_RANK (array);
  rstride0 = rstride[0];
  sstride0 = sstride[0];
  bstride0 = bstride[0];
  rptr = ret->base_addr;
  sptr = array->base_addr;

  if ((shift >= 0 ? shift : -shift ) > len)
    {
      shift = len;
      len = 0;
    }
  else
    {
      if (shift > 0)
	len = len - shift;
      else
	len = len + shift;
    }
  
  if (bound)
    bptr = bound->base_addr;
  else
    bptr = NULL;

  while (rptr)
    {
      /* Do the shift for this dimension.  */
      if (shift > 0)
        {
          src = &sptr[shift * soffset];
          dest = rptr;
        }
      else
        {
          src = sptr;
          dest = &rptr[-shift * roffset];
        }

      /* If the elements are contiguous, perform a single block move.  */
      if (soffset == size && roffset == size)
	{
	  size_t chunk = size * len;
	  memcpy (dest, src, chunk);
	  dest += chunk;
	}
      else
	{
	  for (n = 0; n < len; n++)
	    {
	      memcpy (dest, src, size);
	      dest += roffset;
	      src += soffset;
	    }
	}
      if (shift >= 0)
        {
          n = shift;
        }
      else
        {
          dest = rptr;
          n = -shift;
        }

      if (bptr)
	while (n--)
	  {
	    memcpy (dest, bptr, size);
	    dest += roffset;
	  }
      else
	while (n--)
	  {
	    index_type i;

	    if (filler_len == 1)
	      memset (dest, filler[0], size);
	    else
	      for (i = 0; i < size ; i += filler_len)
		memcpy (&dest[i], filler, filler_len);

	    dest += roffset;
	  }

      /* Advance to the next section.  */
      rptr += rstride0;
      sptr += sstride0;
      bptr += bstride0;
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
          sptr -= sstride[n] * extent[n];
          bptr -= bstride[n] * extent[n];
          n++;
          if (n >= dim - 1)
            {
              /* Break out of the loop.  */
              rptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              rptr += rstride[n];
              sptr += sstride[n];
              bptr += bstride[n];
            }
        }
    }
}


#define DEFINE_EOSHIFT(N)						      \
  extern void eoshift2_##N (gfc_array_char *, const gfc_array_char *,	      \
			    const GFC_INTEGER_##N *, const gfc_array_char *,  \
			    const GFC_INTEGER_##N *);			      \
  export_proto(eoshift2_##N);						      \
									      \
  void									      \
  eoshift2_##N (gfc_array_char *ret, const gfc_array_char *array,	      \
		const GFC_INTEGER_##N *pshift, const gfc_array_char *pbound,  \
		const GFC_INTEGER_##N *pdim)				      \
  {									      \
    eoshift2 (ret, array, *pshift, pbound, pdim ? *pdim : 1,		      \
	      "\0", 1);			      \
  }									      \
									      \
  extern void eoshift2_##N##_char (gfc_array_char *, GFC_INTEGER_4,	      \
				   const gfc_array_char *,		      \
				   const GFC_INTEGER_##N *,		      \
				   const gfc_array_char *,		      \
				   const GFC_INTEGER_##N *,		      \
				   GFC_INTEGER_4, GFC_INTEGER_4);	      \
  export_proto(eoshift2_##N##_char);					      \
									      \
  void									      \
  eoshift2_##N##_char (gfc_array_char *ret,				      \
		       GFC_INTEGER_4 ret_length __attribute__((unused)),      \
		       const gfc_array_char *array,			      \
		       const GFC_INTEGER_##N *pshift,			      \
		       const gfc_array_char *pbound,			      \
		       const GFC_INTEGER_##N *pdim,			      \
		       GFC_INTEGER_4 array_length __attribute__((unused)),    \
		       GFC_INTEGER_4 bound_length __attribute__((unused)))    \
  {									      \
    eoshift2 (ret, array, *pshift, pbound, pdim ? *pdim : 1,		      \
	      " ", 1);							      \
  }									      \
									      \
  extern void eoshift2_##N##_char4 (gfc_array_char *, GFC_INTEGER_4,	      \
				    const gfc_array_char *,		      \
				    const GFC_INTEGER_##N *,		      \
				    const gfc_array_char *,		      \
				    const GFC_INTEGER_##N *,		      \
				    GFC_INTEGER_4, GFC_INTEGER_4);	      \
  export_proto(eoshift2_##N##_char4);					      \
									      \
  void									      \
  eoshift2_##N##_char4 (gfc_array_char *ret,				      \
			GFC_INTEGER_4 ret_length __attribute__((unused)),     \
			const gfc_array_char *array,			      \
			const GFC_INTEGER_##N *pshift,			      \
			const gfc_array_char *pbound,			      \
			const GFC_INTEGER_##N *pdim,			      \
			GFC_INTEGER_4 array_length __attribute__((unused)),   \
			GFC_INTEGER_4 bound_length __attribute__((unused)))   \
  {									      \
    static const gfc_char4_t space = (unsigned char) ' ';		      \
    eoshift2 (ret, array, *pshift, pbound, pdim ? *pdim : 1,		      \
	      (const char *) &space,					      \
	      sizeof (gfc_char4_t));					      \
  }

DEFINE_EOSHIFT (1);
DEFINE_EOSHIFT (2);
DEFINE_EOSHIFT (4);
DEFINE_EOSHIFT (8);
#ifdef HAVE_GFC_INTEGER_16
DEFINE_EOSHIFT (16);
#endif
