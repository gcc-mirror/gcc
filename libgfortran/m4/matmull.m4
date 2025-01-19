`/* Implementation of the MATMUL intrinsic
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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
#include <assert.h>'

include(iparm.m4)dnl

`#if defined (HAVE_'rtype_name`)

/* Dimensions: retarray(x,y) a(x, count) b(count,y).
   Either a or b can be rank 1.  In this case x or y is 1.  */

extern void matmul_'rtype_code` ('rtype` * const restrict, 
	gfc_array_l1 * const restrict, gfc_array_l1 * const restrict);
export_proto(matmul_'rtype_code`);

void
matmul_'rtype_code` ('rtype` * const restrict retarray, 
	gfc_array_l1 * const restrict a, gfc_array_l1 * const restrict b)
{
  const GFC_LOGICAL_1 * restrict abase;
  const GFC_LOGICAL_1 * restrict bbase;
  'rtype_name` * restrict dest;
  index_type rxstride;
  index_type rystride;
  index_type xcount;
  index_type ycount;
  index_type xstride;
  index_type ystride;
  index_type x;
  index_type y;
  int a_kind;
  int b_kind;

  const GFC_LOGICAL_1 * restrict pa;
  const GFC_LOGICAL_1 * restrict pb;
  index_type astride;
  index_type bstride;
  index_type count;
  index_type n;

  assert (GFC_DESCRIPTOR_RANK (a) == 2
          || GFC_DESCRIPTOR_RANK (b) == 2);

  if (retarray->base_addr == NULL)
    {
      if (GFC_DESCRIPTOR_RANK (a) == 1)
        {
	  GFC_DIMENSION_SET(retarray->dim[0], 0,
	                    GFC_DESCRIPTOR_EXTENT(b,1) - 1, 1);
        }
      else if (GFC_DESCRIPTOR_RANK (b) == 1)
        {
	  GFC_DIMENSION_SET(retarray->dim[0], 0,
	                    GFC_DESCRIPTOR_EXTENT(a,0) - 1, 1);
        }
      else
        {
	  GFC_DIMENSION_SET(retarray->dim[0], 0,
	                    GFC_DESCRIPTOR_EXTENT(a,0) - 1, 1);

          GFC_DIMENSION_SET(retarray->dim[1], 0,
	                    GFC_DESCRIPTOR_EXTENT(b,1) - 1,
			    GFC_DESCRIPTOR_EXTENT(retarray,0));
        }
          
      retarray->base_addr
	= xmallocarray (size0 ((array_t *) retarray), sizeof ('rtype_name`));
      retarray->offset = 0;
    }
    else if (unlikely (compile_options.bounds_check))
      {
	index_type ret_extent, arg_extent;

	if (GFC_DESCRIPTOR_RANK (a) == 1)
	  {
	    arg_extent = GFC_DESCRIPTOR_EXTENT(b,1);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,0);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic: is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);
	  }
	else if (GFC_DESCRIPTOR_RANK (b) == 1)
	  {
	    arg_extent = GFC_DESCRIPTOR_EXTENT(a,0);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,0);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic: is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);	    
	  }
	else
	  {
	    arg_extent = GFC_DESCRIPTOR_EXTENT(a,0);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,0);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic for dimension 1:"
			     " is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);

	    arg_extent = GFC_DESCRIPTOR_EXTENT(b,1);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,1);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic for dimension 2:"
			     " is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);
	  }
      }

  abase = a->base_addr;
  a_kind = GFC_DESCRIPTOR_SIZE (a);

  if (a_kind == 1 || a_kind == 2 || a_kind == 4 || a_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
     || a_kind == 16
#endif
     )
    abase = GFOR_POINTER_TO_L1 (abase, a_kind);
  else
    internal_error (NULL, "Funny sized logical array");

  bbase = b->base_addr;
  b_kind = GFC_DESCRIPTOR_SIZE (b);

  if (b_kind == 1 || b_kind == 2 || b_kind == 4 || b_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
     || b_kind == 16
#endif
     )
    bbase = GFOR_POINTER_TO_L1 (bbase, b_kind);
  else
    internal_error (NULL, "Funny sized logical array");

  dest = retarray->base_addr;
'
sinclude(`matmul_asm_'rtype_code`.m4')dnl
`
  if (GFC_DESCRIPTOR_RANK (retarray) == 1)
    {
      rxstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
      rystride = rxstride;
    }
  else
    {
      rxstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
      rystride = GFC_DESCRIPTOR_STRIDE(retarray,1);
    }

  /* If we have rank 1 parameters, zero the absent stride, and set the size to
     one.  */
  if (GFC_DESCRIPTOR_RANK (a) == 1)
    {
      astride = GFC_DESCRIPTOR_STRIDE_BYTES(a,0);
      count = GFC_DESCRIPTOR_EXTENT(a,0);
      xstride = 0;
      rxstride = 0;
      xcount = 1;
    }
  else
    {
      astride = GFC_DESCRIPTOR_STRIDE_BYTES(a,1);
      count = GFC_DESCRIPTOR_EXTENT(a,1);
      xstride = GFC_DESCRIPTOR_STRIDE_BYTES(a,0);
      xcount = GFC_DESCRIPTOR_EXTENT(a,0);
    }
  if (GFC_DESCRIPTOR_RANK (b) == 1)
    {
      bstride = GFC_DESCRIPTOR_STRIDE_BYTES(b,0);
      assert(count == GFC_DESCRIPTOR_EXTENT(b,0));
      ystride = 0;
      rystride = 0;
      ycount = 1;
    }
  else
    {
      bstride = GFC_DESCRIPTOR_STRIDE_BYTES(b,0);
      assert(count == GFC_DESCRIPTOR_EXTENT(b,0));
      ystride = GFC_DESCRIPTOR_STRIDE_BYTES(b,1);
      ycount = GFC_DESCRIPTOR_EXTENT(b,1);
    }

  for (y = 0; y < ycount; y++)
    {
      for (x = 0; x < xcount; x++)
        {
          /* Do the summation for this element.  For real and integer types
             this is the same as DOT_PRODUCT.  For complex types we use do
             a*b, not conjg(a)*b.  */
          pa = abase;
          pb = bbase;
          *dest = 0;

          for (n = 0; n < count; n++)
            {
              if (*pa && *pb)
                {
                  *dest = 1;
                  break;
                }
              pa += astride;
              pb += bstride;
            }

          dest += rxstride;
          abase += xstride;
        }
      abase -= xstride * xcount;
      bbase += ystride;
      dest += rystride - (rxstride * xcount);
    }
}

#endif
'
