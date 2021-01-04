/* Implementation of the RESHAPE intrinsic
   Copyright (C) 2002-2021 Free Software Foundation, Inc.
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


#if defined (HAVE_GFC_REAL_8)

typedef GFC_FULL_ARRAY_DESCRIPTOR(1, index_type) shape_type;


extern void reshape_r8 (gfc_array_r8 * const restrict, 
	gfc_array_r8 * const restrict, 
	shape_type * const restrict,
	gfc_array_r8 * const restrict, 
	shape_type * const restrict);
export_proto(reshape_r8);

void
reshape_r8 (gfc_array_r8 * const restrict ret, 
	gfc_array_r8 * const restrict source, 
	shape_type * const restrict shape,
	gfc_array_r8 * const restrict pad, 
	shape_type * const restrict order)
{
  /* r.* indicates the return array.  */
  index_type rcount[GFC_MAX_DIMENSIONS];
  index_type rextent[GFC_MAX_DIMENSIONS];
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rdim;
  index_type rsize;
  index_type rs;
  index_type rex;
  GFC_REAL_8 *rptr;
  /* s.* indicates the source array.  */
  index_type scount[GFC_MAX_DIMENSIONS];
  index_type sextent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type sdim;
  index_type ssize;
  const GFC_REAL_8 *sptr;
  /* p.* indicates the pad array.  */
  index_type pcount[GFC_MAX_DIMENSIONS];
  index_type pextent[GFC_MAX_DIMENSIONS];
  index_type pstride[GFC_MAX_DIMENSIONS];
  index_type pdim;
  index_type psize;
  const GFC_REAL_8 *pptr;

  const GFC_REAL_8 *src;
  int sempty, pempty, shape_empty;
  index_type shape_data[GFC_MAX_DIMENSIONS];

  rdim = GFC_DESCRIPTOR_EXTENT(shape,0);
  /* rdim is always > 0; this lets the compiler optimize more and
   avoids a potential warning.  */
  GFC_ASSERT(rdim>0);

  if (rdim != GFC_DESCRIPTOR_RANK(ret))
    runtime_error("rank of return array incorrect in RESHAPE intrinsic");

  shape_empty = 0;

  for (index_type n = 0; n < rdim; n++)
    {
      shape_data[n] = shape->base_addr[n * GFC_DESCRIPTOR_STRIDE(shape,0)];
      if (shape_data[n] <= 0)
      {
        shape_data[n] = 0;
	shape_empty = 1;
      }
    }

  if (ret->base_addr == NULL)
    {
      index_type alloc_size;

      rs = 1;
      for (index_type n = 0; n < rdim; n++)
	{
	  rex = shape_data[n];

	  GFC_DIMENSION_SET(ret->dim[n], 0, rex - 1, rs);

	  rs *= rex;
	}
      ret->offset = 0;

      if (unlikely (rs < 1))
        alloc_size = 0;
      else
        alloc_size = rs;

      ret->base_addr = xmallocarray (alloc_size, sizeof (GFC_REAL_8));
      ret->dtype.rank = rdim;
    }

  if (shape_empty)
    return;

  if (pad)
    {
      pdim = GFC_DESCRIPTOR_RANK (pad);
      psize = 1;
      pempty = 0;
      for (index_type n = 0; n < pdim; n++)
        {
          pcount[n] = 0;
          pstride[n] = GFC_DESCRIPTOR_STRIDE(pad,n);
          pextent[n] = GFC_DESCRIPTOR_EXTENT(pad,n);
          if (pextent[n] <= 0)
	    {
	      pempty = 1;
	      pextent[n] = 0;
	    }

          if (psize == pstride[n])
            psize *= pextent[n];
          else
            psize = 0;
        }
      pptr = pad->base_addr;
    }
  else
    {
      pdim = 0;
      psize = 1;
      pempty = 1;
      pptr = NULL;
    }

  if (unlikely (compile_options.bounds_check))
    {
      index_type ret_extent, source_extent;

      rs = 1;
      for (index_type n = 0; n < rdim; n++)
	{
	  rs *= shape_data[n];
	  ret_extent = GFC_DESCRIPTOR_EXTENT(ret,n);
	  if (ret_extent != shape_data[n])
	    runtime_error("Incorrect extent in return value of RESHAPE"
			  " intrinsic in dimension %ld: is %ld,"
			  " should be %ld", (long int) n+1,
			  (long int) ret_extent, (long int) shape_data[n]);
	}

      source_extent = 1;
      sdim = GFC_DESCRIPTOR_RANK (source);
      for (index_type n = 0; n < sdim; n++)
	{
	  index_type se;
	  se = GFC_DESCRIPTOR_EXTENT(source,n);
	  source_extent *= se > 0 ? se : 0;
	}

      if (rs > source_extent && (!pad || pempty))
	runtime_error("Incorrect size in SOURCE argument to RESHAPE"
		      " intrinsic: is %ld, should be %ld",
		      (long int) source_extent, (long int) rs);

      if (order)
	{
	  int seen[GFC_MAX_DIMENSIONS];
	  index_type v;

	  for (index_type n = 0; n < rdim; n++)
	    seen[n] = 0;

	  for (index_type n = 0; n < rdim; n++)
	    {
	      v = order->base_addr[n * GFC_DESCRIPTOR_STRIDE(order,0)] - 1;

	      if (v < 0 || v >= rdim)
		runtime_error("Value %ld out of range in ORDER argument"
			      " to RESHAPE intrinsic", (long int) v + 1);

	      if (seen[v] != 0)
		runtime_error("Duplicate value %ld in ORDER argument to"
			      " RESHAPE intrinsic", (long int) v + 1);
		
	      seen[v] = 1;
	    }
	}
    }

  rsize = 1;
  for (index_type n = 0; n < rdim; n++)
    {
      index_type dim;
      if (order)
        dim = order->base_addr[n * GFC_DESCRIPTOR_STRIDE(order,0)] - 1;
      else
        dim = n;

      rcount[n] = 0;
      rstride[n] = GFC_DESCRIPTOR_STRIDE(ret,dim);
      rextent[n] = GFC_DESCRIPTOR_EXTENT(ret,dim);
      if (rextent[n] < 0)
        rextent[n] = 0;

      if (rextent[n] != shape_data[dim])
        runtime_error ("shape and target do not conform");

      if (rsize == rstride[n])
        rsize *= rextent[n];
      else
        rsize = 0;
      if (rextent[n] <= 0)
        return;
    }

  sdim = GFC_DESCRIPTOR_RANK (source);

  /* sdim is always > 0; this lets the compiler optimize more and
   avoids a warning.  */
  GFC_ASSERT(sdim>0);

  ssize = 1;
  sempty = 0;
  for (index_type n = 0; n < sdim; n++)
    {
      scount[n] = 0;
      sstride[n] = GFC_DESCRIPTOR_STRIDE(source,n);
      sextent[n] = GFC_DESCRIPTOR_EXTENT(source,n);
      if (sextent[n] <= 0)
	{
	  sempty = 1;
	  sextent[n] = 0;
	}

      if (ssize == sstride[n])
        ssize *= sextent[n];
      else
        ssize = 0;
    }

  if (rsize != 0 && ssize != 0 && psize != 0)
    {
      rsize *= sizeof (GFC_REAL_8);
      ssize *= sizeof (GFC_REAL_8);
      psize *= sizeof (GFC_REAL_8);
      reshape_packed ((char *)ret->base_addr, rsize, (char *)source->base_addr,
		      ssize, pad ? (char *)pad->base_addr : NULL, psize);
      return;
    }
  rptr = ret->base_addr;
  src = sptr = source->base_addr;
  rstride0 = rstride[0];
  sstride0 = sstride[0];

  if (sempty && pempty)
    abort ();

  if (sempty)
    {
      /* Pretend we are using the pad array the first time around, too.  */
      src = pptr;
      sptr = pptr;
      sdim = pdim;
      for (index_type dim = 0; dim < pdim; dim++)
	{
	  scount[dim] = pcount[dim];
	  sextent[dim] = pextent[dim];
	  sstride[dim] = pstride[dim];
	  sstride0 = pstride[0];
	}
    }

  while (rptr)
    {
      /* Select between the source and pad arrays.  */
      *rptr = *src;
      /* Advance to the next element.  */
      rptr += rstride0;
      src += sstride0;
      rcount[0]++;
      scount[0]++;

      /* Advance to the next destination element.  */
      index_type n = 0;
      while (rcount[n] == rextent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          rcount[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          rptr -= rstride[n] * rextent[n];
          n++;
          if (n == rdim)
            {
              /* Break out of the loop.  */
              rptr = NULL;
              break;
            }
          else
            {
              rcount[n]++;
              rptr += rstride[n];
            }
        }
      /* Advance to the next source element.  */
      n = 0;
      while (scount[n] == sextent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          scount[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          src -= sstride[n] * sextent[n];
          n++;
          if (n == sdim)
            {
              if (sptr && pad)
                {
                  /* Switch to the pad array.  */
                  sptr = NULL;
                  sdim = pdim;
                  for (index_type dim = 0; dim < pdim; dim++)
                    {
                      scount[dim] = pcount[dim];
                      sextent[dim] = pextent[dim];
                      sstride[dim] = pstride[dim];
                      sstride0 = sstride[0];
                    }
                }
              /* We now start again from the beginning of the pad array.  */
              src = pptr;
              break;
            }
          else
            {
              scount[n]++;
              src += sstride[n];
            }
        }
    }
}

#endif
