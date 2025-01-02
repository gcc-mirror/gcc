`/* Implementation of the BESSEL_JN and BESSEL_YN transformational
   function using a recurrence algorithm.
   Copyright (C) 2010-2025 Free Software Foundation, Inc.
   Contributed by Tobias Burnus <burnus@net-b.de>

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

#include "libgfortran.h"'

include(iparm.m4)dnl
include(`mtype.m4')dnl

mathfunc_macro

`#if defined (HAVE_'rtype_name`)



#if 'hasmathfunc(jn)`
extern void bessel_jn_r'rtype_kind` ('rtype` * const restrict ret, int n1,
				     int n2, 'rtype_name` x);
export_proto(bessel_jn_r'rtype_kind`);

void
bessel_jn_r'rtype_kind` ('rtype` * const restrict ret, int n1, int n2, 'rtype_name` x)
{
  int i;
  index_type stride;

  'rtype_name` last1, last2, x2rev;

  stride = GFC_DESCRIPTOR_STRIDE(ret,0);

  if (ret->base_addr == NULL)
    {
      size_t size = n2 < n1 ? 0 : n2-n1+1; 
      GFC_DIMENSION_SET(ret->dim[0], 0, size-1, 1);
      ret->base_addr = xmallocarray (size, sizeof ('rtype_name`));
      ret->offset = 0;
    }

  if (unlikely (n2 < n1))
    return;

  if (unlikely (compile_options.bounds_check)
      && GFC_DESCRIPTOR_EXTENT(ret,0) != (n2-n1+1))
    runtime_error("Incorrect extent in return value of BESSEL_JN "
		  "(%ld vs. %ld)", (long int) n2-n1,
		  (long int) GFC_DESCRIPTOR_EXTENT(ret,0));

  stride = GFC_DESCRIPTOR_STRIDE(ret,0);

  if (unlikely (x == 0))
    {
      ret->base_addr[0] = 1;
      for (i = 1; i <= n2-n1; i++)
        ret->base_addr[i*stride] = 0;
      return;
    }

  last1 = MATHFUNC(jn) (n2, x);
  ret->base_addr[(n2-n1)*stride] = last1;

  if (n1 == n2)
    return;

  last2 = MATHFUNC(jn) (n2 - 1, x);
  ret->base_addr[(n2-n1-1)*stride] = last2;

  if (n1 + 1 == n2)
    return;

  x2rev = GFC_REAL_'rtype_kind`_LITERAL(2.)/x;

  for (i = n2-n1-2; i >= 0; i--)
    {
      ret->base_addr[i*stride] = x2rev * (i+1+n1) * last2 - last1;
      last1 = last2;
      last2 = ret->base_addr[i*stride];
    }
}

#endif

#if 'hasmathfunc(yn)`
extern void bessel_yn_r'rtype_kind` ('rtype` * const restrict ret,
				     int n1, int n2, 'rtype_name` x);
export_proto(bessel_yn_r'rtype_kind`);

void
bessel_yn_r'rtype_kind` ('rtype` * const restrict ret, int n1, int n2,
			 'rtype_name` x)
{
  int i;
  index_type stride;

  'rtype_name` last1, last2, x2rev;

  stride = GFC_DESCRIPTOR_STRIDE(ret,0);

  if (ret->base_addr == NULL)
    {
      size_t size = n2 < n1 ? 0 : n2-n1+1; 
      GFC_DIMENSION_SET(ret->dim[0], 0, size-1, 1);
      ret->base_addr = xmallocarray (size, sizeof ('rtype_name`));
      ret->offset = 0;
    }

  if (unlikely (n2 < n1))
    return;

  if (unlikely (compile_options.bounds_check)
      && GFC_DESCRIPTOR_EXTENT(ret,0) != (n2-n1+1))
    runtime_error("Incorrect extent in return value of BESSEL_JN "
		  "(%ld vs. %ld)", (long int) n2-n1,
		  (long int) GFC_DESCRIPTOR_EXTENT(ret,0));

  stride = GFC_DESCRIPTOR_STRIDE(ret,0);

  if (unlikely (x == 0))
    {
      for (i = 0; i <= n2-n1; i++)
#if defined('rtype_name`_INFINITY)
        ret->base_addr[i*stride] = -'rtype_name`_INFINITY;
#else
        ret->base_addr[i*stride] = -'rtype_name`_HUGE;
#endif
      return;
    }

  last1 = MATHFUNC(yn) (n1, x);
  ret->base_addr[0] = last1;

  if (n1 == n2)
    return;

  last2 = MATHFUNC(yn) (n1 + 1, x);
  ret->base_addr[1*stride] = last2;

  if (n1 + 1 == n2)
    return;

  x2rev = GFC_REAL_'rtype_kind`_LITERAL(2.)/x;

  for (i = 2; i <= n2 - n1; i++)
    {
#if defined('rtype_name`_INFINITY)
      if (unlikely (last2 == -'rtype_name`_INFINITY))
	{
	  ret->base_addr[i*stride] = -'rtype_name`_INFINITY;
	}
      else
#endif
	{
	  ret->base_addr[i*stride] = x2rev * (i-1+n1) * last2 - last1;
	  last1 = last2;
	  last2 = ret->base_addr[i*stride];
	}
    }
}
#endif

#endif'

