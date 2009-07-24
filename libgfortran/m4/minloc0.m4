`/* Implementation of the MINLOC intrinsic
   Copyright 2002, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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
#include <stdlib.h>
#include <assert.h>
#include <limits.h>'

include(iparm.m4)dnl
include(iforeach.m4)dnl

`#if defined (HAVE_'atype_name`) && defined (HAVE_'rtype_name`)'

FOREACH_FUNCTION(
`    atype_name minval;
#if defined('atype_nan`)
    int fast = 0;
#endif

#if defined('atype_inf`)
    minval = atype_inf;
#else
    minval = atype_max;
#endif',
`#if defined('atype_nan`)
	}
      while (0);
      if (unlikely (!fast))
	{
	  do
	    {
	      if (*base <= minval)
		{
		  fast = 1;
		  minval = *base;
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;
		  break;
		}
	      base += sstride[0];
	    }
	  while (++count[0] != extent[0]);
	  if (likely (fast))
	    continue;
	}
      else do
	{
#endif
	  if (*base < minval)
	    {
	      minval = *base;
	      for (n = 0; n < rank; n++)
		dest[n * dstride] = count[n] + 1;
	    }')

MASKED_FOREACH_FUNCTION(
`  atype_name minval;
   int fast = 0;

#if defined('atype_inf`)
    minval = atype_inf;
#else
    minval = atype_max;
#endif',
`	}
      while (0);
      if (unlikely (!fast))
	{
	  do
	    {
	      if (*mbase)
		{
#if defined('atype_nan`)
		  if (unlikely (dest[0] == 0))
		    for (n = 0; n < rank; n++)
		      dest[n * dstride] = count[n] + 1;
		  if (*base <= minval)
#endif
		    {
		      fast = 1;
		      minval = *base;
		      for (n = 0; n < rank; n++)
			dest[n * dstride] = count[n] + 1;
		      break;
		    }
		}
	      base += sstride[0];
	      mbase += mstride[0];
	    }
	  while (++count[0] != extent[0]);
	  if (likely (fast))
	    continue;
	}
      else do
	{
	  if (*mbase && *base < minval)
	    {
	      minval = *base;
	      for (n = 0; n < rank; n++)
		dest[n * dstride] = count[n] + 1;
	    }')

SCALAR_FOREACH_FUNCTION(`0')
#endif
