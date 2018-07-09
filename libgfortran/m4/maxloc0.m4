`/* Implementation of the MAXLOC intrinsic
   Copyright (C) 2002-2018 Free Software Foundation, Inc.
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
#include <assert.h>'

include(iparm.m4)dnl
include(iforeach.m4)dnl

`#if defined (HAVE_'atype_name`) && defined (HAVE_'rtype_name`)'

FOREACH_FUNCTION(
`    atype_name maxval;
#if defined('atype_nan`)
    int fast = 0;
#endif

#if defined('atype_inf`)
    maxval = -atype_inf;
#else
    maxval = atype_min;
#endif',
`#if defined('atype_nan`)
      if (unlikely (!fast))
	{
	  do
	    {
	      if (*base >= maxval)
		{
		  fast = 1;
		  maxval = *base;
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
      else
#endif
        if (back)
      	  do
            {
	      if (unlikely (*base >= maxval))
	       {
	         maxval = *base;
	      	 for (n = 0; n < rank; n++)
		   dest[n * dstride] = count[n] + 1;
	       }
	     base += sstride[0];
	   }
         while (++count[0] != extent[0]);
       else
         do
	   {
	     if (unlikely (*base > maxval))
	       {
	         maxval = *base;
		 for (n = 0; n < rank; n++)
		   dest[n * dstride] = count[n] + 1;
	       }')
MASKED_FOREACH_FUNCTION(
`  atype_name maxval;
   int fast = 0;

#if defined('atype_inf`)
    maxval = -atype_inf;
#else
    maxval = atype_min;
#endif',
`      if (unlikely (!fast))
	{
	  do
	    {
	      if (*mbase)
		{
#if defined('atype_nan`)
		  if (unlikely (dest[0] == 0))
		    for (n = 0; n < rank; n++)
		      dest[n * dstride] = count[n] + 1;
		  if (*base >= maxval)
#endif
		    {
		      fast = 1;
		      maxval = *base;
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
      else
        if (back)
	  do
	    {
	      if (*mbase && *base >= maxval)
	        {
	          maxval = *base;
	          for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;
		}
	      base += sstride[0];
	    }
	  while (++count[0] != extent[0]);
	else
	  do
	    {
	      if (*mbase && unlikely (*base > maxval))
	        {
		  maxval = *base;
		  for (n = 0; n < rank; n++)
		    dest[n * dstride] = count[n] + 1;
	        }')

SCALAR_FOREACH_FUNCTION(`0')
#endif
