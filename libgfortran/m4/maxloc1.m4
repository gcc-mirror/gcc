`/* Implementation of the MAXLOC intrinsic
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
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

#include "libgfortran.h"'

include(iparm.m4)dnl
include(ifunction.m4)dnl

`#if defined (HAVE_'atype_name`) && defined (HAVE_'rtype_name`)'

ARRAY_FUNCTION(0,
`	atype_name maxval;
#if defined ('atype_inf`)
	maxval = -atype_inf;
#else
	maxval = atype_min;
#endif
	result = 1;',
`#if defined ('atype_nan`)
		if (*src >= maxval)
		  {
		    maxval = *src;
		    result = (rtype_name)n + 1;
		    break;
		  }
	      }
	    for (; n < len; n++, src += delta)
	      {
#endif
		if (*src > maxval)
		  {
		    maxval = *src;
		    result = (rtype_name)n + 1;
		  }', `')

MASKED_ARRAY_FUNCTION(0,
`	atype_name maxval;
#if defined ('atype_inf`)
	maxval = -atype_inf;
#else
	maxval = atype_min;
#endif
#if defined ('atype_nan`)
	rtype_name result2 = 0;
#endif
	result = 0;',
`		if (*msrc)
		  {
#if defined ('atype_nan`)
		    if (!result2)
		      result2 = (rtype_name)n + 1;
		    if (*src >= maxval)
#endif
		      {
			maxval = *src;
			result = (rtype_name)n + 1;
			break;
		      }
		  }
	      }
#if defined ('atype_nan`)
	    if (unlikely (n >= len))
	      result = result2;
	    else
#endif
	    for (; n < len; n++, src += delta, msrc += mdelta)
	      {
		if (*msrc && *src > maxval)
		  {
		    maxval = *src;
		    result = (rtype_name)n + 1;
		  }')

SCALAR_ARRAY_FUNCTION(0)

#endif
