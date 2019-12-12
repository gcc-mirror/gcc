`/* Implementation of the MAXVAL intrinsic
   Copyright (C) 2002-2019 Free Software Foundation, Inc.
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

ARRAY_FUNCTION(atype_min,
`#if defined ('atype_inf`)
	result = -atype_inf;
#else
	result = atype_min;
#endif',
`#if defined ('atype_nan`)
		if (*src >= result)
		  break;
	      }
	    if (unlikely (n >= len))
	      result = atype_nan;
	    else for (; n < len; n++, src += delta)
	      {
#endif
		if (*src > result)
		  result = *src;', `')

MASKED_ARRAY_FUNCTION(atype_min,
`#if defined ('atype_inf`)
	result = -atype_inf;
#else
	result = atype_min;
#endif
#if defined ('atype_nan`)
	int non_empty_p = 0;
#endif',
`#if defined ('atype_inf`) || defined ('atype_nan`)
		if (*msrc)
		  {
#if defined ('atype_nan`)
		    non_empty_p = 1;
		    if (*src >= result)
#endif
		      break;
		  }
	      }
	    if (unlikely (n >= len))
	      {
#if defined ('atype_nan`)
		result = non_empty_p ? atype_nan : atype_min;
#else
		result = atype_min;
#endif
	      }
	    else for (; n < len; n++, src += delta, msrc += mdelta)
	      {
#endif
		if (*msrc && *src > result)
		  result = *src;')

SCALAR_ARRAY_FUNCTION(atype_min)

#endif
