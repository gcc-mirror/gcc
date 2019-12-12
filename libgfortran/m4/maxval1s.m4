`/* Implementation of the MAXVAL intrinsic
   Copyright (C) 2017-2019 Free Software Foundation, Inc.
   Contributed by Thomas Koenig

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
include(ifunction-s2.m4)dnl

`#if defined (HAVE_'atype_name`) && defined (HAVE_'rtype_name`)'

ARRAY_FUNCTION(0,
`	const atype_name *retval;
	retval = base;',
`		if (compare_fcn (src, retval, string_len) > 0)
		  {
		    retval = src;
		  }', `')

MASKED_ARRAY_FUNCTION(0,
`	const atype_name *retval;
	memset (dest, 0, sizeof (*dest) * string_len);
	retval = dest;',
`		if (*msrc)
		      {
			retval = src;
			break;
		      }
	    }
	    for (; n < len; n++, src += delta, msrc += mdelta)
	      {
		if (*msrc && compare_fcn (src, retval, string_len) > 0)
		  {
		    retval = src;
		  }
	      ')

SCALAR_ARRAY_FUNCTION(0)

#endif
