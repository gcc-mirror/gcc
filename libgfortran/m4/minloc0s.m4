`/* Implementation of the MINLOC intrinsic
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

#include "libgfortran.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>'

include(iparm.m4)dnl
include(iforeach-s.m4)dnl

`#if defined (HAVE_'atype_name`) && defined (HAVE_'rtype_name`)'

#define HAVE_BACK_ARG 1

FOREACH_FUNCTION(
`  const atype_name *minval;
   minval = NULL;'
,
`    if (minval == NULL || (back ? compare_fcn (base, minval, len) <= 0 :
       	 	    	    	   compare_fcn (base, minval, len) < 0))
    {
      minval = base;
      for (n = 0; n < rank; n++)
        dest[n * dstride] = count[n] + 1;
    }')

MASKED_FOREACH_FUNCTION(
`  const atype_name *minval;

  minval = NULL;'
,
`  if (*mbase &&
      (minval == NULL || (back ? compare_fcn (base, minval, len) <= 0 :
       	 	    	    	 compare_fcn (base, minval, len) < 0)))
    {
      minval = base;
      for (n = 0; n < rank; n++)
        dest[n * dstride] = count[n] + 1;
    }')

SCALAR_FOREACH_FUNCTION(`0')
#endif
