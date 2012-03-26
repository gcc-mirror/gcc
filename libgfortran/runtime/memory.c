/* Memory management routines.
   Copyright 2002, 2005, 2006, 2007, 2009, 2010, 2012 
   Free Software Foundation, Inc.
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
#include <stdlib.h>


void *
xmalloc (size_t n)
{
  void *p;

  if (n == 0)
    n = 1;

  p = malloc (n);

  if (p == NULL)
    os_error ("Memory allocation failed");

  return p;
}


/* calloc wrapper that aborts on error.  */

void *
xcalloc (size_t nmemb, size_t size)
{
  if (nmemb * size == 0)
    nmemb = size = 1;

  void *p = calloc (nmemb, size);
  if (!p)
    os_error ("Allocating cleared memory failed");

  return p;
}
