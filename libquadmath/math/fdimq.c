/* Return positive difference between arguments.
   Copyright (C) 1997, 2004, 2009 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <errno.h>
#include "quadmath-imp.h"

__float128
fdimq (__float128 x, __float128 y)
{
  int clsx = fpclassifyq (x);
  int clsy = fpclassifyq (y);

  if (clsx == QUADFP_NAN || clsy == QUADFP_NAN
      || (y < 0 && clsx == QUADFP_INFINITE && clsy == QUADFP_INFINITE))
    /* Raise invalid flag.  */
    return x - y;

  if (x <= y)
    return 0.0Q;

  __float128 r = x - y;
  if (isinfq (r))
    errno = ERANGE;

  return r;
}
