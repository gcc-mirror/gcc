/* Wrapper for systems without the various C99 single precision Bessel
   functions.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfortran; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <math.h>
#include "libgfortran.h"

/* Assume we have all or none of these.  */
#if HAVE_J0 && !HAVE_J0F
float
j0f (float x)
{
  return (float) j0 ((double) x);
}
#endif

#if HAVE_J1 && !HAVE_J1F
float j1f (float x)
{
  return (float) j1 ((double) x);
}
#endif

#if HAVE_JN && !HAVE_JNF
float
jnf (int n, float x)
{
  return (float) jn (n, (double) x);
}
#endif

#if HAVE_Y0 && !HAVE_Y0F
float
y0f (float x)
{
  return (float) y0 ((double) x);
}
#endif

#if HAVE_Y1 && !HAVE_Y1F
float
y1f (float x)
{
  return (float) y1 ((double) x);
}
#endif

#if HAVE_YN && !HAVE_YNF
float
ynf (int n, float x)
{
  return (float) yn (n, (double) x);
}
#endif
