/* Wrapper for systems without the various C99 single precision Bessel
   functions.
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <math.h>
#include "libgfortran.h"

#if HAVE_J0 && !HAVE_J0F
extern float j0f (float);

float
j0f (float x)
{
  return (float) j0 ((double) x);
}
#endif

#if HAVE_J1 && !HAVE_J1F
extern float j1f (float);

float j1f (float x)
{
  return (float) j1 ((double) x);
}
#endif

#if HAVE_JN && !HAVE_JNF
extern float jnf (int, float);

float
jnf (int n, float x)
{
  return (float) jn (n, (double) x);
}
#endif

#if HAVE_Y0 && !HAVE_Y0F
extern float y0f (float);

float
y0f (float x)
{
  return (float) y0 ((double) x);
}
#endif

#if HAVE_Y1 && !HAVE_Y1F
extern float y1f (float);

float
y1f (float x)
{
  return (float) y1 ((double) x);
}
#endif

#if HAVE_YN && !HAVE_YNF
extern float ynf (int, float);

float
ynf (int n, float x)
{
  return (float) yn (n, (double) x);
}
#endif
