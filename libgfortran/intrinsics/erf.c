/* Wrapper for systems without the C99 erff() and erfcf() functions
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

#if HAVE_ERF && !HAVE_ERFF
float
erff (float x)
{
  return (float) erf ((double) x);
}
#endif

#if HAVE_ERFC && !HAVE_ERFCF
float
erfcf (float x)
{
  return (float) erfc ((double) x);
}
#endif
