/* target.c provide access to miscellaneous math functions.

Copyright (C) 2005-2022 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <config.h>

#if defined(HAVE_MATH_H)
#include <math.h>
#endif

#if !defined(HAVE_EXP10)
#if defined(M_LN10)
double
exp10 (double x)
{
  return exp (x * M_LN10);
}
#endif
#endif

#if !defined(HAVE_EXP10F)
#if defined(M_LN10)
float
exp10f (float x)
{
  return expf (x * M_LN10);
}
#endif
#endif

#if !defined(HAVE_EXP10L)
#if defined(M_LN10)
long double
exp10l (long double x)
{
  return expl (x * M_LN10);
}
#endif
#endif
