/* Support run-time routines for the standard prelude.

   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   Under Section 7 of GPL version 3, you are granted additional permissions
   described in the GCC Runtime Library Exception, version 3.1, as published by
   the Free Software Foundation.

   You should have received a copy of the GNU General Public License and a copy
   of the GCC Runtime Library Exception along with this program; see the files
   COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "ga68.h"

#include <stdlib.h> /* For rand.  */


/* Implementation of the standard prelude `random' function.  */

float
_libga68_random (void)
{
  float res = (float) rand () / (float) (RAND_MAX);
  return res;
}

double
_libga68_longrandom (void)
{
  double res = (double) rand () / (float) (RAND_MAX);
  return res;
}

long double
_libga68_longlongrandom (void)
{
  long double res = (long double) rand () / (float) (RAND_MAX);
  return res;
}
