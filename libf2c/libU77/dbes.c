/* Copyright (C) 1995, 1996 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#if 0 	/* Don't include these unless necessary -- dnp. */
#include "f2c.h"
#include <math.h>

double G77_dbesj0_0 (const double *x) {
    return j0 (*x);
}

double G77_dbesj1_0 (const double *x) {
    return j1 (*x);
}

double G77_dbesjn_0 (const integer *n, double *x) {
     return jn (*n, *x);
 }

double G77_dbesy0_0 (const double *x) {
    return y0 (*x);
}

double G77_dbesy1_0 (const double *x) {
    return y1 (*x);
}

double G77_dbesyn_0 (const integer *n, double *x) {
    return yn (*n, *x);
}
#endif
