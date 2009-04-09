/* Implementation of the ERFC_SCALED intrinsic.
   Copyright (C) 2008, 2009 Free Software Foundation, Inc.

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

/* This implementation of ERFC_SCALED is based on the netlib algorithm
   available at http://www.netlib.org/specfun/erf  */

#ifdef HAVE_GFC_REAL_4
#undef KIND
#define KIND 4
#include "erfc_scaled_inc.c"
#endif

#ifdef HAVE_GFC_REAL_8
#undef KIND
#define KIND 8
#include "erfc_scaled_inc.c"
#endif

#ifdef HAVE_GFC_REAL_10
#undef KIND
#define KIND 10
#include "erfc_scaled_inc.c"
#endif

#ifdef HAVE_GFC_REAL_16
#undef KIND
#define KIND 16
#include "erfc_scaled_inc.c"
#endif
