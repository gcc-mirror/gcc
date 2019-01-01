/* Copyright (C) 2005-2019 Free Software Foundation, Inc.
   Contributed by Janne Blomqvist

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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


/* This file contains utility functions for determining the size of a
   variable given its kind. */

#include "io.h"

size_t
size_from_real_kind (int kind)
{
  switch (kind)
    {
#ifdef HAVE_GFC_REAL_4
    case 4:
      return sizeof (GFC_REAL_4);
#endif
#ifdef HAVE_GFC_REAL_8
    case 8:
      return sizeof (GFC_REAL_8);
#endif
#ifdef HAVE_GFC_REAL_10
    case 10:
      return sizeof (GFC_REAL_10);
#endif
#ifdef HAVE_GFC_REAL_16
    case 16:
      return sizeof (GFC_REAL_16);
#endif
    default:
      return kind;
    }
}


size_t
size_from_complex_kind (int kind)
{
  switch (kind)
    {
#ifdef HAVE_GFC_COMPLEX_4
    case 4:
      return sizeof (GFC_COMPLEX_4);
#endif
#ifdef HAVE_GFC_COMPLEX_8
    case 8:
      return sizeof (GFC_COMPLEX_8);
#endif
#ifdef HAVE_GFC_COMPLEX_10
    case 10:
      return sizeof (GFC_COMPLEX_10);
#endif
#ifdef HAVE_GFC_COMPLEX_16
    case 16:
      return sizeof (GFC_COMPLEX_16);
#endif
    default:
      return 2 * kind;
    }
}

