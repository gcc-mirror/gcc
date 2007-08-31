/* Copyright (C) 2005, 2007 Free Software Foundation, Inc.
   Contributed by Janne Blomqvist

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

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

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


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

