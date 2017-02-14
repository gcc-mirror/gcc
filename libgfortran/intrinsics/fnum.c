/* Implementation of the FNUM intrinsics.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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

/* FUNCTION FNUM(UNIT)
   INTEGER FNUM
   INTEGER, INTENT(IN), :: UNIT  */

extern GFC_INTEGER_4 fnum_i4 (GFC_INTEGER_4 *);
export_proto(fnum_i4);

GFC_INTEGER_4
fnum_i4 (GFC_INTEGER_4 *unit)
{
  return unit_to_fd (*unit);
}

extern GFC_INTEGER_8 fnum_i8 (GFC_INTEGER_8 *);
export_proto(fnum_i8);

GFC_INTEGER_8
fnum_i8 (GFC_INTEGER_8 * unit)
{
  return unit_to_fd (*unit);
}
