/* Implementation of the FNUM intrinsics.
   Copyright (C) 2004 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

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
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "libgfortran.h"

#include "../io/io.h"

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
