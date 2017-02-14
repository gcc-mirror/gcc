/* Implementation of the MCLOCK and MCLOCK8 g77 intrinsics.
   Copyright (C) 2006-2017 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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
#include <time.h>


/* INTEGER(KIND=4) FUNCTION MCLOCK()  */

extern GFC_INTEGER_4 mclock (void);
export_proto(mclock);

GFC_INTEGER_4
mclock (void)
{
  return (GFC_INTEGER_4) clock ();
}


/* INTEGER(KIND=8) FUNCTION MCLOCK8()  */

extern GFC_INTEGER_8 mclock8 (void);
export_proto(mclock8);

GFC_INTEGER_8
mclock8 (void)
{
  return (GFC_INTEGER_8) clock ();
}

