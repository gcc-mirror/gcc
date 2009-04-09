/* Implementation of the TIME and TIME8 g77 intrinsics.
   Copyright (C) 2005, 2007, 2009 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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

#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    ifdef HAVE_TIME_H
#      include <time.h>
#    endif
#  endif
#endif


/* INTEGER(KIND=4) FUNCTION TIME()  */

#ifdef HAVE_TIME
extern GFC_INTEGER_4 time_func (void);
export_proto(time_func);

GFC_INTEGER_4
time_func (void)
{
  return (GFC_INTEGER_4) time (NULL);
}

/* INTEGER(KIND=8) FUNCTION TIME8()  */

extern GFC_INTEGER_8 time8_func (void);
export_proto(time8_func);

GFC_INTEGER_8
time8_func (void)
{
  return (GFC_INTEGER_8) time (NULL);
}
#endif
