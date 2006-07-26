/* Implementation of the MCLOCK and MCLOCK8 g77 intrinsics.
   Copyright (C) 2006 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

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

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
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


/* INTEGER(KIND=4) FUNCTION MCLOCK()  */

extern GFC_INTEGER_4 mclock (void);
export_proto(mclock);

GFC_INTEGER_4
mclock (void)
{
#ifdef HAVE_CLOCK
  return (GFC_INTEGER_4) clock ();
#else
  return (GFC_INTEGER_4) -1;
#endif
}


/* INTEGER(KIND=8) FUNCTION MCLOCK8()  */

extern GFC_INTEGER_8 mclock8 (void);
export_proto(mclock8);

GFC_INTEGER_8
mclock8 (void)
{
#ifdef HAVE_CLOCK
  return (GFC_INTEGER_8) clock ();
#else
  return (GFC_INTEGER_8) -1;
#endif
}

