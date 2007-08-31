/* Implementation of the SLEEP intrinsic.
   Copyright (C) 2005, 2007 Free Software Foundation, Inc.
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

#include "libgfortran.h"

#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __MINGW32__
# include <windows.h>
# undef sleep
# define sleep(x) Sleep(1000*(x))
# define HAVE_SLEEP 1
#endif

/* SUBROUTINE SLEEP(SECONDS)
   INTEGER, INTENT(IN) :: SECONDS
   
   A choice had to be made if SECONDS is negative. For g77, this is
   equivalent to SLEEP(0).  */

#ifdef HAVE_SLEEP
extern void sleep_i4_sub (GFC_INTEGER_4 *);
iexport_proto(sleep_i4_sub);

void
sleep_i4_sub (GFC_INTEGER_4 *seconds)
{
  sleep (*seconds < 0 ? 0 : (unsigned int) *seconds);
}
iexport(sleep_i4_sub);

extern void sleep_i8_sub (GFC_INTEGER_8 *);
iexport_proto(sleep_i8_sub);

void
sleep_i8_sub (GFC_INTEGER_8 *seconds)
{
  sleep (*seconds < 0 ? 0 : (unsigned int) *seconds);
}
iexport(sleep_i8_sub);
#endif
