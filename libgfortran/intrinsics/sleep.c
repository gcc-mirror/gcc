/* Implementation of the SLEEP intrinsic.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __MINGW32__
# define WIN32_LEAN_AND_MEAN
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
