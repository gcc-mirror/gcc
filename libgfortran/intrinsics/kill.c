/* Implementation of the KILL g77 intrinsic.
   Copyright (C) 2005, 2007, 2009, 2011 Free Software Foundation, Inc.
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
#include <errno.h>
#include <signal.h>


/* SUBROUTINE KILL(PID, SIGNAL, STATUS)
   INTEGER, INTENT(IN) :: PID, SIGNAL
   INTEGER(KIND=1), INTENT(OUT), OPTIONAL :: STATUS

   INTEGER(KIND=1) FUNCTION KILL(PID, SIGNAL)
   INTEGER, INTENT(IN) :: PID, SIGNAL */

#ifdef HAVE_KILL
extern void kill_i4_sub (GFC_INTEGER_4 *, GFC_INTEGER_4 *, GFC_INTEGER_4 *);
iexport_proto(kill_i4_sub);

void
kill_i4_sub (GFC_INTEGER_4 *pid, GFC_INTEGER_4 *signal,
	     GFC_INTEGER_4 *status)
{
  int val;

  val = kill (*pid, *signal);

  if (status != NULL) 
    *status = (val == 0) ? 0 : errno;
}
iexport(kill_i4_sub);

extern void kill_i8_sub (GFC_INTEGER_8 *, GFC_INTEGER_8 *, GFC_INTEGER_8 *);
iexport_proto(kill_i8_sub);

void
kill_i8_sub (GFC_INTEGER_8 *pid, GFC_INTEGER_8 *signal,
	     GFC_INTEGER_8 *status)
{
  int val;

  val = kill (*pid, *signal);

  if (status != NULL) 
    *status = (val == 0) ? 0 : errno;
}
iexport(kill_i8_sub);

extern GFC_INTEGER_4 kill_i4 (GFC_INTEGER_4 *, GFC_INTEGER_4 *);
export_proto(kill_i4);

GFC_INTEGER_4
kill_i4 (GFC_INTEGER_4 *pid, GFC_INTEGER_4 *signal)
{
  GFC_INTEGER_4 val;
  kill_i4_sub (pid, signal, &val);
  return val;
}

extern GFC_INTEGER_8 kill_i8 (GFC_INTEGER_8 *, GFC_INTEGER_8 *);
export_proto(kill_i8);

GFC_INTEGER_8
kill_i8 (GFC_INTEGER_8 *pid, GFC_INTEGER_8 *signal)
{
  GFC_INTEGER_8 val;
  kill_i8_sub (pid, signal, &val);
  return val;
}
#endif
