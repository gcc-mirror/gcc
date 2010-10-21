/* Implementation of the STOP statement.
   Copyright 2002, 2005, 2007, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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
#include <string.h>

/* A numeric STOP statement.  */

extern void stop_numeric (GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(stop_numeric);

void
stop_numeric (GFC_INTEGER_4 code)
{
  if (code == -1)
    code = 0;
  else
    st_printf ("STOP %d\n", (int)code);

  sys_exit (code);
}


/* A Fortran 2008 numeric STOP statement.  */

extern void stop_numeric_f08 (GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(stop_numeric_f08);

void
stop_numeric_f08 (GFC_INTEGER_4 code)
{
  st_printf ("STOP %d\n", (int)code);
  sys_exit (code);
}


/* A character string or blank STOP statement.  */

void
stop_string (const char *string, GFC_INTEGER_4 len)
{
  if (string)
    {
      st_printf ("STOP ");
      while (len--)
	st_printf ("%c", *(string++));
      st_printf ("\n");
    }
  sys_exit (0);
}


/* Per Fortran 2008, section 8.4:  "Execution of a STOP statement initiates
   normal termination of execution. Execution of an ERROR STOP statement
   initiates error termination of execution."  Thus, error_stop_string returns
   a nonzero exit status code.  */

extern void error_stop_string (const char *, GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(error_stop_string);

void
error_stop_string (const char *string, GFC_INTEGER_4 len)
{
  st_printf ("ERROR STOP ");
  while (len--)
    st_printf ("%c", *(string++));
  st_printf ("\n");

  sys_exit (1);
}


/* A numeric ERROR STOP statement.  */

extern void error_stop_numeric (GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(error_stop_numeric);

void
error_stop_numeric (GFC_INTEGER_4 code)
{
  st_printf ("ERROR STOP %d\n", (int) code);
  sys_exit (code);
}
