/* Implementation of the PAUSE statement.
   Copyright (C) 2002-2013 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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
#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


static void
do_pause (void)
{
  char buff[4];
  estr_write ("To resume execution, type go.  "
	      "Other input will terminate the job.\n");

  fgets(buff, 4, stdin);
  if (strncmp(buff, "go\n", 3) != 0)
    stop_string ('\0', 0);
  estr_write ("RESUMED\n");
}

/* A numeric PAUSE statement.  */

extern void pause_numeric (GFC_INTEGER_4);
export_proto(pause_numeric);

void
pause_numeric (GFC_INTEGER_4 code)
{
  st_printf ("PAUSE %d\n", (int) code);
  do_pause ();
}

/* A character string or blank PAUSE statement.  */

extern void pause_string (char *string, GFC_INTEGER_4 len);
export_proto(pause_string);

void
pause_string (char *string, GFC_INTEGER_4 len)
{
  estr_write ("PAUSE ");
  ssize_t w = write (STDERR_FILENO, string, len);
  (void) sizeof (w); /* Avoid compiler warning about not using write
			return val.  */
  estr_write ("\n");

  do_pause ();
}
