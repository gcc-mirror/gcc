
/* Copyright (C) 2002-2003 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "libgfortran.h"
#include "io.h"

/* rewind.c--  Implement the rewind statement */

void
st_rewind (void)
{
  gfc_unit *u;

  library_start ();

  u = find_unit (ioparm.unit);
  if (u != NULL)
    {
      if (u->flags.access != ACCESS_SEQUENTIAL)
	generate_error (ERROR_BAD_OPTION,
			"Cannot REWIND a file opened for DIRECT access");
      else
	{
	  /* If we have been writing to the file, the last written record
	     is the last record in the file, so truncate the file now.
	     Reset to read mode so two consecutive rewind statements
	     don't delete the file contents.  */
          if (u->mode==WRITING)
            struncate(u->s);
	  u->mode = READING;
	  u->last_record = 0;
	  if (sseek (u->s, 0) == FAILURE)
	    generate_error (ERROR_OS, NULL);

	  u->endfile = NO_ENDFILE;
	  u->current_record = 0;
	  test_endfile (u);
	}
    }

  library_end ();
}
