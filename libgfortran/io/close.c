/* Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

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

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "libgfortran.h"
#include "io.h"
#include <limits.h>

typedef enum
{ CLOSE_DELETE, CLOSE_KEEP, CLOSE_UNSPECIFIED }
close_status;

static st_option status_opt[] = {
  {"keep", CLOSE_KEEP},
  {"delete", CLOSE_DELETE},
  {NULL, 0}
};


extern void st_close (void);
export_proto(st_close);

void
st_close (void)
{
  close_status status;
  gfc_unit *u;
#if !HAVE_UNLINK_OPEN_FILE
  char * path;

  path = NULL;
#endif

  library_start ();

  status = (ioparm.status == NULL) ? CLOSE_UNSPECIFIED :
    find_option (ioparm.status, ioparm.status_len, status_opt,
		 "Bad STATUS parameter in CLOSE statement");

  if (ioparm.library_return != LIBRARY_OK)
  {
    library_end ();
    return;
  }

  u = find_unit (ioparm.unit);
  if (u != NULL)
    {
      if (u->flags.status == STATUS_SCRATCH)
	{
	  if (status == CLOSE_KEEP)
	    generate_error (ERROR_BAD_OPTION,
			    "Can't KEEP a scratch file on CLOSE");
#if !HAVE_UNLINK_OPEN_FILE
	  path = (char *) gfc_alloca (u->file_len + 1);
          unpack_filename (path, u->file, u->file_len);
#endif
	}
      else
	{
	  if (status == CLOSE_DELETE)
            {
#if HAVE_UNLINK_OPEN_FILE
	      delete_file (u);
#else
	      path = (char *) gfc_alloca (u->file_len + 1);
              unpack_filename (path, u->file, u->file_len);
#endif
            }
	}

      close_unit (u);

#if !HAVE_UNLINK_OPEN_FILE
      if (path != NULL)
        unlink (path);
#endif
    }

  library_end ();
}
