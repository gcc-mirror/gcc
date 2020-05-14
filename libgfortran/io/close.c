/* Copyright (C) 2002-2020 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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

#include "io.h"
#include "unix.h"
#include "async.h"
#include <limits.h>
#if !HAVE_UNLINK_OPEN_FILE
#include <string.h>
#endif

typedef enum
{ CLOSE_INVALID = - 1, CLOSE_DELETE, CLOSE_KEEP, CLOSE_UNSPECIFIED }
close_status;

static const st_option status_opt[] = {
  {"keep", CLOSE_KEEP},
  {"delete", CLOSE_DELETE},
  {NULL, 0}
};


extern void st_close (st_parameter_close *);
export_proto(st_close);

void
st_close (st_parameter_close *clp)
{
  close_status status;
  gfc_unit *u;
#if !HAVE_UNLINK_OPEN_FILE
  char *path;

  path = NULL;
#endif

  library_start (&clp->common);

  status = !(clp->common.flags & IOPARM_CLOSE_HAS_STATUS) ? CLOSE_UNSPECIFIED :
    find_option (&clp->common, clp->status, clp->status_len,
		 status_opt, "Bad STATUS parameter in CLOSE statement");

  if (status == CLOSE_INVALID)
    {
      library_end ();
      return;
    }

  u = find_unit (clp->common.unit);

  if (ASYNC_IO && u && u->au)
    if (async_wait (&(clp->common), u->au))
      {
	library_end ();
	return;
      }

  if ((clp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
  {
    library_end ();
    return;
  }

  if (u != NULL)
    {
      if (close_share (u) < 0)
	generate_error (&clp->common, LIBERROR_OS, "Problem in CLOSE");
      if (u->flags.status == STATUS_SCRATCH)
	{
	  if (status == CLOSE_KEEP)
	    generate_error (&clp->common, LIBERROR_BAD_OPTION,
			    "Can't KEEP a scratch file on CLOSE");
#if !HAVE_UNLINK_OPEN_FILE
	  path = strdup (u->filename);
#endif
	}
      else
	{
	  if (status == CLOSE_DELETE)
	    {
	      if (u->flags.readonly)
		generate_warning (&clp->common, "STATUS set to DELETE on CLOSE"
				  " but file protected by READONLY specifier");
	      else
		{
#if HAVE_UNLINK_OPEN_FILE

		  if (remove (u->filename))
		    generate_error (&clp->common, LIBERROR_OS,
				    "File cannot be deleted");
#else
		  path = strdup (u->filename);
#endif
		}
	    }
	}

      close_unit (u);

#if !HAVE_UNLINK_OPEN_FILE
      if (path != NULL)
	{
	  if (remove (path))
	    generate_error (&clp->common, LIBERROR_OS,
			    "File cannot be deleted");
	  free (path);
	}
#endif
    }

  /* CLOSE on unconnected unit is legal and a no-op: F95 std., 9.3.5. */ 
  library_end ();
}
