/* Thread/recursion locking
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org> and Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <string.h>
#include "libgfortran.h"
#include "io.h"

st_parameter ioparm;
namelist_info * ionml;
global_t g;


/* library_start()-- Called with a library call is entered.  */

void
library_start (void)
{

  if (g.in_library)
    internal_error ("Recursive library calls not allowed");

/* The in_library flag indicates whether we're currently processing a
 * library call.  Some calls leave immediately, but READ and WRITE
 * processing return control to the caller but are still considered to
 * stay within the library. */

  g.in_library = 1;

  if (ioparm.iostat != NULL && ioparm.library_return == LIBRARY_OK)
    *ioparm.iostat = ERROR_OK;

  ioparm.library_return = LIBRARY_OK;
}


/* library_end()-- Called when a library call is complete in order to
 * clean up for the next call. */

void
library_end (void)
{
  int t;
  namelist_info * t1, *t2;

  g.in_library = 0;
  filename = NULL;
  line = 0;

  t = ioparm.library_return;
  if (ionml != NULL)
    {
      t1 = ionml;
      while (t1 != NULL)
       {
         t2 = t1;
         t1 = t1->next;
         free_mem (t2);
       }
    }
  
  ionml = NULL;
  memset (&ioparm, '\0', sizeof (ioparm));
  ioparm.library_return = t;
}

