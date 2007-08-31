/* Thread/recursion locking
   Copyright 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org> and Andy Vaught

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

#include "io.h"
#include <string.h>

/* library_start()-- Called with a library call is entered.  */

void
library_start (st_parameter_common *cmp)
{
  if ((cmp->flags & IOPARM_LIBRETURN_ERROR) != 0)
    return;

  cmp->flags &= ~IOPARM_LIBRETURN_MASK;
}


void
free_ionml (st_parameter_dt *dtp)
{
  namelist_info * t1, *t2;

  /* Delete the namelist, if it exists.  */

  if (dtp->u.p.ionml != NULL)
    {
      t1 = dtp->u.p.ionml;
      while (t1 != NULL)
	{
	  t2 = t1;
	  t1 = t1->next;
	  free_mem (t2->var_name);
	  if (t2->var_rank)
	    {
	     free_mem (t2->dim);
	     free_mem (t2->ls);
	    }
	  free_mem (t2);
	}
    }
  dtp->u.p.ionml = NULL;
}
