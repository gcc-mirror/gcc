/* Thread/recursion locking
   Copyright (C) 2002-2014 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org> and Andy Vaught

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

#include "io.h"
#include <string.h>
#include <stdlib.h>

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
	  free (t2->var_name);
	  if (t2->var_rank)
	    {
	     free (t2->dim);
	     free (t2->ls);
	    }
	  free (t2);
	}
    }
  dtp->u.p.ionml = NULL;
}
