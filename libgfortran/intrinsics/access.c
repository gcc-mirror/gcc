/* Implementation of the ACCESS intrinsic.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* INTEGER FUNCTION ACCESS(NAME, MODE)
   CHARACTER(len=*), INTENT(IN) :: NAME, MODE  */

#ifdef HAVE_ACCESS
extern int access_func (char *, char *, gfc_charlen_type, gfc_charlen_type);
export_proto(access_func);

int
access_func (char *name, char *mode, gfc_charlen_type name_len,
	     gfc_charlen_type mode_len)
{
  gfc_charlen_type i;
  int m;

  /* Parse the MODE string.  */
  m = F_OK;
  for (i = 0; i < mode_len && mode[i]; i++)
    switch (mode[i])
      {
	case ' ':
	  break;

	case 'r':
	case 'R':
	  m |= R_OK;
	  break;

	case 'w':
	case 'W':
	  m |= W_OK;
	  break;

	case 'x':
	case 'X':
	  m |= X_OK;
	  break;

	default:
	  return -1;
	  break;
      }

  char *path = fc_strdup (name, name_len);

  /* And make the call to access().  */
  int res = (access (path, m) == 0 ? 0 : errno);

  free (path);
  return res;
}
#endif
