/* Implementation of the GERROR g77 intrinsic.
   Copyright (C) 2005, 2007, 2009 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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

#include <errno.h>
#include <string.h>


/* GERROR (MESSAGE), g77 intrinsic for retrieving the system error
   message corresponding to the last system error (C errno).
   CHARACTER(len=*), INTENT(OUT) :: MESSAGE  */

#ifdef HAVE_STRERROR
void PREFIX(gerror) (char *, gfc_charlen_type);
export_proto_np(PREFIX(gerror));

void 
PREFIX(gerror) (char * msg, gfc_charlen_type msg_len)
{
  int p_len;
  char *p;

  memset (msg, ' ', msg_len); /* Blank the string.  */

  p = strerror (errno);
  if (p == NULL)
    return;

  p_len = strlen (p);
  if (msg_len < p_len)
    memcpy (msg, p, msg_len);
  else
    memcpy (msg, p, p_len);
}
#endif
