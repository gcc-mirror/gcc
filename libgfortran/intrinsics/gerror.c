/* Implementation of the GERROR g77 intrinsic.
   Copyright (C) 2005, 2007 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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
