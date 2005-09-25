/* Implementation of the GETLOG g77 intrinsic.
   Copyright (C) 2005 Free Software Foundation, Inc.
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

#include "config.h"
#include "libgfortran.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/* Windows32 version */
#if defined __MINGW32__ && !defined  HAVE_GETLOGIN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <lmcons.h>  /* for UNLEN */ 

static char *
w32_getlogin (void)
{
  static char name [UNLEN + 1];
  DWORD namelen = sizeof (name);

  GetUserName (name, &namelen);
  return (name[0] == 0 ?  NULL : name);
}

#undef getlogin
#define getlogin w32_getlogin
#define HAVE_GETLOGIN 1

#endif


/* GETLOG (LOGIN), g77 intrinsic for retrieving the login name for the
   process.
   CHARACTER(len=*), INTENT(OUT) :: LOGIN  */

#ifdef HAVE_GETLOGIN
void PREFIX(getlog) (char *, gfc_charlen_type);
export_proto_np(PREFIX(getlog));

void
PREFIX(getlog) (char * login, gfc_charlen_type login_len)
{
  int p_len;
  char *p;

  memset (login, ' ', login_len); /* Blank the string.  */

  p = getlogin ();
  if (p == NULL)
    return;

  p_len = strlen (p);
  if (login_len < p_len)
    memcpy (login, p, login_len);
  else
    memcpy (login, p, p_len);
}
#endif
