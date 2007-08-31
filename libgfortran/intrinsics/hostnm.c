/* Implementation of the HOSTNM intrinsic.
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

#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif


/* Windows32 version */
#if defined __MINGW32__ && !defined  HAVE_GETHOSTNAME
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <errno.h>

static int
w32_gethostname (char *name, size_t len)
{
  /* We could try the WinSock API gethostname, but that will
     fail if WSAStartup function has has not been called.  We don't
    really need a name that will be understood by socket API, so avoid
    unnecessary dependence on WinSock libraries by using
    GetComputerName instead.  */

  /* On Win9x GetComputerName fails if the input size is less
     than MAX_COMPUTERNAME_LENGTH + 1.  */
  char buffer[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD size =  sizeof (buffer);

  if (!GetComputerName (buffer, &size))
    return -1;

  if ((size = strlen (buffer) + 1)  > len)
    {
      errno = EINVAL;
      /* Truncate as per POSIX spec.  We do not NUL-terminate. */
      size = len;
    }
  memcpy (name, buffer, (size_t) size);

  return 0;
}

#undef gethostname
#define gethostname w32_gethostname
#define  HAVE_GETHOSTNAME 1

#endif


/* SUBROUTINE HOSTNM(NAME, STATUS)
   CHARACTER(len=*), INTENT(OUT) :: NAME
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS  */

#ifdef HAVE_GETHOSTNAME
extern void hostnm_i4_sub (char *, GFC_INTEGER_4 *, gfc_charlen_type);
iexport_proto(hostnm_i4_sub);

void
hostnm_i4_sub (char *name, GFC_INTEGER_4 *status, gfc_charlen_type name_len)
{
  int val, i;
  char *p;

  memset (name, ' ', name_len);
  p = gfc_alloca (name_len + 1);

  val = gethostname (p, name_len);

  if (val == 0)
  {
    i = -1;
    while (i < name_len && p[++i] != '\0')
      name[i] = p[i];
  }

  if (status != NULL) 
    *status = (val == 0) ? 0 : errno;
}
iexport(hostnm_i4_sub);

extern void hostnm_i8_sub (char *, GFC_INTEGER_8 *, gfc_charlen_type);
iexport_proto(hostnm_i8_sub);

void
hostnm_i8_sub (char *name, GFC_INTEGER_8 *status, gfc_charlen_type name_len)
{
  int val, i;
  char *p;

  memset (name, ' ', name_len);
  p = gfc_alloca (name_len + 1);

  val = gethostname (p, name_len);

  if (val == 0)
  {
    i = -1;
    while (i < name_len && p[++i] != '\0')
      name[i] = p[i];
  }

  if (status != NULL) 
    *status = (val == 0) ? 0 : errno;
}
iexport(hostnm_i8_sub);

extern GFC_INTEGER_4 hostnm (char *, gfc_charlen_type);
export_proto(hostnm);

GFC_INTEGER_4
hostnm (char *name, gfc_charlen_type name_len)
{
  GFC_INTEGER_4 val;
  hostnm_i4_sub (name, &val, name_len);
  return val;
}
#endif
