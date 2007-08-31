/* Implementation of the CHMOD intrinsic.
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.
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
#ifdef  HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

/* INTEGER FUNCTION ACCESS(NAME, MODE)
   CHARACTER(len=*), INTENT(IN) :: NAME, MODE  */

#if defined(HAVE_FORK) && defined(HAVE_EXECL) && defined(HAVE_WAIT)

extern int chmod_func (char *, char *, gfc_charlen_type, gfc_charlen_type);
export_proto(chmod_func);

int
chmod_func (char *name, char *mode, gfc_charlen_type name_len,
	    gfc_charlen_type mode_len)
{
  char * file, * m;
  pid_t pid;
  int status;

  /* Trim trailing spaces.  */
  while (name_len > 0 && name[name_len - 1] == ' ')
    name_len--;
  while (mode_len > 0 && mode[mode_len - 1] == ' ')
    mode_len--;

  /* Make a null terminated copy of the strings.  */
  file = gfc_alloca (name_len + 1);
  memcpy (file, name, name_len);
  file[name_len] = '\0';

  m = gfc_alloca (mode_len + 1);
  memcpy (m, mode, mode_len);
  m[mode_len]= '\0';

  /* Execute /bin/chmod.  */
  if ((pid = fork()) < 0)
    return errno;
  if (pid == 0)
    {
      /* Child process.  */
      execl ("/bin/chmod", "chmod", m, file, (char *) NULL);
      return errno;
    }
  else
    wait (&status);

  if (WIFEXITED(status))
    return WEXITSTATUS(status);
  else
    return -1;
}



extern void chmod_i4_sub (char *, char *, GFC_INTEGER_4 *,
			  gfc_charlen_type, gfc_charlen_type);
export_proto(chmod_i4_sub);

void
chmod_i4_sub (char *name, char *mode, GFC_INTEGER_4 * status,
	      gfc_charlen_type name_len, gfc_charlen_type mode_len)
{
  int val;

  val = chmod_func (name, mode, name_len, mode_len);
  if (status)
    *status = val;
}


extern void chmod_i8_sub (char *, char *, GFC_INTEGER_8 *,
			  gfc_charlen_type, gfc_charlen_type);
export_proto(chmod_i8_sub);

void
chmod_i8_sub (char *name, char *mode, GFC_INTEGER_8 * status,
	      gfc_charlen_type name_len, gfc_charlen_type mode_len)
{
  int val;

  val = chmod_func (name, mode, name_len, mode_len);
  if (status)
    *status = val;
}

#endif
