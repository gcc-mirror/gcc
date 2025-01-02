/* Implementation of the GETLOG g77 intrinsic.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

#include <string.h>

#ifdef HAVE_UNISTD_H
#  if defined __MINGW32__ && defined  HAVE_GETLOGIN
#    define _POSIX 1
#  endif
#include <unistd.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
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

void PREFIX(getlog) (char *, gfc_charlen_type);
export_proto_np(PREFIX(getlog));

void
PREFIX(getlog) (char * login, gfc_charlen_type login_len)
{
  char *p;

  memset (login, ' ', login_len); /* Blank the string.  */

#if defined(HAVE_POSIX_GETPWUID_R) && defined(HAVE_GETEUID)
  struct passwd pwd;
  struct passwd *result;
  char *buf;
  int err;
  /* To be pedantic, buflen should be determined by
     sysconf(_SC_GETPW_R_SIZE_MAX), which is 1024 on some tested
     targets; we do something simple in case the target doesn't
     support sysconf.  */
  static const size_t buflen = 1024;
  buf = xmalloc (buflen);
  err = getpwuid_r (geteuid (), &pwd, buf, buflen, &result);
  if (err != 0 || result == NULL)
    goto cleanup;
  p = pwd.pw_name;
#elif defined(HAVE_GETPWUID) && defined(HAVE_GETEUID)
  {
    struct passwd *pw = getpwuid (geteuid ());
    if (pw)
      p = pw->pw_name;
    else
      return;
  }
#elif HAVE_GETLOGIN
  p = getlogin();
# else
  return;
#endif

  if (p == NULL)
    goto cleanup;

  gfc_charlen_type p_len = strlen (p);
  if (login_len < p_len)
    p_len = login_len;
  memcpy (login, p, p_len);

 cleanup:
#if defined (HAVE_POSIX_GETPWUID_R) && defined(HAVE_GETEUID)
  free (buf);
#else
  ;
#endif
}
