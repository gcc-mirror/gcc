/* Copyright (C) 1997, 2001 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include <errno.h>		/* for ENOSYS */
#include "f2c.h"

#ifndef RETSIGTYPE
/* we shouldn't rely on this... */
#ifdef KR_headers
#define RETSIGTYPE int
#else
#define RETSIGTYPE void
#endif
#endif
typedef RETSIGTYPE (*sig_type)();

#ifdef KR_headers
extern sig_type signal();
#else
#include <signal.h>
typedef int (*sig_proc)(int);
#endif

#ifndef SIG_ERR
#define SIG_ERR ((sig_type) -1)
#endif

#ifdef KR_headers
integer G77_alarm_0 (seconds, proc)
     integer *seconds;
     sig_type proc;
#else
integer G77_alarm_0 (integer *seconds, sig_proc proc)
#endif
{
  int status;
#if defined (HAVE_ALARM) && defined (SIGALRM)
  if (signal(SIGALRM, (sig_type)proc) == SIG_ERR)
    status = -1;
  else
    status = alarm (*seconds);
#else /* ! HAVE_ALARM || ! SIGALRM */
  errno = ENOSYS;
  status = -1;
#endif
  return status;
}
