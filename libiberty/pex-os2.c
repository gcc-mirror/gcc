/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  OS/2 specialization.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003
   Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "pex-common.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

/* ??? Does OS2 have process.h?  */
extern int spawnv ();
extern int spawnvp ();

int
pexecute (program, argv, this_pname, temp_base, errmsg_fmt, errmsg_arg, flags)
     const char *program;
     char * const *argv;
     const char *this_pname;
     const char *temp_base;
     char **errmsg_fmt, **errmsg_arg;
     int flags;
{
  int pid;

  if ((flags & PEXECUTE_ONE) != PEXECUTE_ONE)
    abort ();
  /* ??? Presumably 1 == _P_NOWAIT.  */
  pid = (flags & PEXECUTE_SEARCH ? spawnvp : spawnv) (1, program, argv);
  if (pid == -1)
    {
      *errmsg_fmt = install_error_msg;
      *errmsg_arg = program;
      return -1;
    }
  return pid;
}

int
pwait (pid, status, flags)
     int pid;
     int *status;
     int flags;
{
  /* ??? Here's an opportunity to canonicalize the values in STATUS.
     Needed?  */
  int pid = wait (status);
  return pid;
}
