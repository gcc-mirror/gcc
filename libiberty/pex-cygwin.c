/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  Cygwin specialization.
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
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <process.h>
#include <io.h>
#include <fcntl.h>
#include <signal.h>

extern int _spawnv ();
extern int _spawnvp ();

/* Win32 supports pipes, and Cygwin provides waitpid.  */

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
  int pdes[2], org_stdin, org_stdout;
  int input_desc, output_desc;
  int retries, sleep_interval;

  /* Pipe waiting from last process, to be used as input for the next one.
     Value is STDIN_FILE_NO if no pipe is waiting
     (i.e. the next command is the first of a group).  */
  static int last_pipe_input;

  /* If this is the first process, initialize.  */
  if (flags & PEXECUTE_FIRST)
    last_pipe_input = STDIN_FILE_NO;

  input_desc = last_pipe_input;

  /* If this isn't the last process, make a pipe for its output,
     and record it as waiting to be the input to the next process.  */
  if (! (flags & PEXECUTE_LAST))
    {
      if (_pipe (pdes, 256, O_BINARY) < 0)
	{
	  *errmsg_fmt = "pipe";
	  *errmsg_arg = NULL;
	  return -1;
	}
      output_desc = pdes[WRITE_PORT];
      last_pipe_input = pdes[READ_PORT];
    }
  else
    {
      /* Last process.  */
      output_desc = STDOUT_FILE_NO;
      last_pipe_input = STDIN_FILE_NO;
    }

  if (input_desc != STDIN_FILE_NO)
    {
      org_stdin = dup (STDIN_FILE_NO);
      dup2 (input_desc, STDIN_FILE_NO);
      close (input_desc); 
    }

  if (output_desc != STDOUT_FILE_NO)
    {
      org_stdout = dup (STDOUT_FILE_NO);
      dup2 (output_desc, STDOUT_FILE_NO);
      close (output_desc);
    }

  pid = (flags & PEXECUTE_SEARCH ? _spawnvp : _spawnv)
    (_P_NOWAIT, program, argv);

  if (input_desc != STDIN_FILE_NO)
    {
      dup2 (org_stdin, STDIN_FILE_NO);
      close (org_stdin);
    }

  if (output_desc != STDOUT_FILE_NO)
    {
      dup2 (org_stdout, STDOUT_FILE_NO);
      close (org_stdout);
    }

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
     int flags ATTRIBUTE_UNUSED;
{
  return waitpid (pid, status, 0);
}
