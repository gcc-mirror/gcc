/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  Generic Unix version
   (also used for UWIN and VMS).
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

#include <stdio.h>
#include <errno.h>
#ifdef NEED_DECLARATION_ERRNO
extern int errno;
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef HAVE_WAITPID
#define waitpid(pid, status, flags) wait(status)
#endif

extern int execv ();
extern int execvp ();

int
pexecute (program, argv, this_pname, temp_base, errmsg_fmt, errmsg_arg, flags)
     const char *program;
     char * const *argv;
     const char *this_pname;
     const char *temp_base ATTRIBUTE_UNUSED;
     char **errmsg_fmt, **errmsg_arg;
     int flags;
{
  int (*func)() = (flags & PEXECUTE_SEARCH ? execvp : execv);
  int pid;
  int pdes[2];
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
      if (pipe (pdes) < 0)
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

  /* Fork a subprocess; wait and retry if it fails.  */
  sleep_interval = 1;
  pid = -1;
  for (retries = 0; retries < 4; retries++)
    {
      pid = fork ();
      if (pid >= 0)
	break;
      sleep (sleep_interval);
      sleep_interval *= 2;
    }

  switch (pid)
    {
    case -1:
      *errmsg_fmt = "fork";
      *errmsg_arg = NULL;
      return -1;

    case 0: /* child */
      /* Move the input and output pipes into place, if necessary.  */
      if (input_desc != STDIN_FILE_NO)
	{
	  close (STDIN_FILE_NO);
	  dup (input_desc);
	  close (input_desc);
	}
      if (output_desc != STDOUT_FILE_NO)
	{
	  close (STDOUT_FILE_NO);
	  dup (output_desc);
	  close (output_desc);
	}

      /* Close the parent's descs that aren't wanted here.  */
      if (last_pipe_input != STDIN_FILE_NO)
	close (last_pipe_input);

      /* Exec the program.  */
      (*func) (program, argv);

      fprintf (stderr, "%s: ", this_pname);
      fprintf (stderr, install_error_msg, program);
      fprintf (stderr, ": %s\n", xstrerror (errno));
      exit (-1);
      /* NOTREACHED */
      return 0;

    default:
      /* In the parent, after forking.
	 Close the descriptors that we made for this child.  */
      if (input_desc != STDIN_FILE_NO)
	close (input_desc);
      if (output_desc != STDOUT_FILE_NO)
	close (output_desc);

      /* Return child's process number.  */
      return pid;
    }
}

int
pwait (pid, status, flags)
     int pid;
     int *status;
     int flags ATTRIBUTE_UNUSED;
{
  /* ??? Here's an opportunity to canonicalize the values in STATUS.
     Needed?  */
  pid = waitpid (pid, status, 0);
  return pid;
}
