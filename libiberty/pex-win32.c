/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  Generic Win32 specialization.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif
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

/* mingw32 headers may not define the following.  */

#ifndef _P_WAIT
#  define _P_WAIT	0
#  define _P_NOWAIT	1
#  define _P_OVERLAY	2
#  define _P_NOWAITO	3
#  define _P_DETACH	4

#  define WAIT_CHILD		0
#  define WAIT_GRANDCHILD	1
#endif

/* This is a kludge to get around the Microsoft C spawn functions' propensity
   to remove the outermost set of double quotes from all arguments.  */

static const char * const *
fix_argv (argvec)
     char **argvec;
{
  int i;
  char * command0 = argvec[0];

  /* Ensure that the executable pathname uses Win32 backslashes.  */
  for (; *command0 != '\0'; command0++)
    if (*command0 == '/')
      *command0 = '\\';
 
  for (i = 1; argvec[i] != 0; i++)
    {
      int len, j;
      char *temp, *newtemp;

      temp = argvec[i];
      len = strlen (temp);
      for (j = 0; j < len; j++)
        {
          if (temp[j] == '"')
            {
              newtemp = xmalloc (len + 2);
              strncpy (newtemp, temp, j);
              newtemp [j] = '\\';
              strncpy (&newtemp [j+1], &temp [j], len-j);
              newtemp [len+1] = 0;
              temp = newtemp;
              len++;
              j++;
            }
        }

        argvec[i] = temp;
      }

  for (i = 0; argvec[i] != 0; i++)
    {
      if (strpbrk (argvec[i], " \t"))
        {
	  int len, trailing_backslash;
	  char *temp;

	  len = strlen (argvec[i]);
	  trailing_backslash = 0;

	  /* There is an added complication when an arg with embedded white
	     space ends in a backslash (such as in the case of -iprefix arg
	     passed to cpp). The resulting quoted strings gets misinterpreted
	     by the command interpreter -- it thinks that the ending quote
	     is escaped by the trailing backslash and things get confused. 
	     We handle this case by escaping the trailing backslash, provided
	     it was not escaped in the first place.  */
	  if (len > 1 
	      && argvec[i][len-1] == '\\' 
	      && argvec[i][len-2] != '\\')
	    {
	      trailing_backslash = 1;
	      ++len;			/* to escape the final backslash. */
	    }

	  len += 2;			/* and for the enclosing quotes. */

	  temp = xmalloc (len + 1);
	  temp[0] = '"';
	  strcpy (temp + 1, argvec[i]);
	  if (trailing_backslash)
	    temp[len-2] = '\\';
	  temp[len-1] = '"';
	  temp[len] = '\0';

	  argvec[i] = temp;
	}
    }

  return (const char * const *) argvec;
}

/* Win32 supports pipes */
int
pexecute (program, argv, this_pname, temp_base, errmsg_fmt, errmsg_arg, flags)
     const char *program;
     char * const *argv;
     const char *this_pname ATTRIBUTE_UNUSED;
     const char *temp_base ATTRIBUTE_UNUSED;
     char **errmsg_fmt, **errmsg_arg;
     int flags;
{
  int pid;
  int pdes[2];
  int org_stdin = -1;
  int org_stdout = -1;
  int input_desc, output_desc;

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
    (_P_NOWAIT, program, fix_argv(argv));

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
      *errmsg_arg = (char*) program;
      return -1;
    }

  return pid;
}

/* MS CRTDLL doesn't return enough information in status to decide if the
   child exited due to a signal or not, rather it simply returns an
   integer with the exit code of the child; eg., if the child exited with 
   an abort() call and didn't have a handler for SIGABRT, it simply returns
   with status = 3. We fix the status code to conform to the usual WIF*
   macros. Note that WIFSIGNALED will never be true under CRTDLL. */

int
pwait (pid, status, flags)
     int pid;
     int *status;
     int flags ATTRIBUTE_UNUSED;
{
  int termstat;

  pid = _cwait (&termstat, pid, WAIT_CHILD);

  /* ??? Here's an opportunity to canonicalize the values in STATUS.
     Needed?  */

  /* cwait returns the child process exit code in termstat.
     A value of 3 indicates that the child caught a signal, but not
     which one.  Since only SIGABRT, SIGFPE and SIGINT do anything, we
     report SIGABRT.  */
  if (termstat == 3)
    *status = SIGABRT;
  else
    *status = (((termstat) & 0xff) << 8);

  return pid;
}
