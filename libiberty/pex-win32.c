/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  Generic Win32 specialization.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2005
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
write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "pex-common.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
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
#include <sys/stat.h>

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
fix_argv (char * const *argvec)
{
  char **argv;
  int i;
  char *command0;

  /* See whether we need to change anything.  */
  for (command0 = argvec[0]; *command0 != '\0'; command0++)
    if (*command0 == '/')
      break;
  if (*command0 == '\0')
    {
      for (i = 1; argvec[i] != NULL; i++)
	if (strpbrk (argvec[i], "\" \t") != NULL)
	  break;

      if (argvec[i] == NULL)
	return (const char * const *) argvec;
    }

  for (i = 0; argvec[i] != NULL; i++)
    ;
  argv = XNEWVEC (char *, i + 1);
  for (i = 0; argvec[i] != NULL; i++)
    argv[i] = xstrdup (argvec[i]);
  argv[i] = NULL;

  /* Ensure that the executable pathname uses Win32 backslashes. This
     is not necessary on NT, but on W9x, forward slashes causes
     failure of spawn* and exec* functions (and probably any function
     that calls CreateProcess) *iff* the executable pathname (argv[0])
     is a quoted string.  And quoting is necessary in case a pathname
     contains embedded white space.  You can't win.  */
  for (command0 = argv[0]; *command0 != '\0'; command0++)
    if (*command0 == '/')
      *command0 = '\\';

  for (i = 1; argv[i] != 0; i++)
    {
      int len, j;
      char *temp, *newtemp;

      temp = argv[i];
      len = strlen (temp);
      for (j = 0; j < len; j++)
        {
          if (temp[j] == '"')
            {
              newtemp = XNEWVEC (char, len + 2);
              strncpy (newtemp, temp, j);
              newtemp [j] = '\\';
              strncpy (&newtemp [j+1], &temp [j], len-j);
              newtemp [len+1] = 0;
              temp = newtemp;
              len++;
              j++;
            }
        }

      if (argv[i] != temp)
	{
	  free (argv[i]);
	  argv[i] = temp;
	}
    }

  for (i = 0; argv[i] != 0; i++)
    {
      if (strpbrk (argv[i], " \t"))
        {
	  int len, trailing_backslash;
	  char *temp;

	  len = strlen (argv[i]);
	  trailing_backslash = 0;

	  /* There is an added complication when an arg with embedded white
	     space ends in a backslash (such as in the case of -iprefix arg
	     passed to cpp). The resulting quoted strings gets misinterpreted
	     by the command interpreter -- it thinks that the ending quote
	     is escaped by the trailing backslash and things get confused. 
	     We handle this case by escaping the trailing backslash, provided
	     it was not escaped in the first place.  */
	  if (len > 1 
	      && argv[i][len-1] == '\\' 
	      && argv[i][len-2] != '\\')
	    {
	      trailing_backslash = 1;
	      ++len;			/* to escape the final backslash. */
	    }

	  len += 2;			/* and for the enclosing quotes. */

	  temp = XNEWVEC (char, len + 1);
	  temp[0] = '"';
	  strcpy (temp + 1, argv[i]);
	  if (trailing_backslash)
	    temp[len - 2] = '\\';
	  temp[len - 1] = '"';
	  temp[len] = '\0';

	  free (argv[i]);
	  argv[i] = temp;
	}
    }

  return (const char * const *) argv;
}

static int pex_win32_open_read (struct pex_obj *, const char *, int);
static int pex_win32_open_write (struct pex_obj *, const char *, int);
static long pex_win32_exec_child (struct pex_obj *, int, const char *,
				  char * const *, int, int, int,
				  const char **, int *);
static int pex_win32_close (struct pex_obj *, int);
static int pex_win32_wait (struct pex_obj *, long, int *,
			   struct pex_time *, int, const char **, int *);
static int pex_win32_pipe (struct pex_obj *, int *, int);
static FILE *pex_win32_fdopenr (struct pex_obj *, int, int);

/* The list of functions we pass to the common routines.  */

const struct pex_funcs funcs =
{
  pex_win32_open_read,
  pex_win32_open_write,
  pex_win32_exec_child,
  pex_win32_close,
  pex_win32_wait,
  pex_win32_pipe,
  pex_win32_fdopenr,
  NULL /* cleanup */
};

/* Return a newly initialized pex_obj structure.  */

struct pex_obj *
pex_init (int flags, const char *pname, const char *tempbase)
{
  return pex_init_common (flags, pname, tempbase, &funcs);
}

/* Open a file for reading.  */

static int
pex_win32_open_read (struct pex_obj *obj ATTRIBUTE_UNUSED, const char *name,
		     int binary)
{
  return _open (name, _O_RDONLY | (binary ? _O_BINARY : _O_TEXT));
}

/* Open a file for writing.  */

static int
pex_win32_open_write (struct pex_obj *obj ATTRIBUTE_UNUSED, const char *name,
		      int binary)
{
  /* Note that we can't use O_EXCL here because gcc may have already
     created the temporary file via make_temp_file.  */
  return _open (name,
		(_O_WRONLY | _O_CREAT | _O_TRUNC
		 | (binary ? _O_BINARY : _O_TEXT)),
		_S_IREAD | _S_IWRITE);
}

/* Close a file.  */

static int
pex_win32_close (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd)
{
  return _close (fd);
}

/* Execute a child.  */

static long
pex_win32_exec_child (struct pex_obj *obj ATTRIBUTE_UNUSED, int flags,
		      const char *executable, char * const * argv,
		      int in, int out, int errdes, const char **errmsg,
		      int *err)
{
  int org_in, org_out, org_errdes;
  long pid;

  org_in = -1;
  org_out = -1;
  org_errdes = -1;

  if (in != STDIN_FILE_NO)
    {
      org_in = _dup (STDIN_FILE_NO);
      if (org_in < 0)
	{
	  *err = errno;
	  *errmsg = "_dup";
	  return -1;
	}
      if (_dup2 (in, STDIN_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (in) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (out != STDOUT_FILE_NO)
    {
      org_out = _dup (STDOUT_FILE_NO);
      if (org_out < 0)
	{
	  *err = errno;
	  *errmsg = "_dup";
	  return -1;
	}
      if (_dup2 (out, STDOUT_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (out) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (errdes != STDERR_FILE_NO
      || (flags & PEX_STDERR_TO_STDOUT) != 0)
    {
      org_errdes = _dup (STDERR_FILE_NO);
      if (org_errdes < 0)
	{
	  *err = errno;
	  *errmsg = "_dup";
	  return -1;
	}
      if (_dup2 ((flags & PEX_STDERR_TO_STDOUT) != 0 ? STDOUT_FILE_NO : errdes,
		 STDERR_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (errdes != STDERR_FILE_NO)
	{
	  if (_close (errdes) < 0)
	    {
	      *err = errno;
	      *errmsg = "_close";
	      return -1;
	    }
	}
    }

  pid = (((flags & PEX_SEARCH) != 0 ? _spawnvp : _spawnv)
	 (_P_NOWAIT, executable, fix_argv (argv)));

  if (pid == -1)
    {
      *err = errno;
      *errmsg = ((flags & PEX_SEARCH) != 0) ? "_spawnvp" : "_spawnv";
    }

  if (in != STDIN_FILE_NO)
    {
      if (_dup2 (org_in, STDIN_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (org_in) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (out != STDOUT_FILE_NO)
    {
      if (_dup2 (org_out, STDOUT_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (org_out) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (errdes != STDERR_FILE_NO
      || (flags & PEX_STDERR_TO_STDOUT) != 0)
    {
      if (_dup2 (org_errdes, STDERR_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (org_errdes) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  return pid;
}

/* Wait for a child process to complete.  MS CRTDLL doesn't return
   enough information in status to decide if the child exited due to a
   signal or not, rather it simply returns an integer with the exit
   code of the child; eg., if the child exited with an abort() call
   and didn't have a handler for SIGABRT, it simply returns with
   status == 3.  We fix the status code to conform to the usual WIF*
   macros.  Note that WIFSIGNALED will never be true under CRTDLL. */

static int
pex_win32_wait (struct pex_obj *obj ATTRIBUTE_UNUSED, long pid,
		int *status, struct pex_time *time, int done ATTRIBUTE_UNUSED,
		const char **errmsg, int *err)
{
  int termstat;

  if (time != NULL)
    memset (time, 0, sizeof *time);

  /* FIXME: If done is non-zero, we should probably try to kill the
     process.  */

  if (_cwait (&termstat, pid, WAIT_CHILD) < 0)
    {
      *err = errno;
      *errmsg = "_cwait";
      return -1;
    }

  /* cwait returns the child process exit code in termstat.  A value
     of 3 indicates that the child caught a signal, but not which one.
     Since only SIGABRT, SIGFPE and SIGINT do anything, we report
     SIGABRT.  */

  if (termstat == 3)
    *status = SIGABRT;
  else
    *status = ((termstat & 0xff) << 8);

  return 0;
}

/* Create a pipe.  */

static int
pex_win32_pipe (struct pex_obj *obj ATTRIBUTE_UNUSED, int *p,
		int binary)
{
  return _pipe (p, 256, binary ? _O_BINARY : _O_TEXT);
}

/* Get a FILE pointer to read from a file descriptor.  */

static FILE *
pex_win32_fdopenr (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd,
		   int binary)
{
  return fdopen (fd, binary ? "rb" : "r");
}
