/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.
   Copyright (C) 1996-2000 Free Software Foundation, Inc.

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

/* This file exports two functions: pexecute and pwait.  */

/* This file lives in at least two places: libiberty and gcc.
   Don't change one without the other.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#define ISSPACE (x) isspace(x)
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef vfork /* Autoconf may define this to fork for us. */
# define VFORK_STRING "fork"
#else
# define VFORK_STRING "vfork"
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
#ifdef VMS
#define vfork() (decc$$alloc_vfork_blocks() >= 0 ? \
               lib$get_current_invo_context(decc$$get_vfork_jmpbuf()) : -1)
#endif /* VMS */

#include "libiberty.h"

/* stdin file number.  */
#define STDIN_FILE_NO 0

/* stdout file number.  */
#define STDOUT_FILE_NO 1

/* value of `pipe': port index for reading.  */
#define READ_PORT 0

/* value of `pipe': port index for writing.  */
#define WRITE_PORT 1

static char *install_error_msg = "installation problem, cannot exec `%s'";

/* pexecute: execute a program.

   PROGRAM and ARGV are the arguments to execv/execvp.

   THIS_PNAME is name of the calling program (i.e. argv[0]).

   TEMP_BASE is the path name, sans suffix, of a temporary file to use
   if needed.  This is currently only needed for MSDOS ports that don't use
   GO32 (do any still exist?).  Ports that don't need it can pass NULL.

   (FLAGS & PEXECUTE_SEARCH) is non-zero if $PATH should be searched
   (??? It's not clear that GCC passes this flag correctly).
   (FLAGS & PEXECUTE_FIRST) is nonzero for the first process in chain.
   (FLAGS & PEXECUTE_FIRST) is nonzero for the last process in chain.
   FIRST_LAST could be simplified to only mark the last of a chain of processes
   but that requires the caller to always mark the last one (and not give up
   early if some error occurs).  It's more robust to require the caller to
   mark both ends of the chain.

   The result is the pid on systems like Unix where we fork/exec and on systems
   like WIN32 and OS2 where we use spawn.  It is up to the caller to wait for
   the child.

   The result is the WEXITSTATUS on systems like MSDOS where we spawn and wait
   for the child here.

   Upon failure, ERRMSG_FMT and ERRMSG_ARG are set to the text of the error
   message with an optional argument (if not needed, ERRMSG_ARG is set to
   NULL), and -1 is returned.  `errno' is available to the caller to use.

   pwait: cover function for wait.

   PID is the process id of the task to wait for.
   STATUS is the `status' argument to wait.
   FLAGS is currently unused (allows future enhancement without breaking
   upward compatibility).  Pass 0 for now.

   The result is the pid of the child reaped,
   or -1 for failure (errno says why).

   On systems that don't support waiting for a particular child, PID is
   ignored.  On systems like MSDOS that don't really multitask pwait
   is just a mechanism to provide a consistent interface for the caller.

   pfinish: finish generation of script

   pfinish is necessary for systems like MPW where a script is generated that
   runs the requested programs.
*/

#ifdef __MSDOS__

/* MSDOS doesn't multitask, but for the sake of a consistent interface
   the code behaves like it does.  pexecute runs the program, tucks the
   exit code away, and returns a "pid".  pwait must be called to fetch the
   exit code.  */

#include <process.h>

/* For communicating information from pexecute to pwait.  */
static int last_pid = 0;
static int last_status = 0;
static int last_reaped = 0;

int
pexecute (program, argv, this_pname, temp_base, errmsg_fmt, errmsg_arg, flags)
     const char *program;
     char * const *argv;
     const char *this_pname;
     const char *temp_base;
     char **errmsg_fmt, **errmsg_arg;
     int flags;
{
  int rc;

  last_pid++;
  if (last_pid < 0)
    last_pid = 1;

  if ((flags & PEXECUTE_ONE) != PEXECUTE_ONE)
    abort ();

#ifdef __GO32__
  /* ??? What are the possible return values from spawnv?  */
  rc = (flags & PEXECUTE_SEARCH ? spawnvp : spawnv) (1, program, argv);
#else
  char *scmd, *rf;
  FILE *argfile;
  int i, el = flags & PEXECUTE_SEARCH ? 4 : 0;

  if (temp_base == 0)
    temp_base = choose_temp_base ();
  scmd = (char *) xmalloc (strlen (program) + strlen (temp_base) + 6 + el);
  rf = scmd + strlen(program) + 2 + el;
  sprintf (scmd, "%s%s @%s.gp", program,
	   (flags & PEXECUTE_SEARCH ? ".exe" : ""), temp_base);
  argfile = fopen (rf, "w");
  if (argfile == 0)
    {
      int errno_save = errno;
      free (scmd);
      errno = errno_save;
      *errmsg_fmt = "cannot open `%s.gp'";
      *errmsg_arg = temp_base;
      return -1;
    }

  for (i=1; argv[i]; i++)
    {
      char *cp;
      for (cp = argv[i]; *cp; cp++)
	{
	  if (*cp == '"' || *cp == '\'' || *cp == '\\' || ISSPACE (*cp))
	    fputc ('\\', argfile);
	  fputc (*cp, argfile);
	}
      fputc ('\n', argfile);
    }
  fclose (argfile);

  rc = system (scmd);

  {
    int errno_save = errno;
    remove (rf);
    free (scmd);
    errno = errno_save;
  }
#endif

  if (rc == -1)
    {
      *errmsg_fmt = install_error_msg;
      *errmsg_arg = program;
      return -1;
    }

  /* Tuck the status away for pwait, and return a "pid".  */
  last_status = rc << 8;
  return last_pid;
}

int
pwait (pid, status, flags)
     int pid;
     int *status;
     int flags;
{
  /* On MSDOS each pexecute must be followed by it's associated pwait.  */
  if (pid != last_pid
      /* Called twice for the same child?  */
      || pid == last_reaped)
    {
      /* ??? ECHILD would be a better choice.  Can we use it here?  */
      errno = EINVAL;
      return -1;
    }
  /* ??? Here's an opportunity to canonicalize the values in STATUS.
     Needed?  */
  *status = last_status;
  last_reaped = last_pid;
  return last_pid;
}

#endif /* MSDOS */

#if defined (_WIN32) && ! defined (_UWIN)

#include <process.h>

#ifdef __CYGWIN__

#define fix_argv(argvec) (argvec)

extern int _spawnv ();
extern int _spawnvp ();

#else /* ! __CYGWIN__ */

/* This is a kludge to get around the Microsoft C spawn functions' propensity
   to remove the outermost set of double quotes from all arguments.  */

const char * const *
fix_argv (argvec)
     char **argvec;
{
  int i;

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
#endif /* __CYGWIN__ */

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

#  define WAIT_CHILD	0
#  define WAIT_GRANDCHILD	1
#endif

/* Win32 supports pipes */
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
      *errmsg_arg = program;
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
     int flags;
{
#ifdef __CYGWIN__
  return wait (status);
#else
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
#endif /* __CYGWIN__ */
}

#endif /* _WIN32 && ! _UWIN */

#ifdef OS2

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

#endif /* OS2 */

#ifdef MPW

/* MPW pexecute doesn't actually run anything; instead, it writes out
   script commands that, when run, will do the actual executing.

   For example, in GCC's case, GCC will write out several script commands:

   cpp ...
   cc1 ...
   as ...
   ld ...

   and then exit.  None of the above programs will have run yet.  The task
   that called GCC will then execute the script and cause cpp,etc. to run.
   The caller must invoke pfinish before calling exit.  This adds
   the finishing touches to the generated script.  */

static int first_time = 1;

int
pexecute (program, argv, this_pname, temp_base, errmsg_fmt, errmsg_arg, flags)
     const char *program;
     char * const *argv;
     const char *this_pname;
     const char *temp_base;
     char **errmsg_fmt, **errmsg_arg;
     int flags;
{
  char tmpprogram[255];
  char *cp, *tmpname;
  int i;

  mpwify_filename (program, tmpprogram);
  if (first_time)
    {
      printf ("Set Failed 0\n");
      first_time = 0;
    }

  fputs ("If {Failed} == 0\n", stdout);
  /* If being verbose, output a copy of the command.  It should be
     accurate enough and escaped enough to be "clickable".  */
  if (flags & PEXECUTE_VERBOSE)
    {
      fputs ("\tEcho ", stdout);
      fputc ('\'', stdout);
      fputs (tmpprogram, stdout);
      fputc ('\'', stdout);
      fputc (' ', stdout);
      for (i=1; argv[i]; i++)
	{
	  fputc ('\'', stdout);
	  /* See if we have an argument that needs fixing.  */
	  if (strchr(argv[i], '/'))
	    {
	      tmpname = (char *) xmalloc (256);
	      mpwify_filename (argv[i], tmpname);
	      argv[i] = tmpname;
	    }
	  for (cp = argv[i]; *cp; cp++)
	    {
	      /* Write an Option-d escape char in front of special chars.  */
	      if (strchr("'+", *cp))
		fputc ('\266', stdout);
	      fputc (*cp, stdout);
	    }
	  fputc ('\'', stdout);
	  fputc (' ', stdout);
	}
      fputs ("\n", stdout);
    }
  fputs ("\t", stdout);
  fputs (tmpprogram, stdout);
  fputc (' ', stdout);

  for (i=1; argv[i]; i++)
    {
      /* See if we have an argument that needs fixing.  */
      if (strchr(argv[i], '/'))
	{
	  tmpname = (char *) xmalloc (256);
	  mpwify_filename (argv[i], tmpname);
	  argv[i] = tmpname;
	}
      if (strchr (argv[i], ' '))
	fputc ('\'', stdout);
      for (cp = argv[i]; *cp; cp++)
	{
	  /* Write an Option-d escape char in front of special chars.  */
	  if (strchr("'+", *cp))
	    fputc ('\266', stdout);
	  fputc (*cp, stdout);
	}
      if (strchr (argv[i], ' '))
	fputc ('\'', stdout);
      fputc (' ', stdout);
    }

  fputs ("\n", stdout);

  /* Output commands that arrange to clean up and exit if a failure occurs.
     We have to be careful to collect the status from the program that was
     run, rather than some other script command.  Also, we don't exit
     immediately, since necessary cleanups are at the end of the script.  */
  fputs ("\tSet TmpStatus {Status}\n", stdout);
  fputs ("\tIf {TmpStatus} != 0\n", stdout);
  fputs ("\t\tSet Failed {TmpStatus}\n", stdout);
  fputs ("\tEnd\n", stdout);
  fputs ("End\n", stdout);

  /* We're just composing a script, can't fail here.  */
  return 0;
}

int
pwait (pid, status, flags)
     int pid;
     int *status;
     int flags;
{
  *status = 0;
  return 0;
}

/* Write out commands that will exit with the correct error code
   if something in the script failed.  */

void
pfinish ()
{
  printf ("\tExit \"{Failed}\"\n");
}

#endif /* MPW */

/* include for Unix-like environments but not for Dos-like environments */
#if ! defined (__MSDOS__) && ! defined (OS2) && ! defined (MPW) \
    && ! (defined (_WIN32) && ! defined (_UWIN))

extern int execv ();
extern int execvp ();

int
pexecute (program, argv, this_pname, temp_base, errmsg_fmt, errmsg_arg, flags)
     const char *program;
     char * const *argv;
     const char *this_pname;
     const char *temp_base;
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
  for (retries = 0; retries < 4; retries++)
    {
      pid = vfork ();
      if (pid >= 0)
	break;
      sleep (sleep_interval);
      sleep_interval *= 2;
    }

  switch (pid)
    {
    case -1:
      {
	*errmsg_fmt = VFORK_STRING;
	*errmsg_arg = NULL;
	return -1;
      }

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

      /* Note: Calling fprintf and exit here doesn't seem right for vfork.  */
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
     int flags;
{
  /* ??? Here's an opportunity to canonicalize the values in STATUS.
     Needed?  */
#ifdef VMS
  pid = waitpid (-1, status, 0);
#else
  pid = wait (status);
#endif
  return pid;
}

#endif /* ! __MSDOS__ && ! OS2 && ! MPW && ! (_WIN32 && ! _UWIN) */
