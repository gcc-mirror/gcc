/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  Generic Unix version
   (also used for UWIN and VMS).
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2005, 2009,
   2010 Free Software Foundation, Inc.

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

#include "config.h"
#include "libiberty.h"
#include "pex-common.h"

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#ifdef NEED_DECLARATION_ERRNO
extern int errno;
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/types.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_GETRUSAGE
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_PROCESS_H
#include <process.h>
#endif

#ifdef vfork /* Autoconf may define this to fork for us. */
# define VFORK_STRING "fork"
#else
# define VFORK_STRING "vfork"
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
#if defined(VMS) && defined (__LONG_POINTERS)
#ifndef __CHAR_PTR32
typedef char * __char_ptr32
__attribute__ ((mode (SI)));
#endif

typedef __char_ptr32 *__char_ptr_char_ptr32
__attribute__ ((mode (SI)));

/* Return a 32 bit pointer to an array of 32 bit pointers 
   given a 64 bit pointer to an array of 64 bit pointers.  */

static __char_ptr_char_ptr32
to_ptr32 (char **ptr64)
{
  int argc;
  __char_ptr_char_ptr32 short_argv;

  for (argc=0; ptr64[argc]; argc++);

  /* Reallocate argv with 32 bit pointers.  */
  short_argv = (__char_ptr_char_ptr32) decc$malloc
    (sizeof (__char_ptr32) * (argc + 1));

  for (argc=0; ptr64[argc]; argc++)
    short_argv[argc] = (__char_ptr32) decc$strdup (ptr64[argc]);

  short_argv[argc] = (__char_ptr32) 0;
  return short_argv;

}
#else
#define to_ptr32(argv) argv
#endif

/* File mode to use for private and world-readable files.  */

#if defined (S_IRUSR) && defined (S_IWUSR) && defined (S_IRGRP) && defined (S_IWGRP) && defined (S_IROTH) && defined (S_IWOTH)
#define PUBLIC_MODE  \
    (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#else
#define PUBLIC_MODE 0666
#endif

/* Get the exit status of a particular process, and optionally get the
   time that it took.  This is simple if we have wait4, slightly
   harder if we have waitpid, and is a pain if we only have wait.  */

static pid_t pex_wait (struct pex_obj *, pid_t, int *, struct pex_time *);

#ifdef HAVE_WAIT4

static pid_t
pex_wait (struct pex_obj *obj ATTRIBUTE_UNUSED, pid_t pid, int *status,
	  struct pex_time *time)
{
  pid_t ret;
  struct rusage r;

#ifdef HAVE_WAITPID
  if (time == NULL)
    return waitpid (pid, status, 0);
#endif

  ret = wait4 (pid, status, 0, &r);

  if (time != NULL)
    {
      time->user_seconds = r.ru_utime.tv_sec;
      time->user_microseconds= r.ru_utime.tv_usec;
      time->system_seconds = r.ru_stime.tv_sec;
      time->system_microseconds= r.ru_stime.tv_usec;
    }

  return ret;
}

#else /* ! defined (HAVE_WAIT4) */

#ifdef HAVE_WAITPID

#ifndef HAVE_GETRUSAGE

static pid_t
pex_wait (struct pex_obj *obj ATTRIBUTE_UNUSED, pid_t pid, int *status,
	  struct pex_time *time)
{
  if (time != NULL)
    memset (time, 0, sizeof (struct pex_time));
  return waitpid (pid, status, 0);
}

#else /* defined (HAVE_GETRUSAGE) */

static pid_t
pex_wait (struct pex_obj *obj ATTRIBUTE_UNUSED, pid_t pid, int *status,
	  struct pex_time *time)
{
  struct rusage r1, r2;
  pid_t ret;

  if (time == NULL)
    return waitpid (pid, status, 0);

  getrusage (RUSAGE_CHILDREN, &r1);

  ret = waitpid (pid, status, 0);
  if (ret < 0)
    return ret;

  getrusage (RUSAGE_CHILDREN, &r2);

  time->user_seconds = r2.ru_utime.tv_sec - r1.ru_utime.tv_sec;
  time->user_microseconds = r2.ru_utime.tv_usec - r1.ru_utime.tv_usec;
  if (r2.ru_utime.tv_usec < r1.ru_utime.tv_usec)
    {
      --time->user_seconds;
      time->user_microseconds += 1000000;
    }

  time->system_seconds = r2.ru_stime.tv_sec - r1.ru_stime.tv_sec;
  time->system_microseconds = r2.ru_stime.tv_usec - r1.ru_stime.tv_usec;
  if (r2.ru_stime.tv_usec < r1.ru_stime.tv_usec)
    {
      --time->system_seconds;
      time->system_microseconds += 1000000;
    }

  return ret;
}

#endif /* defined (HAVE_GETRUSAGE) */

#else /* ! defined (HAVE_WAITPID) */

struct status_list
{
  struct status_list *next;
  pid_t pid;
  int status;
  struct pex_time time;
};

static pid_t
pex_wait (struct pex_obj *obj, pid_t pid, int *status, struct pex_time *time)
{
  struct status_list **pp;

  for (pp = (struct status_list **) &obj->sysdep;
       *pp != NULL;
       pp = &(*pp)->next)
    {
      if ((*pp)->pid == pid)
	{
	  struct status_list *p;

	  p = *pp;
	  *status = p->status;
	  if (time != NULL)
	    *time = p->time;
	  *pp = p->next;
	  free (p);
	  return pid;
	}
    }

  while (1)
    {
      pid_t cpid;
      struct status_list *psl;
      struct pex_time pt;
#ifdef HAVE_GETRUSAGE
      struct rusage r1, r2;
#endif

      if (time != NULL)
	{
#ifdef HAVE_GETRUSAGE
	  getrusage (RUSAGE_CHILDREN, &r1);
#else
	  memset (&pt, 0, sizeof (struct pex_time));
#endif
	}

      cpid = wait (status);

#ifdef HAVE_GETRUSAGE
      if (time != NULL && cpid >= 0)
	{
	  getrusage (RUSAGE_CHILDREN, &r2);

	  pt.user_seconds = r2.ru_utime.tv_sec - r1.ru_utime.tv_sec;
	  pt.user_microseconds = r2.ru_utime.tv_usec - r1.ru_utime.tv_usec;
	  if (pt.user_microseconds < 0)
	    {
	      --pt.user_seconds;
	      pt.user_microseconds += 1000000;
	    }

	  pt.system_seconds = r2.ru_stime.tv_sec - r1.ru_stime.tv_sec;
	  pt.system_microseconds = r2.ru_stime.tv_usec - r1.ru_stime.tv_usec;
	  if (pt.system_microseconds < 0)
	    {
	      --pt.system_seconds;
	      pt.system_microseconds += 1000000;
	    }
	}
#endif

      if (cpid < 0 || cpid == pid)
	{
	  if (time != NULL)
	    *time = pt;
	  return cpid;
	}

      psl = XNEW (struct status_list);
      psl->pid = cpid;
      psl->status = *status;
      if (time != NULL)
	psl->time = pt;
      psl->next = (struct status_list *) obj->sysdep;
      obj->sysdep = (void *) psl;
    }
}

#endif /* ! defined (HAVE_WAITPID) */
#endif /* ! defined (HAVE_WAIT4) */

static void pex_child_error (struct pex_obj *, const char *, const char *, int)
     ATTRIBUTE_NORETURN;
static int pex_unix_open_read (struct pex_obj *, const char *, int);
static int pex_unix_open_write (struct pex_obj *, const char *, int);
static pid_t pex_unix_exec_child (struct pex_obj *, int, const char *,
				 char * const *, char * const *,
				 int, int, int, int,
				 const char **, int *);
static int pex_unix_close (struct pex_obj *, int);
static int pex_unix_wait (struct pex_obj *, pid_t, int *, struct pex_time *,
			  int, const char **, int *);
static int pex_unix_pipe (struct pex_obj *, int *, int);
static FILE *pex_unix_fdopenr (struct pex_obj *, int, int);
static FILE *pex_unix_fdopenw (struct pex_obj *, int, int);
static void pex_unix_cleanup (struct pex_obj *);

/* The list of functions we pass to the common routines.  */

const struct pex_funcs funcs =
{
  pex_unix_open_read,
  pex_unix_open_write,
  pex_unix_exec_child,
  pex_unix_close,
  pex_unix_wait,
  pex_unix_pipe,
  pex_unix_fdopenr,
  pex_unix_fdopenw,
  pex_unix_cleanup
};

/* Return a newly initialized pex_obj structure.  */

struct pex_obj *
pex_init (int flags, const char *pname, const char *tempbase)
{
  return pex_init_common (flags, pname, tempbase, &funcs);
}

/* Open a file for reading.  */

static int
pex_unix_open_read (struct pex_obj *obj ATTRIBUTE_UNUSED, const char *name,
		    int binary ATTRIBUTE_UNUSED)
{
  return open (name, O_RDONLY);
}

/* Open a file for writing.  */

static int
pex_unix_open_write (struct pex_obj *obj ATTRIBUTE_UNUSED, const char *name,
		     int binary ATTRIBUTE_UNUSED)
{
  /* Note that we can't use O_EXCL here because gcc may have already
     created the temporary file via make_temp_file.  */
  return open (name, O_WRONLY | O_CREAT | O_TRUNC, PUBLIC_MODE);
}

/* Close a file.  */

static int
pex_unix_close (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd)
{
  return close (fd);
}

/* Report an error from a child process.  We don't use stdio routines,
   because we might be here due to a vfork call.  */

static void
pex_child_error (struct pex_obj *obj, const char *executable,
		 const char *errmsg, int err)
{
  int retval = 0;
#define writeerr(s) retval |= (write (STDERR_FILE_NO, s, strlen (s)) < 0)
  writeerr (obj->pname);
  writeerr (": error trying to exec '");
  writeerr (executable);
  writeerr ("': ");
  writeerr (errmsg);
  writeerr (": ");
  writeerr (xstrerror (err));
  writeerr ("\n");
#undef writeerr
  /* Exit with -2 if the error output failed, too.  */
  _exit (retval == 0 ? -1 : -2);
}

/* Execute a child.  */

extern char **environ;

#if defined(HAVE_SPAWNVE) && defined(HAVE_SPAWNVPE)
/* Implementation of pex->exec_child using the Cygwin spawn operation.  */

/* Subroutine of pex_unix_exec_child.  Move OLD_FD to a new file descriptor
   to be stored in *PNEW_FD, save the flags in *PFLAGS, and arrange for the
   saved copy to be close-on-exec.  Move CHILD_FD into OLD_FD.  If CHILD_FD
   is -1, OLD_FD is to be closed.  Return -1 on error.  */

static int
save_and_install_fd(int *pnew_fd, int *pflags, int old_fd, int child_fd)
{
  int new_fd, flags;

  flags = fcntl (old_fd, F_GETFD);

  /* If we could not retrieve the flags, then OLD_FD was not open.  */
  if (flags < 0)
    {
      new_fd = -1, flags = 0;
      if (child_fd >= 0 && dup2 (child_fd, old_fd) < 0)
	return -1;
    }
  /* If we wish to close OLD_FD, just mark it CLOEXEC.  */
  else if (child_fd == -1)
    {
      new_fd = old_fd;
      if ((flags & FD_CLOEXEC) == 0 && fcntl (old_fd, F_SETFD, FD_CLOEXEC) < 0)
	return -1;
    }
  /* Otherwise we need to save a copy of OLD_FD before installing CHILD_FD.  */
  else
    {
#ifdef F_DUPFD_CLOEXEC
      new_fd = fcntl (old_fd, F_DUPFD_CLOEXEC, 3);
      if (new_fd < 0)
	return -1;
#else
      /* Prefer F_DUPFD over dup in order to avoid getting a new fd
	 in the range 0-2, right where a new stderr fd might get put.  */
      new_fd = fcntl (old_fd, F_DUPFD, 3);
      if (new_fd < 0)
	return -1;
      if (fcntl (new_fd, F_SETFD, FD_CLOEXEC) < 0)
	return -1;
#endif
      if (dup2 (child_fd, old_fd) < 0)
	return -1;
    }

  *pflags = flags;
  if (pnew_fd)
    *pnew_fd = new_fd;
  else if (new_fd != old_fd)
    abort ();

  return 0;
}

/* Subroutine of pex_unix_exec_child.  Move SAVE_FD back to OLD_FD
   restoring FLAGS.  If SAVE_FD < 0, OLD_FD is to be closed.  */

static int
restore_fd(int old_fd, int save_fd, int flags)
{
  /* For SAVE_FD < 0, all we have to do is restore the
     "closed-ness" of the original.  */
  if (save_fd < 0)
    return close (old_fd);

  /* For SAVE_FD == OLD_FD, all we have to do is restore the
     original setting of the CLOEXEC flag.  */
  if (save_fd == old_fd)
    {
      if (flags & FD_CLOEXEC)
	return 0;
      return fcntl (old_fd, F_SETFD, flags);
    }

  /* Otherwise we have to move the descriptor back, restore the flags,
     and close the saved copy.  */
#ifdef HAVE_DUP3
  if (flags == FD_CLOEXEC)
    {
      if (dup3 (save_fd, old_fd, O_CLOEXEC) < 0)
	return -1;
    }
  else
#endif
    {
      if (dup2 (save_fd, old_fd) < 0)
	return -1;
      if (flags != 0 && fcntl (old_fd, F_SETFD, flags) < 0)
	return -1;
    }
  return close (save_fd);
}

static pid_t
pex_unix_exec_child (struct pex_obj *obj ATTRIBUTE_UNUSED,
		     int flags, const char *executable,
		     char * const * argv, char * const * env,
                     int in, int out, int errdes, int toclose,
		     const char **errmsg, int *err)
{
  int fl_in = 0, fl_out = 0, fl_err = 0, fl_tc = 0;
  int save_in = -1, save_out = -1, save_err = -1;
  int max, retries;
  pid_t pid;

  if (flags & PEX_STDERR_TO_STDOUT)
    errdes = out;

  /* We need the three standard file descriptors to be set up as for
     the child before we perform the spawn.  The file descriptors for
     the parent need to be moved and marked for close-on-exec.  */
  if (in != STDIN_FILE_NO
      && save_and_install_fd (&save_in, &fl_in, STDIN_FILE_NO, in) < 0)
    goto error_dup2;
  if (out != STDOUT_FILE_NO
      && save_and_install_fd (&save_out, &fl_out, STDOUT_FILE_NO, out) < 0)
    goto error_dup2;
  if (errdes != STDERR_FILE_NO
      && save_and_install_fd (&save_err, &fl_err, STDERR_FILE_NO, errdes) < 0)
    goto error_dup2;
  if (toclose >= 0
      && save_and_install_fd (NULL, &fl_tc, toclose, -1) < 0)
    goto error_dup2;

  /* Now that we've moved the file descriptors for the child into place,
     close the originals.  Be careful not to close any of the standard
     file descriptors that we just set up.  */
  max = -1;
  if (errdes >= 0)
    max = STDERR_FILE_NO;
  else if (out >= 0)
    max = STDOUT_FILE_NO;
  else if (in >= 0)
    max = STDIN_FILE_NO;
  if (in > max)
    close (in);
  if (out > max)
    close (out);
  if (errdes > max && errdes != out)
    close (errdes);

  /* If we were not given an environment, use the global environment.  */
  if (env == NULL)
    env = environ;

  /* Launch the program.  If we get EAGAIN (normally out of pid's), try
     again a few times with increasing backoff times.  */
  retries = 0;
  while (1)
    {
      typedef const char * const *cc_cp;

      if (flags & PEX_SEARCH)
	pid = spawnvpe (_P_NOWAITO, executable, (cc_cp)argv, (cc_cp)env);
      else
	pid = spawnve (_P_NOWAITO, executable, (cc_cp)argv, (cc_cp)env);

      if (pid > 0)
	break;

      *err = errno;
      *errmsg = "spawn";
      if (errno != EAGAIN || ++retries == 4)
	return (pid_t) -1;
      sleep (1 << retries);
    }

  /* Success.  Restore the parent's file descriptors that we saved above.  */
  if (toclose >= 0
      && restore_fd (toclose, toclose, fl_tc) < 0)
    goto error_dup2;
  if (in != STDIN_FILE_NO
      && restore_fd (STDIN_FILE_NO, save_in, fl_in) < 0)
    goto error_dup2;
  if (out != STDOUT_FILE_NO
      && restore_fd (STDOUT_FILE_NO, save_out, fl_out) < 0)
    goto error_dup2;
  if (errdes != STDERR_FILE_NO
      && restore_fd (STDERR_FILE_NO, save_err, fl_err) < 0)
    goto error_dup2;

  return pid;

 error_dup2:
  *err = errno;
  *errmsg = "dup2";
  return (pid_t) -1;
}

#else
/* Implementation of pex->exec_child using standard vfork + exec.  */

static pid_t
pex_unix_exec_child (struct pex_obj *obj, int flags, const char *executable,
		     char * const * argv, char * const * env,
                     int in, int out, int errdes,
		     int toclose, const char **errmsg, int *err)
{
  pid_t pid;

  /* We declare these to be volatile to avoid warnings from gcc about
     them being clobbered by vfork.  */
  volatile int sleep_interval;
  volatile int retries;

  /* We vfork and then set environ in the child before calling execvp.
     This clobbers the parent's environ so we need to restore it.
     It would be nice to use one of the exec* functions that takes an
     environment as a parameter, but that may have portability issues.  */
  char **save_environ = environ;

  sleep_interval = 1;
  pid = -1;
  for (retries = 0; retries < 4; ++retries)
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
      *err = errno;
      *errmsg = VFORK_STRING;
      return (pid_t) -1;

    case 0:
      /* Child process.  */
      if (in != STDIN_FILE_NO)
	{
	  if (dup2 (in, STDIN_FILE_NO) < 0)
	    pex_child_error (obj, executable, "dup2", errno);
	  if (close (in) < 0)
	    pex_child_error (obj, executable, "close", errno);
	}
      if (out != STDOUT_FILE_NO)
	{
	  if (dup2 (out, STDOUT_FILE_NO) < 0)
	    pex_child_error (obj, executable, "dup2", errno);
	  if (close (out) < 0)
	    pex_child_error (obj, executable, "close", errno);
	}
      if (errdes != STDERR_FILE_NO)
	{
	  if (dup2 (errdes, STDERR_FILE_NO) < 0)
	    pex_child_error (obj, executable, "dup2", errno);
	  if (close (errdes) < 0)
	    pex_child_error (obj, executable, "close", errno);
	}
      if (toclose >= 0)
	{
	  if (close (toclose) < 0)
	    pex_child_error (obj, executable, "close", errno);
	}
      if ((flags & PEX_STDERR_TO_STDOUT) != 0)
	{
	  if (dup2 (STDOUT_FILE_NO, STDERR_FILE_NO) < 0)
	    pex_child_error (obj, executable, "dup2", errno);
	}

      if (env)
	{
	  /* NOTE: In a standard vfork implementation this clobbers the
	     parent's copy of environ "too" (in reality there's only one copy).
	     This is ok as we restore it below.  */
	  environ = (char**) env;
	}

      if ((flags & PEX_SEARCH) != 0)
	{
	  execvp (executable, to_ptr32 (argv));
	  pex_child_error (obj, executable, "execvp", errno);
	}
      else
	{
	  execv (executable, to_ptr32 (argv));
	  pex_child_error (obj, executable, "execv", errno);
	}

      /* NOTREACHED */
      return (pid_t) -1;

    default:
      /* Parent process.  */

      /* Restore environ.
	 Note that the parent either doesn't run until the child execs/exits
	 (standard vfork behaviour), or if it does run then vfork is behaving
	 more like fork.  In either case we needn't worry about clobbering
	 the child's copy of environ.  */
      environ = save_environ;

      if (in != STDIN_FILE_NO)
	{
	  if (close (in) < 0)
	    {
	      *err = errno;
	      *errmsg = "close";
	      return (pid_t) -1;
	    }
	}
      if (out != STDOUT_FILE_NO)
	{
	  if (close (out) < 0)
	    {
	      *err = errno;
	      *errmsg = "close";
	      return (pid_t) -1;
	    }
	}
      if (errdes != STDERR_FILE_NO)
	{
	  if (close (errdes) < 0)
	    {
	      *err = errno;
	      *errmsg = "close";
	      return (pid_t) -1;
	    }
	}

      return pid;
    }
}
#endif /* SPAWN */

/* Wait for a child process to complete.  */

static int
pex_unix_wait (struct pex_obj *obj, pid_t pid, int *status,
	       struct pex_time *time, int done, const char **errmsg,
	       int *err)
{
  /* If we are cleaning up when the caller didn't retrieve process
     status for some reason, encourage the process to go away.  */
  if (done)
    kill (pid, SIGTERM);

  if (pex_wait (obj, pid, status, time) < 0)
    {
      *err = errno;
      *errmsg = "wait";
      return -1;
    }

  return 0;
}

/* Create a pipe.  */

static int
pex_unix_pipe (struct pex_obj *obj ATTRIBUTE_UNUSED, int *p,
	       int binary ATTRIBUTE_UNUSED)
{
  return pipe (p);
}

/* Get a FILE pointer to read from a file descriptor.  */

static FILE *
pex_unix_fdopenr (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd,
		  int binary ATTRIBUTE_UNUSED)
{
  return fdopen (fd, "r");
}

static FILE *
pex_unix_fdopenw (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd,
		  int binary ATTRIBUTE_UNUSED)
{
  if (fcntl (fd, F_SETFD, FD_CLOEXEC) < 0)
    return NULL;
  return fdopen (fd, "w");
}

static void
pex_unix_cleanup (struct pex_obj *obj ATTRIBUTE_UNUSED)
{
#if !defined (HAVE_WAIT4) && !defined (HAVE_WAITPID)
  while (obj->sysdep != NULL)
    {
      struct status_list *this;
      struct status_list *next;

      this = (struct status_list *) obj->sysdep;
      next = this->next;
      free (this);
      obj->sysdep = (void *) next;
    }
#endif
}
