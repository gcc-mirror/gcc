/* 
Copyright (C) 1993 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

/*  written by Per Bothner (bothner@cygnus.com) */

#define _POSIX_SOURCE
#include "libioP.h"
#include <sys/types.h>
#if _IO_HAVE_SYS_WAIT
#include <signal.h>
#include <unistd.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <sys/wait.h>

#ifndef _IO_fork
#define _IO_fork vfork /* defined in libiberty, if needed */
_IO_pid_t _IO_fork();
#endif

#endif /* _IO_HAVE_SYS_WAIT */

#ifndef _IO_pipe
#define _IO_pipe pipe
extern int _IO_pipe();
#endif

#ifndef _IO_dup2
#define _IO_dup2 dup2
extern int _IO_dup2();
#endif

#ifndef _IO_waitpid
#define _IO_waitpid waitpid
#endif

#ifndef _IO_execl
#define _IO_execl execl
#endif
#ifndef _IO__exit
#define _IO__exit _exit
#endif

struct _IO_proc_file
{
  struct _IO_FILE_plus file;
  /* Following fields must match those in class procbuf (procbuf.h) */
  _IO_pid_t pid;
  struct _IO_proc_file *next;
};
typedef struct _IO_proc_file _IO_proc_file;

static struct _IO_proc_file *proc_file_chain = NULL;

_IO_FILE *
DEFUN(_IO_proc_open, (fp, command, mode),
      _IO_FILE* fp AND const char *command AND const char *mode)
{
#if _IO_HAVE_SYS_WAIT
  int read_or_write;
  int pipe_fds[2];
  int parent_end, child_end;
  _IO_pid_t child_pid;
  if (_IO_file_is_open(fp))
    return NULL;
  if (_IO_pipe(pipe_fds) < 0)
    return NULL;
  if (mode[0] == 'r')
    {
      parent_end = pipe_fds[0];
      child_end = pipe_fds[1];
      read_or_write = _IO_NO_WRITES;
    }
  else
    {
      parent_end = pipe_fds[1];
      child_end = pipe_fds[0];
      read_or_write = _IO_NO_READS;
    }
  ((_IO_proc_file*)fp)->pid = child_pid = _IO_fork();
  if (child_pid == 0)
    {
      int child_std_end = mode[0] == 'r' ? 1 : 0;
      _IO_close(parent_end);
      if (child_end != child_std_end)
	{
	  _IO_dup2(child_end, child_std_end);
	  _IO_close(child_end);
	}
      /* Posix.2:  "popen() shall ensure that any streams from previous
         popen() calls that remain open in the parent process are closed
	 in the new child process." */
      while (proc_file_chain)
	{
	  _IO_close (_IO_fileno ((_IO_FILE *) proc_file_chain));
	  proc_file_chain = proc_file_chain->next;
	}

      _IO_execl("/bin/sh", "sh", "-c", command, (char *)0);
      _IO__exit(127);
    }
  _IO_close(child_end);
  if (child_pid < 0)
    {
      _IO_close(parent_end);
      return NULL;
    }
  _IO_fileno(fp) = parent_end;

  /* Link into proc_file_chain. */
  ((_IO_proc_file*)fp)->next = proc_file_chain;
  proc_file_chain = (_IO_proc_file*)fp;

  _IO_mask_flags (fp, read_or_write, _IO_NO_READS|_IO_NO_WRITES);
  return fp;
#else /* !_IO_HAVE_SYS_WAIT */
  return NULL;
#endif
}

_IO_FILE *
DEFUN(_IO_popen, (command, mode),
      const char *command AND const char *mode)
{
  _IO_proc_file *fpx = (_IO_proc_file*)malloc(sizeof(_IO_proc_file));
  _IO_FILE *fp = (_IO_FILE*)fpx;
  if (fp == NULL)
    return NULL;
  _IO_init(fp, 0);
  _IO_JUMPS(fp) = &_IO_proc_jumps;
  _IO_file_init(fp);
#if  !_IO_UNIFIED_JUMPTABLES
  ((struct _IO_FILE_plus*)fp)->vtable = NULL;
#endif
  if (_IO_proc_open (fp, command, mode) != NULL)
    return fp;
  free (fpx);
  return NULL;
}

int
DEFUN(_IO_proc_close, (fp),
      _IO_FILE *fp)
{
  /* This is not name-space clean. FIXME! */
#if _IO_HAVE_SYS_WAIT
  int wstatus;
  _IO_proc_file **ptr = &proc_file_chain;
  _IO_pid_t wait_pid;
  int status = -1;
  
  /* Unlink from proc_file_chain. */
  for ( ; *ptr != NULL; ptr = &(*ptr)->next)
    {
      if (*ptr == (_IO_proc_file*)fp)
	{
	  *ptr = (*ptr)->next;
	  status = 0;
	  break;
	}
    }

  if (status < 0 || _IO_close(_IO_fileno(fp)) < 0)
    return -1;
  /* POSIX.2 Rationale:  "Some historical implementations either block
     or ignore the signals SIGINT, SIGQUIT, and SIGHUP while waiting
     for the child process to terminate.  Since this behavior is not
     described in POSIX.2, such implementations are not conforming." */
  do
    {
      wait_pid = _IO_waitpid (((_IO_proc_file*)fp)->pid, &wstatus, 0);
    } while (wait_pid == -1 && errno == EINTR);
  if (wait_pid == -1)
    return -1;
  return wstatus;
#else /* !_IO_HAVE_SYS_WAIT */
  return -1;
#endif
}

struct _IO_jump_t _IO_proc_jumps = {
  JUMP_INIT_DUMMY,
  JUMP_INIT(finish, _IO_file_finish),
  JUMP_INIT(overflow, _IO_file_overflow),
  JUMP_INIT(underflow, _IO_file_underflow),
  JUMP_INIT(uflow, _IO_default_uflow),
  JUMP_INIT(pbackfail, _IO_default_pbackfail),
  JUMP_INIT(xsputn, _IO_file_xsputn),
  JUMP_INIT(xsgetn, _IO_default_xsgetn),
  JUMP_INIT(seekoff, _IO_file_seekoff),
  JUMP_INIT(seekpos, _IO_default_seekpos),
  JUMP_INIT(setbuf, _IO_file_setbuf),
  JUMP_INIT(sync, _IO_file_sync),
  JUMP_INIT(doallocate, _IO_file_doallocate),
  JUMP_INIT(read, _IO_file_read),
  JUMP_INIT(write, _IO_file_write),
  JUMP_INIT(seek, _IO_file_seek),
  JUMP_INIT(close, _IO_proc_close),
  JUMP_INIT(stat, _IO_file_stat)
};
