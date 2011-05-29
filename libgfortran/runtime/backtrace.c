/* Copyright (C) 2006, 2007, 2009, 2011 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <ctype.h>


/* Macros for common sets of capabilities: can we fork and exec, can
   we use glibc-style backtrace functions, and can we use pipes.  */
#define CAN_FORK (defined(HAVE_FORK) && defined(HAVE_EXECVP) \
		  && defined(HAVE_WAIT))
#define GLIBC_BACKTRACE (defined(HAVE_BACKTRACE) \
			 && defined(HAVE_BACKTRACE_SYMBOLS_FD))
#define CAN_PIPE (CAN_FORK && defined(HAVE_PIPE) \
		  && defined(HAVE_DUP2) && defined(HAVE_FDOPEN) \
		  && defined(HAVE_CLOSE))


/* GDB style #NUM index for each stack frame.  */

static void 
bt_header (int num)
{
  st_printf (" #%d  ", num);
}


/* fgets()-like function that reads a line from a fd, without
   needing to malloc() a buffer, and does not use locks, hence should
   be async-signal-safe.  */

static char *
fd_gets (char *s, int size, int fd)
{
  for (int i = 0; i < size; i++)
    {
      char c;
      ssize_t nread = read (fd, &c, 1);
      if (nread == 1)
	{
	  s[i] = c;
	  if (c == '\n')
	    {
	      if (i + 1 < size)
		s[i+1] = '\0';
	      else
		s[i] = '\0';
	      break;
	    }
	}
      else
	{
	  s[i] = '\0';
	  if (i == 0)
	    return NULL;
	  break;
	}
    }
  return s;
}


extern char *addr2line_path;


/* show_backtrace displays the backtrace, currently obtained by means of
   the glibc backtrace* functions.  */

void
show_backtrace (void)
{
#if GLIBC_BACKTRACE

#define DEPTH 50
#define BUFSIZE 1024

  void *trace[DEPTH];
  int depth;

  depth = backtrace (trace, DEPTH);
  if (depth <= 0)
    return;

#if CAN_PIPE

  if (addr2line_path == NULL)
    goto fallback_noerr;

  /* We attempt to extract file and line information from addr2line.  */
  do
  {
    /* Local variables.  */
    int f[2], pid, bt[2], inp[2];
    char addr_buf[GFC_XTOA_BUF_SIZE], func[BUFSIZE], file[BUFSIZE];
    char *p;

    /* Don't output an error message if something goes wrong, we'll simply
       fall back to the pstack and glibc backtraces.  */
    if (pipe (f) != 0)
      break;
    if (pipe (inp) != 0)
      break;
    if ((pid = fork ()) == -1)
      break;

    if (pid == 0)
      {
	/* Child process.  */
#define NUM_FIXEDARGS 7
	char *arg[NUM_FIXEDARGS];
	char *newenv[] = { NULL };

	close (f[0]);

	close (inp[1]);
	if (dup2 (inp[0], STDIN_FILENO) == -1)
	  _exit (1);
	close (inp[0]);

	close (STDERR_FILENO);

	if (dup2 (f[1], STDOUT_FILENO) == -1)
	  _exit (1);
	close (f[1]);

	arg[0] = addr2line_path;
	arg[1] = (char *) "-e";
	arg[2] = full_exe_path ();
	arg[3] = (char *) "-f";
	arg[4] = (char *) "-s";
	arg[5] = (char *) "-C";
	arg[6] = NULL;
	execve (addr2line_path, arg, newenv);
	_exit (1);
#undef NUM_FIXEDARGS
      }

    /* Father process.  */
    close (f[1]);
    close (inp[0]);
    if (pipe (bt) != 0)
      break;
    backtrace_symbols_fd (trace, depth, bt[1]);
    close (bt[1]);

    estr_write ("\nBacktrace for this error:\n");
    for (int j = 0; j < depth; j++)
      {
	const char *addr = gfc_xtoa 
	  ((GFC_UINTEGER_LARGEST) (intptr_t) trace[j], 
	   addr_buf, sizeof (addr_buf));

	write (inp[1], addr, strlen (addr));
	write (inp[1], "\n", 1);
	
	if (! fd_gets (func, sizeof(func), f[0]))
	  goto fallback;
	if (! fd_gets (file, sizeof(file), f[0]))
	  goto fallback;
	    
	for (p = func; *p != '\n' && *p != '\r'; p++)
	  ;
	*p = '\0';
	
	/* If we only have the address, use the glibc backtrace.  */
	if (func[0] == '?' && func[1] == '?' && file[0] == '?'
	    && file[1] == '?')
	  {
	    bt_header (j);
	    while (1)
	      {
		char bc;
		ssize_t nread = read (bt[0], &bc, 1);
		if (nread != 1 || bc == '\n')
		  break;
		write (STDERR_FILENO, &bc, 1);
	      }
	    estr_write ("\n");
	    continue;
	  }
	else
	  {
	    /* Forward to the next entry in the backtrace. */
	    while (1)
	      {
		char bc;
		ssize_t nread = read (bt[0], &bc, 1);
		if (nread != 1 || bc == '\n')
		  break;
	      }
	  }

	/* _start is a setup routine that calls main(), and main() is
	   the frontend routine that calls some setup stuff and then
	   calls MAIN__, so at this point we should stop.  */
	if (strcmp (func, "_start") == 0 || strcmp (func, "main") == 0)
	  break;
	
	bt_header (j);
	estr_write (full_exe_path ());
	estr_write ("[0x");
	estr_write (addr);
	estr_write ("] in ");
	estr_write (func);
	
	if (strncmp (file, "??", 2) == 0)
	  estr_write ("\n");
	else
	  {
	    estr_write (" at ");
	    estr_write (file);
	  }
      } /* Loop over each hex address.  */
    close (inp[1]);
    close (bt[0]);
    wait (NULL);
    return;

fallback:
    estr_write ("** Something went wrong while running addr2line. **\n"
		"** Falling back  to a simpler  backtrace scheme. **\n");
  }
  while (0);

#undef DEPTH
#undef BUFSIZE

#endif /* CAN_PIPE */

fallback_noerr:
  /* Fallback to the glibc backtrace.  */
  estr_write ("\nBacktrace for this error:\n");
  backtrace_symbols_fd (trace, depth, STDERR_FILENO);
  return;

#elif defined(CAN_FORK) && defined(HAVE_GETPPID)
  /* Try to call pstack.  */
  do
  {
    /* Local variables.  */
    int pid;

    /* Don't output an error message if something goes wrong, we'll simply
       fall back to the pstack and glibc backtraces.  */
    if ((pid = fork ()) == -1)
      break;

    if (pid == 0)
      {
	/* Child process.  */
#define NUM_ARGS 2
	char *arg[NUM_ARGS+1];
	char buf[20];

	estr_write ("\nBacktrace for this error:\n");
	arg[0] = (char *) "pstack";
	snprintf (buf, sizeof(buf), "%d", (int) getppid ());
	arg[1] = buf;
	arg[2] = NULL;
	execvp (arg[0], arg);
#undef NUM_ARGS

	/* pstack didn't work.  */
	estr_write ("  unable to produce a backtrace, sorry!\n");
	_exit (1);
      }

    /* Father process.  */
    wait (NULL);
    return;
  }
  while(0);
#else
  estr_write ("\nBacktrace not yet available on this platform, sorry!\n");
#endif
}
