/* Copyright (C) 2006-2013 Free Software Foundation, Inc.
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
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <limits.h>

#include "unwind.h"


/* Macros for common sets of capabilities: can we fork and exec, and
   can we use pipes to communicate with the subprocess.  */
#define CAN_FORK (defined(HAVE_FORK) && defined(HAVE_EXECVE) \
		  && defined(HAVE_WAIT))
#define CAN_PIPE (CAN_FORK && defined(HAVE_PIPE) \
		  && defined(HAVE_DUP2) && defined(HAVE_CLOSE))

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif


/* GDB style #NUM index for each stack frame.  */

static void 
bt_header (int num)
{
  st_printf ("#%d  ", num);
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

/* Struct containing backtrace state.  */
typedef struct
{
  int frame_number;
  int direct_output;
  int outfd;
  int infd;
  int error;
}
bt_state;

static _Unwind_Reason_Code
trace_function (struct _Unwind_Context *context, void *state_ptr)
{
  bt_state* state = (bt_state*) state_ptr;
  _Unwind_Ptr ip;
#ifdef HAVE_GETIPINFO
  int ip_before_insn = 0;
  ip = _Unwind_GetIPInfo (context, &ip_before_insn);
  
  /* If the unwinder gave us a 'return' address, roll it back a little
     to ensure we get the correct line number for the call itself.  */
  if (! ip_before_insn)
    --ip;
#else  
  ip = _Unwind_GetIP (context);
#endif

  if (state->direct_output)
    {
      bt_header(state->frame_number);
      st_printf ("%p\n", (void*) ip);
    }
  else
    {
      char addr_buf[GFC_XTOA_BUF_SIZE], func[1024], file[PATH_MAX];
      char *p;
      const char* addr = gfc_xtoa (ip, addr_buf, sizeof (addr_buf));
      write (state->outfd, addr, strlen (addr));
      write (state->outfd, "\n", 1);

      if (! fd_gets (func, sizeof(func), state->infd))
	{
	  state->error = 1;
	  goto done;
	}
      if (! fd_gets (file, sizeof(file), state->infd))
	{
	  state->error = 1;
	  goto done;
	}
	    
	for (p = func; *p != '\n' && *p != '\r'; p++)
	  ;
	*p = '\0';
	
	/* _start is a setup routine that calls main(), and main() is
	   the frontend routine that calls some setup stuff and then
	   calls MAIN__, so at this point we should stop.  */
	if (strcmp (func, "_start") == 0 || strcmp (func, "main") == 0)
	  return _URC_END_OF_STACK;
	
	bt_header (state->frame_number);
	estr_write ("0x");
	estr_write (addr);

	if (func[0] != '?' && func[1] != '?')
	  {
	    estr_write (" in ");
	    estr_write (func);
	  }
	
	if (strncmp (file, "??", 2) == 0)
	  estr_write ("\n");
	else
	  {
	    estr_write (" at ");
	    estr_write (file);
	  }
    }

 done:

  state->frame_number++;
  
  return _URC_NO_REASON;
}


/* Display the backtrace.  */

void
backtrace (void)
{
  bt_state state;
  state.frame_number = 0;
  state.error = 0;

#if CAN_PIPE

  if (addr2line_path == NULL)
    goto fallback_noerr;

  /* We attempt to extract file and line information from addr2line.  */
  do
  {
    /* Local variables.  */
    int f[2], pid, inp[2];

    /* Don't output an error message if something goes wrong, we'll simply
       fall back to printing the addresses.  */
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

    state.outfd = inp[1];
    state.infd = f[0];
    state.direct_output = 0;
    _Unwind_Backtrace (trace_function, &state);
    if (state.error)
      goto fallback;
    close (inp[1]);
    close (f[0]);
    wait (NULL);
    return;

fallback:
    estr_write ("** Something went wrong while running addr2line. **\n"
		"** Falling back to a simpler backtrace scheme. **\n");
  }
  while (0);

fallback_noerr:
#endif /* CAN_PIPE */

  /* Fallback to the simple backtrace without addr2line.  */
  state.direct_output = 1;
  _Unwind_Backtrace (trace_function, &state);
}
iexport(backtrace);
