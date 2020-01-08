/* Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

#include <gthr.h>

#include <string.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "backtrace-supported.h"
#include "backtrace.h"


/* Store our own state while backtracing.  */
struct mystate
{
  int frame;
  bool try_simple;
  bool in_signal_handler;
};


/* Does a function name have "_gfortran_" or "_gfortrani_" prefix, possibly
   with additional underscore(s) at the beginning?  Cannot use strncmp()
   because we might be called from a signal handler.  */

static int
has_gfortran_prefix (const char *s)
{
  if (!s)
    return 0;

  while (*s == '_')
    s++;

  return (s[0] == 'g' && s[1] == 'f' && s[2] == 'o' && s[3] == 'r'
	  && s[4] == 't' && s[5] == 'r' && s[6] == 'a' && s[7] == 'n'
	  && (s[8] == '_' || (s[8] == 'i' && s[9] == '_')));
}

static void
error_callback (void *data, const char *msg, int errnum)
{
  struct mystate *state = (struct mystate *) data;
  struct iovec iov[5];
#define ERRHDR "\nCould not print backtrace: "

  if (errnum < 0)
    {
      state->try_simple = true;
      return;
    }
  else if (errnum == 0)
    {
      iov[0].iov_base = (char*) ERRHDR;
      iov[0].iov_len = strlen (ERRHDR);
      iov[1].iov_base = (char*) msg;
      iov[1].iov_len = strlen (msg);
      iov[2].iov_base = (char*) "\n";
      iov[2].iov_len = 1;
      estr_writev (iov, 3);
    }
  else
    {
      char errbuf[256];
      if (state->in_signal_handler)
	{
	  iov[0].iov_base = (char*) ERRHDR;
	  iov[0].iov_len = strlen (ERRHDR);
	  iov[1].iov_base = (char*) msg;
	  iov[1].iov_len = strlen (msg);
	  iov[2].iov_base = (char*) ", errno: ";
	  iov[2].iov_len = strlen (iov[2].iov_base);
	  const char *p = gfc_itoa (errnum, errbuf, sizeof (errbuf));
	  iov[3].iov_base = (char*) p;
	  iov[3].iov_len = strlen (p);
	  iov[4].iov_base = (char*) "\n";
	  iov[4].iov_len = 1;
	  estr_writev (iov, 5);
	}
      else
	st_printf (ERRHDR "%s: %s\n", msg,
		  gf_strerror (errnum, errbuf, sizeof (errbuf)));
    }
}

static int
simple_callback (void *data, uintptr_t pc)
{
  struct mystate *state = (struct mystate *) data;
  st_printf ("#%d  0x%lx\n", state->frame, (unsigned long) pc);
  (state->frame)++;
  return 0;
}

static int
full_callback (void *data, uintptr_t pc, const char *filename,
	       int lineno, const char *function)
{
  struct mystate *state = (struct mystate *) data;

  if (has_gfortran_prefix (function))
    return 0;

  st_printf ("#%d  0x%lx in %s\n", state->frame,
	     (unsigned long) pc, function == NULL ? "???" : function);
  if (filename || lineno != 0)
    st_printf ("\tat %s:%d\n", filename == NULL ? "???" : filename, lineno);
  (state->frame)++;

  if (function != NULL && strcmp (function, "main") == 0)
    return 1;

  return 0;
}


/* Display the backtrace.  */

void
show_backtrace (bool in_signal_handler)
{
  /* Note that libbacktrace allows the state to be accessed from
     multiple threads, so we don't need to use a TLS variable for the
     state here.  */
  static struct backtrace_state *lbstate_saved;
  struct backtrace_state *lbstate;
  struct mystate state = { 0, false, in_signal_handler };

  lbstate = __atomic_load_n (&lbstate_saved, __ATOMIC_RELAXED);
  if (!lbstate)
    {
      lbstate = backtrace_create_state (NULL, __gthread_active_p (),
					error_callback, NULL);
      if (lbstate)
	__atomic_store_n (&lbstate_saved, lbstate, __ATOMIC_RELAXED);
      else
	return;
    }

  if (!BACKTRACE_SUPPORTED || (in_signal_handler && BACKTRACE_USES_MALLOC))
    {
      /* If symbolic backtrace is not supported on this target, or would
	 require malloc() and we are in a signal handler, go with a
	 simple backtrace.  */

      backtrace_simple (lbstate, 0, simple_callback, error_callback, &state);
    }
  else
    {
      /* libbacktrace uses mmap, which is safe to call from a signal handler
	 (in practice, if not in theory).  Thus we can generate a symbolic
	 backtrace, if debug symbols are available.  */

      backtrace_full (lbstate, 0, full_callback, error_callback, &state);
      if (state.try_simple)
	backtrace_simple (lbstate, 0, simple_callback, error_callback, &state);
    }
}



/* Function called by the front-end translating the BACKTRACE intrinsic.  */

extern void backtrace (void);
export_proto (backtrace);

void
backtrace (void)
{
  show_backtrace (false);
}

