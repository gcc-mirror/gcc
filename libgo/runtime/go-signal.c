/* go-signal.c -- signal handling for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>

#include "runtime.h"
#include "go-assert.h"
#include "go-panic.h"

#ifndef SA_RESTART
  #define SA_RESTART 0
#endif

/* What to do for a signal.  */

struct sigtab
{
  /* Signal number.  */
  int sig;
  /* Nonzero if the signal should be caught.  */
  _Bool catch;
  /* Nonzero if the signal should be queued.  */
  _Bool queue;
  /* Nonzero if the signal should be ignored.  */
  _Bool ignore;
  /* Nonzero if we should restart system calls.  */
  _Bool restart;
};

/* What to do for signals.  */

static struct sigtab signals[] =
{
  { SIGHUP, 0, 1, 0, 1 },
  { SIGINT, 0, 1, 0, 1 },
  { SIGQUIT, 0, 1, 0, 1 },
  { SIGALRM, 0, 1, 1, 1 },
  { SIGTERM, 0, 1, 0, 1 },
#ifdef SIGILL
  { SIGILL, 1, 0, 0, 0 },
#endif
#ifdef SIGTRAP
  { SIGTRAP, 1, 0, 0, 0 },
#endif
#ifdef SIGABRT
  { SIGABRT, 1, 0, 0, 0 },
#endif
#ifdef SIGBUS
  { SIGBUS, 1, 0, 0, 0 },
#endif
#ifdef SIGFPE
  { SIGFPE, 1, 0, 0, 0 },
#endif
#ifdef SIGUSR1
  { SIGUSR1, 0, 1, 1, 1 },
#endif
#ifdef SIGSEGV
  { SIGSEGV, 1, 0, 0, 0 },
#endif
#ifdef SIGUSR2
  { SIGUSR2, 0, 1, 1, 1 },
#endif
#ifdef SIGPIPE
  { SIGPIPE, 0, 0, 1, 0 },
#endif
#ifdef SIGSTKFLT
  { SIGSTKFLT, 1, 0, 0, 0 },
#endif
#ifdef SIGCHLD
  { SIGCHLD, 0, 1, 1, 1 },
#endif
#ifdef SIGTSTP
  { SIGTSTP, 0, 1, 1, 1 },
#endif
#ifdef SIGTTIN
  { SIGTTIN, 0, 1, 1, 1 },
#endif
#ifdef SIGTTOU
  { SIGTTOU, 0, 1, 1, 1 },
#endif
#ifdef SIGURG
  { SIGURG, 0, 1, 1, 1 },
#endif
#ifdef SIGXCPU
  { SIGXCPU, 0, 1, 1, 1 },
#endif
#ifdef SIGXFSZ
  { SIGXFSZ, 0, 1, 1, 1 },
#endif
#ifdef SIGVTARLM
  { SIGVTALRM, 0, 1, 1, 1 },
#endif
#ifdef SIGPROF
  { SIGPROF, 0, 1, 1, 1 },
#endif
#ifdef SIGWINCH
  { SIGWINCH, 0, 1, 1, 1 },
#endif
#ifdef SIGIO
  { SIGIO, 0, 1, 1, 1 },
#endif
#ifdef SIGPWR
  { SIGPWR, 0, 1, 1, 1 },
#endif
#ifdef SIGSYS
  { SIGSYS, 1, 0, 0, 0 },
#endif
  { -1, 0, 0, 0, 0 }
};

/* The Go signal handler.  */

static void
sighandler (int sig)
{
  const char *msg;
  int i;

#ifdef SIGPROF
  if (sig == SIGPROF)
    {
      /* FIXME.  */
      runtime_sigprof (0, 0, nil, nil);
      return;
    }
#endif

  /* FIXME: Should check siginfo for more information when
     available.  */
  msg = NULL;
  switch (sig)
    {
#ifdef SIGILL
    case SIGILL:
      msg = "illegal instruction";
      break;
#endif

#ifdef SIGBUS
    case SIGBUS:
      msg = "invalid memory address or nil pointer dereference";
      break;
#endif

#ifdef SIGFPE
    case SIGFPE:
      msg = "integer divide by zero or floating point error";
      break;
#endif

#ifdef SIGSEGV
    case SIGSEGV:
      msg = "invalid memory address or nil pointer dereference";
      break;
#endif

    default:
      break;
    }

  if (msg != NULL)
    {
      sigset_t clear;

      if (runtime_m()->mallocing)
	{
	  fprintf (stderr, "caught signal while mallocing: %s\n", msg);
	  __go_assert (0);
	}

      /* The signal handler blocked signals; unblock them.  */
      i = sigfillset (&clear);
      __go_assert (i == 0);
      i = sigprocmask (SIG_UNBLOCK, &clear, NULL);
      __go_assert (i == 0);

      runtime_panicstring (msg);
    }

  for (i = 0; signals[i].sig != -1; ++i)
    {
      if (signals[i].sig == sig)
	{
	  struct sigaction sa;

	  if (signals[i].queue)
	    {
	      if (__go_sigsend (sig) || signals[i].ignore)
		return;
	      runtime_exit (2);		// SIGINT, SIGTERM, etc
	    }

	  if (runtime_panicking)
	    runtime_exit (2);
	  runtime_panicking = 1;

	  memset (&sa, 0, sizeof sa);

	  sa.sa_handler = SIG_DFL;

	  i = sigemptyset (&sa.sa_mask);
	  __go_assert (i == 0);

	  if (sigaction (sig, &sa, NULL) != 0)
	    abort ();

	  raise (sig);
	  exit (2);
	}
    }
  abort ();
}

/* Ignore a signal.  */

static void
sig_ignore (int sig __attribute__ ((unused)))
{
}

/* Initialize signal handling for Go.  This is called when the program
   starts.  */

void
runtime_initsig (int32 queue)
{
  struct sigaction sa;
  int i;

  siginit ();

  memset (&sa, 0, sizeof sa);

  sa.sa_handler = sighandler;

  i = sigfillset (&sa.sa_mask);
  __go_assert (i == 0);

  for (i = 0; signals[i].sig != -1; ++i)
    {
      if (signals[i].queue != (queue ? 1 : 0))
	continue;
      if (signals[i].catch || signals[i].queue)
	sa.sa_handler = sighandler;
      else
	sa.sa_handler = sig_ignore;
      sa.sa_flags = signals[i].restart ? SA_RESTART : 0;
      if (sigaction (signals[i].sig, &sa, NULL) != 0)
	__go_assert (0);
    }
}

void
runtime_resetcpuprofiler(int32 hz)
{
#ifdef SIGPROF
  struct itimerval it;
  struct sigaction sa;
  int i;

  memset (&it, 0, sizeof it);

  memset (&sa, 0, sizeof sa);
  i = sigfillset (&sa.sa_mask);
  __go_assert (i == 0);

  if (hz == 0)
    {
      i = setitimer (ITIMER_PROF, &it, NULL);
      __go_assert (i == 0);

      sa.sa_handler = SIG_IGN;
      i = sigaction (SIGPROF, &sa, NULL);
      __go_assert (i == 0);
    }
  else
    {
      sa.sa_handler = sighandler;
      sa.sa_flags = SA_RESTART;
      i = sigaction (SIGPROF, &sa, NULL);
      __go_assert (i == 0);

      it.it_interval.tv_sec = 0;
      it.it_interval.tv_usec = 1000000 / hz;
      it.it_value = it.it_interval;
      i = setitimer (ITIMER_PROF, &it, NULL);
      __go_assert (i == 0);
    }
#endif

  runtime_m()->profilehz = hz;
}

/* Used by the os package to raise SIGPIPE.  */

void os_sigpipe (void) __asm__ ("libgo_os.os.sigpipe");

void
os_sigpipe (void)
{
  struct sigaction sa;
  int i;

  memset (&sa, 0, sizeof sa);

  sa.sa_handler = SIG_DFL;

  i = sigemptyset (&sa.sa_mask);
  __go_assert (i == 0);

  if (sigaction (SIGPIPE, &sa, NULL) != 0)
    abort ();

  raise (SIGPIPE);
}
