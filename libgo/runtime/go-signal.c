/* go-signal.c -- signal handling for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <signal.h>
#include <stdlib.h>
#include <sys/time.h>

#include "go-assert.h"
#include "go-panic.h"
#include "go-signal.h"

#include "runtime.h"

#ifndef SA_RESTART
  #define SA_RESTART 0
#endif

/* What to do for a signal.  */

struct sigtab
{
  /* Signal number.  */
  int sig;
  /* Nonzero if the signal should be ignored.  */
  _Bool ignore;
  /* Nonzero if we should restart system calls.  */
  _Bool restart;
};

/* What to do for signals.  */

static struct sigtab signals[] =
{
  { SIGHUP, 0, 1 },
  { SIGINT, 0, 1 },
  { SIGALRM, 1, 1 },
  { SIGTERM, 0, 1 },
#ifdef SIGBUS
  { SIGBUS, 0, 0 },
#endif
#ifdef SIGFPE
  { SIGFPE, 0, 0 },
#endif
#ifdef SIGUSR1
  { SIGUSR1, 1, 1 },
#endif
#ifdef SIGSEGV
  { SIGSEGV, 0, 0 },
#endif
#ifdef SIGUSR2
  { SIGUSR2, 1, 1 },
#endif
#ifdef SIGPIPE
  { SIGPIPE, 1, 0 },
#endif
#ifdef SIGCHLD
  { SIGCHLD, 1, 1 },
#endif
#ifdef SIGTSTP
  { SIGTSTP, 1, 1 },
#endif
#ifdef SIGTTIN
  { SIGTTIN, 1, 1 },
#endif
#ifdef SIGTTOU
  { SIGTTOU, 1, 1 },
#endif
#ifdef SIGURG
  { SIGURG, 1, 1 },
#endif
#ifdef SIGXCPU
  { SIGXCPU, 1, 1 },
#endif
#ifdef SIGXFSZ
  { SIGXFSZ, 1, 1 },
#endif
#ifdef SIGVTARLM
  { SIGVTALRM, 1, 1 },
#endif
#ifdef SIGWINCH
  { SIGWINCH, 1, 1 },
#endif
#ifdef SIGIO
  { SIGIO, 1, 1 },
#endif
#ifdef SIGPWR
  { SIGPWR, 1, 1 },
#endif
  { -1, 0, 0 }
};

/* The Go signal handler.  */

static void
sighandler (int sig)
{
  const char *msg;
  int i;

  if (sig == SIGPROF)
    {
      /* FIXME.  */
      runtime_sigprof (0, 0, nil);
      return;
    }

  /* FIXME: Should check siginfo for more information when
     available.  */
  msg = NULL;
  switch (sig)
    {
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

      if (__sync_bool_compare_and_swap (&m->mallocing, 1, 1))
	{
	  fprintf (stderr, "caught signal while mallocing: %s\n", msg);
	  __go_assert (0);
	}

      /* The signal handler blocked signals; unblock them.  */
      i = sigfillset (&clear);
      __go_assert (i == 0);
      i = sigprocmask (SIG_UNBLOCK, &clear, NULL);
      __go_assert (i == 0);

      __go_panic_msg (msg);
    }

  if (__go_sigsend (sig))
    return;
  for (i = 0; signals[i].sig != -1; ++i)
    {
      if (signals[i].sig == sig)
	{
	  struct sigaction sa;

	  if (signals[i].ignore)
	    return;

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

/* Initialize signal handling for Go.  This is called when the program
   starts.  */

void
__initsig ()
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
      sa.sa_flags = signals[i].restart ? SA_RESTART : 0;
      if (sigaction (signals[i].sig, &sa, NULL) != 0)
	__go_assert (0);
    }
}

void
runtime_resetcpuprofiler(int32 hz)
{
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

  m->profilehz = hz;
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
