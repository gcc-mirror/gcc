/* go-signal.c -- signal handling for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <signal.h>
#include <stdlib.h>

#include "go-assert.h"
#include "go-panic.h"
#include "go-signal.h"

#include "runtime.h"

#undef int 

#ifndef SA_ONSTACK
#define SA_ONSTACK 0
#endif

/* What to do for a signal.  */

struct sigtab
{
  /* Signal number.  */
  int sig;
  /* Nonzero if the signal should be ignored.  */
  _Bool ignore;
};

/* What to do for signals.  */

static struct sigtab signals[] =
{
  { SIGHUP, 0 },
  { SIGINT, 0 },
  { SIGALRM, 1 },
  { SIGTERM, 0 },
#ifdef SIGBUS
  { SIGBUS, 0 },
#endif
#ifdef SIGFPE
  { SIGFPE, 0 },
#endif
#ifdef SIGUSR1
  { SIGUSR1, 1 },
#endif
#ifdef SIGSEGV
  { SIGSEGV, 0 },
#endif
#ifdef SIGUSR2
  { SIGUSR2, 1 },
#endif
#ifdef SIGPIPE
  { SIGPIPE, 1 },
#endif
#ifdef SIGCHLD
  { SIGCHLD, 1 },
#endif
#ifdef SIGTSTP
  { SIGTSTP, 1 },
#endif
#ifdef SIGTTIN
  { SIGTTIN, 1 },
#endif
#ifdef SIGTTOU
  { SIGTTOU, 1 },
#endif
#ifdef SIGURG
  { SIGURG, 1 },
#endif
#ifdef SIGXCPU
  { SIGXCPU, 1 },
#endif
#ifdef SIGXFSZ
  { SIGXFSZ, 1 },
#endif
#ifdef SIGVTARLM
  { SIGVTALRM, 1 },
#endif
#ifdef SIGPROF
  { SIGPROF, 1 },
#endif
#ifdef SIGWINCH
  { SIGWINCH, 1 },
#endif
#ifdef SIGIO
  { SIGIO, 1 },
#endif
#ifdef SIGPWR
  { SIGPWR, 1 },
#endif
  { -1, 0 }
};

/* The Go signal handler.  */

static void
sighandler (int sig)
{
  const char *msg;
  int i;

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
    if (sigaction (signals[i].sig, &sa, NULL) != 0)
      __go_assert (0);
}
