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

#ifdef USING_SPLIT_STACK

extern void __splitstack_getcontext(void *context[10]);

extern void __splitstack_setcontext(void *context[10]);

#endif

#define C SigCatch
#define I SigIgnore
#define R SigRestart
#define Q SigQueue
#define P SigPanic

/* Signal actions.  This collects the sigtab tables for several
   different targets from the master library.  SIGKILL, SIGCONT, and
   SIGSTOP are not listed, as we don't want to set signal handlers for
   them.  */

SigTab runtime_sigtab[] = {
#ifdef SIGHUP
  { SIGHUP,	Q + R },
#endif
#ifdef SIGINT
  { SIGINT, 	Q + R },
#endif
#ifdef SIGQUIT
  { SIGQUIT, 	C },
#endif
#ifdef SIGILL
  { SIGILL, 	C },
#endif
#ifdef SIGTRAP
  { SIGTRAP, 	C },
#endif
#ifdef SIGABRT
  { SIGABRT, 	C },
#endif
#ifdef SIGBUS
  { SIGBUS, 	C + P },
#endif
#ifdef SIGFPE
  { SIGFPE, 	C + P },
#endif
#ifdef SIGUSR1
  { SIGUSR1, 	Q + I + R },
#endif
#ifdef SIGSEGV
  { SIGSEGV, 	C + P },
#endif
#ifdef SIGUSR2
  { SIGUSR2, 	Q + I + R },
#endif
#ifdef SIGPIPE
  { SIGPIPE, 	I },
#endif
#ifdef SIGALRM
  { SIGALRM, 	Q + I + R },
#endif
#ifdef SIGTERM
  { SIGTERM, 	Q + R },
#endif
#ifdef SIGSTKFLT
  { SIGSTKFLT, 	C },
#endif
#ifdef SIGCHLD
  { SIGCHLD, 	Q + I + R },
#endif
#ifdef SIGTSTP
  { SIGTSTP, 	Q + I + R },
#endif
#ifdef SIGTTIN
  { SIGTTIN, 	Q + I + R },
#endif
#ifdef SIGTTOU
  { SIGTTOU, 	Q + I + R },
#endif
#ifdef SIGURG
  { SIGURG, 	Q + I + R },
#endif
#ifdef SIGXCPU
  { SIGXCPU, 	Q + I + R },
#endif
#ifdef SIGXFSZ
  { SIGXFSZ, 	Q + I + R },
#endif
#ifdef SIGVTALRM
  { SIGVTALRM, 	Q + I + R },
#endif
#ifdef SIGPROF
  { SIGPROF, 	Q + I + R },
#endif
#ifdef SIGWINCH
  { SIGWINCH, 	Q + I + R },
#endif
#ifdef SIGIO
  { SIGIO, 	Q + I + R },
#endif
#ifdef SIGPWR
  { SIGPWR, 	Q + I + R },
#endif
#ifdef SIGSYS
  { SIGSYS, 	C },
#endif
#ifdef SIGEMT
  { SIGEMT,	C },
#endif
#ifdef SIGINFO
  { SIGINFO,	Q + I + R },
#endif
#ifdef SIGTHR
  { SIGTHR,	Q + I + R },
#endif
  { -1,		0 }
};
#undef C
#undef I
#undef R
#undef Q
#undef P

/* Handle a signal, for cases where we don't panic.  We can split the
   stack here.  */

static void
sig_handler (int sig)
{
  int i;

#ifdef SIGPROF
  if (sig == SIGPROF)
    {
      /* FIXME.  */
      runtime_sigprof (0, 0, nil, nil);
      return;
    }
#endif

  for (i = 0; runtime_sigtab[i].sig != -1; ++i)
    {
      struct sigaction sa;

      if (runtime_sigtab[i].sig != sig)
	continue;

      if ((runtime_sigtab[i].flags & SigQueue) != 0)
	{
	  if (__go_sigsend (sig)
	      || (runtime_sigtab[sig].flags & SigIgnore) != 0)
	    return;
	  runtime_exit (2);		// SIGINT, SIGTERM, etc
	}

      if (runtime_panicking)
	runtime_exit (2);
      runtime_panicking = 1;

      /* We should do a stack backtrace here.  Until we can do that,
	 we reraise the signal in order to get a slightly better
	 report from the shell.  */

      memset (&sa, 0, sizeof sa);

      sa.sa_handler = SIG_DFL;

      i = sigemptyset (&sa.sa_mask);
      __go_assert (i == 0);

      if (sigaction (sig, &sa, NULL) != 0)
	abort ();

      raise (sig);

      runtime_exit (2);
    }

  __builtin_unreachable ();
}

/* The start of handling a signal which panics.  */

static void
sig_panic_leadin (int sig)
{
  int i;
  sigset_t clear;

  if (runtime_m ()->mallocing)
    {
      runtime_printf ("caught signal while mallocing: %d\n", sig);
      runtime_throw ("caught signal while mallocing");
    }

  /* The signal handler blocked signals; unblock them.  */
  i = sigfillset (&clear);
  __go_assert (i == 0);
  i = sigprocmask (SIG_UNBLOCK, &clear, NULL);
  __go_assert (i == 0);
}

#ifdef SA_SIGINFO

/* Signal dispatch for signals which panic, on systems which support
   SA_SIGINFO.  This is called on the thread stack, and as such it is
   permitted to split the stack.  */

static void
sig_panic_info_handler (int sig, siginfo_t *info,
			void *context __attribute__ ((unused)))
{
  if (runtime_g () == NULL)
    {
      sig_handler (sig);
      return;
    }

  sig_panic_leadin (sig);

  switch (sig)
    {
#ifdef SIGBUS
    case SIGBUS:
      if (info->si_code == BUS_ADRERR && (uintptr_t) info->si_addr < 0x1000)
	runtime_panicstring ("invalid memory address or "
			     "nil pointer dereference");
      runtime_printf ("unexpected fault address %p\n", info->si_addr);
      runtime_throw ("fault");
#endif

#ifdef SIGSEGV
    case SIGSEGV:
      if ((info->si_code == 0
	   || info->si_code == SEGV_MAPERR
	   || info->si_code == SEGV_ACCERR)
	  && (uintptr_t) info->si_addr < 0x1000)
	runtime_panicstring ("invalid memory address or "
			     "nil pointer dereference");
      runtime_printf ("unexpected fault address %p\n", info->si_addr);
      runtime_throw ("fault");
#endif

#ifdef SIGFPE
    case SIGFPE:
      switch (info->si_code)
	{
	case FPE_INTDIV:
	  runtime_panicstring ("integer divide by zero");
	case FPE_INTOVF:
	  runtime_panicstring ("integer overflow");
	}
      runtime_panicstring ("floating point error");
#endif
    }

  /* All signals with SigPanic should be in cases above, and this
     handler should only be invoked for those signals.  */
  __builtin_unreachable ();
}

#else /* !defined (SA_SIGINFO) */

static void
sig_panic_handler (int sig)
{
  if (runtime_g () == NULL)
    {
      sig_handler (sig);
      return;
    }

  sig_panic_leadin (sig);

  switch (sig)
    {
#ifdef SIGBUS
    case SIGBUS:
      runtime_panicstring ("invalid memory address or "
			   "nil pointer dereference");
#endif

#ifdef SIGSEGV
    case SIGSEGV:
      runtime_panicstring ("invalid memory address or "
			   "nil pointer dereference");
#endif

#ifdef SIGFPE
    case SIGFPE:
      runtime_panicstring ("integer divide by zero or floating point error");
#endif
    }

  /* All signals with SigPanic should be in cases above, and this
     handler should only be invoked for those signals.  */
  __builtin_unreachable ();
}

#endif /* !defined (SA_SIGINFO) */

/* Ignore a signal.  This is called on the alternate signal stack so
   it may not split the stack.  */

static void sig_ignore (int) __attribute__ ((no_split_stack));

static void
sig_ignore (int sig __attribute__ ((unused)))
{
}

/* A signal handler used for signals which are not going to panic.
   This is called on the alternate signal stack so it may not split
   the stack.  */

static void
sig_tramp (int) __attribute__ ((no_split_stack));

static void
sig_tramp (int sig)
{
  G *gp;
  M *mp;

  /* We are now running on the stack registered via sigaltstack.
     (Actually there is a small span of time between runtime_siginit
     and sigaltstack when the program starts.)  */
  gp = runtime_g ();
  mp = runtime_m ();

  if (gp != NULL)
    __splitstack_getcontext (&gp->stack_context[0]);

  if (gp != NULL && mp->gsignal != NULL)
    {
      /* We are running on the signal stack.  Set the split stack
	 context so that the stack guards are checked correctly.  */
#ifdef USING_SPLIT_STACK
      __splitstack_setcontext (&mp->gsignal->stack_context[0]);
#endif
    }

  sig_handler (sig);

  /* We are going to return back to the signal trampoline and then to
     whatever we were doing before we got the signal.  Restore the
     split stack context so that stack guards are checked
     correctly.  */

  if (gp != NULL)
    {
#ifdef USING_SPLIT_STACK
      __splitstack_setcontext (&gp->stack_context[0]);
#endif
    }
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

  i = sigfillset (&sa.sa_mask);
  __go_assert (i == 0);

  for (i = 0; runtime_sigtab[i].sig != -1; ++i)
    {
      if (runtime_sigtab[i].flags == 0)
	continue;
      if ((runtime_sigtab[i].flags & SigQueue) != queue)
	continue;

      if ((runtime_sigtab[i].flags & (SigCatch | SigQueue)) != 0)
	{
	  if ((runtime_sigtab[i].flags & SigPanic) == 0)
	    {
	      sa.sa_flags = SA_ONSTACK;
	      sa.sa_handler = sig_tramp;
	    }
	  else
	    {
#ifdef SA_SIGINFO
	      sa.sa_flags = SA_SIGINFO;
	      sa.sa_sigaction = sig_panic_info_handler;
#else
	      sa.sa_flags = 0;
	      sa.sa_handler = sig_panic_handler;
#endif
	    }
	}
      else
	{
	  sa.sa_flags = SA_ONSTACK;
	  sa.sa_handler = sig_ignore;
	}

      if ((runtime_sigtab[i].flags & SigRestart) != 0)
	sa.sa_flags |= SA_RESTART;

      if (sigaction (runtime_sigtab[i].sig, &sa, NULL) != 0)
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
      sa.sa_handler = sig_handler;
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
