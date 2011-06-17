// posix-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999, 2000, 2009, 2011  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

/* Different implementations of MD_FALLBACK_FRAME_STATE_FOR either require
   SA_SIGINFO being set or fail if so.  Cf. gcc/ada/init.c
   (__gnat_install_handler) for details.  */

#if (defined __alpha__ && defined __osf__) \
  || (defined __sun__ && defined __svr4__)
#define SA_FLAGS SA_NODEFER | SA_SIGINFO
#elif defined __sgi__
#define SA_FLAGS SA_NODEFER
#else
#error Must define SA_FLAGS.
#endif

#if SA_FLAGS & SA_SIGINFO
#define SIGNAL_HANDLER(_name)						\
static void _Jv_##_name (int,						\
			 siginfo_t *_si __attribute__ ((__unused__)),	\
			 void *_uc __attribute__ ((__unused__)))
#define sa_signal_handler sa_sigaction
#else
#define SIGNAL_HANDLER(_name)						\
static void _Jv_##_name (int)
#define sa_signal_handler sa_handler
#endif

#define MAKE_THROW_FRAME(_exception)

#define _INIT_SIG_HANDLER(_SIG, _ACTION)     				\
do                                           				\
  {									\
    struct sigaction act;						\
    act.sa_signal_handler = _Jv_##_ACTION;				\
    act.sa_flags = SA_FLAGS;						\
    sigemptyset (&act.sa_mask);						\
    sigaction(_SIG, &act, NULL);					\
  }									\
while (0)

#define INIT_SEGV	_INIT_SIG_HANDLER (SIGSEGV, catch_segv)
#define INIT_FPE	_INIT_SIG_HANDLER (SIGFPE, catch_fpe)

#endif /* JAVA_SIGNAL_H */
