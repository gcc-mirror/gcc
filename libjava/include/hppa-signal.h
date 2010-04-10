/* hppa-signal.h - Catch runtime signals and turn them into exceptions,
   on a HP-UX 11 PA system.  */

/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

#define SIGNAL_HANDLER(_name)						\
static void  _name (int _dummy __attribute__ ((unused)),		\
		    siginfo_t *_info __attribute__ ((__unused__)),	\
		    void *arg __attribute__ ((__unused__)))

#define MAKE_THROW_FRAME(_exception)

#define INIT_SEGV				\
do						\
  {						\
    struct sigaction sa;			\
    sa.sa_sigaction = catch_segv;		\
    sigemptyset (&sa.sa_mask);			\
    sa.sa_flags = SA_SIGINFO | SA_NODEFER;	\
    sigaction (SIGSEGV, &sa, NULL);		\
  }						\
while (0)

#define INIT_FPE				\
do						\
  {						\
    struct sigaction sa;			\
    sa.sa_sigaction = catch_fpe;		\
    sigemptyset (&sa.sa_mask);			\
    sa.sa_flags = SA_SIGINFO | SA_NODEFER;	\
    sigaction (SIGFPE, &sa, NULL);		\
  }						\
while (0)

#endif /* JAVA_SIGNAL_H */
