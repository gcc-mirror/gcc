// sparc-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999, 2000, 2009  Free Software Foundation

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
static void _Jv_##_name (int,						\
			 siginfo_t *_si __attribute__ ((__unused__)),	\
			 void *_uc __attribute__ ((__unused__)))

#define MAKE_THROW_FRAME(_exception)

#define INIT_SEGV						\
do								\
  {								\
    struct sigaction act;					\
    act.sa_sigaction = _Jv_catch_segv;				\
    act.sa_flags = SA_SIGINFO | SA_NODEFER;			\
    sigemptyset (&act.sa_mask);					\
    sigaction (SIGSEGV, &act, NULL);				\
  }								\
while (0)							
								
#define INIT_FPE						\
do								\
  {								\
    struct sigaction act;					\
    act.sa_sigaction = _Jv_catch_fpe;				\
    act.sa_flags = SA_SIGINFO | SA_NODEFER;			\
    sigemptyset (&act.sa_mask);					\
    sigaction (SIGFPE, &act, NULL);				\
  }								\
while (0)

#endif /* JAVA_SIGNAL_H */
