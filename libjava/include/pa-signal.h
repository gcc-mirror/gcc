// pa-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <ucontext.h>
#include <sys/syscall.h>

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

#define SIGNAL_HANDLER(_name) 					\
static void _Jv_##_name (int _dummy, siginfo_t *_info, void *arg)

#define MAKE_THROW_FRAME(_exception)				\
do								\
{								\
  struct ucontext *uc = (struct ucontext *)arg;			\
  struct sigcontext *sc = &uc->uc_mcontext;			\
  (void)_dummy;							\
  (void)_info;							\
  /* Advance the program counter so that it is after the start 	\
     of the instruction:  the exception handler expects		\
     the PC to point to the instruction after a call. */	\
  sc->sc_iaoq[0] = sc->sc_iaoq[1];				\
  sc->sc_iaoq[1] += 4;						\
}								\
while (0)

#define INIT_SEGV						\
do								\
  {								\
    struct sigaction act;					\
    act.sa_sigaction = _Jv_catch_segv;      			\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = SA_SIGINFO;	       				\
    syscall (SYS_rt_sigaction, SIGSEGV, &act, NULL, _NSIG / 8);	\
  }								\
while (0)  

#define INIT_FPE						\
do								\
  { 								\
    struct sigaction act;					\
    act.sa_sigaction = _Jv_catch_fpe;				\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = SA_SIGINFO;		       			\
    syscall (SYS_rt_sigaction, SIGFPE, &act, NULL, _NSIG / 8);	\
  }								\
while (0)  

#endif /* JAVA_SIGNAL_H */
