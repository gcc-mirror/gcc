// sparc-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <ucontext.h>

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

#define SIGNAL_HANDLER(_name) 						\
static void _name (int _dummy, siginfo_t *_info, void *arg)

#ifdef __arch64__
#define FLUSH_REGISTER_WINDOWS					\
  asm volatile ("flushw");
#else
#define FLUSH_REGISTER_WINDOWS					\
  asm volatile ("ta 3");
#endif

#define MAKE_THROW_FRAME(_exception)				\
do								\
{								\
  ucontext_t *_context = (ucontext_t *) arg;                    \
  (void)_dummy;							\
  (void)_info;							\
  register long sp = _context->uc_mcontext.gregs[REG_SP];	\
  register long retaddr = _context->uc_mcontext.gregs[REG_O7];	\
  FLUSH_REGISTER_WINDOWS;					\
  asm volatile ("mov %0, %%i6; mov %1, %%i7"			\
		: : "r"(sp), "r"(retaddr));			\
}								\
while (0)

#define INIT_SEGV						\
do								\
  {								\
    struct sigaction act;					\
    act.sa_sigaction = catch_segv;				\
    act.sa_flags = SA_SIGINFO | SA_NODEFER;			\
    sigemptyset (&act.sa_mask);					\
    sigaction (SIGSEGV, &act, NULL);				\
  }								\
while (0)							
								
#define INIT_FPE						\
do								\
  {								\
    struct sigaction act;					\
    act.sa_flags = SA_SIGINFO | SA_NODEFER;			\
    act.sa_sigaction = catch_fpe;				\
    sigemptyset (&act.sa_mask);					\
    sigaction (SIGFPE, &act, NULL);				\
  }								\
while (0)

#endif /* JAVA_SIGNAL_H */
