// sparc-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

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

#define FLUSH_REGISTER_WINDOWS					\
  asm volatile ("ta 3");

#define MAKE_THROW_FRAME					\
do								\
{								\
  ucontext_t *_context = (ucontext_t *) arg;                    \
  (void)_dummy;							\
  (void)_info;							\
  register int sp = _context->uc_mcontext.gregs[REG_SP];	\
  register int retaddr = _context->uc_mcontext.gregs[REG_O7];	\
  FLUSH_REGISTER_WINDOWS;					\
  asm volatile ("mov %0, %%i6; mov %1, %%i7"			\
		: : "r"(sp), "r"(retaddr));			\
}								\
while (0)

#define INIT_SEGV						\
do								\
  {								\
    nullp = new java::lang::NullPointerException ();		\
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
    arithexception = new java::lang::ArithmeticException 	\
      (JvNewStringLatin1 ("/ by zero"));			\
    struct sigaction act;					\
    act.sa_flags = SA_SIGINFO | SA_NODEFER;			\
    act.sa_sigaction = catch_fpe;				\
    sigemptyset (&act.sa_mask);					\
    sigaction (SIGFPE, &act, NULL);				\
  }								\
while (0)

#endif /* JAVA_SIGNAL_H */
