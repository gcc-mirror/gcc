// dwarf2-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libgcj.

   Use this file for a target for which the dwarf2 unwinder in libgcc
   can unwind through signal handlers.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <sys/syscall.h>

#define HANDLE_SEGV 1
#undef HANDLE_FPE

#define SIGNAL_HANDLER(_name)	\
static void _Jv_##_name (int, siginfo_t *, void *_p)

class java::lang::Throwable;

// Unwind the stack to the point at which the signal was generated and
// then throw an exception.  With the dwarf2 unwinder we don't usually
// need to do anything, with some minor exceptions.

#ifdef __alpha__
#define MAKE_THROW_FRAME(_exception)					\
do									\
{									\
  /* Alpha either leaves PC pointing at a faulting instruction or the	\
   following instruction, depending on the signal.  SEGV always does	\
   the former, so we adjust the saved PC to point to the following	\
   instruction; this is what the handler in libgcc expects.  */		\
  struct sigcontext *_sc = (struct sigcontext *)_p;			\
  _sc->sc_pc += 4;							\
}									\
while (0)
#else
#define MAKE_THROW_FRAME(_exception)		\
do						\
{						\
  (void)_p;					\
}						\
while (0)
#endif

#define INIT_SEGV						\
do								\
  {								\
    nullp = new java::lang::NullPointerException ();    	\
    struct sigaction act;					\
    act.sa_sigaction = _Jv_catch_segv;      			\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = SA_SIGINFO;	       				\
    syscall (SYS_sigaction, SIGSEGV, &act, NULL);		\
  }								\
while (0)  

#define INIT_FPE						\
do								\
  { 								\
    arithexception = new java::lang::ArithmeticException 	\
      (JvNewStringLatin1 ("/ by zero"));			\
    struct sigaction act;					\
    act.sa_sigaction = _Jv_catch_fpe;				\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = SA_SIGINFO;		       			\
    syscall (SYS_sigaction, SIGFPE, &act, NULL);		\
  }								\
while (0)  

/* We use syscall(SYS_sigaction) in INIT_SEGV and INIT_FPE instead of
 * sigaction() because on some systems the pthreads wrappers for
 * signal handlers are not compiled with unwind information, so it's
 * not possible to unwind through them.  This is a problem that will
 * go away once all systems have pthreads libraries that are
 * compiled with full unwind info.  */

#endif /* JAVA_SIGNAL_H */
