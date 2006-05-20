/* hppa-signal.h - Catch runtime signals and turn them into exceptions,
   on a HP-UX 11 PA system.  */

/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* This file is really more of a specification.  The rest of the system
   should be arranged so that this Just Works.  */

#ifndef JAVA_SIGNAL_H
# define JAVA_SIGNAL_H 1

#include <sys/types.h>
#include <signal.h>
#include <sys/syscall.h>
#include <unistd.h>

# define HANDLE_SEGV 1
# undef HANDLE_FPE

#define SIGNAL_HANDLER(_name)						\
  static void _name (int _dummy __attribute__ ((unused)),		\
		     siginfo_t *_info __attribute__ ((__unused__)),	\
		     void *arg __attribute__ ((__unused__)))

#define MAKE_THROW_FRAME(_exception)					\
do									\
{									\
  ucontext_t *_context = (ucontext_t *) arg;				\
  (void)_dummy;								\
  (void)_info;								\
  mcontext_t *mc = &(_context->uc_mcontext);				\
  SetSSReg (mc, ss_pcoq_head, GetSSReg (mc, ss_pcoq_tail));		\
  SetSSReg (mc, ss_pcsq_head, GetSSReg (mc, ss_pcsq_tail));		\
  /* This part is not quit right if the head pc was pointing		\
     at a branch.  The tail needs to be adjusted to the branch		\
     target if the branch is taken.  The tail space register		\
     may need adjustment as well if the branch is an interspace		\
     branch.  */							\
  SetSSReg (mc, ss_pcoq_tail, (GetSSReg (mc, ss_pcoq_tail) + 4));	\
									\
 }									\
while (0)

# define INIT_SEGV				\
  do {						\
      struct sigaction sa;			\
      sa.sa_sigaction = catch_segv;		\
      sigemptyset (&sa.sa_mask);		\
      sa.sa_flags = SA_SIGINFO | SA_NODEFER;	\
      sigaction (SIGSEGV, &sa, NULL);		\
    } while (0)

# define INIT_FPE				\
  do {						\
      struct sigaction sa;			\
      sa.sa_sigaction = catch_fpe;		\
      sigemptyset (&sa.sa_mask);		\
      sa.sa_flags = SA_SIGINFO | SA_NODEFER;	\
      sigaction (SIGFPE, &sa, NULL);		\
    } while (0)

#endif /* JAVA_SIGNAL_H */
