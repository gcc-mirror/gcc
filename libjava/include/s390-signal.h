// s390-signal.h - Catch runtime signals and turn them into exceptions
// on an s390 based Linux system.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

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
static void _name (int, siginfo_t *, void *)

/* We no longer need to fiddle with the PSW address in the signal handler;
   this is now all handled correctly in MD_FALLBACK_FRAME_STATE_FOR.  */
#define MAKE_THROW_FRAME(_exception)


/* For an explanation why we cannot simply use sigaction to
   install the handlers, see i386-signal.h.  */

/* We use old_kernel_sigaction here because we're calling the kernel
   directly rather than via glibc.  The sigaction structure that the
   syscall uses is a different shape from the one in userland and not
   visible to us in a header file so we define it here.  */

struct old_s390_kernel_sigaction {
	void (*k_sa_handler) (int, siginfo_t *, void *);
	unsigned long k_sa_mask;
	unsigned long k_sa_flags;
	void (*sa_restorer) (void);
};

#define INIT_SEGV					\
do							\
  {							\
    struct old_s390_kernel_sigaction kact;		\
    kact.k_sa_handler = catch_segv;			\
    kact.k_sa_mask = 0;					\
    kact.k_sa_flags = SA_SIGINFO;			\
    syscall (SYS_sigaction, SIGSEGV, &kact, NULL);	\
  }							\
while (0)  

#define INIT_FPE						\
do								\
  {								\
    struct old_s390_kernel_sigaction kact;			\
    kact.k_sa_handler = catch_fpe;				\
    kact.k_sa_mask = 0;						\
    kact.k_sa_flags = SA_SIGINFO;				\
    syscall (SYS_sigaction, SIGFPE, &kact, NULL);		\
  }								\
while (0)  

#endif /* JAVA_SIGNAL_H */

