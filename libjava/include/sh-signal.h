// sh-signal.h - Catch runtime signals and turn them into exceptions
// on a SuperH based Linux system.

/* Copyright (C) 2004, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */


#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <sys/syscall.h>

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

/* The third parameter to the signal handler points to something with
 * this structure defined in asm/ucontext.h, but the name clashes with
 * struct ucontext from sys/ucontext.h so this private copy is used.  */
typedef struct _sig_ucontext {
  unsigned long uc_flags;
  struct _sig_ucontext *uc_link;
  stack_t uc_stack;
  struct sigcontext uc_mcontext;
  sigset_t uc_sigmask;
} sig_ucontext_t;

#define SIGNAL_HANDLER(_name)						\
  static void _name (int , siginfo_t *, sig_ucontext_t *_uc)

#define MAKE_THROW_FRAME(_exception)

/* For an explanation why we cannot simply use sigaction to
   install the handlers, see i386-signal.h.  */

/* We use kernel_old_sigaction here because we're calling the kernel
   directly rather than via glibc.  The sigaction structure that the
   syscall uses is a different shape from the one in userland and not
   visible to us in a header file so we define it here.  */

struct kernel_old_sigaction {
  void (*k_sa_handler) (int, siginfo_t *, sig_ucontext_t *);
  unsigned long k_sa_mask;
  unsigned long k_sa_flags;
  void (*k_sa_restorer) (void);
};

#define INIT_SEGV							\
do									\
  {									\
    struct kernel_old_sigaction kact;					\
    kact.k_sa_handler = catch_segv;					\
    kact.k_sa_mask = 0;							\
    kact.k_sa_flags = SA_SIGINFO | SA_NODEFER;				\
    syscall (SYS_sigaction, SIGSEGV, &kact, NULL);			\
  }									\
while (0)  

#define INIT_FPE							\
do									\
  {									\
    struct kernel_old_sigaction kact;					\
    kact.k_sa_handler = catch_fpe;					\
    kact.k_sa_mask = 0;							\
    kact.k_sa_flags = SA_SIGINFO | SA_NODEFER;				\
    syscall (SYS_sigaction, SIGFPE, &kact, NULL);			\
  }									\
while (0)

#endif /* JAVA_SIGNAL_H */
