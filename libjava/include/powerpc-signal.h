// powerpc-signal.h - Catch runtime signals and turn them into exceptions
// on a powerpc based Linux system.

/* Copyright (C) 2003, 2006  Free Software Foundation

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

#define SIGNAL_HANDLER(_name)						\
  static void _name (int /* _signal */, struct sigcontext *_sc)

/* MD_FALLBACK_FRAME_STATE_FOR takes care of special casing PC
   before the faulting instruction, so we don't need to do anything
   here.  */

#define MAKE_THROW_FRAME(_exception)

/* For an explanation why we cannot simply use sigaction to
   install the handlers, see i386-signal.h.  */

/* We use kernel_old_sigaction here because we're calling the kernel
   directly rather than via glibc. The sigaction structure that the
   syscall uses is a different shape from the one in userland and not
   visible to us in a header file so we define it here.
   Additionally we want a proper prototype for the handler function
   with the struct sigcontext pointer passed by the kernel as the 2nd
   argument, which isn't there in userland headers.

   Note that we explicitly avoid the SA_SIGINFO flag in INIT_SEGV and
   INIT_FPE below. Using the ucontext pointer passed as 3rd argument
   of a SA_SIGINFO type handler would need complicated backwards
   compatibility hacks in MAKE_THROW_FRAME, as the ucontext layout
   on PPC changed during the 2.5 kernel series.  */

#ifndef __powerpc64__
struct kernel_old_sigaction {
  void (*k_sa_handler) (int, struct sigcontext *);
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
    kact.k_sa_flags = 0;						\
    if (syscall (SYS_sigaction, SIGSEGV, &kact, NULL) != 0)		\
      __asm__ __volatile__ (".long 0");					\
  }									\
while (0)  

#define INIT_FPE							\
do									\
  {									\
    struct kernel_old_sigaction kact;					\
    kact.k_sa_handler = catch_fpe;					\
    kact.k_sa_mask = 0;							\
    kact.k_sa_flags = 0;						\
    if (syscall (SYS_sigaction, SIGFPE, &kact, NULL) != 0)		\
      __asm__ __volatile__ (".long 0");					\
  }									\
while (0)

#else /* powerpc64 */

struct kernel_sigaction
{
  void (*k_sa_handler) (int, struct sigcontext *);
  unsigned long k_sa_flags;
  void (*k_sa_restorer)(void);
  unsigned long k_sa_mask;
};

#define INIT_SEGV							\
do									\
  {									\
    struct kernel_sigaction kact;					\
    memset (&kact, 0, sizeof (kact));					\
    kact.k_sa_handler = catch_segv;					\
    if (syscall (SYS_rt_sigaction, SIGSEGV, &kact, NULL, 8) != 0)	\
      __asm__ __volatile__ (".long 0");					\
  }									\
while (0)  

#define INIT_FPE							\
do									\
  {									\
    struct kernel_sigaction kact;					\
    memset (&kact, 0, sizeof (kact));					\
    kact.k_sa_handler = catch_fpe;					\
    if (syscall (SYS_rt_sigaction, SIGFPE, &kact, NULL, 8) != 0)	\
      __asm__ __volatile__ (".long 0");					\
  }									\
while (0)
#endif

#endif /* JAVA_SIGNAL_H */
