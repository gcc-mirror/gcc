// x86_64-signal.h - Catch runtime signals and turn them into exceptions
// on an x86_64 based GNU/Linux system.

/* Copyright (C) 2003, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */


#ifdef __x86_64__

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <sys/syscall.h>

#define HANDLE_SEGV 1

#define SIGNAL_HANDLER(_name)	\
static void _Jv_##_name (int, siginfo_t *, void *_p)

extern "C" 
{
  struct kernel_sigaction 
  {
    void (*k_sa_sigaction)(int,siginfo_t *,void *);
    unsigned long k_sa_flags;
    void (*k_sa_restorer) (void);
    sigset_t k_sa_mask;
  };
}

#define MAKE_THROW_FRAME(_exception)

#define RESTORE(name, syscall) RESTORE2 (name, syscall)
#define RESTORE2(name, syscall)			\
asm						\
  (						\
   ".text\n"					\
   ".byte 0  # Yes, this really is necessary\n" \
   ".align 16\n"				\
   "__" #name ":\n"				\
   "	movq $" #syscall ", %rax\n"		\
   "	syscall\n"				\
   );

/* The return code for realtime-signals.  */
RESTORE (restore_rt, __NR_rt_sigreturn)
void restore_rt (void) asm ("__restore_rt")
  __attribute__ ((visibility ("hidden")));

#define INIT_SEGV						\
do								\
  {								\
    struct kernel_sigaction act;				\
    act.k_sa_sigaction = _Jv_catch_segv;			\
    sigemptyset (&act.k_sa_mask);				\
    act.k_sa_flags = SA_SIGINFO|0x4000000;			\
    act.k_sa_restorer = restore_rt;				\
    syscall (SYS_rt_sigaction, SIGSEGV, &act, NULL, _NSIG / 8);	\
  }								\
while (0)  

/* We use syscall(SYS_rt_sigaction) in INIT_SEGV instead of
 * sigaction() because on some systems the pthreads wrappers for
 * signal handlers are not compiled with unwind information, so it's
 * not possible to unwind through them.  This is a problem that will
 * go away if all systems ever have pthreads libraries that are
 * compiled with unwind info.  */

#endif /* JAVA_SIGNAL_H */

#else /* __x86_64__ */

/* This is for the 32-bit subsystem on x86-64.  */

#define sigcontext_struct sigcontext
#include <java-signal-aux.h>

#endif /* __x86_64__ */
