// mips-signal.h - Catch runtime signals and turn them into exceptions
// on an mips based Linux system. 

/* Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Adapted from sparc-signal.h and powerpc-signal.h
   by David Daney <ddaney@avtrex.com> */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>
#include <unistd.h>
#include <sys/syscall.h>
/* #include <asm/ucontext.h> structures we use are here but clash with
   sys/ucontext.h included by java-signal.h from prims.cc */

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

/* The third parameter to the signal handler points to something with
 * this structure defined in asm/ucontext.h, but the name clashes with
 * struct ucontext from sys/ucontext.h so this private copy is used. */
typedef struct _sig_ucontext {
    unsigned long	  uc_flags;
    struct _sig_ucontext  *uc_link;
    stack_t		  uc_stack;
    struct sigcontext uc_mcontext;
    sigset_t	  uc_sigmask;
} sig_ucontext_t;

/* We use kernel_sigaction here because we're calling the kernel
   directly rather than via glibc. The sigaction structure that the
   syscall uses is a different shape from the one in userland and not
   visible to us in a header file so we define it here.
   Additionally we want a proper prototype for the handler function
   with the struct sigcontext pointer passed by the kernel as the 2nd
   argument, which isn't there in userland headers. */

struct kernel_sigaction {
    unsigned int k_sa_flags;
    void       (*k_sa_handler) (int, siginfo_t *, sig_ucontext_t *);
    sigset_t     k_sa_mask;
    void       (*k_sa_restorer)(void);
    int          k_sa_resv[1];	/* reserved */
};



#define SIGNAL_HANDLER(_name) \
static void _name (int _dummy __attribute__ ((__unused__)), \
		   siginfo_t *_info __attribute__ ((__unused__)), \
		   sig_ucontext_t *_arg __attribute__ ((__unused__)))

/*
 *  MIPS leaves pc pointing at the faulting instruction, but the
 *  unwinder expects it to point to the following instruction
 */

#define MAKE_THROW_FRAME(_exception) \
do                                   \
{                                    \
  _arg->uc_mcontext.sc_pc += 4;      \
  (void)_dummy;                      \
  (void)_info;                       \
}                                    \
while (0)

/* For an explanation why we cannot simply use sigaction to
   install the handlers, see i386-signal.h.  */

#define INIT_SEGV                                    \
do                                                   \
  {                                                  \
    struct kernel_sigaction kact;                    \
    kact.k_sa_handler = catch_segv;                  \
    kact.k_sa_flags = SA_SIGINFO | SA_NODEFER;       \
    sigemptyset (&kact.k_sa_mask);                   \
    syscall (SYS_sigaction, SIGSEGV, &kact, NULL);   \
  }                                                  \
while (0)

#define INIT_FPE                                     \
do                                                   \
  {                                                  \
    struct kernel_sigaction kact;                    \
    kact.k_sa_handler = catch_fpe;                   \
    kact.k_sa_flags = SA_SIGINFO | SA_NODEFER;       \
    sigemptyset (&kact.k_sa_mask);                   \
    syscall (SYS_sigaction, SIGFPE, &kact, NULL);    \
  }                                                  \
while (0)

#undef HANDLE_DIVIDE_OVERFLOW

#endif /* JAVA_SIGNAL_H */
  
