// i386-signal.h - Catch runtime signals and turn them into exceptions
// on an i386 based Linux system.

/* Copyright (C) 1998, 1999, 2001, 2002  Free Software Foundation

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

#define SIGNAL_HANDLER(_name)	\
static void _name (int _dummy)

#define MAKE_THROW_FRAME(_exception)					\
do									\
{									\
  void **_p = (void **)&_dummy;						\
  struct sigcontext_struct *_regs = (struct sigcontext_struct *)++_p;	\
									\
  /* Advance the program counter so that it is after the start of the	\
     instruction:  the x86 exception handler expects			\
     the PC to point to the instruction after a call. */		\
  _regs->eip += 2;							\
									\
}									\
while (0)

#define HANDLE_DIVIDE_OVERFLOW						\
do									\
{									\
  void **_p = (void **)&_dummy;						\
  struct sigcontext_struct *_regs = (struct sigcontext_struct *)++_p;	\
									\
  register unsigned char *_eip = (unsigned char *)_regs->eip;		\
									\
  /* According to the JVM spec, "if the dividend is the negative	\
   * integer of the smallest magnitude and the divisor is -1, then	\
   * overflow occurs and the result is equal to the dividend.  Despite	\
   * the overflow, no exception occurs".				\
									\
   * We handle this by inspecting the instruction which generated the	\
   * signal and advancing eip to point to the following instruction.	\
   * As the instructions are variable length it is necessary to do a	\
   * little calculation to figure out where the following instruction	\
   * actually is.							\
									\
   */									\
									\
  if (_eip[0] == 0xf7)							\
    {									\
      unsigned char _modrm = _eip[1];					\
									\
      if (_regs->eax == 0x80000000					\
	  && ((_modrm >> 3) & 7) == 7) /* Signed divide */		\
	{								\
	  _regs->edx = 0; /* the remainder is zero */			\
	  switch (_modrm >> 6)						\
	    {								\
	    case 0:							\
	      if ((_modrm & 7) == 5)					\
		_eip += 4;						\
	      break;							\
	    case 1:							\
	      _eip += 1;						\
	      break;							\
	    case 2:							\
	      _eip += 4;						\
	      break;							\
	    case 3:							\
	      break;							\
	    }								\
	  _eip += 2;							\
	  _regs->eip = (unsigned long)_eip;				\
	  return;							\
	}								\
      else								\
	{								\
	  /* Advance the program counter so that it is after the start	\
	     of the instruction: this is because the x86 exception	\
	     handler expects the PC to point to the instruction after a	\
	     call. */							\
	  _regs->eip += 2;						\
	}								\
    }									\
}									\
while (0)

/* We use old_kernel_sigaction here because we're calling the kernel
   directly rather than via glibc.  The sigaction structure that the
   syscall uses is a different shape from the one in userland and not
   visible to us in a header file so we define it here.  */

struct old_i386_kernel_sigaction {
	void (*k_sa_handler) (int);
	unsigned long k_sa_mask;
	unsigned long k_sa_flags;
	void (*sa_restorer) (void);
};

#define RESTORE(name, syscall) RESTORE2 (name, syscall)
# define RESTORE2(name, syscall) \
asm						\
  (						\
   ".text\n"					\
   ".byte 0  # Yes, this really is necessary\n" \
   "	.align 8\n"				\
   "__" #name ":\n"				\
   "	popl %eax\n"				\
   "	movl $" #syscall ", %eax\n"		\
   "	int  $0x80"				\
   );

RESTORE (restore, __NR_sigreturn)
static void restore (void) asm ("__restore");

#define INIT_SEGV					\
do							\
  {							\
    struct old_i386_kernel_sigaction kact;		\
    kact.k_sa_handler = catch_segv;			\
    kact.k_sa_mask = 0;					\
    kact.k_sa_flags = 0x4000000;			\
    kact.sa_restorer = restore;				\
    syscall (SYS_sigaction, SIGSEGV, &kact, NULL);	\
  }							\
while (0)  

#define INIT_FPE					\
do							\
  {							\
    struct old_i386_kernel_sigaction kact;		\
    kact.k_sa_handler = catch_fpe;			\
    kact.k_sa_mask = 0;					\
    kact.k_sa_flags = 0x4000000;			\
    kact.sa_restorer = restore;				\
    syscall (SYS_sigaction, SIGFPE, &kact, NULL);	\
  }							\
while (0)  

/* You might wonder why we use syscall(SYS_sigaction) in INIT_FPE
 * instead of the standard sigaction().  This is necessary because of
 * the shenanigans above where we increment the PC saved in the
 * context and then return.  This trick will only work when we are
 * called _directly_ by the kernel, because linuxthreads wraps signal
 * handlers and its wrappers do not copy the sigcontext struct back
 * when returning from a signal handler.  If we return from our divide
 * handler to a linuxthreads wrapper, we will lose the PC adjustment
 * we made and return to the faulting instruction again.  Using
 * syscall(SYS_sigaction) causes our handler to be called directly
 * by the kernel, bypassing any wrappers.

 * Also, there is at the present time no unwind info in the
 * linuxthreads library's signal handlers and so we can't unwind
 * through them anyway.  */

#endif /* JAVA_SIGNAL_H */
  
