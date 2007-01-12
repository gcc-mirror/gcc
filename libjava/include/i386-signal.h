// i386-signal.h - Catch runtime signals and turn them into exceptions
// on an i386 based Linux system.

/* Copyright (C) 1998, 1999, 2001, 2002, 2006, 2007  Free Software Foundation

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

#define SIGNAL_HANDLER(_name)					\
static void _Jv_##_name (int, siginfo_t *,			\
			 void *_p __attribute__ ((__unused__)))

#define HANDLE_DIVIDE_OVERFLOW						\
do									\
{									\
  struct ucontext *_uc = (struct ucontext *)_p;				\
  gregset_t &_gregs = _uc->uc_mcontext.gregs;				\
  unsigned char *_eip = (unsigned char *)_gregs[REG_EIP];		\
									\
  /* According to the JVM spec, "if the dividend is the negative	\
   * integer of largest possible magnitude for the type and the		\
   * divisor is -1, then overflow occurs and the result is equal to	\
   * the dividend.  Despite the overflow, no exception occurs".		\
									\
   * We handle this by inspecting the instruction which generated the	\
   * signal and advancing ip to point to the following instruction.	\
   * As the instructions are variable length it is necessary to do a	\
   * little calculation to figure out where the following instruction	\
   * actually is.							\
									\
  */									\
									\
  /* Detect a signed division of Integer.MIN_VALUE.  */			\
  if (_eip[0] == 0xf7)							\
    {									\
      bool _min_value_dividend = false;					\
      unsigned char _modrm = _eip[1];					\
									\
      if (((_modrm >> 3) & 7) == 7) /* Signed divide */			\
	{								\
	  _min_value_dividend =						\
	    _gregs[REG_EAX] == (greg_t)0x80000000UL;			\
	}								\
									\
      if (_min_value_dividend)						\
	{								\
	  unsigned char _rm = _modrm & 7;				\
	  _gregs[REG_EDX] = 0; /* the remainder is zero */		\
	  switch (_modrm >> 6)						\
	    {								\
	    case 0:  /* register indirect */				\
	      if (_rm == 5)   /* 32-bit displacement */			\
		_eip += 4;						\
	      if (_rm == 4)  /* A SIB byte follows the ModR/M byte */	\
		_eip += 1;						\
	      break;							\
	    case 1:  /* register indirect + 8-bit displacement */	\
	      _eip += 1;						\
	      if (_rm == 4)  /* A SIB byte follows the ModR/M byte */	\
		_eip += 1;						\
	      break;							\
	    case 2:  /* register indirect + 32-bit displacement */	\
	      _eip += 4;						\
	      if (_rm == 4)  /* A SIB byte follows the ModR/M byte */	\
		_eip += 1;						\
	      break;							\
	    case 3:							\
	      break;							\
	    }								\
	  _eip += 2;							\
	  _gregs[REG_EIP] = (greg_t)_eip;				\
	  return;							\
	}								\
    }									\
}									\
while (0)

/* We use kernel_sigaction here because we're calling the kernel
   directly rather than via glibc.  The sigaction structure that the
   syscall uses is a different shape from the one in userland and not
   visible to us in a header file so we define it here.  */

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
   "	.align 16\n"				\
   "__" #name ":\n"				\
   "	movl $" #syscall ", %eax\n"		\
   "	int  $0x80"				\
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

#define INIT_FPE						\
do								\
  {								\
    struct kernel_sigaction act;				\
    act.k_sa_sigaction = _Jv_catch_fpe;				\
    sigemptyset (&act.k_sa_mask);				\
    act.k_sa_flags = SA_SIGINFO|0x4000000;			\
    act.k_sa_restorer = restore_rt;				\
    syscall (SYS_rt_sigaction, SIGFPE, &act, NULL, _NSIG / 8);	\
  }								\
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

 * Also, there may not be any unwind info in the linuxthreads
 * library's signal handlers and so we can't unwind through them
 * anyway.  */

#endif /* JAVA_SIGNAL_H */
  
