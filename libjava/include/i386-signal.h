// i386-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* This technique should work for all i386 based Unices which conform
 * to iBCS2.  This includes all versions of Linux more recent than 1.3 
 */


#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#include <signal.h>

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

#define SIGNAL_HANDLER(_name)	\
static void _name (int _dummy)

#define MAKE_THROW_FRAME						\
do									\
{									\
  void **_p = (void **)&_dummy;						\
  struct sigcontext_struct *_regs = (struct sigcontext_struct *)++_p;	\
									\
  register unsigned long _ebp = _regs->ebp;				\
  register unsigned char *_eip = (unsigned char *)_regs->eip;		\
									\
  /* Advance the program counter so that it is after the start of the	\
     instruction:  the x86 exception handler expects			\
     the PC to point to the instruction after a call. */		\
  _eip += 2;								\
									\
  asm volatile ("mov %0, (%%ebp); mov %1, 4(%%ebp)"			\
		: : "r"(_ebp), "r"(_eip));				\
}									\
while (0)

#define HANDLE_DIVIDE_OVERFLOW						\
do									\
{									\
  void **_p = (void **)&_dummy;						\
  struct sigcontext_struct *_regs = (struct sigcontext_struct *)++_p;	\
									\
  register unsigned long *_ebp = (unsigned long *)_regs->ebp;		\
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
      else if (((_modrm >> 3) & 7) == 6) /* Unsigned divide */		\
	{								\
	  /* We assume that unsigned divisions are in library code, so	\
	   * we throw one level down the stack, which was hopefully	\
	   * the place that called the library routine.  This will	\
	   * break if the library is ever compiled with			\
	   * -fomit-frame-pointer, but at least this way we've got a	\
	   * good chance of finding the exception handler. */		\
									\
	  _eip = (unsigned char *)_ebp[1];				\
	  _ebp = (unsigned long *)_ebp[0];				\
	}								\
      else								\
	{								\
	  /* Advance the program counter so that it is after the start	\
	     of the instruction: this is because the x86 exception	\
	     handler expects the PC to point to the instruction after a	\
	     call. */							\
	  _eip += 2;							\
	}								\
    }									\
									\
  asm volatile ("mov %0, (%%ebp); mov %1, 4(%%ebp)"			\
		: : "r"(_ebp), "r"(_eip));				\
}									\
while (0)

#define INIT_SEGV						\
do								\
  {								\
    nullp = new java::lang::NullPointerException ();    	\
    struct sigaction act;					\
    act.sa_handler = catch_segv;				\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = 0;						\
    __sigaction (SIGSEGV, &act, NULL);				\
  }								\
while (0)  

#define INIT_FPE						\
do								\
  { 								\
    arithexception = new java::lang::ArithmeticException 	\
      (JvNewStringLatin1 ("/ by zero"));			\
    struct sigaction act;					\
    act.sa_handler = catch_fpe;					\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = 0;						\
    __sigaction (SIGFPE, &act, NULL);				\
  }								\
while (0)  

#endif /* JAVA_SIGNAL_H */
  
