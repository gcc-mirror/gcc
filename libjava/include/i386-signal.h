// i386-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999  Cygnus Solutions

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
{									\
  void **_p = (void **)&_dummy;						\
  struct sigcontext_struct *_regs = (struct sigcontext_struct *)++_p;	\
									\
  register unsigned long _ebp = _regs->ebp;				\
  register unsigned long _eip = _regs->eip;				\
  									\
  asm volatile ("mov %0, (%%ebp); mov %1, 4(%%ebp)"			\
		: : "r"(_ebp), "r"(_eip));				\
}

#define INIT_SEGV						\
do								\
  {								\
    nullp = new java::lang::NullPointerException ();    	\
    struct sigaction act;					\
    act.sa_handler = catch_segv;				\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = 0;						\
    sigaction (SIGSEGV, &act, NULL);				\
  }								\
while (0)  

#define INIT_FPE						\
do								\
  { 								\
    arithexception = new java::lang::ArithmeticException ();	\
    struct sigaction act;					\
    act.sa_handler = catch_fpe;					\
    sigemptyset (&act.sa_mask);					\
    act.sa_flags = 0;						\
    sigaction (SIGFPE, &act, NULL);				\
  }								\
while (0)  

#endif /* JAVA_SIGNAL_H */
  
