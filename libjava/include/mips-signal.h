// mips-signal.h - Catch runtime signals and turn them into exceptions
// on an mips based Linux system. 

/* Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004, 2006
   Free Software Foundation

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

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

#define SIGNAL_HANDLER(_name) \
static void _name (int _dummy __attribute__ ((__unused__)), \
		   siginfo_t *_info __attribute__ ((__unused__)), \
		   void *_arg __attribute__ ((__unused__)))

#define MAKE_THROW_FRAME(_exception)

#define _INIT_SIG_HANDLER(_SIG, _ACTION)     \
do                                           \
  {                                          \
    struct sigaction act;                    \
    act.sa_sigaction = _ACTION;              \
    act.sa_flags = SA_SIGINFO | SA_NODEFER;  \
    sigemptyset (&act.sa_mask);              \
    sigaction(_SIG, &act, NULL);             \
  }                                          \
while (0)

#define INIT_SEGV _INIT_SIG_HANDLER (SIGSEGV, catch_segv)

#define INIT_FPE _INIT_SIG_HANDLER (SIGFPE, catch_fpe)
#undef HANDLE_DIVIDE_OVERFLOW

#endif /* JAVA_SIGNAL_H */
  
