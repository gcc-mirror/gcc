// default-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#ifdef SJLJ_EXCEPTIONS

#define HANDLE_SEGV
#define HANDLE_FPE

#include <signal.h>

#define SIGNAL_HANDLER(_name)			\
static void _name (int _dummy)

#define INIT_SEGV						\
do								\
  {								\
    nullp = new java::lang::NullPointerException ();		\
    signal (SIGSEGV, catch_segv);				\
  }								\
while (0)							
								
#define INIT_FPE						\
do								\
  {								\
    arithexception = new java::lang::ArithmeticException 	\
      (JvNewStringLatin1 ("/ by zero"));			\
    signal (SIGFPE, catch_fpe);					\
  }								\
while (0)

#define MAKE_THROW_FRAME  do {} while (0)

#else /* SJLJ_EXCEPTIONS */

#undef HANDLE_SEGV
#undef HANDLE_FPE

#define INIT_SEGV   do {} while (0)
#define INIT_FPE   do {} while (0)

#endif /* SJLJ_EXCEPTIONS */

#endif /* JAVA_SIGNAL_H */
  
