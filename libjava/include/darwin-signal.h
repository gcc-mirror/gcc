/* darwin-signal.h - Catch runtime signals and turn them into exceptions,
   on a Darwin system.  */

/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* This file is really more of a specification.  The rest of the system
   should be arranged so that this Just Works.  */

#ifndef JAVA_SIGNAL_H
# define JAVA_SIGNAL_H 1

#include <sys/types.h>
#include <stdio.h>
#include <signal.h>

typedef void (* SIG_PF)(int);

# define HANDLE_SEGV 1
# undef HANDLE_FPE

# define SIGNAL_HANDLER(_name)					\
  static void _name (int _dummy __attribute__ ((unused)))

# define MAKE_THROW_FRAME(_exception)

# define INIT_SEGV				\
  do {						\
      struct sigaction sa;			\
      sa.sa_handler = catch_segv;		\
      sigemptyset (&sa.sa_mask);		\
      sa.sa_flags = SA_NODEFER;			\
      sigaction (SIGBUS, &sa, NULL);		\
      sigaction (SIGSEGV, &sa, NULL);		\
    } while (0)

# define INIT_FPE				\
  do {						\
      struct sigaction sa;			\
      sa.sa_handler = catch_fpe;		\
      sigemptyset (&sa.sa_mask);		\
      sa.sa_flags = SA_NODEFER;			\
      sigaction (SIGFPE, &sa, NULL);		\
    } while (0)

#endif /* JAVA_SIGNAL_H */
