// default-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#undef HANDLE_SEGV
#undef HANDLE_FPE

#define INIT_SEGV   do {} while (0)
#define INIT_FPE   do {} while (0)

#endif /* JAVA_SIGNAL_H */
  
