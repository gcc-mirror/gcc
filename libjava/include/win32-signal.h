// win32-signal.h - Catch runtime signals and turn them into exceptions.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Exception handling is done totally differently on Win32 this stuff
// just keeps it compatible

#ifndef JAVA_SIGNAL_H
#define JAVA_SIGNAL_H 1

#define HANDLE_SEGV 1
#define HANDLE_FPE 1

#define SIGNAL_HANDLER(_name)	\
static void _name (int _dummy)

#define MAKE_THROW_FRAME    do {} while (0)
#define HANDLE_DIVIDE_OVERFLOW	do {} while (0)

#define INIT_SEGV						\
do								\
  {								\
    nullp = new java::lang::NullPointerException ();		\
  }								\
while (0)


#define INIT_FPE						\
do								\
  {								\
    arithexception = new java::lang::ArithmeticException 	\
      (JvNewStringLatin1 ("/ by zero"));			\
  }								\
while (0)


#endif /* JAVA_SIGNAL_H */
