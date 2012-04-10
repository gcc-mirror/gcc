// natVMConsole.cc - Native part of VMConsole class.

/* Copyright (C) 2012
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the ObjectInputStream "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <termios.h>
#include <unistd.h>

#include <gcj/cni.h>

#include <java/io/Console.h>
#include <java/io/VMConsole.h>

#ifndef IUCLC
#define IUCLC 0
#endif

#define TERMIOS_ECHO_IFLAGS (IUCLC|IXON|IXOFF|IXANY)
#define TERMIOS_ECHO_LFLAGS (ECHO|ECHOE|ECHOK|ECHONL|TOSTOP)

jstring
java::io::VMConsole::readPassword(::java::io::Console *con)
{
  struct termios oldt, newt;
  jstring result;

  tcgetattr (STDIN_FILENO, &oldt);

  tcgetattr (STDIN_FILENO, &newt);

  newt.c_iflag &= ~TERMIOS_ECHO_IFLAGS;
  newt.c_lflag &= ~TERMIOS_ECHO_LFLAGS;

  tcsetattr (STDIN_FILENO, TCSANOW, &newt);

  result = con->readLine ();

  tcsetattr (STDIN_FILENO, TCSANOW, &oldt);

  return result;
}
