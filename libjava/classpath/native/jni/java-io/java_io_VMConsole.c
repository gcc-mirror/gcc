/* java_io_VMConsole.c - Native methods for java.io.Console class
   Copyright (C) 2012 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

/* do not move; needed here because of some macro definitions */
#include <config.h>

#include <termios.h>
#include <unistd.h>

#include <jni.h>

#include "java_io_VMConsole.h"

/*************************************************************************/

#ifndef IUCLC
#define IUCLC 0
#endif

#define TERMIOS_ECHO_IFLAGS (IUCLC|IXON|IXOFF|IXANY)
#define TERMIOS_ECHO_LFLAGS (ECHO|ECHOE|ECHOK|ECHONL|TOSTOP)

/*
 * Class:     java_io_VMConsole
 * Method:    echo
 * Signature: (Z)Z
 */
JNIEXPORT jstring JNICALL
Java_java_io_VMConsole_readPassword (JNIEnv * env,
				     jclass clazz
				     __attribute__ ((__unused__)),
				     jobject con)
{
  struct termios old, new;
  jmethodID readLineID;
  jstring result;

  readLineID =
    (*env)->GetMethodID (env, (*env)->GetObjectClass (env, con), "readLine",
			 "()Ljava/lang/String;");
  if (!readLineID)
    {
      return NULL;
    }

  tcgetattr (STDIN_FILENO, &old);

  tcgetattr (STDIN_FILENO, &new);

  new.c_iflag &= ~TERMIOS_ECHO_IFLAGS;
  new.c_lflag &= ~TERMIOS_ECHO_LFLAGS;

  tcsetattr (STDIN_FILENO, TCSANOW, &new);

  result = (*env)->CallObjectMethod (env, con, readLineID);

  tcsetattr (STDIN_FILENO, TCSANOW, &old);

  return result;
}
