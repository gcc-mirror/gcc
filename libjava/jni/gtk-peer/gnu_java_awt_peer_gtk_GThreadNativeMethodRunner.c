/* Native implementation of functions in GThreadNativeMethodRunner
   Copyright (C) 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


#include "gnu_java_awt_peer_gtk_GThreadNativeMethodRunner.h"
#include "gthread-jni.h"

/*
 * Class:     GThreadNativeMethodRunner
 * Method:    nativeRun
 * Signature: (J)V
 *
 * Purpose: Run the C function whose function pointer is
 *
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GThreadNativeMethodRunner_nativeRun
  (JNIEnv *env __attribute__((unused)),
   jobject lcl_obj __attribute__((unused)),
   jlong funcAddr, jlong funcArg)
{
  /* Convert the function's address back into a pointer to a C function. */
  void *(*funcPtr)(void *) = (void *(*)(void *)) (size_t)funcAddr;

  /* We do not need to worry about the return value from funcPtr(); it's
     just thrown away.  That is part of the g_threads spec, so no reason
     to worry about returning it.  */
  (void) funcPtr((void *) (size_t)funcArg);
  /* Fall off the end and terminate the thread of control. */
}

/* Local Variables: */
/* c-file-style: "gnu" */
/* End: */


