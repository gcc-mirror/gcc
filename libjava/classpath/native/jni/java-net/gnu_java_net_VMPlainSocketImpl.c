/* VMPlainSocketImpl.c - Native methods for PlainSocketImpl class
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
 
#include <jni.h>
#include <jcl.h>

#include "javanet.h"

#include "target_native.h"
#ifndef WITHOUT_NETWORK
  #include "target_native_network.h"
#endif /* WITHOUT_NETWORK */

#include "gnu_java_net_VMPlainSocketImpl.h"

/*
 * Note that the functions in this module simply redirect to another
 * internal function.  Why?  Because many of these functions are shared
 * with PlainDatagramSocketImpl.  The unshared ones were done the same
 * way for consistency.
 */

/*************************************************************************/

/*
 * Creates a new stream or datagram socket
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_create(JNIEnv *env,
					   jclass klass __attribute__ ((__unused__)),
					   jobject obj)
{
#ifndef WITHOUT_NETWORK
  _javanet_create(env, obj, JNI_TRUE);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Close the socket.  Any underlying streams will be closed by this
 * action as well.
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_close(JNIEnv *env,
					  jclass klass __attribute__ ((__unused__)),
					  jobject obj)
{
#ifndef WITHOUT_NETWORK
  _javanet_close(env, obj, 1);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Connects to the specified destination.
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_connect(JNIEnv *env,
					    jclass klass __attribute__ ((__unused__)),
					    jobject obj, 
					    jobject addr, jint port)
{
#ifndef WITHOUT_NETWORK
  _javanet_connect(env, obj, addr, port, 1);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method binds the specified address to the specified local port.
 * Note that we have to set the local address and local port public instance 
 * variables. 
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_bind(JNIEnv *env,
					 jclass klass __attribute__ ((__unused__)),
					 jobject obj, jobject addr,
					 jint port)
{
#ifndef WITHOUT_NETWORK
  _javanet_bind(env, obj, addr, port, 1);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Starts listening on a socket with the specified number of pending 
 * connections allowed.
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_listen(JNIEnv *env,
					   jclass klass __attribute__ ((__unused__)),
					   jobject obj, jint queuelen)
{
#ifndef WITHOUT_NETWORK
  _javanet_listen(env, obj, queuelen);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Accepts a new connection and assigns it to the passed in SocketImpl
 * object. Note that we assume this is a PlainSocketImpl just like us.
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_accept(JNIEnv *env,
					   jclass klass __attribute__ ((__unused__)),
					   jobject obj, jobject impl)
{
#ifndef WITHOUT_NETWORK
  _javanet_accept(env, obj, impl);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

JNIEXPORT jint JNICALL
Java_gnu_java_net_VMPlainSocketImpl_available(JNIEnv *env,
					      jclass klass __attribute__ ((__unused__)),
					      jobject obj)
{
#ifndef WITHOUT_NETWORK
  jclass   cls;
  jfieldID fid;
  int      fd;
  int      bytesAvailable;
  int      result;
  
  cls = (*env)->GetObjectClass(env, obj);
  if (cls == 0)
    {
      JCL_ThrowException(env, IO_EXCEPTION, "internal error");
      return 0;
    }
  
  fid = (*env)->GetFieldID(env, cls, "native_fd", "I"); 
  if (fid == 0)
    {
      JCL_ThrowException(env, IO_EXCEPTION, "internal error");
      return 0;
    }

  fd = (*env)->GetIntField(env, obj, fid);
  
  TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_AVAILABLE(fd,bytesAvailable,result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException(env, IO_EXCEPTION, TARGET_NATIVE_LAST_ERROR_STRING());
      return 0;
    }

  return bytesAvailable;
#else /* not WITHOUT_NETWORK */
  return 0;
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method sets the specified option for a socket
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_setOption(JNIEnv *env,
					      jclass klass __attribute__ ((__unused__)),
					      jobject obj, 
					      jint option_id, jobject val)
{
#ifndef WITHOUT_NETWORK
  _javanet_set_option(env, obj, option_id, val);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method gets the specified option for a socket
 */
JNIEXPORT jobject JNICALL
Java_gnu_java_net_VMPlainSocketImpl_getOption(JNIEnv *env,
					      jclass klass __attribute__ ((__unused__)),
					      jobject obj, 
					      jint option_id)
{
#ifndef WITHOUT_NETWORK
  return(_javanet_get_option(env, obj, option_id));
#else /* not WITHOUT_NETWORK */
  return NULL;
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Reads a buffer from a remote host
 */
JNIEXPORT jint JNICALL
Java_gnu_java_net_VMPlainSocketImpl_read(JNIEnv *env,
					 jclass klass __attribute__ ((__unused__)),
					 jobject obj, jarray buf,
					 jint offset, jint len)
{
#ifndef WITHOUT_NETWORK
  return(_javanet_recvfrom(env, obj, buf, offset, len, 0, 0));
#else /* not WITHOUT_NETWORK */
  return 0;
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Writes a buffer to the remote host
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_write(JNIEnv *env,
					  jclass klass __attribute__ ((__unused__)),
					  jobject obj, jarray buf,
					  jint offset, jint len)
{
#ifndef WITHOUT_NETWORK
  _javanet_sendto(env, obj, buf, offset, len, 0, 0);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_shutdownInput (JNIEnv * env,
						   jclass klass __attribute__ ((__unused__)),
						   jobject this)
{
#ifndef WITHOUT_NETWORK
  _javanet_shutdownInput (env, this);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_shutdownOutput (JNIEnv * env,
						    jclass klass __attribute__ ((__unused__)),
						    jobject this)
{
#ifndef WITHOUT_NETWORK
  _javanet_shutdownOutput (env, this);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/* end of file */
