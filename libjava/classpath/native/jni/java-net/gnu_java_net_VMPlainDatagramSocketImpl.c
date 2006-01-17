/* VMPlainDatagramSocketImpl.c - Native methods for PlainDatagramSocketImpl
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


#include "gnu_java_net_VMPlainDatagramSocketImpl.h"

/*
 * Note that most of the functions in this module simply redirect to another
 * internal function.  Why?  Because many of these functions are shared
 * with PlainSocketImpl. 
 */

/*************************************************************************/

/*
 * Creates a new datagram socket
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_create(JNIEnv *env,
						   jclass klass __attribute__ ((__unused__))
						   , jobject obj)
{

#ifndef WITHOUT_NETWORK
  _javanet_create(env, obj, 0);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Close the socket.
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_close(JNIEnv *env,
						  jclass klass __attribute__ ((__unused__)),
						  jobject obj)
{

#ifndef WITHOUT_NETWORK
  _javanet_close(env, obj, 0);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Connects to the specified destination.
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_connect(JNIEnv *env,
						    jclass klass __attribute__ ((__unused__)),
						    jobject obj,
						    jobject addr, jint port)
{
#ifndef WITHOUT_NETWORK

  _javanet_connect(env, obj, addr, port, 0);
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
Java_gnu_java_net_VMPlainDatagramSocketImpl_bind(JNIEnv *env,
						 jclass klass __attribute__ ((__unused__)),
						 jobject obj,
						 jint port, jobject addr)
{

#ifndef WITHOUT_NETWORK
  _javanet_bind(env, obj, addr, port, 0);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method sets the specified option for a socket
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_setOption(JNIEnv *env,
						      jclass klass __attribute__ ((__unused__)),
						      jobject obj, 
						      jint option_id,
						      jobject val)
{

#ifndef WITHOUT_NETWORK
  _javanet_set_option(env, obj, option_id, val);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method sets the specified option for a socket
 */
JNIEXPORT jobject JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_getOption(JNIEnv *env,
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
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_nativeReceive(JNIEnv     *env,
                                                          jclass     klass __attribute__ ((__unused__)),
                                                          jobject    obj,
                                                          jbyteArray arr,
                                                          jint       offset,
                                                          jint       length,
                                                          jbyteArray receivedFromAddress,
                                                          jintArray  receivedFromPort,
                                                          jintArray  receivedLength)
{
#ifndef WITHOUT_NETWORK
  int           addr, *port, *bytes_read;
  char          *addressBytes;

  addr = 0;
    
  port = (int*)(*env)->GetIntArrayElements(env, receivedFromPort, NULL);
  if (port == NULL)
    {
      JCL_ThrowException(env, IO_EXCEPTION, "Internal error: could not access receivedFromPort array");
      return;
    }
  
  bytes_read = (int*)(*env)->GetIntArrayElements(env, receivedLength, NULL);
  if (bytes_read == NULL)
    {
      (*env)->ReleaseIntArrayElements(env, receivedFromPort, (jint*)port, 0);
      JCL_ThrowException(env, IO_EXCEPTION, "Internal error: could not access receivedLength array");
      return;
    }

  /* Receive the packet */
  /* should we try some sort of validation on the length? */
  (*bytes_read) = _javanet_recvfrom(env, obj, arr, offset, length, &addr, port);

  /* Special case the strange situation where the receiver didn't want any
     bytes. */
  if (length == 0 && (*bytes_read) == -1)
    *bytes_read = 0;

  if ((*bytes_read) == -1)
    {
      (*env)->ReleaseIntArrayElements(env, receivedFromPort, (jint*)port, 0);
      (*env)->ReleaseIntArrayElements(env, receivedLength, (jint*)bytes_read, 0);
      JCL_ThrowException(env, IO_EXCEPTION, "Internal error: receive");
      return;
    }

  (*env)->ReleaseIntArrayElements(env, receivedFromPort, (jint*)port, 0);
  (*env)->ReleaseIntArrayElements(env, receivedLength, (jint*)bytes_read, 0);

  if ((*env)->ExceptionOccurred(env))
    {
      return;
    }

  DBG("PlainDatagramSocketImpl.receive(): Received packet\n");
  
  
  /* Store the address */
  addressBytes = (char*)(*env)->GetPrimitiveArrayCritical(env, receivedFromAddress, NULL);
  TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES(addr,
                                               addressBytes[0],
                                               addressBytes[1],
                                               addressBytes[2],
                                               addressBytes[3]
                                              );
  (*env)->ReleasePrimitiveArrayCritical(env, receivedFromAddress, addressBytes, 0);
    
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Writes a buffer to the remote host
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_nativeSendTo(JNIEnv  *env,
                                                         jclass  klass __attribute__ ((__unused__)),
                                                         jobject obj, 
                                                         jobject addr,
                                                         jint    port,
                                                         jarray  buf, 
                                                         jint    offset,
                                                         jint    len)
{
#ifndef WITHOUT_NETWORK
  jint netAddress;

  /* check if address given, tr 7.3.2005 */
  if (addr != NULL)
    {
      netAddress = _javanet_get_netaddr(env, addr);
      if ((*env)->ExceptionOccurred(env))
        {
          return;
        }
    }
  else
    {
      netAddress = 0;
    }

  DBG("PlainDatagramSocketImpl.sendto(): have addr\n");

  _javanet_sendto(env, obj, buf, offset, len, netAddress, port);
  if ((*env)->ExceptionOccurred(env))
    {
      return;
    }

  DBG("PlainDatagramSocketImpl.sendto(): finished\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Joins a multicast group
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_join(JNIEnv *env,
						 jclass klass __attribute__ ((__unused__)),
						 jobject obj, 
						 jobject addr)
{
#ifndef WITHOUT_NETWORK
  jint netAddress;
  int  fd;
  int  result;

  /* check if address given, tr 7.3.2005 */
  if (addr != NULL)
    {
      netAddress = _javanet_get_netaddr(env, addr);
      if ((*env)->ExceptionOccurred(env))
        {
          JCL_ThrowException(env, IO_EXCEPTION, "Internal error");
          return;
        }
    }
  else
    {
      netAddress = 0;
    }

  fd = _javanet_get_int_field(env, obj, "native_fd");
  if ((*env)->ExceptionOccurred(env))
    {
      JCL_ThrowException(env, IO_EXCEPTION, "Internal error");
      return;
    }

  DBG("PlainDatagramSocketImpl.join(): have native fd\n");

  TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_ADD_MEMBERSHIP(fd,netAddress,result);

  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException(env, IO_EXCEPTION, TARGET_NATIVE_LAST_ERROR_STRING());
      return;
    }

  DBG("PlainDatagramSocketImpl.join(): finished\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Leaves a multicast group
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainDatagramSocketImpl_leave(JNIEnv *env,
						  jclass klass __attribute__ ((__unused__)),
						  jobject obj, 
						  jobject addr)
{
#ifndef WITHOUT_NETWORK
  jint netAddress;
  int  fd;
  int  result;

  /* check if address given, tr 7.3.2005 */
  if (addr != NULL)
    {
      netAddress = _javanet_get_netaddr(env, addr);
      if ((*env)->ExceptionOccurred(env))
        {
          JCL_ThrowException(env, IO_EXCEPTION, "Internal error");
          return;
        }
    }
  else
    {
      netAddress = 0;
    }

  fd = _javanet_get_int_field(env, obj, "native_fd");
  if ((*env)->ExceptionOccurred(env))
    { JCL_ThrowException(env, IO_EXCEPTION, "Internal error"); return; }

  DBG("PlainDatagramSocketImpl.leave(): have native fd\n");

  TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_DROP_MEMBERSHIP(fd,netAddress,result);
  if (result!=TARGET_NATIVE_OK)
    {
      JCL_ThrowException(env, IO_EXCEPTION, TARGET_NATIVE_LAST_ERROR_STRING());
      return;
    }

  DBG("PlainDatagramSocketImpl.leave(): finished\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

