/* PlainDatagramSocketImpl.c - Native methods for PlainDatagramSocketImpl class
   Copyright (C) 1998 Free Software Foundation, Inc.

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
#include <assert.h>

#include <jni.h>
#include <jcl.h>

#include "javanet.h"

#include "target_native.h"
#ifndef WITHOUT_NETWORK
#include "target_native_network.h"
#endif /* WITHOUT_NETWORK */

#include "gnu_java_net_PlainDatagramSocketImpl.h"

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
Java_gnu_java_net_PlainDatagramSocketImpl_create (JNIEnv * env, jobject obj)
{
  assert (env != NULL);
  assert ((*env) != NULL);

#ifndef WITHOUT_NETWORK
  _javanet_create (env, obj, 0);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Close the socket.
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_PlainDatagramSocketImpl_close (JNIEnv * env, jobject obj)
{
  assert (env != NULL);
  assert ((*env) != NULL);

#ifndef WITHOUT_NETWORK
  _javanet_close (env, obj, 0);
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
Java_gnu_java_net_PlainDatagramSocketImpl_bind (JNIEnv * env, jobject obj,
						jint port, jobject addr)
{
  assert (env != NULL);
  assert ((*env) != NULL);

#ifndef WITHOUT_NETWORK
  _javanet_bind (env, obj, addr, port, 0);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method sets the specified option for a socket
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_PlainDatagramSocketImpl_setOption (JNIEnv * env,
						     jobject obj,
						     jint option_id,
						     jobject val)
{
  assert (env != NULL);
  assert ((*env) != NULL);

#ifndef WITHOUT_NETWORK
  _javanet_set_option (env, obj, option_id, val);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method sets the specified option for a socket
 */
JNIEXPORT jobject JNICALL
Java_gnu_java_net_PlainDatagramSocketImpl_getOption (JNIEnv * env,
						     jobject obj,
						     jint option_id)
{
  assert (env != NULL);
  assert ((*env) != NULL);

#ifndef WITHOUT_NETWORK
  return (_javanet_get_option (env, obj, option_id));
#else /* not WITHOUT_NETWORK */
  return NULL;
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Reads a buffer from a remote host
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_PlainDatagramSocketImpl_receive0 (JNIEnv * env, jobject obj,
						    jobject packet)
{
#ifndef WITHOUT_NETWORK
  int addr, port, bytes_read;
  unsigned int maxlen, offset;
  jclass cls, addr_cls;
  jfieldID fid;
  jmethodID mid;
  jarray arr;
  unsigned char octets[4];
  char ip_str[16];
  jobject ip_str_obj, addr_obj;

  assert (env != NULL);
  assert ((*env) != NULL);

  addr = 0;
  port = 0;
  maxlen = 0;
  offset = 0;
  bytes_read = 0;

  if (packet == NULL)
    {
      JCL_ThrowException (env, "java/lang/NullPointerException",
			  "Null datagram packet");
      return;
    }

  /* Get the buffer from the packet */
  cls = (*env)->GetObjectClass (env, packet);
  if (cls == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error");
      return;
    }

  mid = (*env)->GetMethodID (env, cls, "getData", "()[B");
  if (mid == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: getData");
      return;
    }

  arr = (*env)->CallObjectMethod (env, packet, mid);
  if ((*env)->ExceptionOccurred (env))
    return;
  if (arr == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: call getData");
      return;
    }

  /* Now get the offset from the packet */
  mid = (*env)->GetMethodID (env, cls, "getOffset", "()I");
  if (mid == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: getOffset");
      return;
    }

  offset = (*env)->CallIntMethod (env, packet, mid);
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.receive(): Got the offset\n");

  /* Now get the maximal available length from the packet */
  fid = (*env)->GetFieldID (env, cls, "maxlen", "I");
  if (fid == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: maxlen");
      return;
    }

  maxlen = (*env)->GetIntField (env, packet, fid);
  if ((*env)->ExceptionOccurred (env))
    return;

  /* Receive the packet */
  /* should we try some sort of validation on the length? */
  bytes_read =
    _javanet_recvfrom (env, obj, arr, offset, maxlen, &addr, &port);
  if ((*env)->ExceptionOccurred (env))
    return;
  if (bytes_read == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: receive");
      return;
    }

  DBG ("PlainDatagramSocketImpl.receive(): Received packet\n");

  /* Store the address */
  TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES (addr,
						octets[0],
						octets[1],
						octets[2], octets[3]);
  sprintf (ip_str, "%d.%d.%d.%d", octets[0], octets[1], octets[2], octets[3]);
  ip_str_obj = (*env)->NewStringUTF (env, ip_str);
  if (ip_str_obj == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: new string");
      return;
    }

  addr_cls = (*env)->FindClass (env, "java/net/InetAddress");
  if (addr_cls == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: InetAddress class");
      return;
    }
  DBG ("PlainDatagramSocketImpl.receive(): Found InetAddress class\n");

  mid = (*env)->GetStaticMethodID (env, addr_cls, "getByName",
				   "(Ljava/lang/String;)Ljava/net/InetAddress;");
  if (mid == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal Error");
      return;
    }
  DBG
    ("PlainDatagramSocketImpl.receive(): Found InetAddress.getByName method\n");

  addr_obj = (*env)->CallStaticObjectMethod (env, addr_cls, mid, ip_str_obj);
  if ((*env)->ExceptionOccurred (env))
    return;

  mid = (*env)->GetMethodID (env, cls, "setAddress",
			     "(Ljava/net/InetAddress;)V");
  if (mid == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: setAddress");
      return;
    }

  (*env)->CallVoidMethod (env, packet, mid, addr_obj);
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.receive(): Stored the address\n");

  /* Store the port */
  mid = (*env)->GetMethodID (env, cls, "setPort", "(I)V");
  if (mid == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: setPort");
      return;
    }

  (*env)->CallVoidMethod (env, packet, mid, port);
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.receive(): Stored the port\n");

  /* Store back the length */
  fid = (*env)->GetFieldID (env, cls, "length", "I");
  if (fid == NULL)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal error: length");
      return;
    }

  (*env)->SetIntField (env, packet, fid, bytes_read);
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.receive(): Stored the length\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Writes a buffer to the remote host
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_PlainDatagramSocketImpl_sendto (JNIEnv * env, jobject obj,
						  jobject addr, jint port,
						  jarray buf, jint offset,
						  jint len)
{
#ifndef WITHOUT_NETWORK
  jint netAddress;

  assert (env != NULL);
  assert ((*env) != NULL);

  netAddress = _javanet_get_netaddr (env, addr);
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.sendto(): have addr\n");

  _javanet_sendto (env, obj, buf, offset, len, netAddress, port);
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.sendto(): finished\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Joins a multicast group
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_PlainDatagramSocketImpl_join (JNIEnv * env, jobject obj,
						jobject addr)
{
#ifndef WITHOUT_NETWORK
  jint netAddress;
  int fd;
  int result;

  assert (env != NULL);
  assert ((*env) != NULL);

  netAddress = _javanet_get_netaddr (env, addr);
  if ((*env)->ExceptionOccurred (env))
    return;

  fd = _javanet_get_int_field (env, obj, "native_fd");
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.join(): have native fd\n");

  TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_ADD_MEMBERSHIP (fd, netAddress,
							  result);

  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }

  DBG ("PlainDatagramSocketImpl.join(): finished\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Leaves a multicast group
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_PlainDatagramSocketImpl_leave (JNIEnv * env, jobject obj,
						 jobject addr)
{
#ifndef WITHOUT_NETWORK
  jint netAddress;
  int fd;
  int result;

  assert (env != NULL);
  assert ((*env) != NULL);

  netAddress = _javanet_get_netaddr (env, addr);
  if ((*env)->ExceptionOccurred (env))
    return;

  fd = _javanet_get_int_field (env, obj, "native_fd");
  if ((*env)->ExceptionOccurred (env))
    return;

  DBG ("PlainDatagramSocketImpl.leave(): have native fd\n");

  TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_DROP_MEMBERSHIP (fd, netAddress,
							   result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }

  DBG ("PlainDatagramSocketImpl.leave(): finished\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}
