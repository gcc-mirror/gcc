/* javanet.c - Common internal functions for the java.net package
   Copyright (C) 1998, 2002, 2004, 2005  Free Software Foundation, Inc.

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

#ifndef WITHOUT_NETWORK
/* Need to have some value for SO_TIMEOUT */
#ifndef SO_TIMEOUT
#ifndef SO_RCVTIMEO
#warning Neither SO_TIMEOUT or SO_RCVTIMEO are defined!
#warning This will cause all get/setOption calls with that value to throw an exception
#else
#define SO_TIMEOUT SO_RCVTIMEO
#endif /* not SO_RCVTIMEO */
#endif /* not SO_TIMEOUT */
#endif /* WITHOUT_NETWORK */

/*************************************************************************/

/*
 * Sets an integer field in the specified object.
 */
static void
_javanet_set_int_field (JNIEnv * env, jobject obj,
			const char *class, const char *field, int val)
{
  jclass cls;
  jfieldID fid;

  assert (env != NULL);
  assert ((*env) != NULL);

  cls = (*env)->FindClass (env, class);
  if (cls == NULL)
    return;

  fid = (*env)->GetFieldID (env, cls, field, "I");
  if (fid == NULL)
    return;

  (*env)->SetIntField (env, obj, fid, val);

  return;
}

/*************************************************************************/

/*
 * Returns the value of the specified integer instance variable field or
 * -1 if an error occurs.
 */
int
_javanet_get_int_field (JNIEnv * env, jobject obj, const char *field)
{
  jclass cls = 0;
  jfieldID fid;
  int fd;

  assert (env != NULL);
  assert ((*env) != NULL);

  DBG ("_javanet_get_int_field(): Entered _javanet_get_int_field\n");

  cls = (*env)->GetObjectClass (env, obj);
  if (cls == NULL)
    return -1;

  fid = (*env)->GetFieldID (env, cls, field, "I");
  if (fid == NULL)
    return -1;
  DBG ("_javanet_get_int_field(): Found field id\n");

  fd = (*env)->GetIntField (env, obj, fid);

  return fd;
}

/*************************************************************************/

/*
 * Creates a FileDescriptor object in the parent class.  It is not used
 * by this implementation, but the docs list it as a variable, so we
 * need to include it.
 */
static void
_javanet_create_localfd (JNIEnv * env, jobject this)
{
  jclass this_cls, fd_cls;
  jfieldID fid;
  jmethodID mid;
  jobject fd_obj;

  assert (env != NULL);
  assert ((*env) != NULL);

  DBG ("_javanet_create_localfd(): Entered _javanet_create_localfd\n");

  /* Look up the fd field */
  this_cls = (*env)->FindClass (env, "java/net/SocketImpl");
  if (this_cls == NULL)
    return;

  fid = (*env)->GetFieldID (env, this_cls, "fd", "Ljava/io/FileDescriptor;");
  if (fid == NULL)
    return;

  DBG ("_javanet_create_localfd(): Found fd variable\n");

  /* Create a FileDescriptor */
  fd_cls = (*env)->FindClass (env, "java/io/FileDescriptor");
  if (fd_cls == NULL)
    return;

  DBG ("_javanet_create_localfd(): Found FileDescriptor class\n");

  mid = (*env)->GetMethodID (env, fd_cls, "<init>", "()V");
  if (mid == NULL)
    return;

  DBG ("_javanet_create_localfd(): Found FileDescriptor constructor\n");

  fd_obj = (*env)->NewObject (env, fd_cls, mid);
  if (fd_obj == NULL)
    return;

  DBG ("_javanet_create_localfd(): Created FileDescriptor\n");

  /* Now set the pointer to the new FileDescriptor */
  (*env)->SetObjectField (env, this, fid, fd_obj);
  DBG ("_javanet_create_localfd(): Set fd field\n");

  return;
}

/*************************************************************************/

/*
 * Returns a Boolean object with the specfied value
 */
static jobject
_javanet_create_boolean (JNIEnv * env, jboolean val)
{
  jclass cls;
  jmethodID mid;
  jobject obj;

  assert (env != NULL);
  assert ((*env) != NULL);

  cls = (*env)->FindClass (env, "java/lang/Boolean");
  if (cls == NULL)
    return NULL;

  mid = (*env)->GetMethodID (env, cls, "<init>", "(Z)V");
  if (mid == NULL)
    return NULL;

  obj = (*env)->NewObject (env, cls, mid, val);
  if (obj == NULL)
    return NULL;

  return obj;
}

/*************************************************************************/

/*
 * Returns an Integer object with the specfied value
 */
static jobject
_javanet_create_integer (JNIEnv * env, jint val)
{
  jclass cls;
  jmethodID mid;
  jobject obj;

  assert (env != NULL);
  assert ((*env) != NULL);

  cls = (*env)->FindClass (env, "java/lang/Integer");
  if (cls == NULL)
    return NULL;

  mid = (*env)->GetMethodID (env, cls, "<init>", "(I)V");
  if (mid == NULL)
    return NULL;

  obj = (*env)->NewObject (env, cls, mid, val);
  if (obj == NULL)
    return NULL;

  return obj;
}

/*************************************************************************/

/*
 * Builds an InetAddress object from a 32 bit address in host byte order
 */
static jobject
_javanet_create_inetaddress (JNIEnv * env, int netaddr)
{
#ifndef WITHOUT_NETWORK
  unsigned char octets[4];
  char buf[16];
  jclass ia_cls;
  jmethodID mid;
  jstring ip_str;
  jobject ia;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Build a string IP address */
  TARGET_NATIVE_NETWORK_INT_TO_IPADDRESS_BYTES (netaddr,
						octets[0],
						octets[1],
						octets[2], octets[3]);
  sprintf (buf, "%d.%d.%d.%d", octets[0], octets[1], octets[2], octets[3]);
  DBG ("_javanet_create_inetaddress(): Created ip addr string\n");

  /* Get an InetAddress object for this IP */
  ia_cls = (*env)->FindClass (env, "java/net/InetAddress");
  if (ia_cls == NULL)
    {
      return NULL;
    }

  DBG ("_javanet_create_inetaddress(): Found InetAddress class\n");

  mid = (*env)->GetStaticMethodID (env, ia_cls, "getByName",
				   "(Ljava/lang/String;)Ljava/net/InetAddress;");
  if (mid == NULL)
    {
      return NULL;
    }

  DBG ("_javanet_create_inetaddress(): Found getByName method\n");

  ip_str = (*env)->NewStringUTF (env, buf);
  if (ip_str == NULL)
    {
      return NULL;
    }

  ia = (*env)->CallStaticObjectMethod (env, ia_cls, mid, ip_str);
  if (ia == NULL)
    {
      return NULL;
    }

  DBG ("_javanet_create_inetaddress(): Called getByName method\n");

  return ia;
#else /* not WITHOUT_NETWORK */
  return NULL;
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

static void
_javanet_set_remhost_addr (JNIEnv * env, jobject this, jobject ia)
{
  jclass this_cls;
  jfieldID fid;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Set the variable in the object */
  this_cls = (*env)->FindClass (env, "java/net/SocketImpl");
  if (this_cls == NULL)
    return;

  fid =
    (*env)->GetFieldID (env, this_cls, "address", "Ljava/net/InetAddress;");
  if (fid == NULL)
    return;

  DBG ("_javanet_set_remhost_addr(): Found address field\n");

  (*env)->SetObjectField (env, this, fid, ia);
  DBG ("_javanet_set_remhost_addr(): Set field\n");
}

/*
 * Set's the value of the "addr" field in PlainSocketImpl with a new
 * InetAddress for the specified addr
 */
static void
_javanet_set_remhost (JNIEnv * env, jobject this, int netaddr)
{
  jobject ia;

  assert (env != NULL);
  assert ((*env) != NULL);

  DBG ("_javanet_set_remhost(): Entered _javanet_set_remhost\n");

  /* Get an InetAddress object */
  ia = _javanet_create_inetaddress (env, netaddr);
  if (ia == NULL)
    return;

  _javanet_set_remhost_addr (env, this, ia);
}


/*************************************************************************/

/*
 * Returns a 32 bit Internet address for the passed in InetAddress object
 */
int
_javanet_get_netaddr (JNIEnv * env, jobject addr)
{
#ifndef WITHOUT_NETWORK
  jclass cls = 0;
  jmethodID mid;
  jarray arr = 0;
  jbyte *octets;
  int netaddr, len;

  assert (env != NULL);
  assert ((*env) != NULL);

  DBG ("_javanet_get_netaddr(): Entered _javanet_get_netaddr\n");

  if (addr == NULL)
    {
      JCL_ThrowException (env, "java/lang/NullPointerException",
			  "Null address");
      return 0;
    }

  /* Call the getAddress method on the object to retrieve the IP address */
  cls = (*env)->GetObjectClass (env, addr);
  if (cls == NULL)
    return 0;

  mid = (*env)->GetMethodID (env, cls, "getAddress", "()[B");
  if (mid == NULL)
    return 0;

  DBG ("_javanet_get_netaddr(): Got getAddress method\n");

  arr = (*env)->CallObjectMethod (env, addr, mid);
  if (arr == NULL)
    return 0;

  DBG ("_javanet_get_netaddr(): Got the address\n");

  /* Turn the IP address into a 32 bit Internet address in network byte order */
  len = (*env)->GetArrayLength (env, arr);
  if (len != 4)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal Error");
      return 0;
    }
  DBG ("_javanet_get_netaddr(): Length ok\n");

  octets = (*env)->GetByteArrayElements (env, arr, 0);
  if (octets == NULL)
    return 0;

  DBG ("_javanet_get_netaddr(): Grabbed bytes\n");

  TARGET_NATIVE_NETWORK_IPADDRESS_BYTES_TO_INT (octets[0],
						octets[1],
						octets[2],
						octets[3], netaddr);

  (*env)->ReleaseByteArrayElements (env, arr, octets, 0);
  DBG ("_javanet_get_netaddr(): Done getting addr\n");

  return netaddr;
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Creates a new stream or datagram socket
 */
void
_javanet_create (JNIEnv * env, jobject this, jboolean stream)
{
#ifndef WITHOUT_NETWORK
  int fd;
  int result;

  assert (env != NULL);
  assert ((*env) != NULL);

  if (stream)
    {
      /* create a stream socket */
      TARGET_NATIVE_NETWORK_SOCKET_OPEN_STREAM (fd, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}
    }
  else
    {
      /* create a datagram socket, set broadcast option */
      TARGET_NATIVE_NETWORK_SOCKET_OPEN_DATAGRAM (fd, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}
      TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_BROADCAST (fd, 1, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}
    }

  if (stream)
    _javanet_set_int_field (env, this, "gnu/java/net/PlainSocketImpl",
			    "native_fd", fd);
  else
    _javanet_set_int_field (env, this, "gnu/java/net/PlainDatagramSocketImpl",
			    "native_fd", fd);

  if ((*env)->ExceptionOccurred (env))
    {
      /* Try to make sure we close the socket since close() won't work. */
      do
	{
	  TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
	  if (result != TARGET_NATIVE_OK
	      && (TARGET_NATIVE_LAST_ERROR ()
		  != TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	    return;
	}
      while (result != TARGET_NATIVE_OK);
      return;
    }

#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Close the socket.  Any underlying streams will be closed by this
 * action as well.
 */
void
_javanet_close (JNIEnv * env, jobject this, int stream)
{
#ifndef WITHOUT_NETWORK
  int fd;
  int result;
  int error = 0;

  assert (env != NULL);
  assert ((*env) != NULL);

  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    return;

  if (stream)
    _javanet_set_int_field (env, this, "gnu/java/net/PlainSocketImpl",
			    "native_fd", -1);
  else
    _javanet_set_int_field (env, this, "gnu/java/net/PlainDatagramSocketImpl",
			    "native_fd", -1);
  do
    {
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
      if (result != TARGET_NATIVE_OK)
	{
	  /* Only throw an error when a "real" error occurs. */
	  error = TARGET_NATIVE_LAST_ERROR ();
	  if (error != TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL
	      && error != ENOTCONN && error != ECONNRESET && error != EBADF)
	    JCL_ThrowException (env, IO_EXCEPTION,
				TARGET_NATIVE_LAST_ERROR_STRING ());
	}
    }
  while (error == TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL);

#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Connects to the specified destination.
 */
void
_javanet_connect (JNIEnv * env, jobject this, jobject addr, jint port)
{
#ifndef WITHOUT_NETWORK
  int netaddr, fd;
  int result;
  int local_address, local_port;
  int remote_address, remote_port;

  assert (env != NULL);
  assert ((*env) != NULL);

  DBG ("_javanet_connect(): Entered _javanet_connect\n");

  /* Pre-process input variables */
  netaddr = _javanet_get_netaddr (env, addr);
  if ((*env)->ExceptionOccurred (env))
    return;

  if (port == -1)
    port = 0;
  DBG ("_javanet_connect(): Got network address\n");

  /* Grab the real socket file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_connect(): no native file descriptor");
      return;
    }
  DBG ("_javanet_connect(): Got native fd\n");

  /* Connect up */
  do
    {
      TARGET_NATIVE_NETWORK_SOCKET_CONNECT (fd, netaddr, port, result);
      if (result != TARGET_NATIVE_OK
	  && (TARGET_NATIVE_LAST_ERROR ()
	      != TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return;
	}
    }
  while (result != TARGET_NATIVE_OK);

  DBG ("_javanet_connect(): Connected successfully\n");

  /* Populate instance variables */
  TARGET_NATIVE_NETWORK_SOCKET_GET_LOCAL_INFO (fd, local_address, local_port,
					       result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
      return;
    }

  _javanet_create_localfd (env, this);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
      return;
    }
  DBG ("_javanet_connect(): Created fd\n");

  _javanet_set_int_field (env, this, "java/net/SocketImpl", "localport",
			  local_port);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
      return;
    }
  DBG ("_javanet_connect(): Set the local port\n");

  TARGET_NATIVE_NETWORK_SOCKET_GET_REMOTE_INFO (fd, remote_address,
						remote_port, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
      return;
    }

  if (remote_address == netaddr)
    {
      _javanet_set_remhost_addr (env, this, addr);
    }
  else
    {
      _javanet_set_remhost (env, this, remote_address);
    }
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
      return;
    }
  DBG ("_javanet_connect(): Set the remote host\n");

  _javanet_set_int_field (env, this, "java/net/SocketImpl", "port",
			  remote_port);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (fd, result);
      return;
    }
  DBG ("_javanet_connect(): Set the remote port\n");
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * This method binds the specified address to the specified local port.
 * Note that we have to set the local address and local
 * port public instance variables. 
 */
void
_javanet_bind (JNIEnv * env, jobject this, jobject addr, jint port,
	       int stream)
{
#ifndef WITHOUT_NETWORK
  jclass cls;
  jmethodID mid;
  jbyteArray arr = 0;
  jbyte *octets;
  jint fd;
  int tmpaddr;
  int result;
  int local_address, local_port;

  assert (env != NULL);
  assert ((*env) != NULL);

  DBG ("_javanet_bind(): Entering native bind()\n");

  /* Get the address to connect to */
  cls = (*env)->GetObjectClass (env, addr);
  if (cls == NULL)
    return;

  mid = (*env)->GetMethodID (env, cls, "getAddress", "()[B");
  if (mid == NULL)
    return;

  DBG ("_javanet_bind(): Past getAddress method id\n");

  arr = (*env)->CallObjectMethod (env, addr, mid);
  if ((arr == NULL) || (*env)->ExceptionOccurred (env))
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_bind()");
      return;
    }

  DBG ("_javanet_bind(): Past call object method\n");

  octets = (*env)->GetByteArrayElements (env, arr, 0);
  if (octets == NULL)
    return;

  DBG ("_javanet_bind(): Past grab array\n");

  /* Get the native socket file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      (*env)->ReleaseByteArrayElements (env, arr, octets, 0);
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_bind(): no native file descriptor");
      return;
    }
  DBG ("_javanet_bind(): Past native_fd lookup\n");

  /* XXX NYI ??? */
  _javanet_set_option (env, this, SOCKOPT_SO_REUSEADDR,
		       _javanet_create_boolean (env, JNI_TRUE));


  /* Bind the socket */
  TARGET_NATIVE_NETWORK_IPADDRESS_BYTES_TO_INT (octets[0],
						octets[1],
						octets[2],
						octets[3], tmpaddr);
  TARGET_NATIVE_NETWORK_SOCKET_BIND (fd, tmpaddr, port, result);

  if (result != TARGET_NATIVE_OK)
    {
      char *errorstr = TARGET_NATIVE_LAST_ERROR_STRING ();
      (*env)->ReleaseByteArrayElements (env, arr, octets, 0);

      JCL_ThrowException (env, BIND_EXCEPTION,
			  errorstr);
      return;
    }
  DBG ("_javanet_bind(): Past bind\n");

  (*env)->ReleaseByteArrayElements (env, arr, octets, 0);

  /* Update instance variables, specifically the local port number */
  TARGET_NATIVE_NETWORK_SOCKET_GET_LOCAL_INFO (fd, local_address, local_port,
					       result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }

  if (stream)
    _javanet_set_int_field (env, this, "java/net/SocketImpl",
			    "localport", local_port);
  else
    _javanet_set_int_field (env, this, "java/net/DatagramSocketImpl",
			    "localPort", local_port);
  DBG ("_javanet_bind(): Past update port number\n");

  return;
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Starts listening on a socket with the specified number of pending 
 * connections allowed.
 */
void
_javanet_listen (JNIEnv * env, jobject this, jint queuelen)
{
#ifndef WITHOUT_NETWORK
  int fd;
  int result;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Get the real file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_listen(): no native file descriptor");
      return;
    }

  /* Start listening */
  TARGET_NATIVE_NETWORK_SOCKET_LISTEN (fd, queuelen, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Accepts a new connection and assigns it to the passed in SocketImpl
 * object. Note that we assume this is a PlainSocketImpl just like us
 */
void
_javanet_accept (JNIEnv * env, jobject this, jobject impl)
{
#ifndef WITHOUT_NETWORK
  int fd, newfd;
  int result;
  int local_address, local_port;
  int remote_address, remote_port;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Get the real file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_accept(): no native file descriptor");
      return;
    }

  /* Accept the connection */
  do
    {
      TARGET_NATIVE_NETWORK_SOCKET_ACCEPT (fd, newfd, result);
      if (result != TARGET_NATIVE_OK
	  && (TARGET_NATIVE_LAST_ERROR ()
	      != TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      "Internal error: _javanet_accept(): ");
	  return;
	}
    }
  while (result != TARGET_NATIVE_OK);

  /* Populate instance variables */
  _javanet_set_int_field (env, impl, "gnu/java/net/PlainSocketImpl",
			  "native_fd", newfd);

  if ((*env)->ExceptionOccurred (env))
    {
      /* Try to make sure we close the socket since close() won't work. */
      do
	{
	  TARGET_NATIVE_NETWORK_SOCKET_CLOSE (newfd, result);
	  if (result != TARGET_NATIVE_OK
	      && (TARGET_NATIVE_LAST_ERROR ()
		  != TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL))
	    return;
	}
      while (result != TARGET_NATIVE_OK);
      return;
    }

  TARGET_NATIVE_NETWORK_SOCKET_GET_LOCAL_INFO (newfd, local_address,
					       local_port, result);
  if (result != TARGET_NATIVE_OK)
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (newfd, result);
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }

  _javanet_create_localfd (env, impl);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (newfd, result);
      return;
    }

  _javanet_set_int_field (env, impl, "java/net/SocketImpl", "localport",
			  local_port);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (newfd, result);
      return;
    }

  TARGET_NATIVE_NETWORK_SOCKET_GET_REMOTE_INFO (newfd, remote_address,
						remote_port, result);
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (newfd, result);
      return;
    }

  _javanet_set_remhost (env, impl, remote_address);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (newfd, result);
      return;
    }

  _javanet_set_int_field (env, impl, "java/net/SocketImpl", "port",
			  remote_port);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      TARGET_NATIVE_NETWORK_SOCKET_CLOSE (newfd, result);
      return;
    }
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Receives a buffer from a remote host. The args are:
 *
 * buf - The byte array into which the data received will be written
 * offset - Offset into the byte array to start writing
 * len - The number of bytes to read.
 * addr - Pointer to 32 bit net address of host to receive from. If null,
 *        this parm is ignored.  If pointing to an address of 0, the 
 *        actual address read is stored here
 * port - Pointer to the port to receive from. If null, this parm is ignored.
 *        If it is 0, the actual remote port received from is stored here
 *
 * The actual number of bytes read is returned.
 */
int
_javanet_recvfrom (JNIEnv * env, jobject this, jarray buf, int offset,
		   int len, int *addr, int *port)
{
#ifndef WITHOUT_NETWORK
  int fd;
  jbyte *p;
  int from_address, from_port;
  int received_bytes;

  assert (env != NULL);
  assert ((*env) != NULL);

  DBG ("_javanet_recvfrom(): Entered _javanet_recvfrom\n");

  /* Get the real file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_recvfrom(): no native file descriptor");
      return 0;
    }
  DBG ("_javanet_recvfrom(): Got native_fd\n");

  /* Get a pointer to the buffer */
  p = (*env)->GetByteArrayElements (env, buf, 0);
  if (p == NULL)
    return 0;

  DBG ("_javanet_recvfrom(): Got buffer\n");

  /* Read the data */
  from_address = 0;
  from_port = 0;
  do
    {
      if (addr != NULL)
	{
	  TARGET_NATIVE_NETWORK_SOCKET_RECEIVE_WITH_ADDRESS_PORT (fd,
								  p + offset,
								  len,
								  from_address,
								  from_port,
								  received_bytes);
	}
      else
	{
	  TARGET_NATIVE_NETWORK_SOCKET_RECEIVE (fd, p + offset, len,
						received_bytes);
	}
    }
  while ((received_bytes == -1) &&
	 (TARGET_NATIVE_LAST_ERROR () ==
	  TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL));

  if (received_bytes == -1)
    {
      if (TARGET_NATIVE_LAST_ERROR () == EAGAIN)
	JCL_ThrowException (env, "java/net/SocketTimeoutException", "Timeout");
      else
	JCL_ThrowException (env, IO_EXCEPTION,
			    TARGET_NATIVE_LAST_ERROR_STRING ());
 
      /* Cleanup and return. */
      (*env)->ReleaseByteArrayElements (env, buf, p, 0);
      return 0;
    }

  (*env)->ReleaseByteArrayElements (env, buf, p, 0);

  /* Handle return addr case */
  if (addr != NULL)
    {
      (*addr) = from_address;
      if (port != NULL)
	(*port) = from_port;
    }

  return (received_bytes);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Sends a buffer to a remote host.  The args are:
 *
 * buf - A byte array
 * offset - Index into the byte array to start sendign
 * len - The number of bytes to write
 * addr - The 32bit address to send to (may be 0)
 * port - The port number to send to (may be 0)
 */
void
_javanet_sendto (JNIEnv * env, jobject this, jarray buf, int offset, int len,
		 int addr, int port)
{
#ifndef WITHOUT_NETWORK
  int fd;
  jbyte *p;
  int bytes_sent;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Get the real file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_sendto(): no native file descriptor");
      return;
    }

  /* Get a pointer to the buffer */
  p = (*env)->GetByteArrayElements (env, buf, 0);
  if (p == NULL)
    return;

  /* We must send all the data, so repeat till done. */
  while (len > 0)
    {
      /* Send the data */
      if (addr == 0)
	{
	  DBG ("_javanet_sendto(): Sending....\n");
	  TARGET_NATIVE_NETWORK_SOCKET_SEND (fd, p + offset, len, bytes_sent);
	}
      else
	{
	  DBG ("_javanet_sendto(): Sending....\n");
	  TARGET_NATIVE_NETWORK_SOCKET_SEND_WITH_ADDRESS_PORT (fd, p + offset,
							       len, addr, port,
							       bytes_sent);
	}

      if (bytes_sent < 0)
	{
	  if (TARGET_NATIVE_LAST_ERROR ()
	      != TARGET_NATIVE_ERROR_INTERRUPT_FUNCTION_CALL)
	    {
	      JCL_ThrowException (env, IO_EXCEPTION,
				  TARGET_NATIVE_LAST_ERROR_STRING ());
	      break;
	    }
	}
      else
	{
	  len -= bytes_sent;
	  addr += bytes_sent;
	}
    }

  (*env)->ReleaseByteArrayElements (env, buf, p, 0);

#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Sets the specified option for a socket
 */
void
_javanet_set_option (JNIEnv * env, jobject this, jint option_id, jobject val)
{
#ifndef WITHOUT_NETWORK
  int fd;
  int optval;
  jclass cls;
  jmethodID mid;
  int address;
  int result;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Get the real file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_set_option(): no native file descriptor");
      return;
    }

  /* We need a class object for all cases below */
  cls = (*env)->GetObjectClass (env, val);
  if (cls == NULL)
    return;

  /* Process the option request */
  result = TARGET_NATIVE_ERROR;
  switch (option_id)
    {
      /* TCP_NODELAY case.  val is a Boolean that tells us what to do */
    case SOCKOPT_TCP_NODELAY:
      mid = (*env)->GetMethodID (env, cls, "booleanValue", "()Z");
      if (mid == NULL)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      "Internal error: _javanet_set_option()");
	  return;
	}

      /* Should be a 0 or a 1 */
      optval = (*env)->CallBooleanMethod (env, val, mid);
      if ((*env)->ExceptionOccurred (env))
	return;

      TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_TCP_NODELAY (fd, optval,
							   result);
      break;

      /* SO_LINGER case.  If val is a boolean, then it will always be set
         to false indicating disable linger, otherwise it will be an
         integer that contains the linger value */
    case SOCKOPT_SO_LINGER:
      mid = (*env)->GetMethodID (env, cls, "booleanValue", "()Z");
      if (mid)
	{
	  /* We are disabling linger */
	  TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_LINGER (fd, 1, 0,
							     result);
	}
      else
	{
	  /* Clear exception if thrown for failure to do method lookup
	     above */
	  if ((*env)->ExceptionOccurred (env))
	    (*env)->ExceptionClear (env);

	  mid = (*env)->GetMethodID (env, cls, "intValue", "()I");
	  if (mid == NULL)
	    {
	      JCL_ThrowException (env, IO_EXCEPTION,
				  "Internal error: _javanet_set_option()");
	      return;
	    }

	  optval = (*env)->CallIntMethod (env, val, mid);
	  if ((*env)->ExceptionOccurred (env))
	    return;

	  TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_LINGER (fd, 0, optval,
							     result);
	}
      break;

      /* SO_TIMEOUT case. Val will be an integer with the new value */
      /* Not writable on Linux */
    case SOCKOPT_SO_TIMEOUT:
#ifdef SO_TIMEOUT
      mid = (*env)->GetMethodID (env, cls, "intValue", "()I");
      if (mid == NULL)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      "Internal error: _javanet_set_option()");
	  return;
	}

      optval = (*env)->CallIntMethod (env, val, mid);
      if ((*env)->ExceptionOccurred (env))
	return;

      TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_TIMEOUT (fd, optval, result);
#else
      result = TARGET_NATIVE_OK;
#endif
      break;

    case SOCKOPT_SO_SNDBUF:
    case SOCKOPT_SO_RCVBUF:
      mid = (*env)->GetMethodID (env, cls, "intValue", "()I");
      if (mid == NULL)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      "Internal error: _javanet_set_option()");
	  return;
	}


      optval = (*env)->CallIntMethod (env, val, mid);
      if ((*env)->ExceptionOccurred (env))
	return;

      if (option_id == SOCKOPT_SO_SNDBUF)
	TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_SNDBUF (fd, optval,
							   result);
      else
	TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_SO_RCDBUF (fd, optval,
							   result);
      break;

      /* TTL case.  Val with be an Integer with the new time to live value */
    case SOCKOPT_IP_TTL:
      mid = (*env)->GetMethodID (env, cls, "intValue", "()I");
      if (!mid)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      "Internal error: _javanet_set_option()");
	  return;
	}

      optval = (*env)->CallIntMethod (env, val, mid);
      if ((*env)->ExceptionOccurred (env))
	return;

      TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_TTL (fd, optval, result);
      break;

      /* Multicast Interface case - val is InetAddress object */
    case SOCKOPT_IP_MULTICAST_IF:
      address = _javanet_get_netaddr (env, val);

      if ((*env)->ExceptionOccurred (env))
	return;

      TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_IP_MULTICAST_IF (fd, address,
							       result);
      break;

    case SOCKOPT_SO_REUSEADDR:
      mid = (*env)->GetMethodID (env, cls, "booleanValue", "()Z");
      if (mid == NULL)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      "Internal error: _javanet_set_option()");
	  return;
	}

      /* Should be a 0 or a 1 */
      optval = (*env)->CallBooleanMethod (env, val, mid);
      if ((*env)->ExceptionOccurred (env))
	return;

      TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_REUSE_ADDRESS (fd, optval,
							     result);
      break;

    case SOCKOPT_SO_KEEPALIVE:
      mid = (*env)->GetMethodID (env, cls, "booleanValue", "()Z");
      if (mid == NULL)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      "Internal error: _javanet_set_option()");
	  return;
	}

      /* Should be a 0 or a 1 */
      optval = (*env)->CallBooleanMethod (env, val, mid);
      if ((*env)->ExceptionOccurred (env))
	return;

      TARGET_NATIVE_NETWORK_SOCKET_SET_OPTION_KEEP_ALIVE (fd, optval, result);
      break;

    case SOCKOPT_SO_BINDADDR:
      JCL_ThrowException (env, SOCKET_EXCEPTION, "This option cannot be set");
      break;

    default:
      JCL_ThrowException (env, SOCKET_EXCEPTION, "Unrecognized option");
      return;
    }

  /* Check to see if above operations succeeded */
  if (result != TARGET_NATIVE_OK)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  TARGET_NATIVE_LAST_ERROR_STRING ());
      return;
    }
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Retrieves the specified option values for a socket
 */
jobject
_javanet_get_option (JNIEnv * env, jobject this, jint option_id)
{
#ifndef WITHOUT_NETWORK
  int fd;
  int flag, optval;
  int address;
  int result;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Get the real file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "Internal error: _javanet_get_option(): no native file descriptor");
      return (0);
    }

  /* Process the option requested */
  switch (option_id)
    {
      /* TCP_NODELAY case.  Return a Boolean indicating on or off */
    case SOCKOPT_TCP_NODELAY:
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_TCP_NODELAY (fd, optval,
							   result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      if (optval)
	return (_javanet_create_boolean (env, JNI_TRUE));
      else
	return (_javanet_create_boolean (env, JNI_FALSE));

      break;

      /* SO_LINGER case.  If disabled, return a Boolean object that represents
         false, else return an Integer that is the value of SO_LINGER */
    case SOCKOPT_SO_LINGER:
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_LINGER (fd, flag, optval,
							 result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      if (optval)
	return (_javanet_create_integer (env, JNI_TRUE));
      else
	return (_javanet_create_boolean (env, JNI_FALSE));

      break;

      /* SO_TIMEOUT case. Return an Integer object with the timeout value */
    case SOCKOPT_SO_TIMEOUT:
#ifdef SO_TIMEOUT
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_TIMEOUT (fd, optval, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}
      return (_javanet_create_integer (env, optval));
#else
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "SO_TIMEOUT not supported on this platform");
      return (0);
#endif /* not SO_TIMEOUT */
      break;

    case SOCKOPT_SO_SNDBUF:
    case SOCKOPT_SO_RCVBUF:
      if (option_id == SOCKOPT_SO_SNDBUF)
	TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_SNDBUF (fd, optval,
							   result);
      else
	TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_SO_RCDBUF (fd, optval,
							   result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      return (_javanet_create_integer (env, optval));
      break;

      /* The TTL case.  Return an Integer with the Time to Live value */
    case SOCKOPT_IP_TTL:
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_TTL (fd, optval, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      return (_javanet_create_integer (env, optval));
      break;

      /* Multicast interface case */
    case SOCKOPT_IP_MULTICAST_IF:
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_IP_MULTICAST_IF (fd, address,
							       result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      return (_javanet_create_inetaddress (env, address));
      break;

    case SOCKOPT_SO_BINDADDR:
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_BIND_ADDRESS (fd, address,
							    result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      return (_javanet_create_inetaddress (env, address));
      break;

    case SOCKOPT_SO_REUSEADDR:
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_REUSE_ADDRESS (fd, optval,
							     result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      if (optval)
	return (_javanet_create_boolean (env, JNI_TRUE));
      else
	return (_javanet_create_boolean (env, JNI_FALSE));

      break;

    case SOCKOPT_SO_KEEPALIVE:
      TARGET_NATIVE_NETWORK_SOCKET_GET_OPTION_KEEP_ALIVE (fd, optval, result);
      if (result != TARGET_NATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      TARGET_NATIVE_LAST_ERROR_STRING ());
	  return (0);
	}

      if (optval)
	return (_javanet_create_boolean (env, JNI_TRUE));
      else
	return (_javanet_create_boolean (env, JNI_FALSE));

      break;

    default:
      JCL_ThrowException (env, SOCKET_EXCEPTION, "No such option");
      return (0);
    }

  return (0);
#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

void
_javanet_shutdownInput (JNIEnv * env, jobject this)
{
  int fd;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Get the real file descriptor. */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "Internal error: _javanet_get_option(): no native file descriptor");
      return;
    }

  /* Shutdown input stream of socket. */
  if (shutdown (fd, SHUT_RD) == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "Can't shutdown input of socket");
      return;
    }
}

void
_javanet_shutdownOutput (JNIEnv * env, jobject this)
{
  int fd;

  assert (env != NULL);
  assert ((*env) != NULL);

  /* Get the real file descriptor. */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "Internal error: _javanet_get_option(): no native file descriptor");
      return;
    }

  /* Shutdown output stream of socket. */
  if (shutdown (fd, SHUT_WR) == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "Can't shutdown output of socket");
      return;
    }
}

/* end of file */
