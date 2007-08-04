/* javanet.c - Common internal functions for the java.net package
   Copyright (C) 1998, 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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

#include "cpnative.h"
#include "cpnet.h"

#include "javanet.h"

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
_javanet_create_localfd (JNIEnv * env, jobject this, jboolean stream)
{
  jclass this_cls, fd_cls;
  jfieldID fid;
  jmethodID mid;
  jobject fd_obj;

  DBG ("_javanet_create_localfd(): Entered _javanet_create_localfd\n");

  /* Look up the fd field */
  if (stream)
    this_cls = (*env)->FindClass(env, "java/net/SocketImpl");
  else
    this_cls = (*env)->FindClass(env, "java/net/DatagramSocketImpl");
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
jobject
_javanet_create_inetaddress (JNIEnv * env, cpnet_address *netaddr)
{
#ifndef WITHOUT_NETWORK
  jbyte octets[4];
  char buf[64];
  jclass ia_cls;
  jmethodID mid;
  jstring ip_str;
  jobject ia;

  /* Build a string IP address */
  cpnet_IPV4AddressToBytes(netaddr, octets);
  sprintf (buf, "%d.%d.%d.%d", (int) (unsigned char)octets[0], (int)(unsigned char)octets[1], (int)(unsigned char)octets[2], (int)(unsigned char)octets[3]);
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
_javanet_set_remhost (JNIEnv * env, jobject this, cpnet_address *netaddr)
{
  jobject ia;

  DBG ("_javanet_set_remhost(): Entered _javanet_set_remhost\n");

  /* Get an InetAddress object */
  ia = _javanet_create_inetaddress (env, netaddr);
  if (ia == NULL)
    return;

  _javanet_set_remhost_addr (env, this, ia);
}


/*************************************************************************/

/*
 * Returns an Internet address for the passed in InetAddress object
 */
cpnet_address *
_javanet_get_ip_netaddr (JNIEnv * env, jobject addr)
{
#ifndef WITHOUT_NETWORK
  jclass cls = 0;
  jmethodID mid;
  jarray arr = 0;
  jbyte *octets;
  cpnet_address *netaddr;
  jint len;

  DBG ("_javanet_get_ip_netaddr(): Entered _javanet_get_ip_netaddr\n");

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

  DBG ("_javanet_get_ip_netaddr(): Got getAddress method\n");

  arr = (*env)->CallObjectMethod (env, addr, mid);
  if (arr == NULL)
    return 0;

  DBG ("_javanet_get_ip_netaddr(): Got the address\n");

  /* Turn the IP address into a system cpnet address.
   * If the length is 4 then it is an IPV4 address, if it
   * is 16 then it is an IPV6 address else it is an InternError. */
  len = (*env)->GetArrayLength (env, arr);
  if (len != 4 && len != 16)
    {
      JCL_ThrowException (env, IO_EXCEPTION, "Internal Error");
      return 0;
    }
  DBG ("_javanet_get_ip_netaddr(): Length ok\n");

  octets = (*env)->GetByteArrayElements (env, arr, 0);
  if (octets == NULL)
    return 0;

  DBG ("_javanet_get_ip_netaddr(): Grabbed bytes\n");

  switch (len)
    {
    case 4:      
      netaddr = cpnet_newIPV4Address(env);
      cpnet_bytesToIPV4Address(netaddr, octets);
      break;
#ifdef HAVE_INET6
    case 16:
      netaddr = cpnet_newIPV6Address(env);
      cpnet_bytesToIPV6Address(netaddr, octets);
      break;
#endif
    default:
      /* This should not happen as we have checked before.
       * But that way we shut the compiler warnings */
      JCL_ThrowException (env, IO_EXCEPTION, "Internal Error");
      return 0;
      
    }

  (*env)->ReleaseByteArrayElements (env, arr, octets, 0);
  DBG ("_javanet_get_ip_netaddr(): Done getting addr\n");

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

  if (stream)
    {
      /* create a stream socket */
      result = cpnet_openSocketStream(env, &fd, AF_INET);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      cpnative_getErrorString (result));
	  return;
	}
    }
  else
    {
      /* create a datagram socket, set broadcast option */
      result = cpnet_openSocketDatagram (env, &fd, AF_INET);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      cpnative_getErrorString (result));
	  return;
	}
      result = cpnet_setBroadcast(env, fd, 1);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, IO_EXCEPTION,
			      cpnative_getErrorString (result));
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
	  result = cpnet_close(env, fd);
	  if (result != CPNATIVE_OK && result != CPNATIVE_EINTR)
	    return;
	}
      while (result != CPNATIVE_OK);
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
      result = cpnet_close (env, fd);
      if (result != CPNATIVE_OK)
	{
	  /* Only throw an error when a "real" error occurs. */
	  if (result != CPNATIVE_EINTR && result != ENOTCONN && result != ECONNRESET && result != EBADF)
	    JCL_ThrowException (env, IO_EXCEPTION,
				cpnative_getErrorString (result));
	}
    }
  while (error == CPNATIVE_EINTR);

#else /* not WITHOUT_NETWORK */
#endif /* not WITHOUT_NETWORK */
}

/*************************************************************************/

/*
 * Connects to the specified destination.
 */
void
_javanet_connect (JNIEnv * env, jobject this, jobject addr, jint port,
		  jboolean stream)
{
#ifndef WITHOUT_NETWORK
  cpnet_address *netaddr;
  int fd;
  int result;
  cpnet_address *local_addr;
  cpnet_address *remote_addr;

  DBG ("_javanet_connect(): Entered _javanet_connect\n");

  /* Pre-process input variables */
  netaddr = _javanet_get_ip_netaddr (env, addr);
  if ((*env)->ExceptionOccurred (env))
    return;

  if (port == -1)
    port = 0;

  cpnet_addressSetPort(netaddr, port);

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
      result = cpnet_connect (env, fd, netaddr);
      if (result != CPNATIVE_OK && result != CPNATIVE_EINTR)
	{
	  JCL_ThrowException (env, CONNECT_EXCEPTION,
			      cpnative_getErrorString (result));
	  return;
	}
    }
  while (result != CPNATIVE_OK);
  
  DBG ("_javanet_connect(): Connected successfully\n");

  /* Populate instance variables */
  result = cpnet_getLocalAddr (env, fd, &local_addr);
  if (result != CPNATIVE_OK)
    {
      cpnet_freeAddress(env, netaddr);
      JCL_ThrowException (env, IO_EXCEPTION,
			  cpnative_getErrorString (result));
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_close (env, fd);
      return;
    }

  _javanet_create_localfd (env, this, stream);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_freeAddress(env, netaddr);
      cpnet_freeAddress(env, local_addr);
      cpnet_close (env, fd);
      return;
    }
  DBG ("_javanet_connect(): Created fd\n");

  if (stream)
    _javanet_set_int_field (env, this, "java/net/SocketImpl", "localport",
			    cpnet_addressGetPort(local_addr));
  else
    _javanet_set_int_field (env, this, "java/net/DatagramSocketImpl",
			    "localPort", cpnet_addressGetPort(local_addr));

  cpnet_freeAddress (env, local_addr);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_freeAddress(env, netaddr);
      cpnet_close (env, fd);
      return;
    }
  DBG ("_javanet_connect(): Set the local port\n");

  result = cpnet_getRemoteAddr (env, fd, &remote_addr);
  if (result != CPNATIVE_OK)
    {
      cpnet_freeAddress(env, netaddr);
      JCL_ThrowException (env, IO_EXCEPTION,
			  cpnative_getErrorString (result));
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_close (env, fd);
      return;
    }

  if (stream)
    {
      if (cpnet_isAddressEqual(remote_addr, netaddr))
	{
	  _javanet_set_remhost_addr (env, this, addr);
	}
      else
	{
	  _javanet_set_remhost (env, this, remote_addr);
	}
      cpnet_freeAddress(env, netaddr);

      if ((*env)->ExceptionOccurred (env))
	{
	  /* We don't care whether this succeeds. close() will cleanup later.
	   */
	  cpnet_freeAddress (env, remote_addr);
	  cpnet_close (env, fd);
	  return;
	}
      DBG ("_javanet_connect(): Set the remote host\n");

      _javanet_set_int_field (env, this, "java/net/SocketImpl", "port",
			      cpnet_addressGetPort(remote_addr));
      cpnet_freeAddress (env, remote_addr);

      if ((*env)->ExceptionOccurred (env))
	{
	  /* We don't care whether this succeeds. close() will cleanup later.
	   */
	  cpnet_close (env, fd);
	  return;
	}
      DBG ("_javanet_connect(): Set the remote port\n");
    }
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
  jint fd;
  cpnet_address *tmpaddr;
  cpnet_address *local_addr;
  int result;

  DBG ("_javanet_bind(): Entering native bind()\n");
 
 /* Grab the real socket file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_connect(): no native file descriptor");
      return;
    }

  cpnet_setReuseAddress (env, fd, 1);

  /* Get the address to connect to */
  tmpaddr = _javanet_get_ip_netaddr (env, addr);
  if ((*env)->ExceptionOccurred (env))
    return;

  cpnet_addressSetPort (tmpaddr, port);
  result = cpnet_bind(env, fd, tmpaddr);
  cpnet_freeAddress (env, tmpaddr);
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, BIND_EXCEPTION,
			  cpnative_getErrorString (result));
      return;
    }
  DBG ("_javanet_bind(): Past bind\n");

  /* Update instance variables, specifically the local port number */
  result = cpnet_getLocalAddr (env, fd, &local_addr);
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  cpnative_getErrorString (result));
      return;
    }

  if (stream)
    _javanet_set_int_field (env, this, "java/net/SocketImpl",
			    "localport", cpnet_addressGetPort (local_addr));
  else
    _javanet_set_int_field (env, this, "java/net/DatagramSocketImpl",
			    "localPort", cpnet_addressGetPort (local_addr));
  DBG ("_javanet_bind(): Past update port number\n");

  cpnet_freeAddress (env, local_addr);

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

  /* Get the real file descriptor */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  "Internal error: _javanet_listen(): no native file descriptor");
      return;
    }

  /* Start listening */
  result = cpnet_listen (env, fd, queuelen);
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  cpnative_getErrorString (result));
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
  cpnet_address *remote_addr, *local_addr;

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
      result = cpnet_accept (env, fd, &newfd);
      if (result != CPNATIVE_OK && result != CPNATIVE_EINTR)
	{
	  if (result == ETIMEDOUT || result == EAGAIN)
	    JCL_ThrowException (env, "java/net/SocketTimeoutException",
				"Accept operation timed out");
	  else
	    JCL_ThrowException (env, IO_EXCEPTION,
				cpnative_getErrorString (result));
	  return;
	}
    }
  while (result != CPNATIVE_OK);

  /* Reset the inherited timeout. */
  cpnet_setSocketTimeout (env, newfd, 0);

  /* Populate instance variables */
  _javanet_set_int_field (env, impl, "gnu/java/net/PlainSocketImpl",
			  "native_fd", newfd);

  if ((*env)->ExceptionOccurred (env))
    {
      /* Try to make sure we close the socket since close() won't work. */
      do
	{
	  result = cpnet_close (env, newfd);
	  if (result != CPNATIVE_OK && result != CPNATIVE_EINTR)
	    return;
	}
      while (result != CPNATIVE_OK);
      return;
    }

  result = cpnet_getLocalAddr (env, newfd, &local_addr);
  if (result != CPNATIVE_OK)
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_close (env, newfd);
      JCL_ThrowException (env, IO_EXCEPTION,
			  cpnative_getErrorString (result));
      return;
    }

  _javanet_create_localfd (env, impl, 1);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_freeAddress (env, local_addr);
      cpnet_close (env, newfd);
      return;
    }

  _javanet_set_int_field (env, impl, "java/net/SocketImpl", "localport",
			  cpnet_addressGetPort (local_addr));
  cpnet_freeAddress (env, local_addr);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_close (env, newfd);
      return;
    }

  result = cpnet_getRemoteAddr (env, newfd, &remote_addr);
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, IO_EXCEPTION,
			  cpnative_getErrorString (result));
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_close (env, newfd);
      return;
    }

  _javanet_set_remhost (env, impl, remote_addr);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_close (env, newfd);
      cpnet_freeAddress (env, remote_addr);
      return;
    }

  _javanet_set_int_field (env, impl, "java/net/SocketImpl", "port",
			  cpnet_addressGetPort (remote_addr));
  cpnet_freeAddress (env, remote_addr);
  if ((*env)->ExceptionOccurred (env))
    {
      /* We don't care whether this succeeds. close() will cleanup later. */
      cpnet_close (env, newfd);
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
		   int len, cpnet_address **addr)
{
#ifndef WITHOUT_NETWORK
  int fd;
  jbyte *p;
  cpnet_address *from_addr;
  jint received_bytes;
  int result;

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
  from_addr = NULL;
  do
    {
      if (addr != NULL)
	{
	  result = cpnet_recvFrom (env, fd, p + offset, len, &from_addr, &received_bytes);
	}
      else
	{
	  result = cpnet_recv (env, fd, p + offset, len, &received_bytes);
	}
    }
  while (result == CPNATIVE_EINTR);
  if (result != 0)
    {
      if (result == EAGAIN || result == ETIMEDOUT)
	JCL_ThrowException (env, "java/net/SocketTimeoutException", "Receive operation timed out");
      else
	JCL_ThrowException (env, IO_EXCEPTION,
			    cpnative_getErrorString (result));
 
      /* Cleanup and return. */
      (*env)->ReleaseByteArrayElements (env, buf, p, 0);
      return 0;
    }

  (*env)->ReleaseByteArrayElements (env, buf, p, 0);

  /* Handle return addr case */
  if (addr != NULL)
    {
      (*addr) = from_addr;
    }

  /* zero bytes received means recv() noticed the other side orderly
     closing the connection. */
  if (received_bytes == 0)
    received_bytes = -1;

  return received_bytes;
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
		 cpnet_address *addr)
{
#ifndef WITHOUT_NETWORK
  int fd;
  jbyte *p;
  jint bytes_sent;
  int result;

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
      if (addr == NULL)
	{
	  DBG ("_javanet_sendto(): Sending....\n");
	  result = cpnet_send (env, fd, p + offset, len, &bytes_sent);
	}
      else
	{
	  DBG ("_javanet_sendto(): Sending....\n");
	  result = cpnet_sendTo (env, fd, p + offset, len, addr, &bytes_sent);
	}

      if (result == EDESTADDRREQ)
	{
	  JCL_ThrowException (env, NULL_EXCEPTION,
			      "Socket is not connected and no address is given");
	  break;
	}
	
      if (bytes_sent < 0)
	{
	  if (result != CPNATIVE_EINTR)
	    {
	      JCL_ThrowException (env, IO_EXCEPTION,
				  cpnative_getErrorString (result));
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
  cpnet_address * address;
  int result = CPNATIVE_OK;

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

      result = cpnet_setSocketTCPNoDelay (env, fd, optval);
      break;

      /* SO_LINGER case.  If val is a boolean, then it will always be set
         to false indicating disable linger, otherwise it will be an
         integer that contains the linger value */
    case SOCKOPT_SO_LINGER:
      mid = (*env)->GetMethodID (env, cls, "booleanValue", "()Z");
      if (mid)
	{
	  /* We are disabling linger */
	  result = cpnet_setLinger (env, fd, JNI_FALSE, 0);
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

	  result = cpnet_setLinger(env, fd, JNI_TRUE, optval);
	}
      break;

      /* SO_TIMEOUT case. Val will be an integer with the new value */
      /* Not writable on Linux */
    case SOCKOPT_SO_TIMEOUT:
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

      result = cpnet_setSocketTimeout (env, fd, optval);
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
	result = cpnet_setSendBuf (env, fd, optval);
      else
	result = cpnet_setRecvBuf (env, fd, optval);
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

      result = cpnet_setTTL (env, fd, optval);
      break;

      /* Multicast Interface case - val is InetAddress object */
    case SOCKOPT_IP_MULTICAST_IF:
      address = _javanet_get_ip_netaddr (env, val);

      if ((*env)->ExceptionOccurred (env))
	return;
      
      result = cpnet_setMulticastIF (env, fd, address);
      cpnet_freeAddress (env, address);
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

      result = cpnet_setReuseAddress (env, fd, optval);
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

      result = cpnet_setKeepAlive (env, fd, optval);
      break;

    case SOCKOPT_SO_BINDADDR:
      JCL_ThrowException (env, SOCKET_EXCEPTION, "This option cannot be set");
      break;

    default:
      JCL_ThrowException (env, SOCKET_EXCEPTION, "Unrecognized option");
      return;
    }

  /* Check to see if above operations succeeded */
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  cpnative_getErrorString (result));
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
  cpnet_address *address;
  int result;
  jobject obj;

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
      result = cpnet_getSocketTCPNoDelay (env, fd, &optval);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
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
      result = cpnet_getLinger (env, fd, &flag, &optval);

      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}

      if (flag)
	return (_javanet_create_integer (env, optval));
      else
	return (_javanet_create_boolean (env, JNI_FALSE));

      break;

      /* SO_TIMEOUT case. Return an Integer object with the timeout value */
    case SOCKOPT_SO_TIMEOUT:
      result = cpnet_getSocketTimeout (env, fd, &optval);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}
      return (_javanet_create_integer (env, optval));
      break;

    case SOCKOPT_SO_SNDBUF:
    case SOCKOPT_SO_RCVBUF:
      if (option_id == SOCKOPT_SO_SNDBUF)
	result = cpnet_getSendBuf (env, fd, &optval);
      else
	result = cpnet_getRecvBuf (env, fd, &optval);

      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}

      return (_javanet_create_integer (env, optval));
      break;

      /* The TTL case.  Return an Integer with the Time to Live value */
    case SOCKOPT_IP_TTL:
      result = cpnet_getTTL (env, fd, &optval);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}

      return (_javanet_create_integer (env, optval));
      break;

      /* Multicast interface case */
    case SOCKOPT_IP_MULTICAST_IF:
      result = cpnet_getMulticastIF (env, fd, &address);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}

      obj = _javanet_create_inetaddress (env, address);
      cpnet_freeAddress (env, address);

      return obj;
      break;

    case SOCKOPT_SO_BINDADDR:
      result = cpnet_getLocalAddr (env, fd, &address);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}

      obj = _javanet_create_inetaddress (env, address);
      cpnet_freeAddress (env, address);

      return obj;
      break;

    case SOCKOPT_SO_REUSEADDR:
      result = cpnet_getReuseAddress (env, fd, &optval);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}

      if (optval)
	return _javanet_create_boolean (env, JNI_TRUE);
      else
	return _javanet_create_boolean (env, JNI_FALSE);

      break;

    case SOCKOPT_SO_KEEPALIVE:
      result = cpnet_getKeepAlive (env, fd, &optval);
      if (result != CPNATIVE_OK)
	{
	  JCL_ThrowException (env, SOCKET_EXCEPTION,
			      cpnative_getErrorString (result));
	  return (0);
	}

      if (optval)
	return _javanet_create_boolean (env, JNI_TRUE);
      else
	return _javanet_create_boolean (env, JNI_FALSE);

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
  int result;
  int fd;

  /* Get the real file descriptor. */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "Internal error: _javanet_get_option(): no native file descriptor");
      return;
    }

  /* Shutdown input stream of socket. */
  result = cpnet_shutdown (env, fd, CPNET_SHUTDOWN_READ);
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  cpnative_getErrorString (result));
      return;
    }
}

void
_javanet_shutdownOutput (JNIEnv * env, jobject this)
{
  int fd;
  int result;

  /* Get the real file descriptor. */
  fd = _javanet_get_int_field (env, this, "native_fd");
  if (fd == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  "Internal error: _javanet_get_option(): no native file descriptor");
      return;
    }

  /* Shutdown output stream of socket. */
  result = cpnet_shutdown (env, fd, CPNET_SHUTDOWN_WRITE);
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
			  cpnative_getErrorString (result));
      return;
    }
}

/* end of file */
