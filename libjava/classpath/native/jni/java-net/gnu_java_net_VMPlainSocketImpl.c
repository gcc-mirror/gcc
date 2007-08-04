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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <config-int.h>

#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <net/if.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
 
#include <jni.h>
#include <jcl.h>

#include "cpnative.h"
#include "cpnet.h"
#include "cpio.h"
#include "javanet.h"

#include "gnu_java_net_VMPlainSocketImpl.h"

#define THROW_NO_NETWORK(env) JCL_ThrowException (env, "java/lang/InternalError", "this platform not configured for network support")
#define THROW_NO_IPV6(env)    JCL_ThrowException (env, "java/lang/InternalError", "IPv6 support not available")

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    bind
 * Signature: (I[BI)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_bind (JNIEnv *env,
                                          jclass clazz __attribute__((unused)),
                                          jint fd, jbyteArray addr, jint port)
{
  struct sockaddr_in sockaddr;
  jbyte *elems = NULL;
  int ret;

  if (addr != NULL)
    elems = (*env)->GetByteArrayElements (env, addr, NULL);

  memset(&sockaddr, 0, sizeof (struct sockaddr_in));
  sockaddr.sin_family = AF_INET;
  sockaddr.sin_port = htons (port);
  /* addr is already in network byte order. */
  if (elems != NULL)
    sockaddr.sin_addr.s_addr = *((uint32_t *) elems);
  else
    sockaddr.sin_addr.s_addr = INADDR_ANY;

  /* bind(2) from BSD says bind will never return EINTR */
  /* bind is not a blocking system call */
  ret = bind (fd, (struct sockaddr *) &sockaddr, sizeof (struct sockaddr_in));

  if (elems != NULL)
    (*env)->ReleaseByteArrayElements (env, addr, elems, JNI_ABORT);

  if (-1 == ret)
    JCL_ThrowException (env, BIND_EXCEPTION, strerror (errno));
    
  cpio_closeOnExec(ret);
}


/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    bind6
 * Signature: (I[BI)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_bind6 (JNIEnv *env,
                                           jclass c __attribute__((unused)),
                                           jint fd, jbyteArray addr, jint port)
{
#ifdef HAVE_INET6
  struct sockaddr_in6 sockaddr;
  jbyte *elems;
  int ret;

  elems = (*env)->GetByteArrayElements (env, addr, NULL);

  memset (&sockaddr, 0, sizeof (struct sockaddr_in6));
  sockaddr.sin6_family = AF_INET6;
  sockaddr.sin6_port = htons (port);
  memcpy (&sockaddr.sin6_addr.s6_addr, elems, 16);

  /* bind(2) from BSD says bind will never return EINTR */
  /* bind is not a blocking system call */
  ret = bind (fd, (struct sockaddr *) &sockaddr,
              sizeof (struct sockaddr_in6));

  (*env)->ReleaseByteArrayElements (env, addr, elems, JNI_ABORT);

  if (-1 == ret)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
#else
  THROW_NO_IPV6(env);
#endif
}


/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    listen
 * Signature: (II)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_listen (JNIEnv *env,
                                            jclass c __attribute__((unused)),
                                            jint fd, jint backlog)
{
  int ret;

  /* listen(2) says that this call will never return EINTR */
  /* listen is not a blocking system call */
  if ((ret = listen (fd, backlog)) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
}


/* These constants are also defined in java/net/SocketOptions.java.
 * Except for CPNET_IP_TTL which is defined in 
 * vm/reference/gnu/java/net/VMPlainSocketImpl.java .
 */
enum java_sockopt {
  CPNET_SO_KEEPALIVE = 0x8,
  CPNET_SO_LINGER = 0x80,
  CPNET_SO_TIMEOUT = 0x1006,
  CPNET_SO_BINDADDR = 0x0F,
  CPNET_SO_SNDBUF = 0x1001,
  CPNET_SO_RCVBUF = 0x1002,
  CPNET_SO_REUSEADDR = 0x04,
  CPNET_SO_BROADCAST = 0x20,
  CPNET_SO_OOBINLINE = 0x1003,
  CPNET_TCP_NODELAY = 0x01,
  CPNET_IP_MULTICAST_IF = 0x10,
  CPNET_IP_MULTICAST_IF2 = 0x1F,
  CPNET_IP_MULTICAST_LOOP = 0x12,
  CPNET_IP_TOS = 0x03,
  CPNET_IP_TTL = 0x1E61
};


/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    setOption
 * Signature: (III)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_setOption (JNIEnv *env,
                                               jclass c __attribute__((unused)),
                                               jint fd, jint option, jint value)
{
  enum java_sockopt joption = (enum java_sockopt) option;
  int optname = -1;
  int level = SOL_SOCKET;
  const int _value = value;
  struct linger _linger;
  struct timeval _timeo;
  void *optval = (void *) &_value;
  socklen_t optlen = sizeof (int);
  
  switch (joption)
    {
    case CPNET_IP_MULTICAST_LOOP:
      level = IPPROTO_IP;
      optname = IP_MULTICAST_LOOP;
      break;

    case CPNET_SO_KEEPALIVE:
      optname = SO_KEEPALIVE;
      break;

    case CPNET_SO_LINGER:
      optname = SO_LINGER;
      if (_value == -1)
        _linger.l_onoff = 0;
      else
        _linger.l_onoff = 1;
      _linger.l_linger = _value;
      optval = &_linger;
      optlen = sizeof (struct linger);
      break;

    case CPNET_SO_TIMEOUT:
      optname = SO_RCVTIMEO;
      _timeo.tv_sec = value / 1000;
      _timeo.tv_usec = (value % 1000) * 1000;
      optval = &_timeo;
      optlen = sizeof (struct timeval);
      break;

    case CPNET_SO_SNDBUF:
      optname = SO_SNDBUF;
      break;

    case CPNET_SO_RCVBUF:
      optname = SO_RCVBUF;
      break;

    case CPNET_SO_REUSEADDR:
      optname = SO_REUSEADDR;
      break;

    case CPNET_SO_BROADCAST:
      optname = SO_BROADCAST;
      break;

    case CPNET_SO_OOBINLINE:
      optname = SO_OOBINLINE;
      break;

    case CPNET_TCP_NODELAY:
      level = IPPROTO_TCP;
      optname = TCP_NODELAY;
      break;

    case CPNET_IP_TOS:
      level = IPPROTO_IP;
      optname = IP_TOS;
      break;

    case CPNET_IP_TTL:
      level = IPPROTO_IP;
      optname = IP_TTL;
      break;

    case CPNET_SO_BINDADDR:
    case CPNET_IP_MULTICAST_IF:
    case CPNET_IP_MULTICAST_IF2:
      JCL_ThrowException (env, IO_EXCEPTION, "argument not a boolean or integer option");
      return;
    }

  if (setsockopt (fd, level, optname, (const void *) optval, optlen) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
}

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    getOption
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_net_VMPlainSocketImpl_getOption (JNIEnv *env,
                                               jclass c __attribute__((unused)),
                                               jint fd, jint option)
{
  enum java_sockopt joption = (enum java_sockopt) option;
  int optname = -1;
  int level = SOL_SOCKET;
  int value;
  struct linger linger;
  struct timeval timeo;
  void *optval = &value;
  socklen_t optlen = sizeof (int);

  switch (joption)
    {
    case CPNET_IP_MULTICAST_LOOP:
      level = IPPROTO_IP;
      optname = IP_MULTICAST_LOOP;
      break;

    case CPNET_SO_KEEPALIVE:
      optname = SO_KEEPALIVE;
      break;

    case CPNET_SO_LINGER:
      optname = SO_LINGER;
      optval = &linger;
      optlen = sizeof (struct linger);
      break;

    case CPNET_SO_TIMEOUT:
      optname = SO_RCVTIMEO;
      optval = &timeo;
      optlen = sizeof (struct timeval);
      break;

    case CPNET_SO_SNDBUF:
      optname = SO_SNDBUF;
      break;

    case CPNET_SO_RCVBUF:
      optname = SO_RCVBUF;
      break;

    case CPNET_SO_REUSEADDR:
      optname = SO_REUSEADDR;
      break;

    case CPNET_SO_BROADCAST:
      optname = SO_BROADCAST;
      break;

    case CPNET_SO_OOBINLINE:
      optname = SO_OOBINLINE;
      break;

    case CPNET_TCP_NODELAY:
      level = IPPROTO_TCP;
      optname = TCP_NODELAY;
      break;

    case CPNET_IP_TOS:
      level = IPPROTO_IP;
      optname = IP_TOS;
      break;

    case CPNET_IP_TTL:
      level = IPPROTO_IP;
      optname = IP_TTL;
      break;

    case CPNET_SO_BINDADDR:
    case CPNET_IP_MULTICAST_IF:
    case CPNET_IP_MULTICAST_IF2:
      JCL_ThrowException (env, IO_EXCEPTION, "argument not a boolean or integer option");
      return -1;
    }

  if (getsockopt (fd, level, optname, optval, &optlen) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));

  /* Returns the linger value if it is enabled or -1 in case
   * it is disabled. This is how the Java API expects it.
   */
  if (joption == CPNET_SO_LINGER)
    return (linger.l_onoff) ? linger.l_linger : -1;
  if (joption == CPNET_SO_TIMEOUT)
    return (timeo.tv_sec * 1000) + (timeo.tv_usec / 1000);

  return value;
}

JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_setMulticastInterface (JNIEnv *env,
                                                           jclass c __attribute__((unused)),
                                                           jint fd,
                                                           jint optionId __attribute__((unused)),
                                                           jobject addr)
{
  int result;
  cpnet_address *cpaddr = _javanet_get_ip_netaddr (env, addr);

  if ((*env)->ExceptionOccurred (env))
    return;

  result = setsockopt(fd, IPPROTO_IP, IP_MULTICAST_IF,
                      (struct sockaddr *) cpaddr->data, cpaddr->len);

  cpnet_freeAddress (env, cpaddr);
  
  if (result == -1)
    JCL_ThrowException (env, SOCKET_EXCEPTION, cpnative_getErrorString (errno));
}

JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_setMulticastInterface6 (JNIEnv *env,
                                                           jclass c __attribute__((unused)),
                                                           jint fd,
                                                           jint optionId __attribute__((unused)),
                                                           jstring ifname)
{
#ifdef HAVE_SETSOCKOPT
#ifdef HAVE_INET6	
  int result;
  const char *str_ifname = JCL_jstring_to_cstring (env, ifname);
  u_int if_index;

  if ((*env)->ExceptionOccurred (env))
    {
      JCL_free_cstring(env, ifname, str_ifname);
      return;
    }

  if_index = if_nametoindex(str_ifname);
  if (!if_index)
    {
      JCL_free_cstring(env, ifname, str_ifname);
      JCL_ThrowException (env, SOCKET_EXCEPTION, "interface does not exist");
      return;
    }

  result = setsockopt(fd, IPPROTO_IPV6, IPV6_MULTICAST_IF,
                      (u_int *) &if_index, sizeof(if_index));

  JCL_free_cstring(env, ifname, str_ifname);
  
  if (result == -1)
    JCL_ThrowException (env, SOCKET_EXCEPTION, cpnative_getErrorString (errno));
#else
  (void) fd;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "IPv6 support not available");
#endif /* HAVE_INET6 */
#else
  (void) fd;
  THROW_NO_IPV6(env);
#endif /* HAVE_SETSOCKOPT */
}

JNIEXPORT jobject JNICALL
Java_gnu_java_net_VMPlainSocketImpl_getMulticastInterface (JNIEnv *env,
                                                           jclass c __attribute__((unused)),
                                                           jint fd,
                                                           jint optionId __attribute__((unused)))
{
  jobject obj;
  cpnet_address *cpaddr;
  int result = cpnet_getMulticastIF (env, fd, &cpaddr);
 
  if (result != CPNATIVE_OK)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION,
                          cpnative_getErrorString (result));
      return (0);
    }

  obj = _javanet_create_inetaddress (env, cpaddr);
  cpnet_freeAddress (env, cpaddr);

  return obj;
}


/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    shutdownInput
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_shutdownInput (JNIEnv *env,
                                                   jclass c __attribute__((unused)),
                                                   jint fd)
{
  if (shutdown (fd, SHUT_RD) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
}

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    shutdownOutput
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_shutdownOutput (JNIEnv *env,
                                                    jclass c __attribute__((unused)),
                                                    jint fd)
{
  if (shutdown (fd, SHUT_WR) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
}


/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    sendUrgentData
 * Signature: (II)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_sendUrgentData (JNIEnv *env,
                                                    jclass c __attribute__((unused)),
                                                    jint fd, jint data)
{
  const char x = (char) data;

  if (send (fd, &x, 1, MSG_OOB) == -1)
    JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
}


/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    join
 * Signature: (I[B)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_join (JNIEnv *env,
                                          jclass clazz __attribute__((unused)),
                                          jint fd, jbyteArray addr)
{
#ifdef HAVE_SETSOCKOPT
  struct ip_mreq maddr;
  jbyte *addr_elems;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  maddr.imr_multiaddr.s_addr = * ((uint32_t *) addr_elems);
  maddr.imr_interface.s_addr = INADDR_ANY;

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                        &maddr, sizeof (struct ip_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) addr;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}


/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    join6
 * Signature: (I[B)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_join6 (JNIEnv *env,
                                           jclass clazz __attribute__((unused)),
                                           jint fd, jbyteArray addr)
{
#ifdef HAVE_SETSOCKOPT
#ifdef HAVE_INET6
  struct ipv6_mreq maddr;
  jbyte *addr_elems;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  memcpy (&(maddr.ipv6mr_multiaddr.s6_addr), addr_elems, 16);
  maddr.ipv6mr_interface = 0;

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IPV6, IPV6_JOIN_GROUP,
                        &maddr, sizeof (struct ipv6_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) addr;
  THROW_NO_IPV6(env);
#endif /* HAVE_INET6 */
#else
  (void) fd;
  (void) addr;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    leave
 * Signature: (I[B)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_leave (JNIEnv *env,
                                           jclass c __attribute__((unused)),
                                           jint fd, jbyteArray addr)
{
#ifdef HAVE_SETSOCKOPT
  struct ip_mreq maddr;
  jbyte *addr_elems;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  maddr.imr_multiaddr.s_addr = * ((uint32_t *) addr_elems);
  maddr.imr_interface.s_addr = INADDR_ANY;

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IP, IP_DROP_MEMBERSHIP,
                        &maddr, sizeof (struct ip_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) addr;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    leave6
 * Signature: (I[B)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_leave6 (JNIEnv *env,
                                            jclass c __attribute__((unused)),
                                            jint fd, jbyteArray addr)
{
#ifdef HAVE_SETSOCKOPT
#ifdef HAVE_INET6
  struct ipv6_mreq maddr;
  jbyte *addr_elems;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  memcpy (&(maddr.ipv6mr_multiaddr.s6_addr), addr_elems, 16);
  maddr.ipv6mr_interface = 0;

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IPV6, IPV6_LEAVE_GROUP,
                        &maddr, sizeof (struct ipv6_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) addr;
  THROW_NO_IPV6(env);
#endif /* HAVE_INET6 */
#else
  (void) fd;
  (void) addr;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}

static uint32_t getif_address (JNIEnv *env, const char *ifname);
static int getif_index (JNIEnv *env, const char *ifname);

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    joinGroup
 * Signature: (I[BILjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_joinGroup (JNIEnv *env,
                                               jclass c __attribute__((unused)),
                                               jint fd, jbyteArray addr,
                                               jstring ifname)
{
#ifdef HAVE_SETSOCKOPT
  struct ip_mreq maddr;
  jbyte *addr_elems;
  const char *str_ifname;

  if (ifname != NULL)
    {
      str_ifname = JCL_jstring_to_cstring(env, ifname);
      maddr.imr_interface.s_addr = getif_address (env, str_ifname);
      JCL_free_cstring(env, ifname, str_ifname);

      if ((*env)->ExceptionCheck (env))
        return;
    }
  else
    maddr.imr_interface.s_addr = INADDR_ANY;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  maddr.imr_multiaddr.s_addr = * ((uint32_t *) addr_elems);

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                        &maddr, sizeof (struct ip_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));

#else
  (void) fd;
  (void) addr;
  (void) ifname;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    joinGroup6
 * Signature: (I[BILjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_joinGroup6 (JNIEnv *env,
                                                jclass c __attribute__((unused)),
                                                jint fd, jbyteArray addr,
                                                jstring ifname)
{
#ifdef HAVE_SETSOCKOPT
#ifdef HAVE_INET6
  struct ipv6_mreq maddr;
  jbyte *addr_elems;
  const char *str_ifname;

  if (ifname == NULL)
    {
      str_ifname = JCL_jstring_to_cstring(env, ifname);
      maddr.ipv6mr_interface = getif_index (env, str_ifname);
      JCL_free_cstring(env, ifname, str_ifname);

      if ((*env)->ExceptionCheck (env))
        return;
    }
  else
    maddr.ipv6mr_interface = 0;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  memcpy (&(maddr.ipv6mr_multiaddr.s6_addr), addr_elems, 16);

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IPV6, IPV6_JOIN_GROUP,
                        &maddr, sizeof (struct ipv6_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) addr;
  THROW_NO_IPV6(env);
#endif /* HAVE_INET6 */
#else
  (void) fd;
  (void) addr;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    leaveGroup
 * Signature: (I[BILjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_leaveGroup (JNIEnv *env,
                                                jclass c __attribute__((unused)),
                                                jint fd, jbyteArray addr,
                                                jstring ifname)
{
#ifdef HAVE_SETSOCKOPT
  struct ip_mreq maddr;
  jbyte *addr_elems;
  const char *str_ifname;

  if (ifname != NULL)
    {
      str_ifname = JCL_jstring_to_cstring(env, ifname);
      maddr.imr_interface.s_addr = getif_address (env, str_ifname);
      JCL_free_cstring(env, ifname, str_ifname);

      if ((*env)->ExceptionCheck (env))
        return;
    }
  else
    maddr.imr_interface.s_addr = INADDR_ANY;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  maddr.imr_multiaddr.s_addr = * ((uint32_t *) addr_elems);

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IP, IP_DROP_MEMBERSHIP,
                        &maddr, sizeof (struct ip_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) addr;
  (void) ifname;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}

/*
 * Class:     gnu_java_net_VMPlainSocketImpl
 * Method:    leaveGroup6
 * Signature: (I[BILjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_net_VMPlainSocketImpl_leaveGroup6 (JNIEnv *env,
                                                jclass c __attribute__((unused)),
                                                jint fd, jbyteArray addr,
                                                jstring ifname)
{
#ifdef HAVE_SETSOCKOPT
#ifdef HAVE_INET6
  struct ipv6_mreq maddr;
  jbyte *addr_elems;
  const char *str_ifname;

  if (ifname == NULL)
    {
      str_ifname = JCL_jstring_to_cstring(env, ifname);
      maddr.ipv6mr_interface = getif_index (env, str_ifname);
      JCL_free_cstring(env, ifname, str_ifname);

      if ((*env)->ExceptionCheck (env))
        return;
    }
  else
    maddr.ipv6mr_interface = 0;

  addr_elems = (*env)->GetByteArrayElements (env, addr, NULL);
  if (addr_elems == NULL)
    return;

  memcpy (&(maddr.ipv6mr_multiaddr.s6_addr), addr_elems, 16);

  (*env)->ReleaseByteArrayElements (env, addr, addr_elems, JNI_ABORT);

  if (-1 == setsockopt (fd, IPPROTO_IPV6, IPV6_LEAVE_GROUP,
                        &maddr, sizeof (struct ipv6_mreq)))
    JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
#else
  (void) fd;
  (void) addr;
  THROW_NO_IPV6(env);
#endif /* HAVE_INET6 */
#else
  (void) fd;
  (void) addr;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "socket options not supported");
#endif /* HAVE_SETSOCKOPT */
}

static uint32_t
getif_address (JNIEnv *env, const char *ifname)
{
#if defined (HAVE_IFADDRS_H) && defined (HAVE_GETIFADDRS)
  struct ifaddrs *ifaddrs, *i;
  uint32_t addr = 0;
  int foundaddr = 0;

  if (getifaddrs (&ifaddrs) == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
      return 0;
    }

  for (i = ifaddrs; i != NULL; i = i->ifa_next)
    {
      if (strcmp (ifname, i->ifa_name) == 0)
        {
          /* Matched the name; see if there is an IPv4 address. */
          if (i->ifa_addr->sa_family == AF_INET)
            {
              foundaddr = 1;
              addr = ((struct sockaddr_in *) i->ifa_addr)->sin_addr.s_addr;
              break;
            }
        }
    }

  if (!foundaddr)
    JCL_ThrowException (env, SOCKET_EXCEPTION, "interface has no IPv4 address");

  freeifaddrs (ifaddrs);

  return addr;
#else
  (void) ifname;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "getifaddrs not available");
  return 0;
#endif /* HAVE_IFADDRS_H && HAVE_GETIFADDRS */
}

static int
getif_index (JNIEnv *env, const char *ifname)
{
#if defined (HAVE_IFADDRS_H) && defined (HAVE_GETIFADDRS)
  struct ifaddrs *ifaddrs, *i;
  char *lastname = NULL;
  int index = 1;
  int foundname = 0;

  if (getifaddrs (&ifaddrs) == -1)
    {
      JCL_ThrowException (env, SOCKET_EXCEPTION, strerror (errno));
      return -1;
    }

  lastname = ifaddrs->ifa_name;
  for (i = ifaddrs; i != NULL; i = i->ifa_next)
    {
      if (strcmp (lastname, ifaddrs->ifa_name) != 0)
        {
          lastname = ifaddrs->ifa_name;
          index++;
        }
      if (strcmp (ifname, ifaddrs->ifa_name) == 0)
        {
          foundname = 1;
          break;
        }
    }

  if (!foundname)
    JCL_ThrowException (env, SOCKET_EXCEPTION,
                        "no interface with that name");

  freeifaddrs (ifaddrs);

  return index;
#else
  (void) ifname;
  JCL_ThrowException (env, "java/lang/InternalError",
                      "getifaddrs not available");
  return -1;
#endif /* HAVE_GETIFADDRS */
}
