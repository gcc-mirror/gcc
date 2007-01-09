/* javanet.h - Declarations for common functions for the java.net package
   Copyright (C) 1998, 2005  Free Software Foundation, Inc.

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


#ifndef _JAVANET_LOADED
#define _JAVANET_LOADED 

#include <jni.h>
#include "jcl.h"
#include "cpnet.h"

/*************************************************************************/

/*
 * Defined constants
 */

/* Exception Classes */
#define BIND_EXCEPTION "java/net/BindException"
#define IO_EXCEPTION "java/io/IOException"
#define CONNECT_EXCEPTION "java/net/ConnectException"
#define SOCKET_EXCEPTION "java/net/SocketException"
#define UNKNOWN_HOST_EXCEPTION "java/net/UnknownHostException"
#define NULL_EXCEPTION "java/lang/NullPointerException"

/* Socket Option Identifiers - Don't change or binary compatibility with 
                               the JDK will be broken! These also need to
                               be kept compatible with java.net.SocketOptions */
#define SOCKOPT_TCP_NODELAY 1
#define SOCKOPT_SO_BINDADDR 15
#define SOCKOPT_SO_LINGER 128
#define SOCKOPT_SO_TIMEOUT 4102
#define SOCKOPT_SO_SNDBUF 4097
#define SOCKOPT_SO_RCVBUF 4098
#define SOCKOPT_SO_REUSEADDR 4
#define SOCKOPT_IP_MULTICAST_IF 16
#define SOCKOPT_SO_KEEPALIVE 8

/* Internal option identifiers. Not needed for JDK compatibility */
#define SOCKOPT_IP_TTL 7777

/*************************************************************************/

/*
 * Function Prototypes
 */

extern int _javanet_get_int_field(JNIEnv *, jobject, const char *);
extern cpnet_address *_javanet_get_ip_netaddr(JNIEnv *, jobject);
extern jobject _javanet_create_inetaddress (JNIEnv *, cpnet_address *);
extern void _javanet_create(JNIEnv *, jobject, jboolean);
extern void _javanet_close(JNIEnv *, jobject, int);
extern void _javanet_connect(JNIEnv *, jobject, jobject, jint, jboolean);
extern void _javanet_bind(JNIEnv *, jobject, jobject, jint, int);
extern void _javanet_listen(JNIEnv *, jobject, jint);
extern void _javanet_accept(JNIEnv *, jobject, jobject);
extern int _javanet_recvfrom(JNIEnv *, jobject, jarray, int, int, cpnet_address **);
extern void _javanet_sendto(JNIEnv *, jobject, jarray, int, int, cpnet_address *);
extern jobject _javanet_get_option(JNIEnv *, jobject, jint);
extern void _javanet_set_option(JNIEnv *, jobject, jint, jobject);
extern void _javanet_shutdownInput (JNIEnv *, jobject);
extern void _javanet_shutdownOutput (JNIEnv *, jobject);

/*************************************************************************/

#endif /* not _JAVANET_H_LOADED */

