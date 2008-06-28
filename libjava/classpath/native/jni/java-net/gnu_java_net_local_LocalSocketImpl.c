/* gnu_java_net_local_LocalSocketImpl.c -- native local socket implementation.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "config.h"

#include <gnu_java_net_local_LocalSocketImpl.h>

#include <stddef.h>
#include "local.h"

#ifdef DEBUG
#define TRACE(msg) fprintf (stderr, "%s(%s:%d) -- %s\n", __FUNCTION__, __FILE__, __LINE__, msg)
#else
#define TRACE(msg)
#endif

static void
_throw (JNIEnv *env, const char *exception, const char *msg)
{
  jclass _theclass = (*env)->FindClass (env, exception);
  TRACE("begin");
  if (!_theclass)
    {
      (*env)->FatalError (env, "exception class not found");
    }
  (*env)->ThrowNew (env, _theclass, msg);
  TRACE("end");
}

void
Java_gnu_java_net_local_LocalSocketImpl_create (JNIEnv *env, jobject this, jboolean stream)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID socket_fd, created;
  jclass clazz;
  jint fd = (jint) local_create ((int) stream);

  TRACE("begin");

  if (fd < 0)
    {
      _throw (env, "java/io/IOException", local_error ());
      return;
    }
  clazz = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  created = (*env)->GetFieldID (env, clazz, "created", "Z");
  if (!created)
    {
      return;
    }
  (*env)->SetIntField (env, this, socket_fd, fd);
  (*env)->SetBooleanField (env, this, created, JNI_TRUE);

  TRACE("end");
#else
  (void) this;
  (void) stream;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_listen (JNIEnv *env, jobject this, jint backlog)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID socket_fd;
  jclass clazz;
  int fd;

  TRACE("begin");

  clazz = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  fd = (int) (*env)->GetIntField (env, this, socket_fd);
  if (local_listen (fd, (int) backlog))
    {
      _throw (env, "java/io/IOException", local_error ());
      return;
    }

  TRACE("end");
#else
  (void) this;
  (void) backlog;
  _throw (env, "java/lang/Error", "support for local sockets not available");  
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_accept (JNIEnv *env, jobject this, jobject socket)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jmethodID addr_init;
  jfieldID socket_fd, remote_addr, local_addr;
  jclass clazz1, clazz2;
  jobject remote, local;
  jint fd;
  char path[108];

  TRACE("begin");

  clazz1 = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz1, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  fd = (*env)->GetIntField (env, this, socket_fd);
  fd = (jint) local_accept ((int) fd, path);
  if (fd < 0)
    {
      _throw (env, "java/io/IOException", local_error ());
      return;
    }

  clazz2 = (*env)->FindClass (env, "gnu/java/net/local/LocalSocketAddress");
  if (!clazz2)
    {
      return;
    }
  addr_init = (*env)->GetMethodID (env, clazz2, "<init>", "(Ljava/lang/String;)V");
  if (!addr_init)
    {
      return;
    }
  remote = (*env)->NewObject (env, clazz2, addr_init, (*env)->NewStringUTF (env, path));

  remote_addr = (*env)->GetFieldID (env, clazz1, "remote", "Lgnu/java/net/local/LocalSocketAddress;");
  if (!remote_addr)
    {
      return;
    }
  local_addr = (*env)->GetFieldID (env, clazz1, "local", "Lgnu/java/net/local/LocalSocketAddress;");
  if (!local_addr)
    {
      return;
    }
  local = (*env)->GetObjectField (env, this, local_addr);
  (*env)->SetIntField (env, socket, socket_fd, fd);
  (*env)->SetObjectField (env, socket, remote_addr, remote);
  (*env)->SetObjectField (env, socket, local_addr, local);

  TRACE("end");
#else
  (void) this;
  (void) socket;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


jint
Java_gnu_java_net_local_LocalSocketImpl_available
(JNIEnv *env, jobject this __attribute__((unused)), jint fd)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jint avail;

  TRACE("begin");

  avail = (jint) local_available (fd);
  if (avail < 0)
    {
      _throw (env, "java/io/IOException", local_error ());
      return 0;
    }

  TRACE("end");

  return avail;
#else
  (void) this;
  (void) fd;
  _throw (env, "java/lang/Error", "support for local sockets not available");
  return -1;
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_close (JNIEnv *env, jobject this)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID socket_fd;
  jclass clazz;
  int fd;

  TRACE("begin");

  clazz = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  fd = (int) (*env)->GetIntField (env, this, socket_fd);
  if (local_close (fd))
    {
      _throw (env, "java/io/IOException", local_error ());
    }

  TRACE("end");
#else
  (void) this;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_unlink (JNIEnv *env, jobject this)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID local;
  jmethodID get_path;
  jclass clazz1, clazz2;
  jobject local_ref, path;
  char *addr_path;

  TRACE("begin");

  clazz1 = (*env)->GetObjectClass (env, this);
  local = (*env)->GetFieldID (env, clazz1, "local", "Lgnu/java/net/local/LocalSocketAddress;");
  if (!local)
    {
      return;
    }
  local_ref = (*env)->GetObjectField (env, this, local);
  clazz2 = (*env)->GetObjectClass (env, local_ref);
  get_path = (*env)->GetMethodID (env, clazz2, "getPath", "()Ljava/lang/String;");
  if (!get_path)
    {
      return;
    }
  path = (*env)->CallObjectMethod (env, local_ref, get_path);
  addr_path = (char *) (*env)->GetStringUTFChars (env, (jstring) path, NULL);
  if (local_unlink (addr_path))
    {
      _throw (env, "java/io/IOException", local_error ());
    }
  (*env)->ReleaseStringUTFChars (env, (jstring) path, addr_path);

  TRACE("end");
#else
  (void) this;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_sendUrgentData (JNIEnv *env, jobject this __attribute__((unused)), jint data __attribute__((unused)))
{
  /* XXX I don't remember why I have this. Probably should just
     remove. */
  (*env)->FatalError (env, "Java_gnu_java_net_local_LocalSocketImpl_shutdownInput (JNIEnv *env, jobject) not implemented");
}


void
Java_gnu_java_net_local_LocalSocketImpl_shutdownInput (JNIEnv *env, jobject this)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID socket_fd;
  jclass clazz;
  int fd;

  TRACE("begin");

  clazz = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  fd = (*env)->GetIntField (env, this, socket_fd);
  if (local_shutdown_input (fd))
    {
      _throw (env, "java/io/IOException", local_error ());
    }

  TRACE("end");
#else
  (void) this;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_shutdownOutput (JNIEnv *env, jobject this)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID socket_fd;
  jclass clazz;
  int fd;

  TRACE("begin");

  clazz = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  fd = (*env)->GetIntField (env, this, socket_fd);
  if (local_shutdown_output (fd))
    {
      _throw (env, "java/io/IOException", local_error ());
    }

  TRACE("end");
#else
  (void) this;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_localBind (JNIEnv *env, jobject this, jobject address)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID socket_fd;
  jmethodID get_path;
  jobject path;
  jclass clazz1, clazz2;
  const char *addr_path;
  int fd;

  TRACE("begin");

  clazz1 = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz1, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  fd = (int) (*env)->GetIntField (env, this, socket_fd);
  clazz2 = (*env)->GetObjectClass (env, address);
  get_path = (*env)->GetMethodID (env, clazz2, "getPath", "()Ljava/lang/String;");
  path = (*env)->CallObjectMethod (env, address, get_path);
  addr_path = (*env)->GetStringUTFChars (env, (jstring) path, NULL);
  if (local_bind (fd, addr_path))
    {
      _throw (env, "java/io/IOException", local_error ());
    }
  (*env)->ReleaseStringUTFChars (env, (jstring) path, addr_path);

  TRACE("end");
#else
  (void) this;
  (void) address;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_localConnect (JNIEnv *env, jobject this, jobject address)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jfieldID socket_fd;
  jmethodID get_path;
  jobject path;
  jclass clazz1, clazz2;
  char *addr_path;
  int fd;

  TRACE("begin");

  clazz1 = (*env)->GetObjectClass (env, this);
  socket_fd = (*env)->GetFieldID (env, clazz1, "socket_fd", "I");
  if (!socket_fd)
    {
      return;
    }
  fd = (int) (*env)->GetIntField (env, this, socket_fd);
  clazz2 = (*env)->GetObjectClass (env, address);
  get_path = (*env)->GetMethodID (env, clazz2, "getPath", "()Ljava/lang/String;");
  path = (*env)->CallObjectMethod (env, address, get_path);
  addr_path = (char *) (*env)->GetStringUTFChars (env, (jstring) path, NULL);
  if (local_connect (fd, addr_path))
    {
      _throw (env, "java/io/IOException", local_error ());
    }
  (*env)->ReleaseStringUTFChars (env, (jstring) path, addr_path);

  TRACE("end");
#else
  (void) this;
  (void) address;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}


jint
Java_gnu_java_net_local_LocalSocketImpl_read
(JNIEnv *env, jobject this __attribute__((unused)), jint fd, jbyteArray buf,
 jint off, jint len)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jbyte *buffer;
  jint count;

  TRACE("begin");

  if (off < 0 || len < 0 || off + len > (*env)->GetArrayLength (env, buf))
    {
      _throw (env, "java/lang/ArrayIndexOutOfBoundsException", "");
    }

  buffer = (*env)->GetByteArrayElements (env, buf, NULL);
  count = (jint) local_read (fd, (void *) (buffer + off), (int) len);
  if (count < 0)
    {
      _throw (env, "java/io/IOException", local_error ());
    }
  (*env)->ReleaseByteArrayElements (env, buf, buffer, 0);

  TRACE("end");

  return count;
#else
  (void) this;
  (void) fd;
  (void) buf;
  (void) off;
  (void) len;
  _throw (env, "java/lang/Error", "support for local sockets not available");
  return -1;
#endif /* ENABLE_LOCAL_SOCKETS */
}


void
Java_gnu_java_net_local_LocalSocketImpl_write
(JNIEnv *env, jobject this __attribute__((unused)), jint fd, jbyteArray buf,
 jint off, jint len)
{
#ifdef ENABLE_LOCAL_SOCKETS
  jbyte *buffer;

  TRACE("begin");

  if (off < 0 || len < 0 || off + len > (*env)->GetArrayLength (env, buf))
    {
      _throw (env, "java/lang/ArrayIndexOutOfBoundsException", "");
    }

  buffer = (*env)->GetByteArrayElements (env, buf, NULL);
  if (local_write (fd, (void *) (buffer + off), (int) len) < 0)
    {
      _throw (env, "java/io/IOException", local_error ());
    }
  (*env)->ReleaseByteArrayElements (env, buf, buffer, JNI_ABORT);

  TRACE("end");
#else
  (void) this;
  (void) fd;
  (void) buf;
  (void) off;
  (void) len;
  _throw (env, "java/lang/Error", "support for local sockets not available");
#endif /* ENABLE_LOCAL_SOCKETS */
}
