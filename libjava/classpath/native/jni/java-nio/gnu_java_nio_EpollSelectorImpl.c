/* gnu_java_nio_EpollSelectorImpl.c -- 
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_SYS_EPOLL_H
#include <sys/epoll.h>
#endif /* HAVE_SYS_EPOLL_H */

#include <gnu_java_nio_EpollSelectorImpl.h>
#include <jcl.h>
#include <errno.h>
#include <string.h>

#define IO_EXCEPTION "java/io/IOException"

/* #define TRACE_EPOLL 1 */


/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    epoll_supported
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_nio_EpollSelectorImpl_epoll_1supported (JNIEnv *e __attribute__((unused)),
                                                      jclass c __attribute__((unused)))
{
#ifdef HAVE_EPOLL_CREATE
  return JNI_TRUE;
#else
  return JNI_FALSE;
#endif /* HAVE_EPOLL_CREATE */
}

/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    sizeof_struct
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_EpollSelectorImpl_sizeof_1struct (JNIEnv *env,
                                                    jclass c __attribute__((unused)))
{
#ifdef HAVE_EPOLL_CREATE
  (void) env;
#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: sizeof is %d\n", __FUNCTION__, sizeof (struct epoll_event));
#endif /* TRACE_EPOLL */
  return sizeof (struct epoll_event);
#else
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
  return -1;
#endif /* HAVE_EPOLL_CREATE */
}

/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    epoll_create
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_EpollSelectorImpl_epoll_1create (JNIEnv *env,
                                                   jclass c __attribute__((unused)),
                                                   jint size)
{
#ifdef HAVE_EPOLL_CREATE
  int fd = epoll_create (size);
  
#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: epoll_create returns %d\n", __FUNCTION__, fd);
#endif /* TRACE_EPOLL */

  if (fd == -1)
    {
      if (ENOSYS == errno)
        JCL_ThrowException (env, "java/lang/InternalError",
                            strerror (errno));
      else
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
  return fd;
#else
  (void) size;
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
  return -1;
#endif /* HAVE_EPOLL_CREATE */
}

/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    epoll_add
 * Signature: (III)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_EpollSelectorImpl_epoll_1add (JNIEnv *env,
                                                jclass c __attribute__((unused)),
                                                jint efd, jint fd, jint ops)
{
#ifdef HAVE_EPOLL_CREATE
  struct epoll_event event;

  memset (&event, 0, sizeof (struct epoll_event));

  if ((ops & gnu_java_nio_EpollSelectorImpl_OP_ACCEPT) != 0
      || (ops & gnu_java_nio_EpollSelectorImpl_OP_READ) != 0)
    event.events = EPOLLIN;

  if ((ops & gnu_java_nio_EpollSelectorImpl_OP_CONNECT) != 0
      || (ops & gnu_java_nio_EpollSelectorImpl_OP_WRITE) != 0)
    event.events |= EPOLLOUT;

  event.data.fd = fd;

#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: adding struct epoll_event { events: %o; data.fd: %d } to %d\n",
           __FUNCTION__, event.events, event.data.fd, efd);
#endif /* TRACE_EPOLL */

  if (epoll_ctl (efd, EPOLL_CTL_ADD, fd, &event) == -1)
    {
      if (ENOSYS == errno)
        JCL_ThrowException (env, "java/lang/InternalError",
                            strerror (errno));
      else
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
#else
  (void) efd;
  (void) fd;
  (void) ops;
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
#endif /* HAVE_EPOLL_CREATE */
}


/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    epoll_modify
 * Signature: (III)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_EpollSelectorImpl_epoll_1modify (JNIEnv *env,
                                                   jclass c __attribute__((unused)),
                                                   jint efd, jint fd, jint ops)
{
#ifdef HAVE_EPOLL_CREATE
  struct epoll_event event;

  memset (&event, 0, sizeof (struct epoll_event));

  if ((ops & gnu_java_nio_EpollSelectorImpl_OP_ACCEPT) != 0
      || (ops & gnu_java_nio_EpollSelectorImpl_OP_READ) != 0)
    event.events = EPOLLIN;

  if ((ops & gnu_java_nio_EpollSelectorImpl_OP_CONNECT) != 0
      || (ops & gnu_java_nio_EpollSelectorImpl_OP_WRITE) != 0)
    event.events |= EPOLLOUT;

  event.data.fd = fd;

#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: modding struct epoll_event { events: %o; data.fd: %d } on %d\n",
           __FUNCTION__, event.events, event.data.fd, efd);
#endif /* TRACE_EPOLL */

  if (epoll_ctl (efd, EPOLL_CTL_MOD, fd, &event) == -1)
    {
      if (ENOSYS == errno)
        JCL_ThrowException (env, "java/lang/InternalError",
                            strerror (errno));
      else
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
#else
  (void) efd;
  (void) fd;
  (void) ops;
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
#endif /* HAVE_EPOLL_CREATE */
}


/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    epoll_delete
 * Signature: (II)V
 */
JNIEXPORT void JNICALL
Java_gnu_java_nio_EpollSelectorImpl_epoll_1delete (JNIEnv *env,
                                                   jclass c __attribute__((unused)),
                                                   jint efd, jint fd)
{
#ifdef HAVE_EPOLL_CREATE
  struct epoll_event event;

  memset (&event, 0, sizeof (struct epoll_event));
  event.data.fd = fd;

#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: delete events on fd %d for %d\n", __FUNCTION__, fd, efd);
#endif /* TRACE_EPOLL */

  /* Older kernel versions require a non-null `event' parameter,
   * even though it is ignored by this call.
   */
  if (epoll_ctl (efd, EPOLL_CTL_DEL, fd, &event) == -1)
    {
      if (ENOSYS == errno)
        JCL_ThrowException (env, "java/lang/InternalError",
                            strerror (errno));
      /* XXX the docs here seem a little strange. If `fd' is closed,
         epoll_ctl returns EBADF; but the docs say that this happens
         only when efd is invalid. Go figure.
       */
      else if (ENOENT == errno || EBADF == errno)
        return; /* fd is closed; it's already removed. */
      else
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }
#else
  (void) efd;
  (void) fd;
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
#endif /* HAVE_EPOLL_CREATE */
}

/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    epoll_wait
 * Signature: (ILjava/nio/ByteBuffer;II)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_EpollSelectorImpl_epoll_1wait (JNIEnv *env,
                                                 jclass c __attribute__((unused)),
                                                 jint efd, jobject nstate,
                                                 jint num_events, jint timeout)
{
#ifdef HAVE_EPOLL_CREATE
  void *p = (*env)->GetDirectBufferAddress (env, nstate);
  struct epoll_event *events = (struct epoll_event *) p;
  int ret;

  if (p == NULL)
    {
      if (!(*env)->ExceptionCheck (env))
        JCL_ThrowException (env, IO_EXCEPTION, "getting native state failed");
      return -1;
    }

#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: events: %p; num_events: %d; timeout: %d; efd: %d\n",
           __FUNCTION__, p, num_events, timeout, efd);
#endif /* TRACE_EPOLL */

  ret = epoll_wait (efd, events, num_events, timeout);

  if (ret == -1)
    {
      if (ENOSYS == errno)
        JCL_ThrowException (env, "java/lang/InternalError",
                            strerror (errno));
      else if (EINTR == errno)
        ret = 0;
      else
        JCL_ThrowException (env, IO_EXCEPTION, strerror (errno));
    }

#ifdef TRACE_EPOLL
  fprintf (stderr, "  epoll_wait returns %d\n", ret);
  {
    int i;
    for (i = 0; i < ret; i++)
      {
        fprintf (stderr, "  [%4i]: events: %o; data.fd: %d\n", i, events[i].events,
                 events[i].data.fd);
      }
  }
  fflush (stderr);
#endif /* TRACE_EPOLL */

  return ret;
#else
  (void) efd;
  (void) nstate;
  (void) num_events;
  (void) timeout;
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
  return -1;
#endif /* HAVE_EPOLL_CREATE */
}


/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    selected_fd
 * Signature: (Ljava/nio/ByteBuffer;)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_EpollSelectorImpl_selected_1fd (JNIEnv *env,
                                                  jclass c __attribute__((unused)),
                                                  jobject value)
{
#ifdef HAVE_EPOLL_CREATE
  void *p = (*env)->GetDirectBufferAddress (env, value);
  struct epoll_event *event = (struct epoll_event *) p;

#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: event: %p\n", __FUNCTION__, p);
#endif /* TRACE_EPOLL */

  if (p == NULL)
    {
      if (!(*env)->ExceptionCheck (env))
        JCL_ThrowException (env, "java/lang/InternalError",
                            "getting native state failed");
      return -1;
    }

#ifdef TRACE_EPOLL
  fprintf (stderr, "  data.fd: %d\n", event->data.fd);
  fflush (stderr);
#endif /* TRACE_EPOLL */

  return event->data.fd;
#else
  (void) value;
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
  return -1;
#endif /* HAVE_EPOLL_CREATE */
}


/*
 * Class:     gnu_java_nio_EpollSelectorImpl
 * Method:    selected_ops
 * Signature: (Ljava/nio/ByteBuffer;)I
 */
JNIEXPORT jint JNICALL
Java_gnu_java_nio_EpollSelectorImpl_selected_1ops (JNIEnv *env,
                                                   jclass c __attribute__((unused)),
                                                   jobject value)
{
#ifdef HAVE_EPOLL_CREATE
  void *p = (*env)->GetDirectBufferAddress (env, value);
  struct epoll_event *event = (struct epoll_event *) p;
  int ret = 0;

#ifdef TRACE_EPOLL
  fprintf (stderr, "%s: event: %p\n", __FUNCTION__, p);
#endif /* TRACE_EPOLL */

  if (p == NULL)
    {
      if (!(*env)->ExceptionCheck (env))
        JCL_ThrowException (env, "java/lang/InternalError",
                            "getting native state failed");
      return -1;
    }

  if ((event->events & EPOLLIN) != 0)
    ret |= gnu_java_nio_EpollSelectorImpl_OP_ACCEPT | gnu_java_nio_EpollSelectorImpl_OP_READ;
  if ((event->events & EPOLLOUT) != 0)
    ret |= gnu_java_nio_EpollSelectorImpl_OP_CONNECT | gnu_java_nio_EpollSelectorImpl_OP_WRITE;

#ifdef TRACE_EPOLL
  fprintf (stderr, "  events: %o\n", event->events);
  fflush (stderr);
#endif /* TRACE_EPOLL */

  return ret;
#else
  (void) value;
  JCL_ThrowException (env, "java/lang/InternalError", "epoll support not available");
  return -1;
#endif /* HAVE_EPOLL_CREATE */
}
