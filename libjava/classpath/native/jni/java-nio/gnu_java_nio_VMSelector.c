/* gnu_java_nio_VMSelector.c - Native methods for SelectorImpl class
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

#include "config.h"

/* <sys/types.h> needs to be included on OSX before <sys/select.h> */
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include <sys/select.h>
#include <sys/time.h>

#include <string.h>

#include <errno.h>

#include <jni.h>
#include <jcl.h>

#include "gnu_java_nio_VMSelector.h"

/* Amount of characters in the error message buffer for strerror_r. */
#define BUF_SIZE 250

void helper_put_filedescriptors (JNIEnv *, jintArray, fd_set *, int *);

void helper_get_filedescriptors (JNIEnv *, jintArray *, fd_set *);

void helper_reset (JNIEnv *, jintArray *);

int
helper_select (JNIEnv *, jclass, jmethodID,
	       int, fd_set *, fd_set *, fd_set *, struct timeval *);

void
helper_put_filedescriptors (JNIEnv * env, jintArray fdArray, fd_set * fds,
			    int *max_fd)
{
  jint *tmpFDArray = (*env)->GetIntArrayElements (env, fdArray, 0);
  int size = (*env)->GetArrayLength (env, fdArray);
  int index, fd;

  for (index = 0; index < size; index++)
    {
      fd = tmpFDArray[index];

      if (fd > 0)
	{
	  FD_SET (tmpFDArray[index], fds);

	  if (tmpFDArray[index] > (*max_fd))
	    (*max_fd) = tmpFDArray[index];
	}
    }
}

void
helper_get_filedescriptors (JNIEnv * env, jintArray * fdArray, fd_set * fds)
{
  jint *tmpFDArray = (*env)->GetIntArrayElements (env, fdArray, 0);
  int size = (*env)->GetArrayLength (env, fdArray);
  int index, fd;

  for (index = 0; index < size; index++)
    {
      fd = tmpFDArray[index];
      if (fd < 0 || !FD_ISSET (fd, fds))
	tmpFDArray[index] = 0;
    }
}

void
helper_reset (JNIEnv * env, jintArray * fdArray)
{
  jint *tmpFDArray = (*env)->GetIntArrayElements (env, fdArray, 0);
  int size = (*env)->GetArrayLength (env, fdArray);
  int index;

  for (index = 0; index < size; index++)
    tmpFDArray[index] = 0;
}

/* A wrapper for select() which ignores EINTR.
 * Taken from gclib's posix.cc
 */
int
helper_select (JNIEnv * env, jclass thread_class,
	       jmethodID thread_interrupted, int n, fd_set * readfds,
	       fd_set * writefds, fd_set * exceptfds, struct timeval *timeout)
{
#ifdef HAVE_SYS_SELECT_H
  /* If we have a timeout, compute the absolute ending time. */
  struct timeval end, delay, after;
  int r;

  if (timeout)
    {
      gettimeofday (&end, NULL);

      end.tv_usec += timeout->tv_usec;

      if (end.tv_usec >= 1000000)
	{
	  ++end.tv_sec;
	  end.tv_usec -= 1000000;
	}

      end.tv_sec += timeout->tv_sec;
      delay = *timeout;
    }
  else
    {
      /* Placate compiler. */
      delay.tv_sec = delay.tv_usec = 0;
    }

  while (1)
    {
      r = select (n, readfds, writefds, exceptfds, timeout ? &delay : NULL);

      if (r < 0 && errno != EINTR)
	return -errno;
      else if (r >= 0)
	return r;

      /* Here we know we got EINTR. */
      if ((*env)->
	  CallStaticBooleanMethod (env, thread_class, thread_interrupted))
	{
	  return -EINTR;
	}

      if (timeout)
	{
	  gettimeofday (&after, NULL);

	  /* Now compute new timeout argument. */
	  delay.tv_usec = end.tv_usec - after.tv_usec;
	  delay.tv_sec = end.tv_sec - after.tv_sec;

	  if (delay.tv_usec < 0)
	    {
	      --delay.tv_sec;
	      delay.tv_usec += 1000000;
	    }

	  if (delay.tv_sec < 0)
	    {
	      /* We assume that the user wants a valid select() call
	       * more than precise timing.  So if we get a series of
	       * EINTR we just keep trying with delay 0 until we get a
	       * valid result.
	       */
	      delay.tv_sec = 0;
	    }
	}
    }
#else /* HAVE_SYS_SELECT_H */
  return 0;
#endif

}

JNIEXPORT jint JNICALL
Java_gnu_java_nio_VMSelector_select (JNIEnv * env,
				     jclass obj __attribute__ ((__unused__)),
				     jintArray read,
				     jintArray write,
				     jintArray except, jlong timeout)
{
  jint result;
  jclass thread_class = (*env)->FindClass (env, "java/lang/Thread");
  jmethodID thread_current_thread =
    (*env)->GetStaticMethodID (env, thread_class, "currentThread",
			       "()Ljava/lang/Thread;");
  jmethodID thread_interrupt =
    (*env)->GetMethodID (env, thread_class, "interrupt", "()V");
  jmethodID thread_interrupted =
    (*env)->GetStaticMethodID (env, thread_class, "interrupted", "()Z");
  jobject current_thread;
  int max_fd = 0;
  fd_set read_fds;
  fd_set write_fds;
  fd_set except_fds;
  struct timeval real_time_data;
  struct timeval *time_data = NULL;
  char message_buf[BUF_SIZE + 1];

  /* If a legal timeout value isn't given, use NULL.
   * This means an infinite timeout. The specification
   * also says that a zero timeout should be treated
   * as infinite. Otherwise (if the timeout value is legal),
   * fill our timeval struct and use it for the select.
   */
  if (timeout > 0)
    {
      real_time_data.tv_sec = timeout / 1000;
      real_time_data.tv_usec = (timeout % 1000) * 1000;
      time_data = &real_time_data;
    }

  /* Reset all fd_set structures */
  FD_ZERO (&read_fds);
  FD_ZERO (&write_fds);
  FD_ZERO (&except_fds);

  /* Fill the fd_set data structures for the _Jv_select() call. */
  helper_put_filedescriptors (env, read, &read_fds, &max_fd);
  helper_put_filedescriptors (env, write, &write_fds, &max_fd);
  helper_put_filedescriptors (env, except, &except_fds, &max_fd);

  /* Actually do the select */
  result =
    helper_select (env, thread_class, thread_interrupted, max_fd + 1,
		   &read_fds, &write_fds, &except_fds, time_data);

  if (result == -EINTR)
    {
      /* The behavior of JRE 1.4.1 is that no exception is thrown
       * when the thread is interrupted, but the thread's interrupt
       * status is set. Clear all of our select sets and return 0,
       * indicating that nothing was selected.
       */
      current_thread =
	(*env)->CallStaticObjectMethod (env, thread_class,
					thread_current_thread);
      (*env)->CallVoidMethod (env, current_thread, thread_interrupt);

      helper_reset (env, read);
      helper_reset (env, write);
      helper_reset (env, except);

      return 0;
    }

  if (result < 0)
    {

      int errorcode = -result;

      if (strerror_r (errorcode, message_buf, BUF_SIZE))
	{
	  /* This would mean that message_buf was to small
	   * to hold the error message.
	   */
	  JCL_ThrowException (env, "java/lang/InternalError",
			      "Not enough space in message buffer.");
	  return 0;
	}

      JCL_ThrowException (env, "java/io/IOException", message_buf);
      return 0;
    }

  /* Set the file descriptors according to the values returned from select(). */
  helper_get_filedescriptors (env, read, &read_fds);
  helper_get_filedescriptors (env, write, &write_fds);
  helper_get_filedescriptors (env, except, &except_fds);

  return result;
}
