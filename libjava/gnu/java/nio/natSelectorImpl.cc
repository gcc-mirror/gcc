// natSelectorImpl.cc

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <errno.h>
#include <string.h>

#if HAVE_BSTRING_H
// Needed for bzero, implicitly used by FD_ZERO on IRIX 5.2 
#include <bstring.h>
#endif

//#include <gcj/cni.h>
#include <gnu/java/nio/SelectorImpl.h>
#include <java/io/IOException.h>

void
helper_put_filedescriptors (jintArray java_fd_array, fd_set& fds, int& max_fd)
{
  int counter;
  jint* java_fds;

  java_fds = elements (java_fd_array);

  for (counter = 0; counter < JvGetArrayLength (java_fd_array); counter++)
    {
      FD_SET (java_fds [counter], &fds);

      if (java_fds [counter] > max_fd)
        {
          max_fd = java_fds [counter];
        }
    }
}

void
helper_get_filedescriptors (jintArray& java_fd_array, fd_set fds)
{
  int counter;
  int counter_fds;
  jint* java_fds;
  jintArray new_array_fds;
  jint* new_data_fds;

  counter_fds = 0;
  java_fds = elements (java_fd_array);

  for (counter = 0; counter < JvGetArrayLength (java_fd_array); counter++)
    {
      if (FD_ISSET (java_fds[counter], &fds))
        {
          counter_fds++;
        }
    }

  new_array_fds = JvNewIntArray (counter_fds);
  new_data_fds = elements (new_array_fds);

  for (counter = 0; counter < JvGetArrayLength (java_fd_array); counter++)
    {
      if (FD_ISSET (java_fds[counter], &fds))
        {
          new_data_fds[counter] = java_fds[counter];
        }      
    }

  java_fd_array = new_array_fds;
}

jint
gnu::java::nio::SelectorImpl::java_do_select (jintArray read, jintArray write,
                                              jintArray except, jlong timeout)
{
  jint result;
  int max_fd = 0;
  fd_set read_fds;
  fd_set write_fds;
  fd_set except_fds;
  struct timeval real_time_data;
  struct timeval *time_data = NULL;

  real_time_data.tv_sec = 0;
  real_time_data.tv_usec = timeout;

  // If not legal timeout value is given, use NULL.
  // This means an infinite timeout.
  if (timeout >= 0)
    {
      time_data = &real_time_data;
    }

  // Reset all fd_set structures
  FD_ZERO (&read_fds);
  FD_ZERO (&write_fds);
  FD_ZERO (&except_fds);

  // Fill the fd_set data structures for the _Jv_select() call.
  helper_put_filedescriptors (read, read_fds, max_fd);
  helper_put_filedescriptors (write, write_fds, max_fd);
  helper_put_filedescriptors (except, except_fds, max_fd);

  // Actually do the select
  result = _Jv_select (max_fd + 1, &read_fds, &write_fds, &except_fds, time_data);

  if (result < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  // Set the file descriptors according to the values returned from select().
  helper_get_filedescriptors (read, read_fds);
  helper_get_filedescriptors (write, write_fds);
  helper_get_filedescriptors (except, except_fds);

  return result;
}
