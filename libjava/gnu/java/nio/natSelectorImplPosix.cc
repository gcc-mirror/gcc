// natSelectorImplPosix.cc

/* Copyright (C) 2002, 2003, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <errno.h>
#include <string.h>

#include <gnu/java/nio/VMSelector.h>
#include <java/io/InterruptedIOException.h>
#include <java/io/IOException.h>
#include <java/lang/Thread.h>

static void
helper_put_filedescriptors (jintArray fdArray, fd_set& fds, int& max_fd)
{
  jint* tmpFDArray = elements (fdArray);

  for (int index = 0; index < JvGetArrayLength (fdArray); index++)
    {
      int fd = tmpFDArray [index];
      if (fd > 0)
        {
          FD_SET (tmpFDArray [index], &fds);

          if (tmpFDArray [index] > max_fd)
            max_fd = tmpFDArray [index];
        }
    }
}

static void
helper_get_filedescriptors (jintArray& fdArray, fd_set fds)
{
  jint* tmpFDArray = elements (fdArray);
  
  for (int index = 0; index < JvGetArrayLength (fdArray); index++)
    {
      int fd = tmpFDArray [index];
      if (fd < 0 || !FD_ISSET (fd, &fds))
        tmpFDArray [index] = 0;
    }
}

static void
helper_reset (jintArray& fdArray)
{
  jint* tmpFDArray = elements (fdArray);
  
  for (int index = 0; index < JvGetArrayLength (fdArray); index++)
    tmpFDArray [index] = 0;
}

jint
gnu::java::nio::VMSelector::select (jintArray read, jintArray write,
                                    jintArray except, jlong timeout)
{
  jint result;
  int max_fd = 0;
  fd_set read_fds;
  fd_set write_fds;
  fd_set except_fds;
  struct timeval real_time_data;
  struct timeval *time_data = NULL;

  // If a legal timeout value isn't given, use NULL.
  // This means an infinite timeout. The specification
  // also says that a zero timeout should be treated
  // as infinite. Otherwise (if the timeout value is legal),
  // fill our timeval struct and use it for the select.
  if (timeout > 0)
    {
      real_time_data.tv_sec = timeout / 1000;
      real_time_data.tv_usec = (timeout % 1000) * 1000;
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
  try
    {
      result = _Jv_select (max_fd + 1, &read_fds, &write_fds,
                           &except_fds, time_data);
    }
  catch (::java::io::InterruptedIOException *e)
    {
      // The behavior of JRE 1.4.1 is that no exception is thrown
      // when the thread is interrupted, but the thread's interrupt
      // status is set. Clear all of our select sets and return 0,
      // indicating that nothing was selected.
      ::java::lang::Thread::currentThread ()->interrupt ();
       helper_reset (read);
       helper_reset (write);
       helper_reset (except);
       return 0;
    }

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
