// natFileDescriptor.cc - Native part of FileDescriptor class.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>

#ifdef HAVE_SYS_IOCTL_H
#define BSD_COMP /* Get FIONREAD on Solaris2. */
#include <sys/ioctl.h>
#endif

// Pick up FIONREAD on Solaris 2.5.
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#include <cni.h>
#include <jvm.h>
#include <java/io/FileDescriptor.h>
#include <java/io/SyncFailedException.h>
#include <java/io/IOException.h>
#include <java/io/InterruptedIOException.h>
#include <java/io/EOFException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/String.h>
#include <java/lang/Thread.h>
#include <java/io/FileNotFoundException.h>

#define NO_FSYNC_MESSAGE "sync unsupported"

jboolean
java::io::FileDescriptor::valid (void)
{
  struct stat sb;
  return ::fstat (fd, &sb) == 0;
}

void
java::io::FileDescriptor::sync (void)
{
  // Some files don't support fsync.  We don't bother reporting these
  // as errors.
#ifdef HAVE_FSYNC
  if (::fsync (fd) && errno != EROFS && errno != EINVAL)
    JvThrow (new SyncFailedException (JvNewStringLatin1 (strerror (errno))));
#else
  JvThrow (new SyncFailedException (JvNewStringLatin1 (NO_FSYNC_MESSAGE)));
#endif
}

jint
java::io::FileDescriptor::open (jstring path, jint jflags)
{
  // FIXME: eww.
  char buf[MAXPATHLEN];
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  // FIXME?
  buf[total] = '\0';
  int flags = 0;
#ifdef O_BINARY
  flags |= O_BINARY;
#endif

  JvAssert ((jflags & READ) || (jflags & WRITE));
  if ((jflags & READ) && (jflags & WRITE))
    flags |= O_RDWR;
  else if ((jflags & READ))
    flags |= O_RDONLY;
  else
    {
      flags |= O_WRONLY | O_CREAT;
      if ((jflags & APPEND))
	flags |= O_APPEND;
      else
	flags |= O_TRUNC;
    }

  int fd = ::open (buf, flags, 0644);
  if (fd == -1)
    {
      char msg[MAXPATHLEN + 200];
      sprintf (msg, "%s: %s", buf, strerror (errno));
      JvThrow (new FileNotFoundException (JvNewStringLatin1 (msg)));
    }
  return fd;
}

void
java::io::FileDescriptor::write (jint b)
{
  jbyte d = (jbyte) b;
  int r = ::write (fd, &d, 1);
  if (java::lang::Thread::interrupted())
    {
      InterruptedIOException *iioe
	= new InterruptedIOException (JvNewStringLatin1 ("write interrupted"));
      iioe->bytesTransferred = r == -1 ? 0 : r;
      JvThrow (iioe);
    }
  else if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  // FIXME: loop if r != 1.
}

void
java::io::FileDescriptor::write (jbyteArray b, jint offset, jint len)
{
  if (! b)
    JvThrow (new java::lang::NullPointerException);
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    JvThrow (new java::lang::ArrayIndexOutOfBoundsException);
  jbyte *bytes = elements (b) + offset;
  int r = ::write (fd, bytes, len);
  if (java::lang::Thread::interrupted())
    {
      InterruptedIOException *iioe
	= new InterruptedIOException (JvNewStringLatin1 ("write interrupted"));
      iioe->bytesTransferred = r == -1 ? 0 : r;
      JvThrow (iioe);
    }
  else if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  // FIXME: loop if r != len.
}

void
java::io::FileDescriptor::close (void)
{
  jint save = fd;
  fd = -1;
  if (::close (save))
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
}

jint
java::io::FileDescriptor::seek (jlong pos, jint whence)
{
  JvAssert (whence == SET || whence == CUR);

  jlong len = length ();
  jlong here = getFilePointer ();

  if ((whence == SET && pos > len) || (whence == CUR && here + pos > len))
    JvThrow (new EOFException);

  off_t r = ::lseek (fd, (off_t) pos, whence == SET ? SEEK_SET : SEEK_CUR);
  if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  return r;
}

jlong
java::io::FileDescriptor::length (void)
{
  struct stat sb;
  if (::fstat (fd, &sb))
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  return sb.st_size;
}

jlong
java::io::FileDescriptor::getFilePointer (void)
{
  off_t r = ::lseek (fd, 0, SEEK_CUR);
  if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  return r;
}

jint
java::io::FileDescriptor::read (void)
{
  jbyte b;
  int r = ::read (fd, &b, 1);
  if (r == 0)
    return -1;
  if (java::lang::Thread::interrupted())
    {
      InterruptedIOException *iioe
	= new InterruptedIOException (JvNewStringLatin1 ("read interrupted"));
      iioe->bytesTransferred = r == -1 ? 0 : r;
      JvThrow (iioe);
    }
  else if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  return b & 0xFF;
}

jint
java::io::FileDescriptor::read (jbyteArray buffer, jint offset, jint count)
{
  if (! buffer)
    JvThrow (new java::lang::NullPointerException);
  jsize bsize = JvGetArrayLength (buffer);
  if (offset < 0 || count < 0 || offset + count > bsize)
    JvThrow (new java::lang::ArrayIndexOutOfBoundsException);
  jbyte *bytes = elements (buffer) + offset;
  int r = ::read (fd, bytes, count);
  if (r == 0)
    return -1;
  if (java::lang::Thread::interrupted())
    {
      InterruptedIOException *iioe
	= new InterruptedIOException (JvNewStringLatin1 ("read interrupted"));
      iioe->bytesTransferred = r == -1 ? 0 : r;
      JvThrow (iioe);
    }
  else if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  return r;
}

jint
java::io::FileDescriptor::available (void)
{
#if defined (FIONREAD)
  long num;
  int r = ::ioctl (fd, FIONREAD, &num);
  if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  return (jint) num;
#elif defined (HAVE_SELECT)
  int r = -1;
  if (fd < 0)
    errno = EBADF;
  else
    {
      fd_set rd;
      FD_ZERO (&rd);
      FD_SET (fd, &rd);
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 0;
      r = ::select (fd + 1, &rd, NULL, NULL, &tv);
    }
  if (r == -1)
    JvThrow (new IOException (JvNewStringLatin1 (strerror (errno))));
  return r == 0 ? 0 : 1;
#else
  JvThrow (new IOException (JvNewStringLatin1 ("unimplemented")));
#endif
}
