// natFileDescriptor.cc - Native part of FileDescriptor class.

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include "posix.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/param.h>

#ifdef HAVE_SYS_IOCTL_H
#define BSD_COMP /* Get FIONREAD on Solaris2. */
#include <sys/ioctl.h>
#endif

// Pick up FIONREAD on Solaris 2.5.
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#include <gcj/cni.h>
#include <jvm.h>
#include <java/io/FileDescriptor.h>
#include <java/io/SyncFailedException.h>
#include <java/io/IOException.h>
#include <java/io/InterruptedIOException.h>
#include <java/io/EOFException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/System.h>
#include <java/lang/String.h>
#include <java/lang/Thread.h>
#include <java/io/FileNotFoundException.h>

#define NO_FSYNC_MESSAGE "sync unsupported"

void
java::io::FileDescriptor::init(void)
{
  in = new java::io::FileDescriptor(0);
  out = new java::io::FileDescriptor(1);
  err = new java::io::FileDescriptor(2);
}

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
    throw new SyncFailedException (JvNewStringLatin1 (strerror (errno)));
#else
  throw new SyncFailedException (JvNewStringLatin1 (NO_FSYNC_MESSAGE));
#endif
}

jint
java::io::FileDescriptor::open (jstring path, jint jflags)
{
  char *buf = (char *) _Jv_AllocBytes (_Jv_GetStringUTFLength (path) + 1);
  jsize total = JvGetStringUTFRegion (path, 0, path->length(), buf);
  buf[total] = '\0';
  int flags = 0;
#ifdef O_BINARY
  flags |= O_BINARY;
#endif

  JvAssert ((jflags & READ) || (jflags & WRITE));
  int mode = 0666;
  if ((jflags & READ) && (jflags & WRITE))
    flags |= O_RDWR | O_CREAT;
  else if ((jflags & READ))
    flags |= O_RDONLY;
  else
    {
      flags |= O_WRONLY | O_CREAT;
      if ((jflags & APPEND))
	flags |= O_APPEND;
      else
	flags |= O_TRUNC;

      if ((jflags & EXCL))
	{
	  flags |= O_EXCL;
	  // In this case we are making a temp file.
	  mode = 0600;
	}
    }

  int fd = ::open (buf, flags, mode);
  if (fd == -1 && errno == EMFILE)
    {
      // Because finalize () calls close () we might be able to continue.
      java::lang::System::gc ();
      java::lang::System::runFinalization ();
      fd = ::open (buf, flags, mode);
    }
  if (fd == -1)
    {
      char msg[MAXPATHLEN + 200];
      // We choose the formatting here for JDK compatibility, believe
      // it or not.
      sprintf (msg, "%s (%s)", buf, strerror (errno));
      throw new FileNotFoundException (JvNewStringLatin1 (msg));
    }

  _Jv_platform_close_on_exec (fd);

  return fd;
}

void
java::io::FileDescriptor::write (jint b)
{
  jbyte d = (jbyte) b;
  int r = 0;
  while (r != 1)
    {
      r = ::write (fd, &d, 1);
      if (r == -1)
        {
	  if (java::lang::Thread::interrupted())
	    {
	      InterruptedIOException *iioe
		= new InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = r == -1 ? 0 : r;
	      throw iioe;
	    }	    
	  throw new IOException (JvNewStringLatin1 (strerror (errno)));
	}
    }
}

void
java::io::FileDescriptor::write (jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new java::lang::NullPointerException;
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new java::lang::ArrayIndexOutOfBoundsException;
  jbyte *bytes = elements (b) + offset;

  int written = 0;
  while (len > 0)
    {
      int r = ::write (fd, bytes, len);
      if (r == -1)
        {
	  if (java::lang::Thread::interrupted())
	    {
	      InterruptedIOException *iioe
		= new InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = written;
	      throw iioe;
	    }
	  throw new IOException (JvNewStringLatin1 (strerror (errno)));
	}

      written += r;
      len -= r;
      bytes += r;
    }
}

void
java::io::FileDescriptor::close (void)
{
  jint save = fd;
  fd = -1;
  if (::close (save))
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
}

jint
java::io::FileDescriptor::seek (jlong pos, jint whence, jboolean eof_trunc)
{
  JvAssert (whence == SET || whence == CUR);

  jlong len = length ();
  jlong here = getFilePointer ();

  if (eof_trunc
      && ((whence == SET && pos > len) || (whence == CUR && here + pos > len)))
    {
      whence = SET;
      pos = len;
    }

  off_t r = ::lseek (fd, (off_t) pos, whence == SET ? SEEK_SET : SEEK_CUR);
  if (r == -1)
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
  return r;
}

jlong
java::io::FileDescriptor::length (void)
{
  struct stat sb;
  if (::fstat (fd, &sb))
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
  return sb.st_size;
}

jlong
java::io::FileDescriptor::getFilePointer (void)
{
  off_t r = ::lseek (fd, 0, SEEK_CUR);
  if (r == -1)
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
  return r;
}

jint
java::io::FileDescriptor::read (void)
{
  jbyte b;
  int r = ::read (fd, &b, 1);
  if (r == 0)
    return -1;
  if (r == -1)
    {
      if (java::lang::Thread::interrupted())
	{
	  InterruptedIOException *iioe
	    = new InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	  iioe->bytesTransferred = r == -1 ? 0 : r;
	  throw iioe;
	}
      throw new IOException (JvNewStringLatin1 (strerror (errno)));
    }
  return b & 0xFF;
}

jint
java::io::FileDescriptor::read (jbyteArray buffer, jint offset, jint count)
{
  if (! buffer)
    throw new java::lang::NullPointerException;
  jsize bsize = JvGetArrayLength (buffer);
  if (offset < 0 || count < 0 || offset + count > bsize)
    throw new java::lang::ArrayIndexOutOfBoundsException;
  jbyte *bytes = elements (buffer) + offset;
  int r = ::read (fd, bytes, count);
  if (r == 0)
    return -1;
  if (r == -1)
    {    
      if (java::lang::Thread::interrupted())
	{
	  InterruptedIOException *iioe
	    = new InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	  iioe->bytesTransferred = r == -1 ? 0 : r;
	  throw iioe;
	}
      throw new IOException (JvNewStringLatin1 (strerror (errno)));
    }
  return r;
}

jint
java::io::FileDescriptor::available (void)
{
#if defined (FIONREAD) || defined (HAVE_SELECT) || defined (HAVE_FSTAT)
  long num = 0;
  int r = 0;
  bool num_set = false;

#if defined (FIONREAD)
  r = ::ioctl (fd, FIONREAD, &num);
  if (r == -1 && errno == ENOTTY)
    {
      // If the ioctl doesn't work, we don't care.
      r = 0;
      num = 0;
    }
  else
    num_set = true;
#elif defined (HAVE_SELECT)
  if (fd < 0)
    {
      errno = EBADF;
      r = -1;
    }
#endif

  if (r == -1)
    {
    posix_error:
      throw new IOException (JvNewStringLatin1 (strerror (errno)));
    }

  // If we didn't get anything, and we have fstat, then see if see if
  // we're reading a regular file.  On many systems, FIONREAD does not
  // work on regular files; select() likewise returns a useless
  // result.  This is run incorrectly when FIONREAD does work on
  // regular files and we are at the end of the file.  However, this
  // case probably isn't very important.
#if defined (HAVE_FSTAT)
  if (! num_set)
    {
      struct stat sb;
      off_t where = 0;
      if (fstat (fd, &sb) != -1
	  && S_ISREG (sb.st_mode)
	  && (where = lseek (fd, SEEK_CUR, 0)) != (off_t) -1)
	{
	  num = (long) (sb.st_size - where);
	  num_set = true;
	}
    }
#endif /* HAVE_FSTAT */

#if defined (HAVE_SELECT)
  if (! num_set)
    {
      fd_set rd;
      FD_ZERO (&rd);
      FD_SET (fd, &rd);
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 0;
      r = _Jv_select (fd + 1, &rd, NULL, NULL, &tv);
      if (r == -1)
	goto posix_error;
      num = r == 0 ? 0 : 1;
    }
#endif /* HAVE_SELECT */

  return (jint) num;
#else
  throw new IOException (JvNewStringLatin1 ("unimplemented"));
#endif
}
