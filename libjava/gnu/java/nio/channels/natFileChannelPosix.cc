
// natFileChannelImplPosix.cc - Native part of FileChannelImpl class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gcj/cni.h>
#include <gcj/javaprims.h>
#include <jvm.h>

#include "posix.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/param.h>

#include <gnu/gcj/RawData.h>
#include <gnu/java/nio/FileLockImpl.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>
#include <java/io/FileNotFoundException.h>
#include <java/io/IOException.h>
#include <java/io/SyncFailedException.h>
#include <java/io/InterruptedIOException.h>
#include <java/io/EOFException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/System.h>
#include <java/lang/String.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/Thread.h>
#include <java/nio/ByteBuffer.h>
#include <java/nio/MappedByteBufferImpl.h>
#include <java/nio/channels/FileChannel.h>
#include <java/nio/channels/FileLock.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>

#ifdef HAVE_SYS_IOCTL_H
#define BSD_COMP /* Get FIONREAD on Solaris2. */
#include <sys/ioctl.h>
#endif

// Pick up FIONREAD on Solaris 2.5.
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#ifdef HAVE_MMAP
#include <sys/mman.h>

// Use overload resolution to find out the argument types.
// E.g. Solaris 2.6 uses different argument types for munmap and msync.
// This is in case _POSIX_C_SOURCES is smaller than 3.

template <typename T_implPtr, typename T_implLen>
static inline int
munmap_adaptor(int (*munmap)(T_implPtr caddr, T_implLen sizet),
		 void* caddr, size_t sizet)
{
  return munmap ((T_implPtr) caddr, (T_implLen) sizet);
}

template <typename T_implPtr, typename T_implLen, typename T_msync>
static inline int
msync_adaptor(int (*msync)(T_implPtr caddr, T_implLen sizet, T_msync msynct),
	      void* caddr, size_t sizet, int msynct)
{
  return msync ((T_implPtr) caddr, (T_implLen) sizet, (T_msync) msynct);
}
#endif

using gnu::gcj::RawData;
using java::io::IOException;
using java::nio::MappedByteBufferImpl;
using java::io::InterruptedIOException;
using java::io::FileNotFoundException;
using java::lang::ArrayIndexOutOfBoundsException;
using gnu::java::nio::channels::FileChannelImpl;

#define NO_FSYNC_MESSAGE "sync unsupported"

void
FileChannelImpl::init(void)
{
  in = new FileChannelImpl((jint) 0, FileChannelImpl::READ);
  out = new FileChannelImpl((jint) 1, FileChannelImpl::WRITE);
  err = new FileChannelImpl((jint) 2, FileChannelImpl::WRITE);
}

#if 0
jboolean
FileChannelImpl::valid (void)
{
  struct stat sb;
  return fd >= 0 && ::fstat (fd, &sb) == 0;
}

void
FileChannelImpl::sync (void)
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
#endif

jint
FileChannelImpl::open (jstring path, jint jflags)
{
  fd = -1;
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

  if ((jflags & SYNC))
    flags |= O_SYNC;

  if ((jflags & DSYNC))
    flags |= O_DSYNC;

  int fd = ::open (buf, flags, mode);
  if (fd == -1 && errno == EMFILE)
    {
      // Because finalize () calls close () we might be able to continue.
      ::java::lang::System::gc ();
      ::java::lang::System::runFinalization ();
      fd = ::open (buf, flags, mode);
    }
  if (fd == -1)
    {
      // We choose the formatting here for JDK compatibility, believe
      // it or not.
      ::java::lang::StringBuffer *msg = new ::java::lang::StringBuffer (path);
      msg->append (JvNewStringUTF (" ("));
      msg->append (JvNewStringUTF (strerror (errno)));
      msg->append (JvNewStringUTF (")"));
      throw new ::java::io::FileNotFoundException (msg->toString ());
    }

  _Jv_platform_close_on_exec (fd);

  return fd;
}

void
FileChannelImpl::write (jint b)
{
  jbyte d = (jbyte) b;
  int r = 0;
  while (r != 1)
    {
      r = ::write (fd, &d, 1);
      if (r == -1)
        {
	  if (::java::lang::Thread::interrupted())
	    {
	      ::java::io::InterruptedIOException *iioe
		= new ::java::io::InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = r == -1 ? 0 : r;
	      throw iioe;
	    }	    
	  if (errno != EINTR)
	    throw new IOException (JvNewStringLatin1 (strerror (errno)));
	}
    }
  pos++;
}

void
FileChannelImpl::write (jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new ::java::lang::NullPointerException;
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new ArrayIndexOutOfBoundsException;
  jbyte *bytes = elements (b) + offset;

  int written = 0;
  while (len > 0)
    {
      int r = ::write (fd, bytes, len);
      if (r == -1)
        {
	  if (::java::lang::Thread::interrupted())
	    {
	      InterruptedIOException *iioe
		= new InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = written;
	      throw iioe;
	    }
	  if (errno != EINTR)
	    throw new IOException (JvNewStringLatin1 (strerror (errno)));
	  continue;
	}

      written += r;
      len -= r;
      bytes += r;
      pos += r;
    }
}

void
FileChannelImpl::implCloseChannel (void)
{
  jint save = fd;
  fd = -1;
  if (::close (save))
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
}

void
FileChannelImpl::implTruncate (jlong size)
{
  struct stat sb;

#ifdef HAVE_FTRUNCATE
  if (::fstat (fd, &sb))
    throw new IOException (JvNewStringLatin1 (strerror (errno)));

  if ((jlong) sb.st_size == size) 
    return;

  // If the file is too short, we extend it.  We can't rely on
  // ftruncate() extending the file.  So we lseek() to 1 byte less
  // than we want, and then we write a single byte at the end.
  if ((jlong) sb.st_size < size)
    {
      if (::lseek (fd, (off_t) (size - 1), SEEK_SET) == -1)
	throw new IOException (JvNewStringLatin1 (strerror (errno)));
      char out = '\0';
      int r = ::write (fd, &out, 1);
      if (r <= 0 || ::lseek (fd, pos, SEEK_SET) == -1)
	throw new IOException (JvNewStringLatin1 (strerror (errno)));
    }
  else
    {
      if (::ftruncate (fd, (off_t) size))
	throw new IOException (JvNewStringLatin1 (strerror (errno)));
      if (pos > size
	  && ::lseek (fd, (off_t) size, SEEK_SET) == -1)
	throw new IOException (JvNewStringLatin1 (strerror (errno)));
      pos = size;
    }
#else /* HAVE_FTRUNCATE */
  throw new IOException (JvNewStringLatin1 ("FileDescriptor.setLength not implemented"));
#endif /* HAVE_FTRUNCATE */
}

void
FileChannelImpl::seek (jlong newPos)
{
  off_t r = ::lseek (fd, (off_t) newPos, SEEK_SET);
  if (r == -1)
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
  pos = r;
}

jlong
FileChannelImpl::size (void)
{
  struct stat sb;
  if (::fstat (fd, &sb))
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
  return sb.st_size;
}

jlong
FileChannelImpl::implPosition (void)
{
  return pos;
}

jint
FileChannelImpl::read (void)
{
  jbyte b;
  int r;
  do
    {
      r = ::read (fd, &b, 1);
      if (r == 0)
	return -1;
      if (r == -1)
	{
	  if (::java::lang::Thread::interrupted())
	    {
	      InterruptedIOException *iioe
		= new InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = r == -1 ? 0 : r;
	      throw iioe;
	    }
	  if (errno != EINTR)
	    throw new IOException (JvNewStringLatin1 (strerror (errno)));
	}
    }
  while (r != 1);
  pos++;
  return b & 0xFF;
}

jint
FileChannelImpl::read (jbyteArray buffer, jint offset, jint count)
{
  if (! buffer)
    throw new ::java::lang::NullPointerException;
  jsize bsize = JvGetArrayLength (buffer);
  if (offset < 0 || count < 0 || offset + count > bsize)
    throw new ::java::lang::ArrayIndexOutOfBoundsException;

  // Must return 0 if an attempt is made to read 0 bytes.
  if (count == 0)
    return 0;

  jbyte *bytes = elements (buffer) + offset;
  int r;
  do
    {
      r = ::read (fd, bytes, count);
      if (r == 0)
	return -1;
      if (r == -1)
	{
	  if (::java::lang::Thread::interrupted())
	    {
	      InterruptedIOException *iioe
		= new InterruptedIOException (JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = r == -1 ? 0 : r;
	      throw iioe;
	    }
	  if (errno != EINTR)
	    throw new IOException (JvNewStringLatin1 (strerror (errno)));
	}
    }
  while (r <= 0);
  pos += r;
  return r;
}

jint
FileChannelImpl::available (void)
{
#if defined (FIONREAD) || defined (HAVE_SELECT) || defined (HAVE_FSTAT)
  int num = 0;
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
	  && (where = lseek (fd, 0, SEEK_CUR)) != (off_t) -1)
	{
	  num = (int) (sb.st_size - where);
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
  return 0;
#endif
}

jboolean
FileChannelImpl::lock
(jlong pos, jlong len, jboolean shared, jboolean wait)
{
  struct flock lockdata;

  lockdata.l_type = shared ? F_RDLCK : F_WRLCK;
  lockdata.l_whence = SEEK_SET;
  lockdata.l_start = pos;
  lockdata.l_len = len;

  if (::fcntl (fd, wait ? F_SETLKW : F_SETLK, &lockdata) == -1)
    {
      if (! wait && (errno == EACCES || errno == EAGAIN))
	return false;
      throw new IOException (JvNewStringLatin1 (strerror (errno)));
    }
  return true;
}

void
FileChannelImpl::unlock (jlong pos, jlong len)
{
  struct flock lockdata;

  lockdata.l_type = F_UNLCK;
  lockdata.l_whence = SEEK_SET;
  lockdata.l_start = pos;
  lockdata.l_len = len;

  if (::fcntl (fd, F_SETLK, &lockdata) == -1)
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
}

java::nio::MappedByteBuffer *
FileChannelImpl::mapImpl (jchar mmode, jlong position, jint size)
{
#if defined(HAVE_MMAP)
  int prot, flags;
  if (mmode == 'r')
    {
      prot = PROT_READ;
      flags = MAP_PRIVATE;
    }
  else
    {
      prot = PROT_READ|PROT_WRITE;
      flags = mmode == '+' ? MAP_SHARED : MAP_PRIVATE;
    }
  jint page_size = ::getpagesize();
  jint offset = position & ~(page_size-1);
  jint align = position - offset;
  void* ptr = ::mmap(NULL, size + align, prot, flags, fd, offset);
  MappedByteBufferImpl *buf
    = new MappedByteBufferImpl ((RawData *) ((char *) ptr + align),
				size, mmode == 'r');
  if (ptr == (void *) MAP_FAILED)
    throw new IOException (JvNewStringLatin1 (strerror (errno)));
  buf->implPtr = reinterpret_cast<RawData*> (ptr);
  buf->implLen = size+align;
  return buf;
#else
  throw new IOException (JvNewStringUTF ("mmap not implemented"));
#endif
}

void
MappedByteBufferImpl::unmapImpl ()
{
#if defined(HAVE_MMAP)
  munmap_adaptor(munmap, implPtr, implLen);
#endif
}

void
MappedByteBufferImpl::loadImpl ()
{
}

jboolean
MappedByteBufferImpl::isLoadedImpl ()
{
  return true;
}

void
MappedByteBufferImpl::forceImpl ()
{
#if defined(HAVE_MMAP)
  ::msync_adaptor(msync, implPtr, implLen, MS_SYNC);
#endif
}
