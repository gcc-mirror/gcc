// natFileDescriptor.cc - Native part of FileDescriptor class.

/* Copyright (C) 1998, 1999, 2001, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/io/FileDescriptor.h>
#include <java/io/SyncFailedException.h>
#include <java/io/IOException.h>
#include <java/io/EOFException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/String.h>
#include <java/io/FileNotFoundException.h>
#include <gnu/java/nio/MappedByteBufferImpl.h>
#include <java/nio/channels/FileChannel.h>
#include <java/nio/channels/FileLock.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>

using gnu::gcj::RawData;
using java::io::IOException;
using gnu::java::nio::MappedByteBufferImpl;
using java::io::InterruptedIOException;
using java::io::FileNotFoundException;
using java::lang::ArrayIndexOutOfBoundsException;
using java::lang::NullPointerException;
using gnu::java::nio::channels::FileChannelImpl;

extern "C" void diag_write_char (char c);

static void 
diag_write (char *data, int len)
{
  while (len > 0)
    {
      diag_write_char (*data++);
      len--;
    }
}

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
  return true;
}

void
FileChannelImpl::sync (void)
{
  // Some files don't support fsync.  We don't bother reporting these
  // as errors.
#ifdef HAVE_FSYNC
#else
  throw new SyncFailedException (JvNewStringLatin1 (NO_FSYNC_MESSAGE));
#endif
}
#endif

jint
FileChannelImpl::open (jstring, jint)
{
  return fd;
}

void
FileChannelImpl::write (jint)
{
  char d = (char) b;
  ::diag_write (&d, 1);
}

void
FileChannelImpl::write (jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new NullPointerException;
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new ArrayIndexOutOfBoundsException;
  char *bytes = (char *)elements (b) + offset;
  ::diag_write (bytes, len);
}

void
FileChannelImpl::implCloseChannel (void)
{
}

void
FileChannelImpl::implTruncate (jlong)
{
}

void
FileChannelImpl::seek (jlong)
{
}

jlong
FileChannelImpl::size (void)
{
  return 0;
}

jlong
FileChannelImpl::implPosition (void)
{
  return 0;
}

jint
FileChannelImpl::read (void)
{
  return 0;
}

jint
FileChannelImpl::read (jbyteArray buffer, jint offset, jint count)
{
  return 0;
}

jint
FileChannelImpl::available (void)
{
  return 0;
}

jboolean
FileChannelImpl::lock (jlong, jlong, jboolean, jboolean)
{
  throw new IOException (JvNewStringLatin1
    ("gnu.java.nio.FileChannelImpl.lock() not implemented"));
}

void
FileChannelImpl::unlock (jlong, jlong)
{
  throw new IOException (JvNewStringLatin1
    ("gnu.java.nio.channels.FileChannelImpl.unlock() not implemented"));
}

java::nio::MappedByteBuffer *
FileChannelImpl::mapImpl (jchar, jlong, jint)
{
  return NULL;
}

void
MappedByteBufferImpl::unmapImpl ()
{
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
}
