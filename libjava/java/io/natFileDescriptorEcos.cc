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
java::io::FileDescriptor::init(void)
{
  in = new java::io::FileDescriptor(0);
  out = new java::io::FileDescriptor(1);
  err = new java::io::FileDescriptor(2);
}

jboolean
java::io::FileDescriptor::valid (void)
{
  return true;
}

void
java::io::FileDescriptor::sync (void)
{
  // Some files don't support fsync.  We don't bother reporting these
  // as errors.
#ifdef HAVE_FSYNC
#else
  throw new SyncFailedException (JvNewStringLatin1 (NO_FSYNC_MESSAGE));
#endif
}

jint
java::io::FileDescriptor::open (jstring path, jint jflags)
{
  return fd;
}

void
java::io::FileDescriptor::write (jint b)
{
  char d = (char) b;
  ::diag_write (&d, 1);
}

void
java::io::FileDescriptor::write (jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new java::lang::NullPointerException;
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new java::lang::ArrayIndexOutOfBoundsException;
  char *bytes = (char *)elements (b) + offset;
  ::diag_write (bytes, len);
}

void
java::io::FileDescriptor::close (void)
{
}

void
java::io::FileDescriptor::setLength (long)
{
}

jint
java::io::FileDescriptor::seek (jlong pos, jint whence, jboolean)
{
  JvAssert (whence == SET || whence == CUR);
  return 0;
}

jlong
java::io::FileDescriptor::getLength (void)
{
  return 0;
}

jlong
java::io::FileDescriptor::getFilePointer (void)
{
  return 0;
}

jint
java::io::FileDescriptor::read (void)
{
  return 0;
}

jint
java::io::FileDescriptor::read (jbyteArray buffer, jint offset, jint count)
{
  return 0;
}

jint
java::io::FileDescriptor::available (void)
{
  return 0;
}
