// natFileChannelImpl.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <jvm.h>

#include <errno.h>
#include <string.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <gnu/java/nio/FileChannelImpl.h>
#include <java/io/FileDescriptor.h>
#include <java/io/IOException.h>
#include <java/nio/channels/FileChannel.h>

jlong
gnu::java::nio::FileChannelImpl::size ()
{
  return fd->length ();
}

jlong
gnu::java::nio::FileChannelImpl::implPosition ()
{
  return fd->getFilePointer ();
}

java::nio::channels::FileChannel*
gnu::java::nio::FileChannelImpl::implPosition (jlong newPosition)
{
  fd->seek (newPosition, ::java::io::FileDescriptor::SET, true);
  return this;
}

java::nio::channels::FileChannel*
gnu::java::nio::FileChannelImpl::implTruncate (jlong size)
{
  fd->setLength (size);
  return this;
}

jlong
gnu::java::nio::FileChannelImpl::nio_mmap_file (jlong, jlong, jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("mmap not implemented"));
}

void
gnu::java::nio::FileChannelImpl::nio_unmmap_file (jlong, jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("munmap not implemented"));
}

void
gnu::java::nio::FileChannelImpl::nio_msync (jlong, jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("msync not implemented"));
}
