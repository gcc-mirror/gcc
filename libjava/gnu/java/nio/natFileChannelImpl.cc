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
#include <java/io/IOException.h>
#include <java/nio/channels/FileChannel.h>

jlong
gnu::java::nio::FileChannelImpl::lengthInternal (jint fd)
{
  throw new ::java::io::IOException (JvNewStringUTF ("lengthInternal not implemented"));
}

jlong
gnu::java::nio::FileChannelImpl::nio_mmap_file (jint, jlong, jint, jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("mmap not implemented"));
}

void
gnu::java::nio::FileChannelImpl::nio_unmmap_file (jint, jlong, jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("munmap not implemented"));
}

void
gnu::java::nio::FileChannelImpl::nio_msync (jint, jlong, jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("msync not implemented"));
}
