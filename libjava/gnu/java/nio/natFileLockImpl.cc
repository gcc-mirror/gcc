// natFileLockImpl.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <jvm.h>
#include <errno.h>

#include <gnu/java/nio/FileLockImpl.h>
#include <java/io/FileDescriptor.h>
#include <java/io/IOException.h>

void
gnu::java::nio::FileLockImpl::releaseImpl ()
{
  throw new ::java::io::IOException
    (JvNewStringUTF ("releaseImpl not implemented"));
}
