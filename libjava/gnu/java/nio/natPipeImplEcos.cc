// natPipeImplEcos.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <errno.h>
#include <string.h>
#include <unistd.h>

#include <gnu/java/nio/PipeImpl.h>
#include <java/io/IOException.h>

void
gnu::java::nio::PipeImpl::nativeInit (::java::nio::channels::spi::SelectorProvider*)
{
  throw new ::java::io::IOException (JvNewStringUTF ("nativeInit() not implemented"));
}
