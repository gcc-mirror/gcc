// natPipeImplWin32.cc

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
//#include <gnu/java/nio/PipeImpl$SinkChannelImpl.h>
//#include <gnu/java/nio/PipeImpl$SourceChannelImpl.h>
#include <java/io/IOException.h>
#include <java/nio/channels/spi/SelectorProvider.h>

void
gnu::java::nio::PipeImpl::nativeInit (::java::nio::channels::spi::SelectorProvider* /*provider*/)
{
  int filedes [2];

  if (_Jv_pipe (filedes) < 0)
    throw new ::java::io::IOException (JvNewStringUTF (strerror (errno)));

  /* FIXME
  source = new gnu::java::nio::PipeImpl$SourceChannelImpl
    (this, provider, filedes [0]);
  sink = new gnu::java::nio::PipeImpl$SinkChannelImpl
    (this, provider, filedes [1]);
  */
}
