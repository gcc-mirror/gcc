// natServerSocketChannelImpl.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <errno.h>
#include <string.h>

#include <gnu/java/net/PlainSocketImpl.h>
#include <gnu/java/nio/ServerSocketChannelImpl.h>
#include <java/net/ServerSocket.h>

void
gnu::java::nio::ServerSocketChannelImpl::initServerSocket()
{
  serverSocket = new ::java::net::ServerSocket (impl);
}
