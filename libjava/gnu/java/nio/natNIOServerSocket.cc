// natNIOServerSocket.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gnu/java/net/PlainSocketImpl.h>
#include <gnu/java/nio/NIOServerSocket.h>
#include <java/net/ServerSocket.h>
#include <java/net/SocketImpl.h>

gnu::java::net::PlainSocketImpl*
gnu::java::nio::NIOServerSocket::getPlainSocketImpl()
{
  return (gnu::java::net::PlainSocketImpl*)
    ::java::net::ServerSocket::getImpl();
}
