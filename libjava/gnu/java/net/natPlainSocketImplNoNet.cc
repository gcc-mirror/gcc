/* Copyright (C) 2003 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gnu/java/net/PlainSocketImpl.h>
#include <gnu/java/net/PlainSocketImpl$SocketInputStream.h>
#include <gnu/java/net/PlainSocketImpl$SocketOutputStream.h>
#include <java/io/IOException.h>
#include <java/net/BindException.h>
#include <java/net/ConnectException.h>
#include <java/net/SocketException.h>

void
gnu::java::net::PlainSocketImpl::create (jboolean)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("SocketImpl.create: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::bind (::java::net::InetAddress *, jint)
{
  throw new ::java::net::BindException (
    JvNewStringLatin1 ("SocketImpl.bind: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::connect (::java::net::SocketAddress *, jint)
{
  throw new ::java::net::ConnectException (
    JvNewStringLatin1 ("SocketImpl.connect: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::listen (jint)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("SocketImpl.listen: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::accept (gnu::java::net::PlainSocketImpl *)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("SocketImpl.accept: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::setOption (jint, ::java::lang::Object *)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.setOption: unimplemented"));
}

::java::lang::Object *
gnu::java::net::PlainSocketImpl::getOption (jint)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.getOption: unimplemented"));
}

jint
gnu::java::net::PlainSocketImpl$SocketInputStream::read(void)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.read: unimplemented"));
}

jint
gnu::java::net::PlainSocketImpl$SocketInputStream::read(jbyteArray buffer, 
  jint offset, jint count)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.read: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl$SocketOutputStream::write(jint b)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.write: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl$SocketOutputStream::write(jbyteArray b, 
  jint offset, jint len)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.write: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::sendUrgentData(jint data)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.sendUrgentData: unimplemented"));
}

jint
gnu::java::net::PlainSocketImpl::available(void)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.available: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::close(void)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.close: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::shutdownInput (void)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.shutdownInput: unimplemented"));
}

void
gnu::java::net::PlainSocketImpl::shutdownOutput (void)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("SocketImpl.shutdownOutput: unimplemented"));
}
