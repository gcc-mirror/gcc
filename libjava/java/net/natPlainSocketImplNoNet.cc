/* Copyright (C) 2003 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <java/net/PlainSocketImpl.h>

void
java::net::PlainSocketImpl::create (jboolean)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("SocketImpl.create: unimplemented"));
}

void
java::net::PlainSocketImpl::bind (java::net::InetAddress *, jint)
{
  throw new BindException (
    JvNewStringLatin1 ("SocketImpl.bind: unimplemented"));
}

void
java::net::PlainSocketImpl::connect (java::net::SocketAddress *, jint)
{
  throw new ConnectException (
    JvNewStringLatin1 ("SocketImpl.connect: unimplemented"));
}

void
java::net::PlainSocketImpl::listen (jint)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("SocketImpl.listen: unimplemented"));
}

void
java::net::PlainSocketImpl::accept (java::net::PlainSocketImpl *)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("SocketImpl.accept: unimplemented"));
}

void
java::net::PlainSocketImpl::setOption (jint, java::lang::Object *)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.setOption: unimplemented"));
}

java::lang::Object *
java::net::PlainSocketImpl::getOption (jint)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.getOption: unimplemented"));
}

jint
java::net::PlainSocketImpl::read(void)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.read: unimplemented"));
}

jint
java::net::PlainSocketImpl::read(jbyteArray buffer, jint offset, jint count)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.read: unimplemented"));
}

void
java::net::PlainSocketImpl::write(jint b)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.write: unimplemented"));
}

void
java::net::PlainSocketImpl::write(jbyteArray b, jint offset, jint len)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.write: unimplemented"));
}

void
java::net::PlainSocketImpl::sendUrgentData(jint data)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.sendUrgentData: unimplemented"));
}

jint
java::net::PlainSocketImpl::available(void)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.available: unimplemented"));
}

void
java::net::PlainSocketImpl::close(void)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.close: unimplemented"));
}

void
java::net::PlainSocketImpl::shutdownInput (void)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.shutdownInput: unimplemented"));
}

void
java::net::PlainSocketImpl::shutdownOutput (void)
{
  throw new SocketException (
    JvNewStringLatin1 ("SocketImpl.shutdownOutput: unimplemented"));
}
