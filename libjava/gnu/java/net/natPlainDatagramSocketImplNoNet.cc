/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gnu/java/net/PlainDatagramSocketImpl.h>
#include <java/io/IOException.h>
#include <java/lang/Object.h>
#include <java/net/BindException.h>
#include <java/net/DatagramPacket.h>
#include <java/net/InetAddress.h>
#include <java/net/NetworkInterface.h>
#include <java/net/SocketException.h>

void
gnu::java::net::PlainDatagramSocketImpl::create ()
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.create: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::bind (jint, ::java::net::InetAddress *)
{
  throw new ::java::net::BindException (
    JvNewStringLatin1 ("DatagramSocketImpl.bind: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::connect (::java::net::InetAddress *, jint)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.connect: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::disconnect ()
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.disconnect: unimplemented"));
}

jint
gnu::java::net::PlainDatagramSocketImpl::peek (::java::net::InetAddress *)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.peek: unimplemented"));
}

jint
gnu::java::net::PlainDatagramSocketImpl::peekData (::java::net::DatagramPacket *)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.peekData: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::close ()
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.close: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::send (::java::net::DatagramPacket *)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.send: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::receive (::java::net::DatagramPacket *)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.receive: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::setTimeToLive (jint)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.setTimeToLive: unimplemented"));
}

jint
gnu::java::net::PlainDatagramSocketImpl::getTimeToLive ()
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.getTimeToLive: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::mcastGrp (::java::net::InetAddress *,
                                              ::java::net::NetworkInterface *,
					      jboolean)
{
  throw new ::java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.mcastGrp: unimplemented"));
}

void
gnu::java::net::PlainDatagramSocketImpl::setOption (jint, ::java::lang::Object *)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.setOption: unimplemented"));
}

::java::lang::Object *
gnu::java::net::PlainDatagramSocketImpl::getOption (jint)
{
  throw new ::java::net::SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.getOption: unimplemented"));
}
