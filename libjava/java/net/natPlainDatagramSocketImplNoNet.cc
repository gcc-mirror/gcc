/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <java/io/IOException.h>
#include <java/lang/Object.h>
#include <java/net/BindException.h>
#include <java/net/DatagramPacket.h>
#include <java/net/InetAddress.h>
#include <java/net/NetworkInterface.h>
#include <java/net/PlainDatagramSocketImpl.h>
#include <java/net/SocketException.h>

void
java::net::PlainDatagramSocketImpl::create ()
{
  throw new SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.create: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::bind (jint, java::net::InetAddress *)
{
  throw new BindException (
    JvNewStringLatin1 ("DatagramSocketImpl.bind: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::connect (java::net::InetAddress *, jint)
{
  throw new SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.connect: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::disconnect ()
{
  throw new SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.disconnect: unimplemented"));
}

jint
java::net::PlainDatagramSocketImpl::peek (java::net::InetAddress *)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.peek: unimplemented"));
}

jint
java::net::PlainDatagramSocketImpl::peekData(java::net::DatagramPacket *)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.peekData: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::close ()
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.close: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::send (java::net::DatagramPacket *)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.send: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::receive (java::net::DatagramPacket *)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.receive: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::setTimeToLive (jint)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.setTimeToLive: unimplemented"));
}

jint
java::net::PlainDatagramSocketImpl::getTimeToLive ()
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.getTimeToLive: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::mcastGrp (java::net::InetAddress *,
                                              java::net::NetworkInterface *,
					      jboolean)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.mcastGrp: unimplemented"));
}

void
java::net::PlainDatagramSocketImpl::setOption (jint, java::lang::Object *)
{
  throw new SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.setOption: unimplemented"));
}

java::lang::Object *
java::net::PlainDatagramSocketImpl::getOption (jint)
{
  throw new SocketException (
    JvNewStringLatin1 ("DatagramSocketImpl.getOption: unimplemented"));
}
