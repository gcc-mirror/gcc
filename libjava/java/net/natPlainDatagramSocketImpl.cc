/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <cni.h>
#include <java/io/IOException.h>
#include <java/io/FileDescriptor.h>
#include <java/net/BindException.h>
#include <java/net/SocketException.h>
#include <java/net/PlainDatagramSocketImpl.h>
#include <java/net/InetAddress.h>
#include <java/net/DatagramPacket.h>
#include <java/lang/Boolean.h>

#ifndef HAVE_SOCKLEN_T
typedef int socklen_t;
#endif

union SockAddr
{
  struct sockaddr_in address;
#ifdef HAVE_INET6
  struct sockaddr_in6 address6;
#endif
};

union McastReq
{
  struct ip_mreq mreq;
#ifdef HAVE_INET6
  struct ipv6_mreq mreq6;
#endif
};


// FIXME: routines here and/or in natPlainSocketImpl.cc could throw
// NoRouteToHostException; also consider UnknownHostException, ConnectException.

void
java::net::PlainDatagramSocketImpl::create ()
{
  int sock = ::socket (AF_INET, SOCK_DGRAM, 0);
  if (sock < 0)
    {
      char msg[100];
      char* strerr = strerror (errno);
      sprintf (msg, "DatagramSocketImpl.create: %.*s", 80, strerr);
      JvThrow (new java::net::SocketException (JvNewStringUTF (msg)));
    }
  fnum = sock;
  fd = new java::io::FileDescriptor (sock);
}

void
java::net::PlainDatagramSocketImpl::bind (jint lport,
  java::net::InetAddress *host)
{
  // FIXME: prob. need to do a setsockopt with SO_BROADCAST to allow multicast.
  union SockAddr u;
  jbyteArray haddress = host->address;
  jbyte *bytes = elements (haddress);
  int len = haddress->length;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  if (len == 4)
    {
      u.address.sin_family = AF_INET;
      memcpy (&u.address.sin_addr, bytes, len);
      len = sizeof (struct sockaddr_in);
      u.address.sin_port = htons (lport);
    }
#ifdef HAVE_INET6
  else if (len == 16)
    {
      u.address6.sin6_family = AF_INET6;
      memcpy (&u.address6.sin6_addr, bytes, len);
      len = sizeof (struct sockaddr_in6);
      u.address6.sin6_port = htons (lport);
    }
#endif
  else
    goto error;
  if (::bind (fnum, ptr, len) == 0)
    {
      address = host;
      localport = lport;
      return;
    }
 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.bind: %.*s", 80, strerr);
  JvThrow (new java::net::BindException (JvNewStringUTF (msg)));
}

jint
java::net::PlainDatagramSocketImpl::peek (java::net::InetAddress *i)
{
  // FIXME: Deal with Multicast and if the socket is connected.
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  ssize_t retlen =
    ::recvfrom (fnum, (char *) NULL, 0, MSG_PEEK, (sockaddr*) &u,
      &addrlen);
  if (retlen < 0)
    goto error;
  // FIXME: Deal with Multicast addressing and if the socket is connected.
  jbyteArray raddr;
  jint rport;
  if (u.address.sin_family == AF_INET)
    {
      raddr = JvNewByteArray (4);
      memcpy (elements (raddr), &u.address.sin_addr, 4);
      rport = ntohs (u.address.sin_port);
    }
#ifdef HAVE_INET6
  else if (u.address.sin_family == AF_INET6)
    {
      raddr = JvNewByteArray (16);
      memcpy (elements (raddr), &u.address6.sin6_addr, 16);
      rport = ntohs (u.address6.sin6_port);
    }
#endif
  else
    goto error;
  // FIXME: Multicast:  s->address = new InetAddress (raddr, NULL);
  i->address = raddr;
  return rport;
 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.peek: %.*s", 80, strerr);
  JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
}

void
java::net::PlainDatagramSocketImpl::send (java::net::DatagramPacket *p)
{
  // FIXME: Deal with Multicast and if the socket is connected.
  jint rport = p->getPort();
  union SockAddr u;
  jbyteArray haddress = p->getAddress()->address;
  jbyte *bytes = elements (haddress);
  int len = haddress->length;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  jbyte *dbytes = elements (p->getData());
  if (len == 4)
    {
      u.address.sin_family = AF_INET;
      memcpy (&u.address.sin_addr, bytes, len);
      len = sizeof (struct sockaddr_in);
      u.address.sin_port = htons (rport);
    }
#ifdef HAVE_INET6
  else if (len == 16)
    {
      u.address6.sin6_family = AF_INET6;
      memcpy (&u.address6.sin6_addr, bytes, len);
      len = sizeof (struct sockaddr_in6);
      u.address6.sin6_port = htons (rport);
    }
#endif
  else
    goto error;
  if (::sendto (fnum, (char *) dbytes, p->getLength(), 0, ptr, len) >= 0)
    return;
 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.send: %.*s", 80, strerr);
  JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
}

void
java::net::PlainDatagramSocketImpl::receive (java::net::DatagramPacket *p)
{
  // FIXME: Deal with Multicast and if the socket is connected.
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  jbyte *dbytes = elements (p->getData());
  ssize_t retlen =
    ::recvfrom (fnum, (char *) dbytes, p->getLength(), 0, (sockaddr*) &u,
      &addrlen);
  if (retlen < 0)
    goto error;
  // FIXME: Deal with Multicast addressing and if the socket is connected.
  jbyteArray raddr;
  jint rport;
  if (u.address.sin_family == AF_INET)
    {
      raddr = JvNewByteArray (4);
      memcpy (elements (raddr), &u.address.sin_addr, 4);
      rport = ntohs (u.address.sin_port);
    }
#ifdef HAVE_INET6
  else if (u.address.sin_family == AF_INET6)
    {
      raddr = JvNewByteArray (16);
      memcpy (elements (raddr), &u.address6.sin6_addr, 16);
      rport = ntohs (u.address6.sin6_port);
    }
#endif
  else
    goto error;
  // FIXME: Multicast:  s->address = new InetAddress (raddr, NULL);
  p->setPort (rport);
  p->setLength ((jint) retlen);
  return;
 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.receive: %.*s", 80, strerr);
  JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
}

void
java::net::PlainDatagramSocketImpl::setTimeToLive (jint ttl)
{
  // throws IOException;
  // FIXME: TODO - PlainDatagramSocketImpl::setTimeToLive
}

jint
java::net::PlainDatagramSocketImpl::getTimeToLive ()
{
  // throws IOException;
  // FIXME: TODO - PlainDatagramSocketImpl::getTimeToLive
  return 0;
}

void
java::net::PlainDatagramSocketImpl::mcastGrp (java::net::InetAddress *inetaddr,
  jboolean join)
{
  union McastReq u;
  jbyteArray haddress = inetaddr->address;
  jbyte *bytes = elements (haddress);
  int len = haddress->length;
  int level, opname;
  const char *ptr;
  if (len == 4)
    {
      level = IPPROTO_IP;
      opname = join ? IP_ADD_MEMBERSHIP : IP_DROP_MEMBERSHIP;
      memcpy (&u.mreq.imr_multiaddr, bytes, len);
      // FIXME:  If a non-default interface is set, use it; see Stevens p. 501.
      u.mreq.imr_interface.s_addr = htonl (INADDR_ANY); 
      len = sizeof (struct ip_mreq);
      ptr = (const char *) &u.mreq;
    }
#ifdef HAVE_INET6
  else if (len == 16)
    {
      level = IPPROTO_IPV6;
      opname = join ? IPV6_ADD_MEMBERSHIP : IPV6_DROP_MEMBERSHIP;
      memcpy (&u.mreq6.ipv6mr_multiaddr, bytes, len);
      // FIXME:  If a non-default interface is set, use it; see Stevens p. 501.
      u.mreq6.ipv6mr_interface = 0;
      len = sizeof (struct ipv6_mreq);
      ptr = (const char *) &u.mreq6;
    }
#endif
  else
    goto error;
  if (::setsockopt (fnum, level, opname, ptr, len) == 0)
    return;
 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.%s: %.*s", join ? "join" : "leave", 80,
    strerr);
  JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
}

void
java::net::PlainDatagramSocketImpl::setOption (jint optID,
  java::lang::Object *value)
{
  if (optID == _Jv_SO_REUSEADDR_)
    {
      // FIXME: Is it possible that a Boolean wasn't passed in?
      const int on = (((java::lang::Boolean *) value)->booleanValue()) ? 1 : 0;
      if (::setsockopt (fnum, SOL_SOCKET, SO_REUSEADDR, (char *) &on,
	  sizeof (int)) == 0)
        return;
    }
  else
    errno = ENOPROTOOPT;

  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.setOption: %.*s", 80, strerr);
  JvThrow (new java::net::SocketException (JvNewStringUTF (msg)));
}

java::lang::Object *
java::net::PlainDatagramSocketImpl::getOption (jint optID)
{
  if (optID == _Jv_SO_REUSEADDR_)
    {
      int on;
      socklen_t len;
      if (::getsockopt (fnum, SOL_SOCKET, SO_REUSEADDR, (char *) &on,
	  (socklen_t *) &len) == 0)
        return new java::lang::Boolean (on == 1);
    }
  else
    errno = ENOPROTOOPT;

  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.getOption: %.*s", 80, strerr);
  JvThrow (new java::net::SocketException (JvNewStringUTF (msg)));
}
