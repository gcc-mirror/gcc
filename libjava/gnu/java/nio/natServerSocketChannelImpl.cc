// natSelectorImpl.cc

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <errno.h>
#include <netinet/in.h>

#include <gcj/cni.h>
#include <gnu/java/nio/ServerSocketChannelImpl.h>
#include <gnu/java/nio/SocketChannelImpl.h>
#include <java/io/IOException.h>
#include <java/net/InetSocketAddress.h>
#include <java/net/SocketException.h>

union SockAddr
{
  struct sockaddr_in address;
#ifdef HAVE_INET6
  struct sockaddr_in6 address6;
#endif
};

jint
gnu::java::nio::ServerSocketChannelImpl::SocketAccept (
                                           ServerSocketChannelImpl* socket,
                                           SocketChannelImpl* s)
{
  union SockAddr u;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  socklen_t addrlen = sizeof(struct sockaddr);
/*
  jbyteArray haddress = socket->sa->getAddress ()->addr;
  jbyte *bytes = elements (haddress);
  int len = haddress->length;

  if (len == 4)
    {
      u.address.sin_family = AF_INET;
      memcpy (&u.address.sin_addr, bytes, len);
      len = sizeof (struct sockaddr_in);
      u.address.sin_port = htons ( socket->sa->getPort ());
    }
#ifdef HAVE_INET6
  else if (len == 16)
    {
      u.address6.sin6_family = AF_INET6;
      memcpy (&u.address6.sin6_addr, bytes, len);
      len = sizeof (struct sockaddr_in6);
      u.address6.sin6_port = htons (socket->sa->getPort ());
    }
#endif
  else
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid length"));
*/

  int sock = _Jv_accept (socket->fd, ptr, &addrlen);

  // FIXME: write address/port in ptr into java variables

  if (sock < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  s->fd = sock;
  return sock;
}
