/* Copyright (C) 1998, 1999  Cygnus Solutions

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
#include <java/net/PlainSocketImpl.h>
#include <java/net/InetAddress.h>

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

void
java::net::PlainSocketImpl::create (jboolean stream)
{
  int sock = ::socket (AF_INET, stream ? SOCK_STREAM : SOCK_DGRAM, 0);
  if (sock < 0)
    {
      char msg[100];
      char* strerr = strerror (errno);
      sprintf (msg, "SocketImpl.create: %.*s", 80, strerr);
      JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
    }
  fnum = sock;
  fd = new java::io::FileDescriptor (sock);
}

void
java::net::PlainSocketImpl::bind (java::net::InetAddress *host, jint lport)
{
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
  sprintf (msg, "SocketImpl.bind: %.*s", 80, strerr);
  JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
}

void
java::net::PlainSocketImpl::connect (java::net::InetAddress *host, jint rport)
{
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
  if (::connect (fnum, ptr, len) == 0)
    {
      address = host;
      port = rport;
      return;
    }
 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "SocketImpl.connect: %.*s", 80, strerr);
  JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
}

void
java::net::PlainSocketImpl::listen (jint backlog)
{
  if (::listen (fnum, backlog) != 0)
    {
      char msg[100];
      char* strerr = strerror (errno);
      sprintf (msg, "SocketImpl.listen: %.*s", 80, strerr);
      JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
    }
}

void
java::net::PlainSocketImpl::accept (java::net::PlainSocketImpl *s)
{
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  int new_socket = ::accept (fnum, (sockaddr*) &u, &addrlen);
  if (new_socket < 0)
    goto error;
  jbyteArray raddr;
  jint rport;
  if (u.address.sin_family == AF_INET)
    {
      raddr = JvNewByteArray (4);
      memcpy (elements (raddr), &u.address.sin_addr, 4);
      rport = ntohs (u.address.sin_port);
    }
#ifdef HAVE_INET6
    {
      raddr = JvNewByteArray (16);
      memcpy (elements (raddr), &u.address6.sin6_addr, 16);
      rport = ntohs (u.address6.sin6_port);
    }
#endif
  else
    goto error;
  s->fnum = new_socket;
  s->localport = localport;
  s->address = new InetAddress (raddr, NULL);
  s->port = rport;
  s->fd = new java::io::FileDescriptor (new_socket);
  return;
 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "SocketImpl.accept: %.*s", 80, strerr);
  JvThrow (new java::io::IOException (JvNewStringUTF (msg)));
}
