/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <cni.h>
#include <java/io/IOException.h>
#include <java/io/FileDescriptor.h>
#include <java/io/InterruptedIOException.h>
#include <java/net/BindException.h>
#include <java/net/SocketException.h>
#include <java/net/PlainDatagramSocketImpl.h>
#include <java/net/InetAddress.h>
#include <java/net/DatagramPacket.h>
#include <java/lang/InternalError.h>
#include <java/lang/Object.h>
#include <java/lang/Boolean.h>
#include <java/lang/Integer.h>

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
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  jbyte *bytes = NULL;
  // FIXME: Use getaddrinfo() to get actual protocol instead of assuming ipv4.
  int len = 4;	// Initialize for INADDR_ANY in case host is NULL.

  if (host != NULL)
    {
      jbyteArray haddress = host->address;
      bytes = elements (haddress);
      len = haddress->length;
    }

  if (len == 4)
    {
      u.address.sin_family = AF_INET;
      if (host != NULL)
	memcpy (&u.address.sin_addr, bytes, len);
      else
	u.address.sin_addr.s_addr = htonl (INADDR_ANY);
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
      socklen_t addrlen = sizeof(u);
      if (lport != 0)
        localPort = lport;
      else if (::getsockname (fnum, (sockaddr*) &u, &addrlen) == 0)
        localPort = ntohs (u.address.sin_port);
      else
        goto error;
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
  ssize_t retlen = 0;

  // Do timeouts via select since SO_RCVTIMEO is not always available.
  if (timeout > 0)
    {
      fd_set rset;
      struct timeval tv;
      FD_ZERO(&rset);
      FD_SET(fnum, &rset);
      tv.tv_sec = timeout / 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      int retval;
      if ((retval = select (fnum + 1, &rset, NULL, NULL, &tv)) < 0)
	goto error;
      else if (retval == 0)
	JvThrow (new java::io::InterruptedIOException ());
    }

  retlen =
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
  this->ttl = ttl;
  // throws IOException;
  // FIXME: TODO - PlainDatagramSocketImpl::setTimeToLive
}

jint
java::net::PlainDatagramSocketImpl::getTimeToLive ()
{
  // throws IOException;
  // FIXME: TODO - PlainDatagramSocketImpl::getTimeToLive
  return ttl;
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
  int val;
  socklen_t val_len = sizeof (val);

  if ( _Jv_IsInstanceOf(value,
    java::lang::Class::forName(JvNewStringUTF("java.lang.Boolean"))))
    {
      java::lang::Boolean *boolobj = 
        static_cast<java::lang::Boolean *> (value);
      val = boolobj->booleanValue() ? 1 : 0;
    }
  else if ( _Jv_IsInstanceOf(value,
      java::lang::Class::forName(JvNewStringUTF("java.lang.Integer"))))
    {
      java::lang::Integer *intobj = 
        static_cast<java::lang::Integer *> (value);          
      val = (int) intobj->intValue();
    }
  // Else assume value to be an InetAddress for use with IP_MULTICAST_IF.

  switch (optID) 
    {
      case _Jv_TCP_NODELAY_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("TCP_NODELAY not valid for UDP")));      
        return;
      case _Jv_SO_LINGER_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("SO_LINGER not valid for UDP")));      
        return;
      case _Jv_SO_SNDBUF_ :
      case _Jv_SO_RCVBUF_ :
#if defined(SO_SNDBUF) && defined(SO_RCVBUF)
        int opt;
        optID == _Jv_SO_SNDBUF_ ? opt = SO_SNDBUF : opt = SO_RCVBUF;
        if (::setsockopt (fnum, SOL_SOCKET, opt, (char *) &val, val_len) != 0)
	  goto error;    
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported")));
#endif 
        return;
      case _Jv_SO_REUSEADDR_ :
#if defined(SO_REUSEADDR)
	if (::setsockopt (fnum, SOL_SOCKET, SO_REUSEADDR, (char *) &val,
	    val_len) != 0)
	  goto error;
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported")));
#endif 
	return;
      case _Jv_SO_BINDADDR_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("SO_BINDADDR: read only option")));
        return;
      case _Jv_IP_MULTICAST_IF_ :
	// FIXME: TODO - Implement IP_MULTICAST_IF.
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("IP_MULTICAST_IF: option not implemented")));
        return;
      case _Jv_SO_TIMEOUT_ :
	timeout = val;
        return;
      default :
        errno = ENOPROTOOPT;
    }

 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.setOption: %.*s", 80, strerr);
  JvThrow (new java::net::SocketException (JvNewStringUTF (msg)));
}

java::lang::Object *
java::net::PlainDatagramSocketImpl::getOption (jint optID)
{
  int val;
  socklen_t val_len = sizeof(val);
  union SockAddr u;
  socklen_t addrlen = sizeof(u);

  switch (optID)
    {
      case _Jv_TCP_NODELAY_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("TCP_NODELAY not valid for UDP")));      
        break;

      case _Jv_SO_LINGER_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("SO_LINGER not valid for UDP")));      
        break;    
      case _Jv_SO_RCVBUF_ :
      case _Jv_SO_SNDBUF_ :
#if defined(SO_SNDBUF) && defined(SO_RCVBUF)
        int opt;
        optID == _Jv_SO_SNDBUF_ ? opt = SO_SNDBUF : opt = SO_RCVBUF;
        if (::getsockopt (fnum, SOL_SOCKET, opt, (char *) &val, &val_len) != 0)
	  goto error;    
        else
	  return new java::lang::Integer (val);
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported")));
#endif    
	break;
      case _Jv_SO_BINDADDR_:
	// cache the local address
	if (localAddress == NULL)
	  {	
	    jbyteArray laddr;
	    if (::getsockname (fnum, (sockaddr*) &u, &addrlen) != 0)
	      goto error;
	    if (u.address.sin_family == AF_INET)
	      {
		laddr = JvNewByteArray (4);
		memcpy (elements (laddr), &u.address.sin_addr, 4);
	      }
#ifdef HAVE_INET6
            else if (u.address.sin_family == AF_INET6)
	      {
		laddr = JvNewByteArray (16);
		memcpy (elements (laddr), &u.address6.sin6_addr, 16);
	      }
#endif
	    else
	      goto error;
	    localAddress = new java::net::InetAddress (laddr, NULL);
	  }
	return localAddress;  
	break;
      case _Jv_SO_REUSEADDR_ :
#if defined(SO_REUSEADDR)
	if (::getsockopt (fnum, SOL_SOCKET, SO_REUSEADDR, (char *) &val,
	    &val_len) != 0)
	  goto error;
	return new java::lang::Boolean (val != 0);
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported")));
#endif 
	break;
      case _Jv_IP_MULTICAST_IF_ :
	// FIXME: TODO - Implement IP_MULTICAST_IF.
	JvThrow (new java::lang::InternalError (
	  JvNewStringUTF ("IP_MULTICAST_IF: option not implemented")));
	break;
      case _Jv_SO_TIMEOUT_ :
	return new java::lang::Integer (timeout);
	break;
      default :
	errno = ENOPROTOOPT;
    }

 error:
  char msg[100];
  char* strerr = strerror (errno);
  sprintf (msg, "DatagramSocketImpl.getOption: %.*s", 80, strerr);
  JvThrow (new java::net::SocketException (JvNewStringUTF (msg)));
}
