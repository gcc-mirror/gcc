/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#include <errno.h>
#include <string.h>

#if HAVE_BSTRING_H
// Needed for bzero, implicitly used by FD_ZERO on IRIX 5.2 
#include <bstring.h>
#endif

#include <gcj/cni.h>
#include <gnu/java/net/PlainDatagramSocketImpl.h>
#include <java/io/IOException.h>
#include <java/io/InterruptedIOException.h>
#include <java/net/BindException.h>
#include <java/net/SocketException.h>
#include <java/net/SocketTimeoutException.h>
#include <java/net/InetAddress.h>
#include <java/net/NetworkInterface.h>
#include <java/net/DatagramPacket.h>
#include <java/net/PortUnreachableException.h>
#include <java/lang/InternalError.h>
#include <java/lang/Object.h>
#include <java/lang/Boolean.h>
#include <java/lang/Integer.h>

union SockAddr
{
  struct sockaddr_in address;
#ifdef HAVE_INET6
  struct sockaddr_in6 address6;
#endif
};

union McastReq
{
#if HAVE_STRUCT_IP_MREQ
  struct ip_mreq mreq;
#endif
#if HAVE_STRUCT_IPV6_MREQ
  struct ipv6_mreq mreq6;
#endif
};

union InAddr
{
  struct in_addr addr;
#ifdef HAVE_INET6
  struct in6_addr addr6;
#endif
};


// FIXME: routines here and/or in natPlainSocketImpl.cc could throw
// NoRouteToHostException; also consider UnknownHostException, ConnectException.

void
gnu::java::net::PlainDatagramSocketImpl::create ()
{
  int sock = _Jv_socket (AF_INET, SOCK_DGRAM, 0);

  if (sock < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::net::SocketException (JvNewStringUTF (strerr));
    }

  _Jv_platform_close_on_exec (sock);

  // We use native_fd in place of fd here.  From leaving fd null we avoid
  // the double close problem in FileDescriptor.finalize.
  native_fd = sock;
}

void
gnu::java::net::PlainDatagramSocketImpl::bind (jint lport,
                                               ::java::net::InetAddress *host)
{
  union SockAddr u;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  // FIXME: Use getaddrinfo() to get actual protocol instead of assuming ipv4.
  jbyteArray haddress = host->addr;
  jbyte *bytes = elements (haddress);
  int len = haddress->length;

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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (_Jv_bind (native_fd, ptr, len) == 0)
    {
      socklen_t addrlen = sizeof(u);

      if (lport != 0)
        localPort = lport;
      else if (::getsockname (native_fd, (sockaddr*) &u, &addrlen) == 0)
        localPort = ntohs (u.address.sin_port);
      else
        goto error;

      /* Allow broadcast by default. */
      int broadcast = 1;
      if (::setsockopt (native_fd, SOL_SOCKET, SO_BROADCAST, (char *) &broadcast, 
                        sizeof (broadcast)) != 0)
        goto error;

      return;
    }

 error:
  char* strerr = strerror (errno);
  throw new ::java::net::BindException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainDatagramSocketImpl::connect (::java::net::InetAddress *, jint)
{ 
  throw new ::java::lang::InternalError (JvNewStringLatin1 (
	    "PlainDatagramSocketImpl::connect: not implemented yet"));
}

void
gnu::java::net::PlainDatagramSocketImpl::disconnect ()
{
  throw new ::java::lang::InternalError (JvNewStringLatin1 (
	    "PlainDatagramSocketImpl::disconnect: not implemented yet"));
}

jint
gnu::java::net::PlainDatagramSocketImpl::peek (::java::net::InetAddress *i)
{
  // FIXME: Deal with Multicast and if the socket is connected.
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  ssize_t retlen =
    ::recvfrom (native_fd, (char *) NULL, 0, MSG_PEEK, (sockaddr*) &u,
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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid family"));

  i->addr = raddr;
  return rport;
 error:
  char* strerr = strerror (errno);

  if (errno == ECONNREFUSED)
    throw new ::java::net::PortUnreachableException (JvNewStringUTF (strerr));

  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

jint
gnu::java::net::PlainDatagramSocketImpl::peekData (::java::net::DatagramPacket *p)
{
  // FIXME: Deal with Multicast and if the socket is connected.
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  jbyte *dbytes = elements (p->getData()) + p->getOffset();
  jint maxlen = p->maxlen - p->getOffset();
  ssize_t retlen = 0;

  // Do timeouts via select since SO_RCVTIMEO is not always available.
  if (timeout > 0 && native_fd >= 0 && native_fd < FD_SETSIZE)
    {
      fd_set rset;
      struct timeval tv;
      FD_ZERO(&rset);
      FD_SET(native_fd, &rset);
      tv.tv_sec = timeout / 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      int retval;
      if ((retval = _Jv_select (native_fd + 1, &rset, NULL, NULL, &tv)) < 0)
        goto error;
      else if (retval == 0)
        throw new ::java::net::SocketTimeoutException
          (JvNewStringUTF ("PeekData timed out") );
    }

  retlen =
    ::recvfrom (native_fd, (char *) dbytes, maxlen, MSG_PEEK, (sockaddr*) &u,
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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid family"));

  p->setAddress (new ::java::net::InetAddress (raddr, NULL));
  p->setPort (rport);
  p->length = (int) retlen;
  return rport;

 error:
  char* strerr = strerror (errno);

  if (errno == ECONNREFUSED)
    throw new ::java::net::PortUnreachableException (JvNewStringUTF (strerr));

  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

// Close(shutdown) the socket.
void
gnu::java::net::PlainDatagramSocketImpl::close ()
{
  // Avoid races from asynchronous finalization.
  JvSynchronize sync (this);

  // The method isn't declared to throw anything, so we disregard
  // the return value.
  _Jv_close (native_fd);
  native_fd = -1;
  timeout = 0;
}

void
gnu::java::net::PlainDatagramSocketImpl::send (::java::net::DatagramPacket *p)
{
  JvSynchronize lock (SEND_LOCK);
  
  // FIXME: Deal with Multicast and if the socket is connected.
  jint rport = p->getPort();
  union SockAddr u;
  jbyteArray haddress = p->getAddress()->addr;
  jbyte *bytes = elements (haddress);
  int len = haddress->length;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  jbyte *dbytes = elements (p->getData()) + p->getOffset();
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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (::sendto (native_fd, (char *) dbytes, p->getLength(), 0, ptr, len) >= 0)
    return;

  char* strerr = strerror (errno);

  if (errno == ECONNREFUSED)
    throw new ::java::net::PortUnreachableException (JvNewStringUTF (strerr));

  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainDatagramSocketImpl::receive (::java::net::DatagramPacket *p)
{
  JvSynchronize lock (RECEIVE_LOCK);

  // FIXME: Deal with Multicast and if the socket is connected.
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  jbyte *dbytes = elements (p->getData()) + p->getOffset();
  jint maxlen = p->maxlen - p->getOffset();
  ssize_t retlen = 0;

  // Do timeouts via select since SO_RCVTIMEO is not always available.
  if (timeout > 0 && native_fd >= 0 && native_fd < FD_SETSIZE)
    {
      fd_set rset;
      struct timeval tv;
      FD_ZERO(&rset);
      FD_SET(native_fd, &rset);
      tv.tv_sec = timeout / 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      int retval;
      if ((retval = _Jv_select (native_fd + 1, &rset, NULL, NULL, &tv)) < 0)
        goto error;
      else if (retval == 0)
        throw new ::java::net::SocketTimeoutException
          (JvNewStringUTF ("Receive timed out") );
    }

  retlen =
    ::recvfrom (native_fd, (char *) dbytes, maxlen, 0, (sockaddr*) &u,
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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid family"));

  p->setAddress (new ::java::net::InetAddress (raddr, NULL));
  p->setPort (rport);
  p->length = (jint) retlen;
  return;

 error:
  char* strerr = strerror (errno);

  if (errno == ECONNREFUSED)
    throw new ::java::net::PortUnreachableException (JvNewStringUTF (strerr));

  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainDatagramSocketImpl::setTimeToLive (jint ttl)
{
  // Assumes IPPROTO_IP rather than IPPROTO_IPV6 since socket created is IPv4.
  char val = (char) ttl;
  socklen_t val_len = sizeof(val);

  if (::setsockopt (native_fd, IPPROTO_IP, IP_MULTICAST_TTL, &val, val_len) == 0)
    return;

  char* strerr = strerror (errno);
  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

jint
gnu::java::net::PlainDatagramSocketImpl::getTimeToLive ()
{
  // Assumes IPPROTO_IP rather than IPPROTO_IPV6 since socket created is IPv4.
  char val;
  socklen_t val_len = sizeof(val);

  if (::getsockopt (native_fd, IPPROTO_IP, IP_MULTICAST_TTL, &val, &val_len) == 0)
    return ((int) val) & 0xFF;

  char* strerr = strerror (errno);
  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainDatagramSocketImpl::mcastGrp (::java::net::InetAddress *inetaddr,
                                                   ::java::net::NetworkInterface *,
                                                   jboolean join)
{
  // FIXME: implement use of NetworkInterface

  jbyteArray haddress = inetaddr->addr;
#if HAVE_STRUCT_IP_MREQ || HAVE_STRUCT_IPV6_MREQ
  union McastReq u;
  jbyte *bytes = elements (haddress);
#endif

  int len = haddress->length;
  int level, opname;
  const char *ptr;
  if (0)
    ;
#if HAVE_STRUCT_IP_MREQ
  else if (len == 4)
    {
      level = IPPROTO_IP;
      opname = join ? IP_ADD_MEMBERSHIP : IP_DROP_MEMBERSHIP;
      memcpy (&u.mreq.imr_multiaddr, bytes, len);
      // FIXME:  If a non-default interface is set, use it; see Stevens p. 501.
      // Maybe not, see note in last paragraph at bottom of Stevens p. 497.
      u.mreq.imr_interface.s_addr = htonl (INADDR_ANY); 
      len = sizeof (struct ip_mreq);
      ptr = (const char *) &u.mreq;
    }
#endif
#if HAVE_STRUCT_IPV6_MREQ
  else if (len == 16)
    {
      level = IPPROTO_IPV6;

      /* Prefer new RFC 2553 names.  */
#ifndef IPV6_JOIN_GROUP
#define IPV6_JOIN_GROUP IPV6_ADD_MEMBERSHIP
#endif
#ifndef IPV6_LEAVE_GROUP
#define IPV6_LEAVE_GROUP IPV6_DROP_MEMBERSHIP
#endif

      opname = join ? IPV6_JOIN_GROUP : IPV6_LEAVE_GROUP;
      memcpy (&u.mreq6.ipv6mr_multiaddr, bytes, len);
      // FIXME:  If a non-default interface is set, use it; see Stevens p. 501.
      // Maybe not, see note in last paragraph at bottom of Stevens p. 497.
      u.mreq6.ipv6mr_interface = 0;
      len = sizeof (struct ipv6_mreq);
      ptr = (const char *) &u.mreq6;
    }
#endif
  else
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (::setsockopt (native_fd, level, opname, ptr, len) == 0)
    return;

  char* strerr = strerror (errno);
  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainDatagramSocketImpl::setOption (jint optID,
                                                    ::java::lang::Object *value)
{
  int val;
  socklen_t val_len = sizeof (val);

  if (native_fd < 0)
    throw new ::java::net::SocketException (JvNewStringUTF ("Socket closed"));

  if (_Jv_IsInstanceOf (value, &::java::lang::Boolean::class$))
    {
      ::java::lang::Boolean *boolobj = 
        static_cast< ::java::lang::Boolean *> (value);
      val = boolobj->booleanValue() ? 1 : 0;
    }
  else if (_Jv_IsInstanceOf (value, &::java::lang::Integer::class$))
    {
      ::java::lang::Integer *intobj = 
        static_cast< ::java::lang::Integer *> (value);          
      val = (int) intobj->intValue();
    }
  // Else assume value to be an InetAddress for use with IP_MULTICAST_IF.

  switch (optID) 
    {
      case _Jv_TCP_NODELAY_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("TCP_NODELAY not valid for UDP"));
        return;
      case _Jv_SO_LINGER_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_LINGER not valid for UDP"));
        return;
      case _Jv_SO_KEEPALIVE_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_KEEPALIVE not valid for UDP"));
        return;

      case _Jv_SO_BROADCAST_ :
        if (::setsockopt (native_fd, SOL_SOCKET, SO_BROADCAST, (char *) &val,
                          val_len) != 0)
          goto error;
	return;
	
      case _Jv_SO_OOBINLINE_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_OOBINLINE: not valid for UDP"));
        return;
	
      case _Jv_SO_SNDBUF_ :
      case _Jv_SO_RCVBUF_ :
#if defined(SO_SNDBUF) && defined(SO_RCVBUF)
        int opt;
        optID == _Jv_SO_SNDBUF_ ? opt = SO_SNDBUF : opt = SO_RCVBUF;
        if (::setsockopt (native_fd, SOL_SOCKET, opt, (char *) &val, val_len) != 0)
	  goto error;    
#else
        throw new ::java::lang::InternalError (
          JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported"));
#endif 
        return;
      case _Jv_SO_REUSEADDR_ :
#if defined(SO_REUSEADDR)
	if (::setsockopt (native_fd, SOL_SOCKET, SO_REUSEADDR, (char *) &val,
	    val_len) != 0)
	  goto error;
#else
        throw new ::java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported"));
#endif 
	return;
      case _Jv_SO_BINDADDR_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_BINDADDR: read only option"));
        return;
      case _Jv_IP_MULTICAST_IF_ :
	union InAddr u;
        jbyteArray haddress;
	jbyte *bytes;
	int len;
	int level, opname;
	const char *ptr;

	haddress = ((::java::net::InetAddress *) value)->addr;
	bytes = elements (haddress);
	len = haddress->length;
	if (len == 4)
	  {
	    level = IPPROTO_IP;
	    opname = IP_MULTICAST_IF;
	    memcpy (&u.addr, bytes, len);
	    len = sizeof (struct in_addr);
	    ptr = (const char *) &u.addr;
	  }
// Tru64 UNIX V5.0 has struct sockaddr_in6, but no IPV6_MULTICAST_IF
#if defined (HAVE_INET6) && defined (IPV6_MULTICAST_IF)
	else if (len == 16)
	  {
	    level = IPPROTO_IPV6;
	    opname = IPV6_MULTICAST_IF;
	    memcpy (&u.addr6, bytes, len);
	    len = sizeof (struct in6_addr);
	    ptr = (const char *) &u.addr6;
	  }
#endif
	else
	  throw
	    new ::java::net::SocketException (JvNewStringUTF ("invalid length"));

	if (::setsockopt (native_fd, level, opname, ptr, len) != 0)
	  goto error;
        return;
	
      case _Jv_IP_MULTICAST_IF2_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_IF2: not yet implemented"));
        return;
	
      case _Jv_IP_MULTICAST_LOOP_ :
	haddress = ((::java::net::InetAddress *) value)->addr;
	len = haddress->length;
	if (len == 4)
	  {
	    level = IPPROTO_IP;
	    opname = IP_MULTICAST_LOOP;
	  }
#if defined (HAVE_INET6) && defined (IPV6_MULTICAST_LOOP)
	else if (len == 16)
	  {
	    level = IPPROTO_IPV6;
	    opname = IPV6_MULTICAST_LOOP;
	  }
#endif
	else
	  throw
	    new ::java::net::SocketException (JvNewStringUTF ("invalid address length"));
	if (::setsockopt (native_fd, level, opname, (char *) &val,
			  val_len) != 0)
	  goto error;
	return;
	
      case _Jv_IP_TOS_ :
        if (::setsockopt (native_fd, SOL_SOCKET, IP_TOS, (char *) &val,
	   val_len) != 0)
	  goto error;    
	return;
	
      case _Jv_SO_TIMEOUT_ :
	timeout = val;
        return;
      default :
        errno = ENOPROTOOPT;
    }

 error:
  char* strerr = strerror (errno);
  throw new ::java::net::SocketException (JvNewStringUTF (strerr));
}

::java::lang::Object *
gnu::java::net::PlainDatagramSocketImpl::getOption (jint optID)
{
  int val;
  socklen_t val_len = sizeof(val);
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  int level, opname;

  switch (optID)
    {
      case _Jv_TCP_NODELAY_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("TCP_NODELAY not valid for UDP"));
        break;
      case _Jv_SO_LINGER_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_LINGER not valid for UDP"));
        break;    
      case _Jv_SO_KEEPALIVE_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_KEEPALIVE not valid for UDP"));
        break;
	
      case _Jv_SO_BROADCAST_ :
	if (::getsockopt (native_fd, SOL_SOCKET, SO_BROADCAST, (char *) &val,
	    &val_len) != 0)
	  goto error;
	return new ::java::lang::Boolean (val != 0);
	
      case _Jv_SO_OOBINLINE_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_OOBINLINE not valid for UDP"));
        break;
      
      case _Jv_SO_RCVBUF_ :
      case _Jv_SO_SNDBUF_ :
#if defined(SO_SNDBUF) && defined(SO_RCVBUF)
        int opt;
        optID == _Jv_SO_SNDBUF_ ? opt = SO_SNDBUF : opt = SO_RCVBUF;
        if (::getsockopt (native_fd, SOL_SOCKET, opt, (char *) &val, &val_len) != 0)
	  goto error;    
        else
	  return new ::java::lang::Integer (val);
#else
        throw new ::java::lang::InternalError (
          JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported"));
#endif    
	break;
      case _Jv_SO_BINDADDR_:
	// cache the local address
	if (localAddress == NULL)
	  {	
	    jbyteArray laddr;
	    if (::getsockname (native_fd, (sockaddr*) &u, &addrlen) != 0)
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
	      throw new ::java::net::SocketException (
			      JvNewStringUTF ("invalid family"));
	    localAddress = new ::java::net::InetAddress (laddr, NULL);
	  }
	return localAddress;  
	break;
      case _Jv_SO_REUSEADDR_ :
#if defined(SO_REUSEADDR)
	if (::getsockopt (native_fd, SOL_SOCKET, SO_REUSEADDR, (char *) &val,
	    &val_len) != 0)
	  goto error;
	return new ::java::lang::Boolean (val != 0);
#else
        throw new ::java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported"));
#endif 
	break;
      case _Jv_IP_MULTICAST_IF_ :
#ifdef HAVE_INET_NTOA
	struct in_addr inaddr;
  	socklen_t inaddr_len;
	char *bytes;

  	inaddr_len = sizeof(inaddr);
	if (::getsockopt (native_fd, IPPROTO_IP, IP_MULTICAST_IF, (char *) &inaddr,
	    &inaddr_len) != 0)
	  goto error;

	bytes = inet_ntoa (inaddr);

	return ::java::net::InetAddress::getByName (JvNewStringLatin1 (bytes));
#else
	throw new ::java::net::SocketException (
	  JvNewStringUTF ("IP_MULTICAST_IF: not available - no inet_ntoa()"));
#endif
	break;
      case _Jv_SO_TIMEOUT_ :
	return new ::java::lang::Integer (timeout);
	break;
	
      case _Jv_IP_MULTICAST_IF2_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_IF2: not yet implemented"));
        break;
	
      case _Jv_IP_MULTICAST_LOOP_ :
	// cache the local address
	if (localAddress == NULL)
	  {	
	    jbyteArray laddr;
	    if (::getsockname (native_fd, (sockaddr*) &u, &addrlen) != 0)
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
	      throw new ::java::net::SocketException (
			      JvNewStringUTF ("invalid family"));
	    localAddress = new ::java::net::InetAddress (laddr, NULL);
	    
	  }
	if (localAddress->addr->length == 4) 
	  {
	    level = IPPROTO_IP;
	    opname = IP_MULTICAST_LOOP;
	  }
#if defined (HAVE_INET6) && defined (IPV6_MULTICAST_LOOP)
	else if (localAddress->addr->length == 16)
	  {
	    level = IPPROTO_IPV6;
	    opname = IPV6_MULTICAST_LOOP;
	  }
#endif
	else
	  throw
	    new ::java::net::SocketException (JvNewStringUTF ("invalid address length"));
	if (::getsockopt (native_fd, level, opname, (char *) &val,
			  &val_len) != 0)
	  goto error;
	return new ::java::lang::Boolean (val != 0);
	
      case _Jv_IP_TOS_ :
        if (::getsockopt (native_fd, SOL_SOCKET, IP_TOS, (char *) &val,
           &val_len) != 0)
          goto error;
        return new ::java::lang::Integer (val);
	
      default :
	errno = ENOPROTOOPT;
    }

 error:
  char* strerr = strerror (errno);
  throw new ::java::net::SocketException (JvNewStringUTF (strerr));
}
