/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <platform.h>

#ifdef WIN32
#include <errno.h>
#include <string.h>
#ifndef ENOPROTOOPT
#define ENOPROTOOPT 109
#endif

static inline int
close(int s)
{
  return closesocket(s);
}

#else /* WIN32 */
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#include <errno.h>
#include <string.h>
#endif /* WIN32 */

#if HAVE_BSTRING_H
// Needed for bzero, implicitly used by FD_ZERO on IRIX 5.2 
#include <bstring.h>
#endif

#ifndef DISABLE_JAVA_NET
// Avoid macro definitions of bind from system headers, e.g. on
// Solaris 7 with _XOPEN_SOURCE.  FIXME
static inline int
_Jv_bind (int fd, struct sockaddr *addr, int addrlen)
{
  return ::bind (fd, addr, addrlen);
}
#endif /* DISABLE_JAVA_NET */

#ifdef bind
#undef bind
#endif

#include <gcj/cni.h>
#include <java/io/IOException.h>
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

// FIXME: remove these
#define BooleanClass java::lang::Boolean::class$
#define IntegerClass java::lang::Integer::class$

#ifdef DISABLE_JAVA_NET

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

jint
java::net::PlainDatagramSocketImpl::peek (java::net::InetAddress *)
{
  throw new java::io::IOException (
    JvNewStringLatin1 ("DatagramSocketImpl.peek: unimplemented"));
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

#else /* DISABLE_JAVA_NET */

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
java::net::PlainDatagramSocketImpl::create ()
{
  int sock = ::socket (AF_INET, SOCK_DGRAM, 0);
  if (sock < 0)
    {
      char* strerr = strerror (errno);
      throw new java::net::SocketException (JvNewStringUTF (strerr));
    }

  _Jv_platform_close_on_exec (sock);

  // We use fnum in place of fd here.  From leaving fd null we avoid
  // the double close problem in FileDescriptor.finalize.
  fnum = sock;
}

void
java::net::PlainDatagramSocketImpl::bind (jint lport,
					  java::net::InetAddress *host)
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
    throw new java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (_Jv_bind (fnum, ptr, len) == 0)
    {
      socklen_t addrlen = sizeof(u);
      if (lport != 0)
        localPort = lport;
      else if (::getsockname (fnum, (sockaddr*) &u, &addrlen) == 0)
        localPort = ntohs (u.address.sin_port);
      else
        goto error;
      /* Allow broadcast by default. */
      int broadcast = 1;
      if (::setsockopt (fnum, SOL_SOCKET, SO_BROADCAST, (char *) &broadcast, 
                        sizeof (broadcast)) != 0)
        goto error;
      return;
    }
 error:
  char* strerr = strerror (errno);
  throw new java::net::BindException (JvNewStringUTF (strerr));
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
    throw new java::net::SocketException (JvNewStringUTF ("invalid family"));

  i->addr = raddr;
  return rport;
 error:
  char* strerr = strerror (errno);
  throw new java::io::IOException (JvNewStringUTF (strerr));
}

// Close(shutdown) the socket.
void
java::net::PlainDatagramSocketImpl::close ()
{
  // Avoid races from asynchronous finalization.
  JvSynchronize sync (this);

  // The method isn't declared to throw anything, so we disregard
  // the return value.
  ::close (fnum);
  fnum = -1;
  timeout = 0;
}

void
java::net::PlainDatagramSocketImpl::send (java::net::DatagramPacket *p)
{
  // FIXME: Deal with Multicast and if the socket is connected.
  jint rport = p->getPort();
  union SockAddr u;
  jbyteArray haddress = p->getAddress()->addr;
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
    throw new java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (::sendto (fnum, (char *) dbytes, p->getLength(), 0, ptr, len) >= 0)
    return;

  char* strerr = strerror (errno);
  throw new java::io::IOException (JvNewStringUTF (strerr));
}

void
java::net::PlainDatagramSocketImpl::receive (java::net::DatagramPacket *p)
{
  // FIXME: Deal with Multicast and if the socket is connected.
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  jbyte *dbytes = elements (p->getData());
  ssize_t retlen = 0;

// FIXME: implement timeout support for Win32
#ifndef WIN32
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
      if ((retval = _Jv_select (fnum + 1, &rset, NULL, NULL, &tv)) < 0)
	goto error;
      else if (retval == 0)
	throw new java::io::InterruptedIOException ();
    }
#endif /* WIN32 */

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
    throw new java::net::SocketException (JvNewStringUTF ("invalid family"));

  p->setAddress (new InetAddress (raddr, NULL));
  p->setPort (rport);
  p->setLength ((jint) retlen);
  return;
 error:
  char* strerr = strerror (errno);
  throw new java::io::IOException (JvNewStringUTF (strerr));
}

void
java::net::PlainDatagramSocketImpl::setTimeToLive (jint ttl)
{
  // Assumes IPPROTO_IP rather than IPPROTO_IPV6 since socket created is IPv4.
  char val = (char) ttl;
  socklen_t val_len = sizeof(val);
  if (::setsockopt (fnum, IPPROTO_IP, IP_MULTICAST_TTL, &val, val_len) == 0)
    return;

  char* strerr = strerror (errno);
  throw new java::io::IOException (JvNewStringUTF (strerr));
}

jint
java::net::PlainDatagramSocketImpl::getTimeToLive ()
{
  // Assumes IPPROTO_IP rather than IPPROTO_IPV6 since socket created is IPv4.
  char val;
  socklen_t val_len = sizeof(val);
  if (::getsockopt (fnum, IPPROTO_IP, IP_MULTICAST_TTL, &val, &val_len) == 0)
    return ((int) val) & 0xFF;

  char* strerr = strerror (errno);
  throw new java::io::IOException (JvNewStringUTF (strerr));
}

void
java::net::PlainDatagramSocketImpl::mcastGrp (java::net::InetAddress *inetaddr,
					      jboolean join)
{
  union McastReq u;
  jbyteArray haddress = inetaddr->addr;
  jbyte *bytes = elements (haddress);
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
    throw new java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (::setsockopt (fnum, level, opname, ptr, len) == 0)
    return;

  char* strerr = strerror (errno);
  throw new java::io::IOException (JvNewStringUTF (strerr));
}

void
java::net::PlainDatagramSocketImpl::setOption (jint optID,
					       java::lang::Object *value)
{
  int val;
  socklen_t val_len = sizeof (val);

  if (_Jv_IsInstanceOf (value, &BooleanClass))
    {
      java::lang::Boolean *boolobj = 
        static_cast<java::lang::Boolean *> (value);
      val = boolobj->booleanValue() ? 1 : 0;
    }
  else if (_Jv_IsInstanceOf (value, &IntegerClass))
    {
      java::lang::Integer *intobj = 
        static_cast<java::lang::Integer *> (value);          
      val = (int) intobj->intValue();
    }
  // Else assume value to be an InetAddress for use with IP_MULTICAST_IF.

  switch (optID) 
    {
      case _Jv_TCP_NODELAY_ :
        throw new java::net::SocketException (
          JvNewStringUTF ("TCP_NODELAY not valid for UDP"));
        return;
      case _Jv_SO_LINGER_ :
        throw new java::net::SocketException (
          JvNewStringUTF ("SO_LINGER not valid for UDP"));
        return;
      case _Jv_SO_SNDBUF_ :
      case _Jv_SO_RCVBUF_ :
#if defined(SO_SNDBUF) && defined(SO_RCVBUF)
        int opt;
        optID == _Jv_SO_SNDBUF_ ? opt = SO_SNDBUF : opt = SO_RCVBUF;
        if (::setsockopt (fnum, SOL_SOCKET, opt, (char *) &val, val_len) != 0)
	  goto error;    
#else
        throw new java::lang::InternalError (
          JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported"));
#endif 
        return;
      case _Jv_SO_REUSEADDR_ :
#if defined(SO_REUSEADDR)
	if (::setsockopt (fnum, SOL_SOCKET, SO_REUSEADDR, (char *) &val,
	    val_len) != 0)
	  goto error;
#else
        throw new java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported"));
#endif 
	return;
      case _Jv_SO_BINDADDR_ :
        throw new java::net::SocketException (
          JvNewStringUTF ("SO_BINDADDR: read only option"));
        return;
      case _Jv_IP_MULTICAST_IF_ :
	union InAddr u;
        jbyteArray haddress;
	jbyte *bytes;
	int len;
	int level, opname;
	const char *ptr;

	haddress = ((java::net::InetAddress *) value)->addr;
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
	    new java::net::SocketException (JvNewStringUTF ("invalid length"));

	if (::setsockopt (fnum, level, opname, ptr, len) != 0)
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
  throw new java::net::SocketException (JvNewStringUTF (strerr));
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
        throw new java::net::SocketException (
          JvNewStringUTF ("TCP_NODELAY not valid for UDP"));
        break;

      case _Jv_SO_LINGER_ :
        throw new java::net::SocketException (
          JvNewStringUTF ("SO_LINGER not valid for UDP"));
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
        throw new java::lang::InternalError (
          JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported"));
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
	      throw new java::net::SocketException (JvNewStringUTF ("invalid family"));
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
        throw new java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported"));
#endif 
	break;
      case _Jv_IP_MULTICAST_IF_ :
#ifdef HAVE_INET_NTOA
	struct in_addr inaddr;
  	socklen_t inaddr_len;
	char *bytes;

  	inaddr_len = sizeof(inaddr);
	if (::getsockopt (fnum, IPPROTO_IP, IP_MULTICAST_IF, (char *) &inaddr,
	    &inaddr_len) != 0)
	  goto error;

	bytes = inet_ntoa (inaddr);

	return java::net::InetAddress::getByName (JvNewStringLatin1 (bytes));
#else
	throw new java::net::SocketException (
	  JvNewStringUTF ("IP_MULTICAST_IF: not available - no inet_ntoa()"));
#endif
	break;
      case _Jv_SO_TIMEOUT_ :
	return new java::lang::Integer (timeout);
	break;
      default :
	errno = ENOPROTOOPT;
    }

 error:
  char* strerr = strerror (errno);
  throw new java::net::SocketException (JvNewStringUTF (strerr));
}

#endif /* DISABLE_JAVA_NET */
