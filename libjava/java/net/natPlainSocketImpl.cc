/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>


#ifndef DISABLE_JAVA_NET
#ifdef USE_WINSOCK
#include <windows.h>
#include <winsock.h>
#include <errno.h>
#include <string.h>
#ifndef ENOPROTOOPT
#define ENOPROTOOPT 109
#endif
#else /* USE_WINSOCK */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <errno.h>
#include <string.h>
#endif /* USE_WINSOCK */
#endif /* DISABLE_JAVA_NET */

#if HAVE_BSTRING_H
// Needed for bzero, implicitly used by FD_ZERO on IRIX 5.2 
#include <bstring.h>
#endif

#include <gcj/cni.h>
#include <gcj/javaprims.h>
#include <java/io/IOException.h>
#include <java/io/FileDescriptor.h>
#include <java/io/InterruptedIOException.h>
#include <java/net/BindException.h>
#include <java/net/ConnectException.h>
#include <java/net/PlainSocketImpl.h>
#include <java/net/InetAddress.h>
#include <java/net/SocketException.h>
#include <java/lang/InternalError.h>
#include <java/lang/Object.h>
#include <java/lang/Boolean.h>
#include <java/lang/Class.h>
#include <java/lang/Integer.h>

#define BooleanClass _CL_Q34java4lang7Boolean
extern java::lang::Class BooleanClass;

#ifdef DISABLE_JAVA_NET

void
java::net::PlainSocketImpl::create (jboolean)
{
  JvThrow (new java::io::IOException (JvNewStringLatin1 ("SocketImpl.create: unimplemented")));
}

void
java::net::PlainSocketImpl::bind (java::net::InetAddress *, jint)
{
  JvThrow (new BindException (JvNewStringLatin1 ("SocketImpl.bind: unimplemented")));
}

void
java::net::PlainSocketImpl::connect (java::net::InetAddress *, jint)
{
  JvThrow (new ConnectException (JvNewStringLatin1 ("SocketImpl.connect: unimplemented")));
}

void
java::net::PlainSocketImpl::listen (jint)
{
  JvThrow (new java::io::IOException (JvNewStringLatin1 ("SocketImpl.listen: unimplemented")));
}

void
java::net::PlainSocketImpl::accept (java::net::PlainSocketImpl *)
{
  JvThrow (new java::io::IOException (JvNewStringLatin1 ("SocketImpl.accept: unimplemented")));
}

void
java::net::PlainSocketImpl::setOption (jint, java::lang::Object *)
{
  JvThrow (new SocketException (JvNewStringLatin1 ("SocketImpl.setOption: unimplemented")));
}

java::lang::Object *
java::net::PlainSocketImpl::getOption (jint)
{
  JvThrow (new SocketException (JvNewStringLatin1 ("SocketImpl.getOption: unimplemented")));
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

void
java::net::PlainSocketImpl::create (jboolean stream)
{
  int sock = ::socket (AF_INET, stream ? SOCK_STREAM : SOCK_DGRAM, 0);
  if (sock < 0)
    {
      char* strerr = strerror (errno);
      JvThrow (new java::io::IOException (JvNewStringUTF (strerr)));
    }
  fnum = sock;
  fd = new java::io::FileDescriptor (sock);
}

void
java::net::PlainSocketImpl::bind (java::net::InetAddress *host, jint lport)
{
  union SockAddr u;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  jbyteArray haddress = host->address;
  jbyte *bytes = elements (haddress);
  int len = haddress->length;
  int i = 1;

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

  // Enable SO_REUSEADDR, so that servers can reuse ports left in TIME_WAIT.
  ::setsockopt(fnum, SOL_SOCKET, SO_REUSEADDR, (char *) &i, sizeof(i));
  
  if (::bind (fnum, ptr, len) == 0)
    {
      address = host;
      socklen_t addrlen = sizeof(u);
      if (lport != 0)
        localport = lport;
      else if (::getsockname (fnum, (sockaddr*) &u, &addrlen) == 0)
        localport = ntohs (u.address.sin_port);
      else
        goto error;
      return;
    }
 error:
  char* strerr = strerror (errno);
  JvThrow (new java::net::BindException (JvNewStringUTF (strerr)));
}

void
java::net::PlainSocketImpl::connect (java::net::InetAddress *host, jint rport)
{
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
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
  if (::connect (fnum, ptr, len) != 0)
    goto error;
  address = host;
  port = rport;
  // A bind may not have been done on this socket; if so, set localport now.
  if (localport == 0)
    if (::getsockname (fnum, (sockaddr*) &u, &addrlen) == 0)
      localport = ntohs (u.address.sin_port);
    else
      goto error;
  return;  
 error:
  char* strerr = strerror (errno);
  JvThrow (new java::net::ConnectException (JvNewStringUTF (strerr)));
}

void
java::net::PlainSocketImpl::listen (jint backlog)
{
  if (::listen (fnum, backlog) != 0)
    {
      char* strerr = strerror (errno);
      JvThrow (new java::io::IOException (JvNewStringUTF (strerr)));
    }
}

void
java::net::PlainSocketImpl::accept (java::net::PlainSocketImpl *s)
{
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  int new_socket = 0; 

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
	JvThrow (new java::io::InterruptedIOException (
	         JvNewStringUTF("Accept timed out")));
    }

  new_socket = ::accept (fnum, (sockaddr*) &u, &addrlen);
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
  else if (u.address.sin_family == AF_INET6)
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
  char* strerr = strerror (errno);
  JvThrow (new java::io::IOException (JvNewStringUTF (strerr)));
}

void
java::net::PlainSocketImpl::setOption (jint optID, java::lang::Object *value)
{
  int val;
  socklen_t val_len = sizeof (val);

  if (_Jv_IsInstanceOf (value, &BooleanClass))
    {
      java::lang::Boolean *boolobj = 
        static_cast<java::lang::Boolean *> (value);
      if (boolobj->booleanValue())
        val = 1; 
      else 
        {
	  if (optID == _Jv_SO_LINGER_)
	    val = -1;
	  else
	    val = 0;
        }
    }
  else  // assume value is an Integer
    {
      java::lang::Integer *intobj = 
        static_cast<java::lang::Integer *> (value);          
      val = (int) intobj->intValue();
    }

  switch (optID) 
    {
      case _Jv_TCP_NODELAY_ :
#ifdef TCP_NODELAY
        if (::setsockopt (fnum, IPPROTO_TCP, TCP_NODELAY, (char *) &val,
	    val_len) != 0)
	  goto error;    
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("TCP_NODELAY not supported")));      
#endif /* TCP_NODELAY */
        return;
      case _Jv_SO_LINGER_ :
#ifdef SO_LINGER
        struct linger l_val;
        l_val.l_onoff = (val != -1);
        l_val.l_linger = val;
        if (::setsockopt (fnum, SOL_SOCKET, SO_LINGER, (char *) &l_val,
	    sizeof(l_val)) != 0)
	  goto error;    
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("SO_LINGER not supported")));      
#endif /* SO_LINGER */
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
      case _Jv_SO_BINDADDR_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("SO_BINDADDR: read only option")));
        return;
      case _Jv_IP_MULTICAST_IF_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_IF: not valid for TCP")));
        return;
      case _Jv_SO_REUSEADDR_ :
        JvThrow (new java::net::SocketException (
          JvNewStringUTF ("SO_REUSEADDR: not valid for TCP")));
        return;
      case _Jv_SO_TIMEOUT_ :
	timeout = val;
        return;
      default :
        errno = ENOPROTOOPT;
    }

 error:
  char* strerr = strerror (errno);
  JvThrow (new java::net::SocketException (JvNewStringUTF (strerr)));
}

java::lang::Object *
java::net::PlainSocketImpl::getOption (jint optID)
{
  int val;
  socklen_t val_len = sizeof(val);
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  struct linger l_val;
  socklen_t l_val_len = sizeof(l_val);

  switch (optID)
    {
#ifdef TCP_NODELAY
      case _Jv_TCP_NODELAY_ :
        if (::getsockopt (fnum, IPPROTO_TCP, TCP_NODELAY, (char *) &val,
	    &val_len) != 0)
          goto error;
        else
	  return new java::lang::Boolean (val != 0);
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("TCP_NODELAY not supported")));      
#endif       
        break;

      case _Jv_SO_LINGER_ :
#ifdef SO_LINGER
        if (::getsockopt (fnum, SOL_SOCKET, SO_LINGER, (char *) &l_val,
	    &l_val_len) != 0)
	  goto error;    
        if (l_val.l_onoff)
          return new java::lang::Integer (l_val.l_linger);
        else
	  return new java::lang::Boolean ((__java_boolean)false);
#else
        JvThrow (new java::lang::InternalError (
          JvNewStringUTF ("SO_LINGER not supported")));      
#endif
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
      case _Jv_IP_MULTICAST_IF_ :
	JvThrow (new java::net::SocketException (
	  JvNewStringUTF ("IP_MULTICAST_IF: not valid for TCP")));
	break;
      case _Jv_SO_REUSEADDR_ :
	JvThrow (new java::net::SocketException (
	  JvNewStringUTF ("SO_REUSEADDR: not valid for TCP")));
	break;
      case _Jv_SO_TIMEOUT_ :
	return new java::lang::Integer (timeout);
	break;
      default :
	errno = ENOPROTOOPT;
    }

 error:
  char* strerr = strerror (errno);
  JvThrow (new java::net::SocketException (JvNewStringUTF (strerr)));
}

#endif /* DISABLE_JAVA_NET */
