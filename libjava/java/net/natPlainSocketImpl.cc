/* Copyright (C) 1998, 1999, 2000, 2002 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#ifndef DISABLE_JAVA_NET
#ifdef WIN32
#include <windows.h>
#include <winsock.h>
#include <errno.h>
#include <string.h>
#undef STRICT
#undef MAX_PRIORITY
#undef MIN_PRIORITY
#undef FIONREAD

// These functions make the Win32 socket API look more POSIXy
static inline int
close(int s)
{
  return closesocket(s);
}

static inline int
write(int s, void *buf, int len)
{
  return send(s, (char*)buf, len, 0);
}

static inline int
read(int s, void *buf, int len)
{
  return recv(s, (char*)buf, len, 0);
}

// these errors cannot occur on Win32
#define ENOTCONN 0
#define ECONNRESET 0
#ifndef ENOPROTOOPT
#define ENOPROTOOPT 109
#endif
#else /* WIN32 */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <errno.h>
#include <string.h>
#endif /* WIN32 */
#endif /* DISABLE_JAVA_NET */

#if HAVE_BSTRING_H
// Needed for bzero, implicitly used by FD_ZERO on IRIX 5.2 
#include <bstring.h>
#endif

#ifndef HAVE_SOCKLEN_T
typedef int socklen_t;
#endif

#ifndef DISABLE_JAVA_NET

// Avoid macro definitions of bind, connect from system headers, e.g. on
// Solaris 7 with _XOPEN_SOURCE.  FIXME
static inline int
_Jv_bind (int fd, struct sockaddr *addr, int addrlen)
{
  return ::bind (fd, addr, addrlen);
}

#ifdef bind
#undef bind
#endif

static inline int
_Jv_connect (int fd, struct sockaddr *addr, int addrlen)
{
  return ::connect (fd, addr, addrlen);
}

#ifdef connect
#undef connect
#endif

// Same problem with accept on Tru64 UNIX with _POSIX_PII_SOCKET
static inline int
_Jv_accept (int fd, struct sockaddr *addr, socklen_t *addrlen)
{
  return ::accept (fd, addr, addrlen);
}

#ifdef accept
#undef accept
#endif

#endif /* DISABLE_JAVA_NET */

#include <gcj/cni.h>
#include <gcj/javaprims.h>
#include <java/io/IOException.h>
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
#include <java/lang/Thread.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/IllegalArgumentException.h>

#ifdef DISABLE_JAVA_NET

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
java::net::PlainSocketImpl::connect (java::net::InetAddress *, jint)
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

#else /* DISABLE_JAVA_NET */

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
      throw new java::io::IOException (JvNewStringUTF (strerr));
    }

  _Jv_platform_close_on_exec (sock);

  // We use fnum in place of fd here.  From leaving fd null we avoid
  // the double close problem in FileDescriptor.finalize.
  fnum = sock;
}

void
java::net::PlainSocketImpl::bind (java::net::InetAddress *host, jint lport)
{
  union SockAddr u;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
  jbyteArray haddress = host->addr;
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
    throw new java::net::SocketException (JvNewStringUTF ("invalid length"));

  // Enable SO_REUSEADDR, so that servers can reuse ports left in TIME_WAIT.
  ::setsockopt(fnum, SOL_SOCKET, SO_REUSEADDR, (char *) &i, sizeof(i));
  
  if (_Jv_bind (fnum, ptr, len) == 0)
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
  throw new java::net::BindException (JvNewStringUTF (strerr));
}

void
java::net::PlainSocketImpl::connect (java::net::InetAddress *host, jint rport)
{
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  jbyteArray haddress = host->addr;
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
    throw new java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (_Jv_connect (fnum, ptr, len) != 0)
    goto error;
  address = host;
  port = rport;
  // A bind may not have been done on this socket; if so, set localport now.
  if (localport == 0)
    {
      if (::getsockname (fnum, (sockaddr*) &u, &addrlen) == 0)
	localport = ntohs (u.address.sin_port);
      else
	goto error;
    }
  return;  
 error:
  char* strerr = strerror (errno);
  throw new java::net::ConnectException (JvNewStringUTF (strerr));
}

void
java::net::PlainSocketImpl::listen (jint backlog)
{
  if (::listen (fnum, backlog) != 0)
    {
      char* strerr = strerror (errno);
      throw new java::io::IOException (JvNewStringUTF (strerr));
    }
}

void
java::net::PlainSocketImpl::accept (java::net::PlainSocketImpl *s)
{
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  int new_socket = 0; 

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
	throw new java::io::InterruptedIOException (
	         JvNewStringUTF("Accept timed out"));
    }
#endif /* WIN32 */

  new_socket = _Jv_accept (fnum, (sockaddr*) &u, &addrlen);
  if (new_socket < 0)
    goto error;

  _Jv_platform_close_on_exec (new_socket);

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

  s->fnum = new_socket;
  s->localport = localport;
  s->address = new InetAddress (raddr, NULL);
  s->port = rport;
  return;
 error:
  char* strerr = strerror (errno);
  throw new java::io::IOException (JvNewStringUTF (strerr));
}

// Close(shutdown) the socket.
void
java::net::PlainSocketImpl::close()
{
  // Avoid races from asynchronous finalization.
  JvSynchronize sync (this);

  // should we use shutdown here? how would that effect so_linger?
  int res = ::close (fnum);

  if (res == -1)
    {
      // These three errors are not errors according to tests performed
      // on the reference implementation.
      if (errno != ENOTCONN && errno != ECONNRESET && errno != EBADF)
	throw new java::io::IOException  (JvNewStringUTF (strerror (errno)));
    }
  // Safe place to reset the file pointer.
  fnum = -1;
  timeout = 0;
}

// Write a byte to the socket.
void
java::net::PlainSocketImpl::write(jint b)
{
  jbyte d =(jbyte) b;
  int r = 0;

  while (r != 1)
    {
      r = ::write (fnum, &d, 1);
      if (r == -1)
	{
	  if (java::lang::Thread::interrupted())
	    {
	      java::io::InterruptedIOException *iioe
		= new java::io::InterruptedIOException 
		(JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = 0;
	      throw iioe;
	    }
	  // Some errors should not cause exceptions.
	  if (errno != ENOTCONN && errno != ECONNRESET && errno != EBADF)
	    throw new java::io::IOException (JvNewStringUTF (strerror (errno)));
	  break;
	}
    }
}

// Write some bytes to the socket.
void
java::net::PlainSocketImpl::write(jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new java::lang::NullPointerException;
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new java::lang::ArrayIndexOutOfBoundsException;

  jbyte *bytes = elements (b) + offset;
  int written = 0;
  while (len > 0)
    {
      int r = ::write (fnum, bytes, len);
      if (r == -1)
        {
	  if (java::lang::Thread::interrupted())
	    {
	      java::io::InterruptedIOException *iioe
		= new java::io::InterruptedIOException
		(JvNewStringLatin1 (strerror (errno)));
	      iioe->bytesTransferred = written;
	      throw iioe;
	    }
	  // Some errors should not cause exceptions.
	  if (errno != ENOTCONN && errno != ECONNRESET && errno != EBADF)
	    throw new java::io::IOException (JvNewStringUTF (strerror (errno)));
	  break;
	}
      written += r;
      len -= r;
      bytes += r;
    }
}


// Read a single byte from the socket.
jint
java::net::PlainSocketImpl::read(void)
{
  jbyte b;

// FIXME: implement timeout support for Win32
#ifndef WIN32
  // Do timeouts via select.
  if (timeout > 0)
  {
    // Create the file descriptor set.
    fd_set read_fds;
    FD_ZERO (&read_fds);
    FD_SET (fnum,&read_fds);
    // Create the timeout struct based on our internal timeout value.
    struct timeval timeout_value;
    timeout_value.tv_sec = timeout / 1000;
    timeout_value.tv_usec = (timeout % 1000) * 1000;
    // Select on the fds.
    int sel_retval = _Jv_select (fnum + 1, &read_fds, NULL, NULL, &timeout_value);
    // If select returns 0 we've waited without getting data...
    // that means we've timed out.
    if (sel_retval == 0)
      throw new java::io::InterruptedIOException
	(JvNewStringUTF ("read timed out") );
    // If select returns ok we know we either got signalled or read some data...
    // either way we need to try to read.
  }
#endif /* WIN32 */

  int r = ::read (fnum, &b, 1);

  if (r == 0)
    return -1;
  if (java::lang::Thread::interrupted())
    {
      java::io::InterruptedIOException *iioe =
	new java::io::InterruptedIOException
	(JvNewStringUTF("read interrupted"));
      iioe->bytesTransferred = r == -1 ? 0 : r;
      throw iioe;
    }
  else if (r == -1)
    {
      // Some errors cause us to return end of stream...
      if (errno == ENOTCONN)
	return -1;
      // Other errors need to be signalled.
      throw new java::io::IOException (JvNewStringUTF (strerror (errno)));
    }
  return b & 0xFF;
}

// Read count bytes into the buffer, starting at offset.
jint
java::net::PlainSocketImpl::read(jbyteArray buffer, jint offset, jint count)
{
  if (! buffer)
    throw new java::lang::NullPointerException;
  jsize bsize = JvGetArrayLength (buffer);
  if (offset < 0 || count < 0 || offset + count > bsize)
    throw new java::lang::ArrayIndexOutOfBoundsException;
  jbyte *bytes = elements (buffer) + offset;

// FIXME: implement timeout support for Win32
#ifndef WIN32
  // Do timeouts via select.
  if (timeout > 0)
  {
    // Create the file descriptor set.
    fd_set read_fds;
    FD_ZERO (&read_fds);
    FD_SET (fnum, &read_fds);
    // Create the timeout struct based on our internal timeout value.
    struct timeval timeout_value;
    timeout_value.tv_sec = timeout / 1000;
    timeout_value.tv_usec =(timeout % 1000) * 1000;
    // Select on the fds.
    int sel_retval = _Jv_select (fnum + 1, &read_fds, NULL, NULL, &timeout_value);
    // We're only interested in the 0 return.
    // error returns still require us to try to read 
    // the socket to see what happened.
    if (sel_retval == 0)
      {
	java::io::InterruptedIOException *iioe =
	  new java::io::InterruptedIOException
	  (JvNewStringUTF ("read interrupted"));
	iioe->bytesTransferred = 0;
	throw iioe;
      }
  }
#endif

  // Read the socket.
  int r = ::recv (fnum, (char *) bytes, count, 0);
  if (r == 0)
    return -1;
  if (java::lang::Thread::interrupted())
    {
      java::io::InterruptedIOException *iioe =
	new java::io::InterruptedIOException
	(JvNewStringUTF ("read interrupted"));
      iioe->bytesTransferred = r == -1 ? 0 : r;
      throw iioe;
    }
  else if (r == -1)
    {
      // Some errors cause us to return end of stream...
      if (errno == ENOTCONN)
	return -1;
      // Other errors need to be signalled.
      throw new java::io::IOException (JvNewStringUTF (strerror (errno)));
    }
  return r;
}

// How many bytes are available?
jint
java::net::PlainSocketImpl::available(void)
{
#if defined(FIONREAD) || defined(HAVE_SELECT)
  long num = 0;
  int r = 0;
  bool num_set = false;

#if defined(FIONREAD)
  r = ::ioctl (fnum, FIONREAD, &num);
  if (r == -1 && errno == ENOTTY)
    {
      // If the ioctl doesn't work, we don't care.
      r = 0;
      num = 0;
    }
  else
    num_set = true;
#elif defined(HAVE_SELECT)
  if (fnum < 0)
    {
      errno = EBADF;
      r = -1;
    }
#endif

  if (r == -1)
    {
    posix_error:
      throw new java::io::IOException(JvNewStringUTF(strerror(errno)));

    }

  // If we didn't get anything we can use select.

#if defined(HAVE_SELECT)
  if (! num_set)
    {
      fd_set rd;
      FD_ZERO (&rd);
      FD_SET (fnum, &rd);
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 0;
      r = _Jv_select (fnum + 1, &rd, NULL, NULL, &tv);
      if(r == -1)
	goto posix_error;
      num = r == 0 ? 0 : 1;
    }
#endif /* HAVE_SELECT */

  return (jint) num;
#else
  throw new java::io::IOException (JvNewStringUTF ("unimplemented"));
#endif
 }


void
java::net::PlainSocketImpl::setOption (jint optID, java::lang::Object *value)
{
  int val;
  socklen_t val_len = sizeof (val);

  if (_Jv_IsInstanceOf (value, &java::lang::Boolean::class$))
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
  else if (_Jv_IsInstanceOf (value, &java::lang::Integer::class$))
    {
      java::lang::Integer *intobj = 
        static_cast<java::lang::Integer *> (value);          
      val = (int) intobj->intValue();
    }
  else
    {
      throw new java::lang::IllegalArgumentException (JvNewStringLatin1 ("`value' must be Boolean or Integer"));
    }

  switch (optID) 
    {
      case _Jv_TCP_NODELAY_ :
#ifdef TCP_NODELAY
        if (::setsockopt (fnum, IPPROTO_TCP, TCP_NODELAY, (char *) &val,
	    val_len) != 0)
	  goto error;    
#else
        throw new java::lang::InternalError (
          JvNewStringUTF ("TCP_NODELAY not supported"));
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
        throw new java::lang::InternalError (
          JvNewStringUTF ("SO_LINGER not supported"));
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
        throw new java::lang::InternalError (
          JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported"));
#endif 
        return;
      case _Jv_SO_BINDADDR_ :
        throw new java::net::SocketException (
          JvNewStringUTF ("SO_BINDADDR: read only option"));
        return;
      case _Jv_IP_MULTICAST_IF_ :
        throw new java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_IF: not valid for TCP"));
        return;
      case _Jv_SO_REUSEADDR_ :
        throw new java::net::SocketException (
          JvNewStringUTF ("SO_REUSEADDR: not valid for TCP"));
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
        throw new java::lang::InternalError (
          JvNewStringUTF ("TCP_NODELAY not supported"));
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
        throw new java::lang::InternalError (
          JvNewStringUTF ("SO_LINGER not supported"));
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
	      throw
		new java::net::SocketException (JvNewStringUTF ("invalid family"));
	    localAddress = new java::net::InetAddress (laddr, NULL);
	  }
	return localAddress;
	break;
      case _Jv_IP_MULTICAST_IF_ :
	throw new java::net::SocketException (
	  JvNewStringUTF ("IP_MULTICAST_IF: not valid for TCP"));
	break;
      case _Jv_SO_REUSEADDR_ :
	throw new java::net::SocketException (
	  JvNewStringUTF ("SO_REUSEADDR: not valid for TCP"));
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
