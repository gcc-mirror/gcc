/* Copyright (C) 2003, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#ifdef HAVE_SYS_IOCTL_H
#define BSD_COMP /* Get FIONREAD on Solaris2. */
#include <sys/ioctl.h>
#endif

// Pick up FIONREAD on Solaris 2.5.
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#include <netinet/in.h>
#include <netinet/tcp.h>
#include <errno.h>
#include <string.h>

#if HAVE_BSTRING_H
// Needed for bzero, implicitly used by FD_ZERO on IRIX 5.2 
#include <bstring.h>
#endif

#include <gcj/cni.h>
#include <gcj/javaprims.h>
#include <gnu/java/net/PlainSocketImpl.h>
#include <gnu/java/net/PlainSocketImpl$SocketInputStream.h>
#include <gnu/java/net/PlainSocketImpl$SocketOutputStream.h>
#include <java/io/IOException.h>
#include <java/io/InterruptedIOException.h>
#include <java/net/BindException.h>
#include <java/net/ConnectException.h>
#include <java/net/InetAddress.h>
#include <java/net/InetSocketAddress.h>
#include <java/net/SocketException.h>
#include <java/net/SocketTimeoutException.h>
#include <java/lang/InternalError.h>
#include <java/lang/Object.h>
#include <java/lang/Boolean.h>
#include <java/lang/Class.h>
#include <java/lang/Integer.h>
#include <java/lang/Thread.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/IllegalArgumentException.h>

union SockAddr
{
  struct sockaddr_in address;
#ifdef HAVE_INET6
  struct sockaddr_in6 address6;
#endif
};

void
gnu::java::net::PlainSocketImpl::create (jboolean stream)
{
  int sock = _Jv_socket (AF_INET, stream ? SOCK_STREAM : SOCK_DGRAM, 0);

  if (sock < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  _Jv_platform_close_on_exec (sock);

  // We use native_fd in place of fd here.  From leaving fd null we avoid
  // the double close problem in FileDescriptor.finalize.
  native_fd = sock;
}

void
gnu::java::net::PlainSocketImpl::bind (::java::net::InetAddress *host, jint lport)
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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid length"));

  // Enable SO_REUSEADDR, so that servers can reuse ports left in TIME_WAIT.
  ::setsockopt(native_fd, SOL_SOCKET, SO_REUSEADDR, (char *) &i, sizeof(i));
  
  if (_Jv_bind (native_fd, ptr, len) == 0)
    {
      socklen_t addrlen = sizeof(u);

      if (lport != 0)
        localport = lport;
      else if (::getsockname (native_fd, (sockaddr*) &u, &addrlen) == 0)
        localport = ntohs (u.address.sin_port);
      else
        goto error;

      return;
    }

 error:
  char* strerr = strerror (errno);
  throw new ::java::net::BindException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainSocketImpl::connect (::java::net::SocketAddress *addr,
                                     jint timeout)
{
  ::java::net::InetSocketAddress *tmp = (::java::net::InetSocketAddress*) addr;
  ::java::net::InetAddress *host = tmp->getAddress();
  jint rport = tmp->getPort();
	
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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid length"));

  if (timeout > 0)
    {
      int flags = ::fcntl (native_fd, F_GETFL);
      ::fcntl (native_fd, F_SETFL, flags | O_NONBLOCK);
      
      if ((_Jv_connect (native_fd, ptr, len) != 0) && (errno != EINPROGRESS))
        goto error;

      fd_set fset;
      struct timeval tv;
      FD_ZERO(&fset);
      FD_SET(native_fd, &fset);
      tv.tv_sec = timeout / 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      int retval;
      
      if ((retval = _Jv_select (native_fd + 1, &fset, &fset, NULL, &tv)) < 0)
        goto error;
      else if (retval == 0)
        throw new ::java::net::SocketTimeoutException
          (JvNewStringUTF ("Connect timed out"));
       // Set the socket back into a blocking state.
       ::fcntl (native_fd, F_SETFL, flags);
    }
  else
    {
      if (_Jv_connect (native_fd, ptr, len) != 0)
        goto error;
    }

  address = host;
  port = rport;

  // A bind may not have been done on this socket; if so, set localport now.
  if (localport == 0)
    {
      if (::getsockname (native_fd, (sockaddr*) &u, &addrlen) == 0)
        localport = ntohs (u.address.sin_port);
      else
        goto error;
    }

  return;  

 error:
  char* strerr = strerror (errno);
  throw new ::java::net::ConnectException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainSocketImpl::listen (jint backlog)
{
  if (::listen (native_fd, backlog) != 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }
}

void
gnu::java::net::PlainSocketImpl::accept (gnu::java::net::PlainSocketImpl *s)
{
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  int new_socket = 0; 

  // Do timeouts via select since SO_RCVTIMEO is not always available.
  if (timeout > 0 && native_fd >= 0 && native_fd < FD_SETSIZE)
    {
      fd_set fset;
      struct timeval tv;
      FD_ZERO(&fset);
      FD_SET(native_fd, &fset);
      tv.tv_sec = timeout / 1000;
      tv.tv_usec = (timeout % 1000) * 1000;
      int retval;
      if ((retval = _Jv_select (native_fd + 1, &fset, &fset, NULL, &tv)) < 0)
        goto error;
      else if (retval == 0)
        throw new ::java::net::SocketTimeoutException (
	                                  JvNewStringUTF("Accept timed out"));
    }

  new_socket = _Jv_accept (native_fd, (sockaddr*) &u, &addrlen);

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
    throw new ::java::net::SocketException (JvNewStringUTF ("invalid family"));

  s->native_fd = new_socket;
  s->localport = localport;
  s->address = new ::java::net::InetAddress (raddr, NULL);
  s->port = rport;
  return;

 error:
  char* strerr = strerror (errno);
  throw new ::java::io::IOException (JvNewStringUTF (strerr));
}

// Close(shutdown) the socket.
void
gnu::java::net::PlainSocketImpl::close()
{
  // Avoid races from asynchronous finalization.
  JvSynchronize sync (this);

  // should we use shutdown here? how would that effect so_linger?
  int res = _Jv_close (native_fd);

  if (res == -1)
    {
      // These three errors are not errors according to tests performed
      // on the reference implementation.
      if (errno != ENOTCONN && errno != ECONNRESET && errno != EBADF)
        throw new ::java::io::IOException  (JvNewStringUTF (strerror (errno)));
    }
  // Safe place to reset the file pointer.
  native_fd = -1;
  timeout = 0;
}

static void
write_helper (jint native_fd, jbyte *bytes, jint len);

// Write a byte to the socket.
void
gnu::java::net::PlainSocketImpl$SocketOutputStream::write(jint b)
{
  jbyte data = (jbyte) b;
  write_helper (this$0->native_fd, &data, 1);
}

// Write some bytes to the socket.
void
gnu::java::net::PlainSocketImpl$SocketOutputStream::write(jbyteArray b, jint offset, jint len)
{
  if (! b)
    throw new ::java::lang::NullPointerException;
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new ::java::lang::ArrayIndexOutOfBoundsException;

  write_helper (this$0->native_fd, elements (b) + offset * sizeof (jbyte), len);
}

static void
write_helper(jint native_fd, jbyte *bytes, jint len)
{
  int written = 0;

  while (len > 0)
    {
      int r = _Jv_write (native_fd, bytes, len);

      if (r == -1)
        {
          if (::java::lang::Thread::interrupted())
            {
              ::java::io::InterruptedIOException *iioe
                = new ::java::io::InterruptedIOException
                (JvNewStringLatin1 (strerror (errno)));
              iioe->bytesTransferred = written;
              throw iioe;
            }
          // Some errors should not cause exceptions.
          if (errno != ENOTCONN && errno != ECONNRESET && errno != EBADF)
            throw new ::java::io::IOException (JvNewStringUTF (strerror (errno)));
          break;
        }

      written += r;
      len -= r;
      bytes += r;
    }
}

void
gnu::java::net::PlainSocketImpl::sendUrgentData (jint)
{
  throw new ::java::net::SocketException (JvNewStringLatin1 (
    "PlainSocketImpl: sending of urgent data not supported by this socket"));
}

static jint
read_helper (jint native_fd, jint timeout, jbyte *bytes, jint count);

// Read a single byte from the socket.
jint
gnu::java::net::PlainSocketImpl$SocketInputStream::read(void)
{
  jbyte data;

  if (read_helper (this$0->native_fd, this$0->timeout, &data, 1) == 1)
    return data & 0xFF;

  return -1;
}

// Read count bytes into the buffer, starting at offset.
jint
gnu::java::net::PlainSocketImpl$SocketInputStream::read(jbyteArray buffer, jint offset, 
  jint count)
{
 if (! buffer)
    throw new ::java::lang::NullPointerException;

  jsize bsize = JvGetArrayLength (buffer);

  if (offset < 0 || count < 0 || offset + count > bsize)
    throw new ::java::lang::ArrayIndexOutOfBoundsException;

  return read_helper (this$0->native_fd, this$0->timeout,
		      elements (buffer) + offset * sizeof (jbyte), count);
}

static jint
read_helper (jint native_fd, jint timeout, jbyte *bytes, jint count)
{
  // If zero bytes were requested, short circuit so that recv
  // doesn't signal EOF.
  if (count == 0)
    return 0;
    
  // Do timeouts via select.
  if (timeout > 0 && native_fd >= 0 && native_fd < FD_SETSIZE)
    {
      // Create the file descriptor set.
      fd_set read_fds;
      FD_ZERO (&read_fds);
      FD_SET (native_fd, &read_fds);
      // Create the timeout struct based on our internal timeout value.
      struct timeval timeout_value;
      timeout_value.tv_sec = timeout / 1000;
      timeout_value.tv_usec =(timeout % 1000) * 1000;
      // Select on the fds.
      int sel_retval =
        _Jv_select (native_fd + 1, &read_fds, NULL, NULL, &timeout_value);
      // We're only interested in the 0 return.
      // error returns still require us to try to read 
      // the socket to see what happened.
      if (sel_retval == 0)
        {
          ::java::net::SocketTimeoutException *timeoutException =
            new ::java::net::SocketTimeoutException
            (JvNewStringUTF ("Read timed out"));
	  throw timeoutException;
        }
    }

  // Read the socket.
  int r = ::recv (native_fd, (char *) bytes, count, 0);

  if (r == 0)
    return -1;

  if (::java::lang::Thread::interrupted())
    {
      ::java::io::InterruptedIOException *iioe =
        new ::java::io::InterruptedIOException
        (JvNewStringUTF ("Read interrupted"));
      iioe->bytesTransferred = r == -1 ? 0 : r;
      throw iioe;
    }
  else if (r == -1)
    {
      // Some errors cause us to return end of stream...
      if (errno == ENOTCONN)
        return -1;

      // Other errors need to be signalled.
      throw new ::java::io::IOException (JvNewStringUTF (strerror (errno)));
    }

  return r;
}

// How many bytes are available?
jint
gnu::java::net::PlainSocketImpl::available(void)
{
#if defined(FIONREAD) || defined(HAVE_SELECT)
  int num = 0;
  int r = 0;
  bool num_set = false;

#if defined(FIONREAD)
  r = ::ioctl (native_fd, FIONREAD, &num);

  if (r == -1 && errno == ENOTTY)
    {
      // If the ioctl doesn't work, we don't care.
      r = 0;
      num = 0;
    }
  else
    num_set = true;
#elif defined(HAVE_SELECT)
  if (native_fd < 0)
    {
      errno = EBADF;
      r = -1;
    }
#endif

  if (r == -1)
    {
    posix_error:
      throw new ::java::io::IOException(JvNewStringUTF(strerror(errno)));
    }

  // If we didn't get anything we can use select.

#if defined(HAVE_SELECT)
  if (! num_set)
    if (! num_set && native_fd >= 0 && native_fd < FD_SETSIZE)
      {
        fd_set rd;
        FD_ZERO (&rd);
        FD_SET (native_fd, &rd);
        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = 0;
        r = _Jv_select (native_fd + 1, &rd, NULL, NULL, &tv);
        if(r == -1)
          goto posix_error;
        num = r == 0 ? 0 : 1;
      }
#endif /* HAVE_SELECT */

  return (jint) num;
#else
  throw new ::java::io::IOException (JvNewStringUTF ("unimplemented"));
#endif
}

void
gnu::java::net::PlainSocketImpl::setOption (jint optID, ::java::lang::Object *value)
{
  int val;
  socklen_t val_len = sizeof (val);

  if (native_fd < 0)
    throw new ::java::net::SocketException (JvNewStringUTF ("Socket closed"));

  if (_Jv_IsInstanceOf (value, &::java::lang::Boolean::class$))
    {
      ::java::lang::Boolean *boolobj = 
        static_cast< ::java::lang::Boolean *> (value);
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
  else if (_Jv_IsInstanceOf (value, &::java::lang::Integer::class$))
    {
      ::java::lang::Integer *intobj = 
        static_cast< ::java::lang::Integer *> (value);          
      val = (int) intobj->intValue();
    }
  else
    {
      throw new ::java::lang::IllegalArgumentException (
        JvNewStringLatin1 ("`value' must be Boolean or Integer"));
    }

  switch (optID) 
    {
      case _Jv_TCP_NODELAY_ :
#ifdef TCP_NODELAY
        if (::setsockopt (native_fd, IPPROTO_TCP, TCP_NODELAY, (char *) &val,
                          val_len) != 0)
          goto error;
#else
        throw new ::java::lang::InternalError
          (JvNewStringUTF ("TCP_NODELAY not supported"));
#endif /* TCP_NODELAY */
        return;

      case _Jv_SO_KEEPALIVE_ :
        if (::setsockopt (native_fd, SOL_SOCKET, SO_KEEPALIVE, (char *) &val,
                          val_len) != 0)
          goto error;
        return;
      
      case _Jv_SO_BROADCAST_ :
        throw new ::java::net::SocketException
          (JvNewStringUTF ("SO_BROADCAST not valid for TCP"));
        return;
	
      case _Jv_SO_OOBINLINE_ :
        if (::setsockopt (native_fd, SOL_SOCKET, SO_OOBINLINE, (char *) &val,
                          val_len) != 0)
          goto error;
        return;

      case _Jv_SO_LINGER_ :
#ifdef SO_LINGER
        struct linger l_val;
        l_val.l_onoff = (val != -1);
        l_val.l_linger = val;

        if (::setsockopt (native_fd, SOL_SOCKET, SO_LINGER, (char *) &l_val,
                          sizeof(l_val)) != 0)
          goto error;    
#else
        throw new ::java::lang::InternalError (
          JvNewStringUTF ("SO_LINGER not supported"));
#endif /* SO_LINGER */
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

      case _Jv_SO_BINDADDR_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_BINDADDR: read only option"));
        return;

      case _Jv_IP_MULTICAST_IF_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_IF: not valid for TCP"));
        return;
	
      case _Jv_IP_MULTICAST_IF2_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_IF2: not valid for TCP"));
        return;
	
      case _Jv_IP_MULTICAST_LOOP_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_LOOP: not valid for TCP"));
        return;
	
      case _Jv_IP_TOS_ :
        if (::setsockopt (native_fd, SOL_SOCKET, IP_TOS, (char *) &val,
                          val_len) != 0)
          goto error;    
        return;
	
      case _Jv_SO_REUSEADDR_ :
#if defined(SO_REUSEADDR)
	if (::setsockopt (native_fd, SOL_SOCKET, SO_REUSEADDR, (char *) &val,
	    val_len) != 0)
	  goto error;
	return;
#else
        throw new ::java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported"));
#endif 

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
gnu::java::net::PlainSocketImpl::getOption (jint optID)
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
      if (::getsockopt (native_fd, IPPROTO_TCP, TCP_NODELAY, (char *) &val,
                        &val_len) != 0)
        goto error;
      else
        return new ::java::lang::Boolean (val != 0);
#else
      throw new ::java::lang::InternalError
        (JvNewStringUTF ("TCP_NODELAY not supported"));
#endif       
      break;
      
    case _Jv_SO_LINGER_ :
#ifdef SO_LINGER
      if (::getsockopt (native_fd, SOL_SOCKET, SO_LINGER, (char *) &l_val,
                        &l_val_len) != 0)
        goto error;    
 
      if (l_val.l_onoff)
        return new ::java::lang::Integer (l_val.l_linger);
      else
        return new ::java::lang::Boolean ((jboolean)false);
#else
      throw new ::java::lang::InternalError
        (JvNewStringUTF ("SO_LINGER not supported"));
#endif
      break;    

    case _Jv_SO_KEEPALIVE_ :
      if (::getsockopt (native_fd, SOL_SOCKET, SO_KEEPALIVE, (char *) &val,
                        &val_len) != 0)
        goto error;
      else
        return new ::java::lang::Boolean (val != 0);

    case _Jv_SO_BROADCAST_ :
      if (::getsockopt (native_fd, SOL_SOCKET, SO_BROADCAST, (char *) &val,
                        &val_len) != 0)
        goto error;    
      return new ::java::lang::Boolean ((jboolean)val);
	
    case _Jv_SO_OOBINLINE_ :
      if (::getsockopt (native_fd, SOL_SOCKET, SO_OOBINLINE, (char *) &val,
                        &val_len) != 0)
        goto error;    
      return new ::java::lang::Boolean ((jboolean)val);
	
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
      throw new ::java::lang::InternalError
        (JvNewStringUTF ("SO_RCVBUF/SO_SNDBUF not supported"));
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
            throw new ::java::net::SocketException
              (JvNewStringUTF ("invalid family"));
          localAddress = new ::java::net::InetAddress (laddr, NULL);
        }

      return localAddress;
      break;
    case _Jv_IP_MULTICAST_IF_ :
      throw new ::java::net::SocketException
        (JvNewStringUTF ("IP_MULTICAST_IF: not valid for TCP"));
      break;
	
    case _Jv_IP_MULTICAST_IF2_ :
      throw new ::java::net::SocketException
        (JvNewStringUTF ("IP_MULTICAST_IF2: not valid for TCP"));
      break;
	
    case _Jv_IP_MULTICAST_LOOP_ :
      throw new ::java::net::SocketException
        (JvNewStringUTF ("IP_MULTICAST_LOOP: not valid for TCP"));
      break;
	
    case _Jv_IP_TOS_ :
      if (::getsockopt (native_fd, SOL_SOCKET, IP_TOS, (char *) &val,
                        &val_len) != 0)
        goto error;
      return new ::java::lang::Integer (val);
      break;
	
    case _Jv_SO_REUSEADDR_ :
#if defined(SO_REUSEADDR)
      if (::getsockopt (native_fd, SOL_SOCKET, SO_REUSEADDR, (char *) &val,
                        &val_len) != 0)
        goto error;    
#else
        throw new ::java::lang::InternalError (
          JvNewStringUTF ("SO_REUSEADDR not supported"));
#endif 
      break;

    case _Jv_SO_TIMEOUT_ :
      return new ::java::lang::Integer (timeout);
      break;

    default :
      errno = ENOPROTOOPT;
    }

 error:
  char* strerr = strerror (errno);
  throw new ::java::net::SocketException (JvNewStringUTF (strerr));
}

void
gnu::java::net::PlainSocketImpl::shutdownInput (void)
{
  if (::shutdown (native_fd, 0))
    throw new ::java::net::SocketException (JvNewStringUTF (strerror (errno)));
}

void
gnu::java::net::PlainSocketImpl::shutdownOutput (void)
{
  if (::shutdown (native_fd, 1))
    throw new ::java::net::SocketException (JvNewStringUTF (strerror (errno)));
}
