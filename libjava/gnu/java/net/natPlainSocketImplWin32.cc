/* Copyright (C) 2003 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#undef STRICT
#undef MAX_PRIORITY
#undef MIN_PRIORITY

#include <gnu/java/net/PlainSocketImpl.h>
#include <gnu/java/net/PlainSocketImpl$SocketInputStream.h>
#include <gnu/java/net/PlainSocketImpl$SocketOutputStream.h>
#include <java/io/IOException.h>
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
  SOCKET sock = ::socket (AF_INET, stream ? SOCK_STREAM : SOCK_DGRAM, 0);

  if (sock == INVALID_SOCKET)
    {
      _Jv_ThrowIOException ();
    }

  // Cast this to a HANDLE so we can make
  // it non-inheritable via _Jv_platform_close_on_exec.
  HANDLE hSocket = (HANDLE) sock;
  _Jv_platform_close_on_exec (hSocket);

  // We use native_fd in place of fd here.  From leaving fd null we avoid
  // the double close problem in FileDescriptor.finalize.
  native_fd = (jint) hSocket;
}

void
gnu::java::net::PlainSocketImpl::bind (::java::net::InetAddress *host, jint lport)
{
  union SockAddr u;
  struct sockaddr *ptr = (struct sockaddr *) &u.address;
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

  if (::bind (native_fd, ptr, len) != SOCKET_ERROR)
    {
      socklen_t addrlen = sizeof(u);

      if (lport != 0)
        localport = lport;
      else if (::getsockname (native_fd, (sockaddr*) &u, &addrlen) != SOCKET_ERROR)
        localport = ntohs (u.address.sin_port);
      else
        goto error;

      return;
    }

error:
  DWORD dwErrorCode = WSAGetLastError ();
  throw new ::java::net::BindException (_Jv_WinStrError (dwErrorCode));
}

static void
throwConnectException (DWORD dwErrorCode)
{
  throw new ::java::net::ConnectException (_Jv_WinStrError (dwErrorCode));
}

static void
throwConnectException ()
{
  throwConnectException (WSAGetLastError ());
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
      // FIXME: we're creating a fresh WSAEVENT for each connect().
      WSAEventWrapper aWSAEventWrapper(native_fd, FD_CONNECT);
      WSAEVENT hEvent = aWSAEventWrapper.getEventHandle ();

      if (::connect (native_fd, ptr, len) == SOCKET_ERROR)
      {
        if (WSAGetLastError () != WSAEWOULDBLOCK)
          throwConnectException ();

        DWORD dwRet =
          WSAWaitForMultipleEvents (1, &hEvent, true, timeout, false);
            // use true, false instead of TRUE, FALSE because the
            // MS constants got undefined

        // Reset and ignore our thread's interrupted flag.
        // It's not possible to interrupt these sort of
        // operations on Win32 anyway.
        ::java::lang::Thread::interrupted();

        if (dwRet == WSA_WAIT_FAILED)
          throwConnectException ();
        else if (dwRet == WSA_WAIT_TIMEOUT)
          throw new ::java::net::SocketTimeoutException
            (JvNewStringUTF ("connect timed out"));
            
        // If we get here, we still need to check whether the actual
        // connect() succeeded. Use any socket-specific error code
        // instead of the thread-based one.
        int nErrCode; int nErrLen=sizeof(nErrCode);
        if (::getsockopt(native_fd, SOL_SOCKET, SO_ERROR, (char*) &nErrCode,
          &nErrLen) == SOCKET_ERROR)
          {
            throwConnectException ();
          }
        
        if (nErrCode != NO_ERROR)
          {
            throwConnectException (nErrCode);
          }
      }
    }
  else
    {
      if (::connect (native_fd, ptr, len) == SOCKET_ERROR)
        throwConnectException();
    }

  address = host;
  port = rport;

  // A bind may not have been done on this socket; if so, set localport now.
  if (localport == 0)
    {
      if (::getsockname (native_fd, (sockaddr*) &u, &addrlen) != SOCKET_ERROR)
        localport = ntohs (u.address.sin_port);
      else
        throwConnectException();
    }
}

void
gnu::java::net::PlainSocketImpl::listen (jint backlog)
{
  if (::listen (native_fd, backlog) == SOCKET_ERROR)
    {
      _Jv_ThrowIOException ();
    }
}

void
gnu::java::net::PlainSocketImpl::accept (gnu::java::net::PlainSocketImpl *s)
{
  union SockAddr u;
  socklen_t addrlen = sizeof(u);
  HANDLE hSocket = 0;
  SOCKET new_socket = 0;

  if (timeout > 0)
    {
      // FIXME: we're creating a fresh WSAEVENT for each accept().
      // One possible alternative would be that native_fd really points
      // to an extended structure consisting of the SOCKET, its
      // associated WSAEVENT, etc.
      WSAEventWrapper aWSAEventWrapper(native_fd, FD_ACCEPT);
      WSAEVENT hEvent = aWSAEventWrapper.getEventHandle ();

      for (;;)
      {
        new_socket = ::accept (native_fd, (sockaddr*) &u, &addrlen);

        if (new_socket != INVALID_SOCKET)
        {
          // This new child socket is nonblocking because the parent
          // socket became nonblocking via the WSAEventSelect() call,
          // so we set its mode back to blocking.
          WSAEventSelect (new_socket, hEvent, 0);
            // undo the hEvent <-> FD_ACCEPT association inherited
            // inherited from our parent socket

          unsigned long lSockOpt = 0L;
            // blocking mode
          if (ioctlsocket(new_socket, FIONBIO, &lSockOpt) == SOCKET_ERROR)
          {
            goto error;
          }
          break;
        }
        else if (WSAGetLastError () != WSAEWOULDBLOCK)
          {
            goto error;
          }

        DWORD dwRet =
          WSAWaitForMultipleEvents (1, &hEvent, true, timeout, false);
            // use true, false instead of TRUE, FALSE because the
            // MS constants got undefined

        // Reset and ignore our thread's interrupted flag.
        ::java::lang::Thread::interrupted();

        if (dwRet == WSA_WAIT_FAILED)
          goto error;
        else if (dwRet == WSA_WAIT_TIMEOUT)
          throw new ::java::net::SocketTimeoutException
            (JvNewStringUTF ("Accept timed out"));
      }
    }
  else
    {
      new_socket = ::accept (native_fd, (sockaddr*) &u, &addrlen);
    }

  if (new_socket == INVALID_SOCKET)
    goto error;

  // Cast this to a HANDLE so we can make
  // it non-inheritable via _Jv_platform_close_on_exec.
  hSocket = (HANDLE) new_socket;
  _Jv_platform_close_on_exec (hSocket);

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

  s->native_fd = (jint) hSocket;
  s->localport = localport;
  s->address = new ::java::net::InetAddress (raddr, NULL);
  s->port = rport;
  return;

 error:
  _Jv_ThrowIOException ();
}

// Close(shutdown) the socket.
void
gnu::java::net::PlainSocketImpl::close()
{
  // Avoid races from asynchronous finalization.
  JvSynchronize sync (this);

  // should we use shutdown here? how would that effect so_linger?
  int res = ::closesocket (native_fd);

  if (res == -1)
    {
      // These three errors are not errors according to tests performed
      // on the reference implementation.
      DWORD dwErr = WSAGetLastError();
      if (dwErr != WSAENOTCONN && dwErr != WSAECONNRESET
        && dwErr != WSAENOTSOCK)
        _Jv_ThrowIOException ();
    }
  // Safe place to reset the file pointer.
  native_fd = -1;
  timeout = 0;
}

// Write a byte to the socket.
void
gnu::java::net::PlainSocketImpl$SocketOutputStream::write(jint b)
{
  jbyte d =(jbyte) b;
  int r = 0;

  while (r != 1)
    {
      r = ::send (this$0->native_fd, (char*) &d, 1, 0);
      if (r == -1)
        {
          DWORD dwErr = WSAGetLastError();
          
          // Reset and ignore our thread's interrupted flag.
          // It's not possible to interrupt these sort of
          // operations on Win32 anyway.
          ::java::lang::Thread::interrupted();

          // Some errors should not cause exceptions.
          if (dwErr != WSAENOTCONN && dwErr != WSAECONNRESET
            && dwErr != WSAENOTSOCK)
            _Jv_ThrowIOException ();
          break;
        }
    }
}

// Write some bytes to the socket.
void
gnu::java::net::PlainSocketImpl$SocketOutputStream::write(jbyteArray b, 
  jint offset, jint len)
{
  if (! b)
    throw new ::java::lang::NullPointerException;
  if (offset < 0 || len < 0 || offset + len > JvGetArrayLength (b))
    throw new ::java::lang::ArrayIndexOutOfBoundsException;

  jbyte *bytes = elements (b) + offset;
  int written = 0;
  while (len > 0)
    {
      int r = ::send (this$0->native_fd, (char*) bytes, len, 0);

      if (r == -1)
        {
          DWORD dwErr = WSAGetLastError();

          // Reset and ignore our thread's interrupted flag.
          ::java::lang::Thread::interrupted();

          // Some errors should not cause exceptions.
          if (dwErr != WSAENOTCONN && dwErr != WSAECONNRESET
            && dwErr != WSAENOTSOCK)
            _Jv_ThrowIOException ();
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

// read() helper
static jint
doRead(int native_fd, void* buf, int count, int timeout)
{
  int r = 0;
  DWORD dwErrorCode = 0;
    // we are forced to declare this here because
    // a call to Thread::interrupted() blanks out
    // WSAGetLastError().

  // FIXME: we unconditionally set SO_RCVTIMEO here
  // because we can't detect whether someone has
  // gone from a non-zero to zero timeout. What we'd
  // really need is a member state variable in addition
  // to timeout
  int nRet= ::setsockopt(native_fd, SOL_SOCKET, SO_RCVTIMEO,
    (char*)&timeout, sizeof(timeout));
  if (nRet != NO_ERROR)
  {
    dwErrorCode = WSAGetLastError ();
    goto error;
  }
  
  r = ::recv (native_fd, (char*) buf, count, 0);

  if (r == 0)
    return -1;

  dwErrorCode = WSAGetLastError ();
    // save WSAGetLastError() before calling Thread.interrupted()
  
  // Reset and ignore our thread's interrupted flag.
  ::java::lang::Thread::interrupted();
  
  if (r == -1)
    {
error:
      // Some errors cause us to return end of stream...
      if (dwErrorCode == WSAENOTCONN)
        return -1;

      // Other errors need to be signalled.
      if (dwErrorCode == WSAETIMEDOUT)
        throw new ::java::net::SocketTimeoutException
          (JvNewStringUTF ("Read timed out") );
      else
        _Jv_ThrowIOException (dwErrorCode);
    }
    
   return r;
}

// Read a single byte from the socket.
jint
gnu::java::net::PlainSocketImpl$SocketInputStream::read(void)
{
  jbyte b;
  doRead(this$0->native_fd, &b, 1, this$0->timeout);
  return b & 0xFF;
}

// Read count bytes into the buffer, starting at offset.
jint
gnu::java::net::PlainSocketImpl$SocketInputStream::read(jbyteArray buffer,
  jint offset, jint count)
{
  if (! buffer)
    throw new ::java::lang::NullPointerException;

  jsize bsize = JvGetArrayLength (buffer);

  if (offset < 0 || count < 0 || offset + count > bsize)
    throw new ::java::lang::ArrayIndexOutOfBoundsException;

  jbyte *bytes = elements (buffer) + offset;

  // Read the socket.
  return doRead(this$0->native_fd, bytes, count, this$0->timeout);
}

// How many bytes are available?
jint
gnu::java::net::PlainSocketImpl::available(void)
{
  unsigned long num = 0;

  if (::ioctlsocket (native_fd, FIONREAD, &num) == SOCKET_ERROR)
    _Jv_ThrowIOException ();

  return (jint) num;
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
        if (::setsockopt (native_fd, IPPROTO_TCP, TCP_NODELAY, (char *) &val,
                          val_len) == SOCKET_ERROR)
          goto error;
        return;

      case _Jv_SO_KEEPALIVE_ :
        if (::setsockopt (native_fd, SOL_SOCKET, SO_KEEPALIVE, (char *) &val,
                          val_len) == SOCKET_ERROR)
          goto error;
        break;

      case _Jv_SO_BROADCAST_ :
        throw new ::java::net::SocketException
          (JvNewStringUTF ("SO_BROADCAST not valid for TCP"));
        break;

      case _Jv_SO_OOBINLINE_ :
        if (::setsockopt (native_fd, SOL_SOCKET, SO_OOBINLINE, (char *) &val,
                          val_len) == SOCKET_ERROR)
          goto error;
        break;

      case _Jv_SO_LINGER_ :
        struct linger l_val;
        l_val.l_onoff = (val != -1);
        l_val.l_linger = val;

        if (::setsockopt (native_fd, SOL_SOCKET, SO_LINGER, (char *) &l_val,
                          sizeof(l_val)) == SOCKET_ERROR)
          goto error;
        return;

      case _Jv_SO_SNDBUF_ :
      case _Jv_SO_RCVBUF_ :
        int opt;
        optID == _Jv_SO_SNDBUF_ ? opt = SO_SNDBUF : opt = SO_RCVBUF;
        if (::setsockopt (native_fd, SOL_SOCKET, opt, (char *) &val,
                          val_len) == SOCKET_ERROR)
          goto error;
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
        break;

      case _Jv_IP_MULTICAST_LOOP_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("IP_MULTICAST_LOOP: not valid for TCP"));
        break;

      case _Jv_IP_TOS_ :
        if (::setsockopt (native_fd, SOL_SOCKET, IP_TOS, (char *) &val,
                          val_len) == SOCKET_ERROR)
          goto error;
        break;

      case _Jv_SO_REUSEADDR_ :
        throw new ::java::net::SocketException (
          JvNewStringUTF ("SO_REUSEADDR: not valid for TCP"));
        return;

      case _Jv_SO_TIMEOUT_ :
        timeout = val;
        return;

      default :
        WSASetLastError (WSAENOPROTOOPT);
    }

error:
  _Jv_ThrowSocketException ();
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
    case _Jv_TCP_NODELAY_ :
      if (::getsockopt (native_fd, IPPROTO_TCP, TCP_NODELAY, (char *) &val,
                        &val_len) == SOCKET_ERROR)
        goto error;
      else
        return new ::java::lang::Boolean (val != 0);
      break;

    case _Jv_SO_LINGER_ :
      if (::getsockopt (native_fd, SOL_SOCKET, SO_LINGER, (char *) &l_val,
                        &l_val_len) == SOCKET_ERROR)
        goto error;

      if (l_val.l_onoff)
        return new ::java::lang::Integer (l_val.l_linger);
      else
        return new ::java::lang::Boolean ((jboolean)false);
      break;

    case _Jv_SO_KEEPALIVE_ :
      if (::getsockopt (native_fd, SOL_SOCKET, SO_KEEPALIVE, (char *) &val,
                        &val_len) == SOCKET_ERROR)
        goto error;
      else
        return new ::java::lang::Boolean (val != 0);

    case _Jv_SO_BROADCAST_ :
      if (::getsockopt (native_fd, SOL_SOCKET, SO_BROADCAST, (char *) &val,
                        &val_len) == SOCKET_ERROR)
        goto error;
      return new ::java::lang::Boolean ((jboolean)val);

    case _Jv_SO_OOBINLINE_ :
      if (::getsockopt (native_fd, SOL_SOCKET, SO_OOBINLINE, (char *) &val,
                        &val_len) == SOCKET_ERROR)
        goto error;
      return new ::java::lang::Boolean ((jboolean)val);

    case _Jv_SO_RCVBUF_ :
    case _Jv_SO_SNDBUF_ :
      int opt;
      optID == _Jv_SO_SNDBUF_ ? opt = SO_SNDBUF : opt = SO_RCVBUF;
      if (::getsockopt (native_fd, SOL_SOCKET, opt, (char *) &val,
                        &val_len) == SOCKET_ERROR)
        goto error;
      else
        return new ::java::lang::Integer (val);
      break;
    case _Jv_SO_BINDADDR_:
      // cache the local address
      if (localAddress == NULL)
        {
          jbyteArray laddr;

          if (::getsockname (native_fd, (sockaddr*) &u,
                             &addrlen) == SOCKET_ERROR)
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
                        &val_len) == SOCKET_ERROR)
        goto error;
      return new ::java::lang::Integer (val);
      break;

    case _Jv_SO_REUSEADDR_ :
      throw new ::java::net::SocketException
        (JvNewStringUTF ("SO_REUSEADDR: not valid for TCP"));
      break;

    case _Jv_SO_TIMEOUT_ :
      return new ::java::lang::Integer (timeout);
      break;

    default :
      WSASetLastError (WSAENOPROTOOPT);
    }

error:
  _Jv_ThrowSocketException ();
  return 0;
    // we should never get here
}

void
gnu::java::net::PlainSocketImpl::shutdownInput (void)
{
  if (::shutdown (native_fd, 0))
    _Jv_ThrowSocketException ();
}

void
gnu::java::net::PlainSocketImpl::shutdownOutput (void)
{
  if (::shutdown (native_fd, 1))
    _Jv_ThrowSocketException ();
}
