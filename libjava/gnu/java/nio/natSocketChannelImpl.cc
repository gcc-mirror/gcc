// natSocketChannelImpl.cc

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <errno.h>

#include <gcj/cni.h>
#include <gnu/java/nio/SocketChannelImpl.h>
#include <java/io/IOException.h>
#include <java/net/InetAddress.h>
#include <java/net/SocketException.h>


#ifdef DISABLE_JAVA_NET

jint
gnu::java::nio::SocketChannelImpl::SocketCreate ()
{
  throw new ::java::io::IOException (JvNewStringUTF ("SocketCreate not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketConnect (jint,
                                                  ::java::net::InetAddress *,
                                                  jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("SocketConnect not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketBind (jint, ::java::net::InetAddress *,
                                               jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("SocketBind not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketListen (jint, jint)
{
  throw new ::java::io::IOException (JvNewStringUTF ("SocketList not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketAvailable (jint)
{
  throw new ::java::net::SocketException (JvNewStringLatin1 ("SocketAvailable: not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketClose (jint)
{
  throw new ::java::net::SocketException (JvNewStringLatin1 ("SocketClose: not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketRead (jint, jbyteArray, jint, jint)
{
  throw new ::java::net::SocketException (JvNewStringLatin1 ("SocketRead: not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketWrite (jint, jbyteArray, jint, jint)
{
  throw new ::java::net::SocketException (JvNewStringLatin1 ("SocketWrite: not implemented"));
}

#else // DISABLE_JAVA_NET

jint
gnu::java::nio::SocketChannelImpl::SocketCreate ()
{
  int sock = _Jv_socket (AF_INET, SOCK_STREAM, 0);

  if (sock < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  return sock;
}

jint
gnu::java::nio::SocketChannelImpl::SocketConnect (jint fd,
                                                  ::java::net::InetAddress *addr,
                                                  jint port)
{
  throw new ::java::io::IOException (JvNewStringUTF ("SocketConnect not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketBind (jint fd,
                                               ::java::net::InetAddress *addr,
                                               jint port)
{
  throw new ::java::io::IOException (JvNewStringUTF ("SocketBind not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketListen (jint fd, jint backlog)
{
  int result = _Jv_listen (fd, backlog);

  if (result < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  return result;
}

jint
gnu::java::nio::SocketChannelImpl::SocketAvailable (jint /*fd*/)
{
  throw new ::java::net::SocketException (JvNewStringLatin1 ("SocketAvailable: not implemented"));
}

jint
gnu::java::nio::SocketChannelImpl::SocketClose (jint fd)
{
  int result = _Jv_close (fd);

  if (result < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  return result;
}

jint
gnu::java::nio::SocketChannelImpl::SocketRead (jint fd, jbyteArray data,
                                               jint offset, jint length)
{
  /* The cast to char* is needed to placate the Win32 API.  */
  int result =
    ::recv (fd, reinterpret_cast<char*>(elements(data)), offset, length);

  if (result < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  return result;
}

jint
gnu::java::nio::SocketChannelImpl::SocketWrite (jint fd, jbyteArray data,
                                                jint offset, jint length)
{
  /* The cast to char* is needed to placate the Win32 API. I used char*
     instead of const char* because I wasn't sure about the API on all
     UNICES.  */
  int result =
    ::send (fd, reinterpret_cast<char*>(elements(data)), offset, length);

  if (result < 0)
    {
      char* strerr = strerror (errno);
      throw new ::java::io::IOException (JvNewStringUTF (strerr));
    }

  return result;
}

#endif // DISABLE_JAVA_NET
