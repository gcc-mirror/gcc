/* javanio.c -- implementations of functions in javanio.h.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version.  */


/*
 * Note, because these functions are trivial, and should be inlined,
 * we include this file in the header, and do not compile it.
 */

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <sys/uio.h>

CPNIO_EXPORT ssize_t
cpnio_read (int fd, void *buf, size_t nbytes)
{
  return read (fd, buf, nbytes);
}

CPNIO_EXPORT ssize_t
cpnio_readv (int fd, const struct iovec *iov, int iovcnt)
{
  return readv (fd, iov, iovcnt);
}

CPNIO_EXPORT ssize_t
cpnio_write (int fd, const void *buf, size_t nbytes)
{
  return write (fd, buf, nbytes);
}

CPNIO_EXPORT ssize_t
cpnio_writev (int fd, const struct iovec *iov, size_t iovcnt)
{
  return writev (fd, iov, iovcnt);
}

CPNIO_EXPORT int
cpnio_socket (int domain, int type, int protocol)
{
  return socket (domain, type, protocol);
}

CPNIO_EXPORT int
cpnio_connect (int fd, const struct sockaddr *addr, socklen_t addrlen)
{
  return connect (fd, addr, addrlen);
}

CPNIO_EXPORT int
cpnio_accept (int fd, struct sockaddr *addr, socklen_t *addrlen)
{
  fd_set rset;
  struct timeval tv;
  socklen_t tvlen = sizeof(tv);
  int ret;

  tv.tv_sec = 0;
  tv.tv_usec = 0;
  getsockopt (fd, SOL_SOCKET, SO_RCVTIMEO, &tv, &tvlen);
  if (tv.tv_sec > 0 || tv.tv_usec > 0)
    {
      FD_ZERO(&rset);
      FD_SET(fd,&rset);
      ret = select (fd+1,&rset,NULL,NULL,&tv);
      if (ret == 0)
        {
          errno = EAGAIN;
          return -1;
        }
    }
  return accept (fd, addr, addrlen);
}

CPNIO_EXPORT ssize_t
cpnio_sendto (int fd, const void *msg, size_t len, int flags,
              const struct sockaddr *to, socklen_t tolen)
{
  return sendto (fd, msg, len, flags, to, tolen);
}

CPNIO_EXPORT ssize_t
cpnio_recvfrom (int fd, void *buf, size_t len, int flags,
                struct sockaddr *from, socklen_t *fromlen)
{
  return recvfrom (fd, buf, len, flags, from, fromlen);
}

CPNIO_EXPORT int
cpnio_fcntl (int fd, int cmd, long arg)
{
#ifdef HAVE_FCNTL
  return fcntl (fd, cmd, arg);
#else
  errno = ENOSUP;
  return -1;
#endif /* HAVE_FCNTL */
}

CPNIO_EXPORT int
cpnio_select (int nfds, fd_set *readfds, fd_set *writefds,
              fd_set *excepfds, struct timeval *timeo)
{
  return select (nfds, readfds, writefds, excepfds, timeo);
}
