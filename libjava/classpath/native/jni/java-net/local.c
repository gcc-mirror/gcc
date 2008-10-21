/* local.c -- implementation of unix-domain sockets.
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


#include "config.h"

#ifdef ENABLE_LOCAL_SOCKETS

#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <stdio.h>

#if defined(HAVE_SYS_IOCTL_H)
#define BSD_COMP /* Get FIONREAD on Solaris2 */
#include <sys/ioctl.h>
#endif
#if defined(HAVE_SYS_FILIO_H) /* Get FIONREAD on Solaris 2.5 */
#include <sys/filio.h>
#endif

#include "local.h"

const char *
local_error (void)
{
  return strerror (errno);
}

int
local_create (int stream)
{
  return socket (PF_UNIX, stream ? SOCK_STREAM : SOCK_DGRAM, 0);
}

int
local_bind (int fd, const char *addr)
{
  struct sockaddr_un saddr;

  if (strlen (addr) >= sizeof (saddr.sun_path))
    {
      errno = ENAMETOOLONG;
      return -1;
    }

  strcpy (saddr.sun_path, addr);
  saddr.sun_family = AF_LOCAL;

  return bind (fd, (struct sockaddr *) &saddr, SUN_LEN (&saddr));
}

int
local_listen (int fd, int backlog)
{
  return listen (fd, backlog);
}

int
local_accept (int fd, char *path)
{
  int newfd;
  struct sockaddr_un addr;
  socklen_t sz = SUN_LEN(&addr);

  newfd = accept (fd, (struct sockaddr *) &addr, &sz);
  if (newfd >= 0)
    {
      /** sun_path is some crazy statically-sized buffer, and it's
          size is different on different OSes. */
      int n = sizeof (addr.sun_path);
      strncpy (path, addr.sun_path, n);
      path[n] = '\0';
    }
  return newfd;
}

int
local_available (int fd)
{
  int val;
  if (ioctl (fd, FIONREAD, &val))
    {
      return -1;
    }
  return val;
}

int
local_close (int fd)
{
  return close (fd);
}

int
local_unlink (char *path)
{
  return unlink (path);
}

int
local_shutdown_input (int fd)
{
  return shutdown (fd, 0);
}

int
local_shutdown_output (int fd)
{
  return shutdown (fd, 1);
}

int
local_connect (int fd, char *path)
{
  struct sockaddr_un saddr;

  strncpy (saddr.sun_path, path, sizeof (saddr.sun_path));
  saddr.sun_path[sizeof (saddr.sun_path) - 1] = '\0';
  saddr.sun_family = AF_UNIX;

  return connect (fd, (struct sockaddr *) &saddr, SUN_LEN(&saddr));
}

int
local_read (int fd, void *buf, int len)
{
  int count = -1;
  do
    {
      count = read (fd, buf, len);
    }
  while (count == -1 && errno == EINTR);
  return count;
}

int
local_write (int fd, void *buf, int len)
{
  int count = -1;
  do
    {
      count = write (fd, buf, len);
    }
  while (count == -1 && errno == EINTR);
  return count;
}

#endif /* ENABLE_LOCAL_SOCKETS */
