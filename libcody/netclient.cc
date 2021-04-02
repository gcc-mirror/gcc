// CODYlib		-*- mode:c++ -*-
// Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
// License: Apache v2.0

// Cody
#include "internal.hh"

#if CODY_NETWORKING
// C
#include <cerrno>
#include <cstring>
// OS
#include <netdb.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/un.h>

#ifndef AI_NUMERICSERV
#define AI_NUMERICSERV 0
#endif

// Client-side networking helpers

namespace Cody {

int OpenSocket (char const **e, sockaddr const *addr, socklen_t len)
{
  char const *errstr = nullptr;

  int fd = socket (addr->sa_family, SOCK_STREAM, 0);
  if (fd < 0)
    {
      errstr = "creating socket";

    fail:;
      int err = errno;
      if (e)
	*e = errstr;
      if (fd >= 0)
	close (fd);
      errno = err;
      return -1;
    }

  if (connect (fd, addr, len) < 0)
    {
      errstr = "connecting socket";
      goto fail;
    }

  return fd;
}

int OpenLocal (char const **e, char const *name)
{
  sockaddr_un addr;
  size_t len = strlen (name);

  if (len >= sizeof (addr.sun_path))
    {
      errno = ENAMETOOLONG;
      return -1;
    }

  memset (&addr, 0, offsetof (sockaddr_un, sun_path));
  addr.sun_family = AF_UNIX;
  memcpy (addr.sun_path, name, len + 1);
  return OpenSocket (e, (sockaddr *)&addr, sizeof (addr));
}

int OpenInet6 (char const **e, char const *name, int port)
{
  addrinfo *addrs = nullptr;
  int fd = -1;
  char const *errstr = nullptr;

  fd = socket (AF_INET6, SOCK_STREAM, 0);
  if (fd < 0)
    {
      errstr = "socket";

    fail:;
      int err = errno;
      if (e)
	*e = errstr;
      if (fd >= 0)
	close (fd);
      if (addrs)
	freeaddrinfo (addrs);
      errno = err;
      return -1;
    }

  addrinfo hints;
  hints.ai_flags = AI_NUMERICSERV;
  hints.ai_family = AF_INET6;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = 0;
  hints.ai_addrlen = 0;
  hints.ai_addr = nullptr;
  hints.ai_canonname = nullptr;
  hints.ai_next = nullptr;

  /* getaddrinfo requires a port number, but is quite happy to accept
     invalid ones.  So don't rely on it.  */
  if (int err = getaddrinfo (name, "0", &hints, &addrs))
    {
      errstr = gai_strerror (err);
      // What's the best errno to set?
      errno = 0;
      goto fail;
    }

  sockaddr_in6 addr;
  memset (&addr, 0, sizeof (addr));
  addr.sin6_family = AF_INET6;

  for (struct addrinfo *next = addrs; next; next = next->ai_next)
    if (next->ai_family == AF_INET6
	&& next->ai_socktype == SOCK_STREAM)
      {
	sockaddr_in6 *in6 = (sockaddr_in6 *)next->ai_addr;
	in6->sin6_port = htons (port);
	if (ntohs (in6->sin6_port) != port)
	  errno = EINVAL;
	else if (!connect (fd, next->ai_addr, next->ai_addrlen))
	  goto done;
      }
  errstr = "connecting";
  goto fail;

 done:;
  freeaddrinfo (addrs);

  return fd;
}

}

#endif
