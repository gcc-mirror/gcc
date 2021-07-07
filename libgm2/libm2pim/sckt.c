/* sckt.c provide access to the socket layer.

Copyright (C) 2005-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include <config.h>

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#if defined(HAVE_SYS_SOCKET_H)
#include <sys/socket.h>
#endif

#if defined(HAVE_NETINET_IN_H)
#include <netinet/in.h>
#endif

#if defined(HAVE_NETDB_H)
#include <netdb.h>
#endif

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(HAVE_SIGNAL_H)
#include <signal.h>
#endif

#if defined(HAVE_SYS_ERRNO_H)
#include <sys/errno.h>
#endif

#if defined(HAVE_ERRNO_H)
#include <errno.h>
#endif

#if defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif

#if defined(HAVE_STRING_H)
#include <string.h>
#endif

#if defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif

#if defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#define PORTSTART 7000
#define NOOFTRIES 100
#define MAXHOSTNAME 256

#undef DEBUGGING

#if !defined(TRUE)
#define TRUE (1 == 1)
#endif
#if !defined(FALSE)
#define FALSE (1 == 0)
#endif

#if defined(HAVE_SYS_SOCKET_H)

#define ERROR(X)                                                              \
  {                                                                           \
    printf ("%s:%d:%s\n", __FILE__, __LINE__, X);                             \
    localExit (1);                                                            \
  }

#define ASSERT(X)                                                             \
  {                                                                           \
    if (!(X))                                                                 \
      {                                                                       \
        printf ("%s:%d: assert(%s) failed\n", __FILE__, __LINE__, #X);        \
        exit (1);                                                             \
      }                                                                       \
  }

typedef struct
{
  char hostname[MAXHOSTNAME];
  struct hostent *hp;
  struct sockaddr_in sa, isa;
  int sockFd;
  int portNo;
} tcpServerState;

int
localExit (int i)
{
  exit (1);
}

/*
 *  tcpServerEstablishPort - returns a tcpState containing the relevant
 *                           information about a socket declared to recieve
 *                           tcp connections. This method attempts to use
 *                           the port specified by the parameter.
 */

tcpServerState *
tcpServerEstablishPort (int portNo)
{
  tcpServerState *s = (tcpServerState *)malloc (sizeof (tcpServerState));
  int b, p, n;

  if (s == NULL)
    ERROR ("no more memory");

  /* remove SIGPIPE which is raised on the server if the client is killed */
  signal (SIGPIPE, SIG_IGN);

  if (gethostname (s->hostname, MAXHOSTNAME) < 0)
    ERROR ("cannot find our hostname");

  s->hp = gethostbyname (s->hostname);
  if (s->hp == NULL)
    ERROR ("cannot get host name");

  p = -1;
  n = 0;
  do
    {
      p++;
      /*
       * Open a TCP socket (an Internet stream socket)
       */

      s->sockFd = socket (s->hp->h_addrtype, SOCK_STREAM, 0);
      if (s->sockFd < 0)
        ERROR ("socket");

      memset ((void *)&s->sa, 0, sizeof (s->sa));
      ASSERT ((s->hp->h_addrtype == AF_INET));
      s->sa.sin_family = s->hp->h_addrtype;
      s->sa.sin_addr.s_addr = htonl (INADDR_ANY);
      s->sa.sin_port = htons (portNo + p);

      b = bind (s->sockFd, (struct sockaddr *)&s->sa, sizeof (s->sa));
    }
  while ((b < 0) && (n < NOOFTRIES));

  if (b < 0)
    ERROR ("bind");

  s->portNo = portNo + p;
#if defined(DEBUGGING)
  printf ("the receiving host is: %s, the port is %d\n", s->hostname,
          s->portNo);
#endif
  listen (s->sockFd, 1);
  return s;
}

/*
 *  tcpServerEstablish - returns a tcpServerState containing the relevant
 *                       information about a socket declared to recieve
 *                       tcp connections.
 */

tcpServerState *
tcpServerEstablish (void)
{
  return tcpServerEstablishPort (PORTSTART);
}

/*
 *  tcpServerAccept - returns a file descriptor once a client has connected and
 *                    been accepted.
 */

int
tcpServerAccept (tcpServerState *s)
{
  int i = sizeof (s->isa);
  int t;

#if defined(DEBUGGING)
  printf ("before accept %d\n", s->sockFd);
#endif
  t = accept (s->sockFd, (struct sockaddr *)&s->isa, &i);
  return t;
}

/*
 *  tcpServerPortNo - returns the portNo from structure, s.
 */

int
tcpServerPortNo (tcpServerState *s)
{
  return s->portNo;
}

/*
 *  tcpServerSocketFd - returns the sockFd from structure, s.
 */

int
tcpServerSocketFd (tcpServerState *s)
{
  return s->sockFd;
}

/*
 *  getLocalIP - returns the IP address of this machine.
 */

unsigned int
getLocalIP (tcpServerState *s)
{
  char hostname[1024];
  struct hostent *hp;
  struct sockaddr_in sa;
  unsigned int ip;
  int ret = gethostname (hostname, sizeof (hostname));

  if (ret == -1)
    {
      ERROR ("gethostname");
      return 0;
    }

  hp = gethostbyname (hostname);
  if (hp == NULL)
    {
      ERROR ("gethostbyname");
      return 0;
    }

  if (sizeof (unsigned int) != sizeof (in_addr_t))
    {
      ERROR ("bad ip length");
      return 0;
    }

  memset (&sa, sizeof (struct sockaddr_in), 0);
  sa.sin_family = AF_INET;
  sa.sin_port = htons (80);
  if (hp->h_length == sizeof (unsigned int))
    {
      memcpy (&ip, hp->h_addr_list[0], hp->h_length);
      return ip;
    }

  return 0;
}

/*
 *  tcpServerIP - returns the IP address from structure, s.
 */

int
tcpServerIP (tcpServerState *s)
{
  return *((int *)s->hp->h_addr_list[0]);
}

/*
 *  tcpServerClientIP - returns the IP address of the client who
 *                      has connected to server, s.
 */

unsigned int
tcpServerClientIP (tcpServerState *s)
{
  unsigned int ip;

  ASSERT (s->isa.sin_family == AF_INET);
  ASSERT (sizeof (ip) == 4);
  memcpy (&ip, &s->isa.sin_addr, sizeof (ip));
  return ip;
}

/*
 *  tcpServerClientPortNo - returns the port number of the client who
 *                          has connected to server, s.
 */

unsigned int
tcpServerClientPortNo (tcpServerState *s)
{
  return s->isa.sin_port;
}

/*
****************************************************************
***             C L I E N T     R O U T I N E S
****************************************************************
 */

typedef struct
{
  char hostname[MAXHOSTNAME];
  struct hostent *hp;
  struct sockaddr_in sa;
  int sockFd;
  int portNo;
} tcpClientState;

/*
 *  tcpClientSocket - returns a file descriptor (socket) which has
 *                    connected to, serverName:portNo.
 */

tcpClientState *
tcpClientSocket (char *serverName, int portNo)
{
  tcpClientState *s = (tcpClientState *)malloc (sizeof (tcpClientState));

  if (s == NULL)
    ERROR ("no more memory");

  /* remove SIGPIPE which is raised on the server if the client is killed */
  signal (SIGPIPE, SIG_IGN);

  s->hp = gethostbyname (serverName);
  if (s->hp == NULL)
    {
      fprintf (stderr, "cannot find host: %s\n", serverName);
      exit (1);
    }

  memset ((void *)&s->sa, 0, sizeof (s->sa));
  s->sa.sin_family = AF_INET;
  memcpy ((void *)&s->sa.sin_addr, (void *)s->hp->h_addr, s->hp->h_length);
  s->portNo = portNo;
  s->sa.sin_port = htons (portNo);

  /*
   *  Open a TCP socket (an Internet stream socket)
   */

  s->sockFd = socket (s->hp->h_addrtype, SOCK_STREAM, 0);
  return s;
}

/*
 *  tcpClientSocketIP - returns a file descriptor (socket) which has
 *                      connected to, ip:portNo.
 */

tcpClientState *
tcpClientSocketIP (unsigned int ip, int portNo)
{
  tcpClientState *s = (tcpClientState *)malloc (sizeof (tcpClientState));

  if (s == NULL)
    ERROR ("no more memory");

  /* remove SIGPIPE which is raised on the server if the client is killed */
  signal (SIGPIPE, SIG_IGN);

  memset ((void *)&s->sa, 0, sizeof (s->sa));
  s->sa.sin_family = AF_INET;
  memcpy ((void *)&s->sa.sin_addr, (void *)&ip, sizeof (ip));
  s->portNo = portNo;
  s->sa.sin_port = htons (portNo);

  /*
   * Open a TCP socket (an Internet stream socket)
   */

  s->sockFd = socket (PF_INET, SOCK_STREAM, 0);
  return s;
}

/*
 *  tcpClientConnect - returns the file descriptor associated with, s,
 *                     once a connect has been performed.
 */

int
tcpClientConnect (tcpClientState *s)
{
  if (connect (s->sockFd, (struct sockaddr *)&s->sa, sizeof (s->sa)) < 0)
    ERROR ("failed to connect to the TCP server");

  return s->sockFd;
}

/*
 *  tcpClientPortNo - returns the portNo from structure, s.
 */

int
tcpClientPortNo (tcpClientState *s)
{
  return s->portNo;
}

/*
 *  tcpClientSocketFd - returns the sockFd from structure, s.
 */

int
tcpClientSocketFd (tcpClientState *s)
{
  return s->sockFd;
}

/*
 *  tcpClientIP - returns the sockFd from structure, s.
 */

int
tcpClientIP (tcpClientState *s)
{
#if defined(DEBUGGING)
  printf ("client ip = %s\n", inet_ntoa (s->sa.sin_addr.s_addr));
#endif
  return s->sa.sin_addr.s_addr;
}
#endif

/*
 *  GNU Modula-2 link fodder.
 */

void
_M2_sckt_init (void)
{
}

void
_M2_sckt_finish (void)
{
}
