// posix.h -- Helper functions for POSIX-flavored OSs.

/* Copyright (C) 2000, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_POSIX_H__
#define __JV_POSIX_H__

/* Required on Tru64 UNIX V4/V5 so <sys/socket.h> defines prototypes of
   socket functions with socklen_t instead of size_t.  This must be defined
   early so <standards.h> defines the correct version of __PIIX.  */
#define _POSIX_PII_SOCKET

#include <time.h>
#include <sys/types.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <fcntl.h>

#include <gcj/cni.h>
#include <java/util/Properties.h>

// Prefix and suffix for shared libraries.
#define _Jv_platform_solib_prefix "lib"
#define _Jv_platform_solib_suffix ".so"

#ifndef DISABLE_JAVA_NET
#include <java/net/InetAddress.h>
#endif

extern int _Jv_select (int n, fd_set *, fd_set *, fd_set *, struct timeval *);
extern jlong _Jv_platform_gettimeofday ();
extern void _Jv_platform_initialize (void);
extern void _Jv_platform_initProperties (java::util::Properties*);

inline void
_Jv_platform_close_on_exec (jint fd)
{
  // Ignore errors.
  ::fcntl (fd, F_SETFD, FD_CLOEXEC);
}

#undef fcntl

#ifdef JV_HASH_SYNCHRONIZATION
inline void
_Jv_platform_usleep (unsigned long usecs)
{
  usleep (usecs);
}
#endif /* JV_HASH_SYNCHRONIZATION */

#ifndef DISABLE_JAVA_NET

#ifndef HAVE_SOCKLEN_T
#define socklen_t int
#endif

static inline int
_Jv_socket (int domain, int type, int protocol)
{
  return ::socket (domain, type, protocol);
}

#undef socket

inline int
_Jv_connect (jint fd, sockaddr *ptr, int len)
{
   return ::connect (fd, ptr, len);
}

#undef connect

inline int
_Jv_close (jint fd)
{
  return ::close (fd);
}

#undef close

// Avoid macro definitions of bind from system headers, e.g. on
// Solaris 7 with _XOPEN_SOURCE.  FIXME
inline int
_Jv_bind (int fd, struct sockaddr *addr, int addrlen)
{
  return ::bind (fd, addr, addrlen);
}

#undef bind

// Same problem with accept on Tru64 UNIX with _POSIX_PII_SOCKET
inline int
_Jv_accept (int fd, struct sockaddr *addr, socklen_t *addrlen)
{
  return ::accept (fd, addr, addrlen);
}

#undef accept

inline int
_Jv_listen (int fd, int backlog)
{
  return ::listen (fd, backlog);
}

#undef listen

inline int
_Jv_write(int s, void *buf, int len)
{
  return ::write (s, buf, len);
}

#undef write

inline int
_Jv_read(int s, void *buf, int len)
{
  return ::read (s, buf, len);
}

#undef read

#endif /* DISABLE_JAVA_NET */

#endif /* __JV_POSIX_H__ */
