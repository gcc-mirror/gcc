// win32.h -- Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_WIN32_H__
#define __JV_WIN32_H__

#include <windows.h>
#undef STRICT

#include <ws2tcpip.h>
#include <gcj/cni.h>
#include <java/util/Properties.h>

#include <io.h>

// Prefix and suffix for shared libraries.
#define _Jv_platform_solib_prefix ""
#define _Jv_platform_solib_suffix ".dll"

#ifndef DISABLE_JAVA_NET

// these errors cannot occur on Win32
#define ENOTCONN 0
#define ECONNRESET 0

/* This is incorrect, but allows java/net/natPlainDatagramSocketImpl.cc
   to compile under MingW. This will be remedied in a subsequent gcj
   release where the Win32 and Posix networking code have been forked.  */
#define ECONNREFUSED 0

#ifndef ENOPROTOOPT
#define ENOPROTOOPT 109
#endif

#endif // DISABLE_JAVA_NET

extern void _Jv_platform_initialize (void);
extern void _Jv_platform_initProperties (java::util::Properties*);
extern jlong _Jv_platform_gettimeofday ();
extern int _Jv_select (int n, fd_set *, fd_set *, fd_set *, struct timeval *);

inline void
_Jv_platform_close_on_exec (jint)
{
  // Ignore.
}

#ifdef JV_HASH_SYNCHRONIZATION
/* Suspends the execution of the current thread for the specified
   number of microseconds.  Tries to emulate the behaviour of usleep()
   on UNIX and provides a granularity of 1 millisecond.  */
inline void
_Jv_platform_usleep (unsigned long usecs)
{
  if (usecs > 0UL)
    {
      unsigned long millis = ((usecs + 999UL) / 1000UL);
      Sleep (millis);
    }
}
#endif /* JV_HASH_SYNCHRONIZATION */

#ifndef DISABLE_JAVA_NET

static inline int
_Jv_socket (int domain, int type, int protocol)
{
  return ::socket (domain, type, protocol);
}

inline int
_Jv_connect (jint fd, sockaddr *ptr, int len)
{
  return ::connect (fd, ptr, len);
}

inline int
_Jv_close (jint fd)
{
  return ::closesocket (fd);
}

inline int
_Jv_bind (int fd, struct sockaddr *addr, int addrlen)
{
  return ::bind (fd, addr, addrlen);
}

inline int
_Jv_accept (int fd, struct sockaddr *addr, socklen_t *addrlen)
{
  return ::accept (fd, addr, addrlen);
}

inline int
_Jv_listen (int fd, int backlog)
{
  return ::listen (fd, backlog);
}

inline int
_Jv_write(int s, void *buf, int len)
{
  return ::send (s, (char*) buf, len, 0);
}

inline int
_Jv_read(int s, void *buf, int len)
{
  return ::recv (s, (char*) buf, len, 0);
}

#endif /* DISABLE_JAVA_NET */

/* Store up to SIZE return address of the current program state in
   ARRAY and return the exact number of values stored.  */
extern int backtrace (void **__array, int __size);

#endif /* __JV_WIN32_H__ */
