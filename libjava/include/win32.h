// win32.h -- Helper functions for Microsoft-flavored OSs.

/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_WIN32_H__
#define __JV_WIN32_H__

#include <windows.h>
#undef STRICT

#undef __INSIDE_CYGWIN__
#include <winsock.h>
#define IP_TOS 3
#include <gcj/cni.h>
#include <java/util/Properties.h>

#include <io.h>

// these errors cannot occur on Win32
#define ENOTCONN 0
#define ECONNRESET 0

#ifndef ENOPROTOOPT
#define ENOPROTOOPT 109
#endif

extern void _Jv_platform_initialize (void);
extern void _Jv_platform_initProperties (java::util::Properties*);
extern jlong _Jv_platform_gettimeofday ();

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

inline void
_Jv_platform_close_on_exec (jint)
{
  // Ignore.
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
#define HAVE_BACKTRACE

/* Store up to SIZE return address of the current program state in
   ARRAY and return the exact number of values stored.  */
extern int backtrace (void **__array, int __size);

#endif /* __JV_WIN32_H__ */
