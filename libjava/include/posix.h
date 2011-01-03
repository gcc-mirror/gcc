// posix.h -- Helper functions for POSIX-flavored OSs.

/* Copyright (C) 2000, 2002, 2003, 2006, 2010  Free Software Foundation

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

/* The header file <sys/rw_lock.h> needs to be included before javaprims.h
   on HP-UX 11 to avoid a compilation error.  */
#ifdef HAVE_SYS_RW_LOCK_H
#include <sys/rw_lock.h>
#endif

#include <gcj/cni.h>
#include <java/util/Properties.h>

// Prefix and suffix for shared libraries.
#define _Jv_platform_solib_prefix "lib"
#if defined(__APPLE__) && defined(__MACH__)
#define _Jv_platform_solib_suffix ".dylib"
#elif defined(HPUX) && defined(HP_PA)
#define _Jv_platform_solib_suffix ".sl"
#else
#define _Jv_platform_solib_suffix ".so"
#endif

#if __MACH__ && (__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1060)
#  undef _Unwind_FindEnclosingFunction
#  define _Unwind_FindEnclosingFunction(PC) _darwin10_Unwind_FindEnclosingFunction(PC)
#endif

// Some POSIX systems don't have O_SYNC and O_DYSNC so we define them here.
// Needed in java/io/natFileDescriptorPosix.cc.
#if !defined (O_SYNC) && defined (O_FSYNC)
#define O_SYNC O_FSYNC
#endif
#if !defined (O_DSYNC) && defined (O_FSYNC)
#define O_DSYNC O_FSYNC
#endif
// If O_DSYNC is still not defined, use O_SYNC (needed for newlib)
#if !defined (O_DSYNC) 
#define O_DSYNC O_SYNC
#endif

// Name of the Process implementation.
#ifdef ECOS
#define _Jv_platform_process ::java::lang::EcosProcess
#else
#define _Jv_platform_process ::java::lang::PosixProcess
#endif

// Separator for file name components.
#define _Jv_platform_file_separator ((jchar) '/')
// Separator for path components.
#define _Jv_platform_path_separator ((jchar) ':')

// List of names for `JNI_OnLoad'.
#define _Jv_platform_onload_names { "JNI_OnLoad", NULL }

// Type of libffi ABI used by JNICALL methods.  NOTE: This must agree
// with the JNICALL definition in jni.h
#define _Jv_platform_ffi_abi FFI_DEFAULT_ABI

#ifndef DISABLE_JAVA_NET
#include <java/net/InetAddress.h>
#endif

extern int _Jv_select (int n, fd_set *, fd_set *, fd_set *, struct timeval *);
extern jlong _Jv_platform_gettimeofday ();
extern jlong _Jv_platform_nanotime ();
extern void _Jv_platform_initialize (void);
extern void _Jv_platform_initProperties (java::util::Properties*);

#ifdef JV_HASH_SYNCHRONIZATION
#ifndef HAVE_USLEEP_DECL
extern "C" int usleep (useconds_t useconds);
#endif /* not HAVE_USLEEP_DECL */

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

// Wraps ::pipe
static inline int
_Jv_pipe (int filedes[2])
{
  return ::pipe (filedes);
}

// Forward declaration.  See java-stack.h for definition.
struct _Jv_AddrInfo;

// Given an address, determine the executable or shared object that defines
// it and the nearest named symbol.
extern int _Jv_platform_dladdr (void *addr, _Jv_AddrInfo *info);

#endif /* __JV_POSIX_H__ */
