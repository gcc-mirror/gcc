// posix.h -- Helper functions for POSIX-flavored OSs.

/* Copyright (C) 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <fcntl.h>

#include <gcj/cni.h>
#include <java/util/Properties.h>

extern int _Jv_select (int n, fd_set *, fd_set *, fd_set *, struct timeval *);
extern jlong _Jv_platform_gettimeofday ();
extern void _Jv_platform_initialize (void);
extern void _Jv_platform_initProperties (java::util::Properties*);

inline void
_Jv_platform_close_on_exec (jint fd)
{
  // Ignore errors.
  fcntl (fd, F_SETFD, FD_CLOEXEC);
}
