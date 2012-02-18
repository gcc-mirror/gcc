/* go-nosys.c -- functions missing from system.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* This file exists to provide definitions for functions that are
   missing from libc, according to the configure script.  This permits
   the Go syscall package to not worry about whether the functions
   exist or not.  */

#include "config.h"

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#ifndef HAVE_OFF64_T
typedef signed int off64_t __attribute__ ((mode (DI)));
#endif

#ifndef HAVE_LOFF_T
typedef off64_t loff_t;
#endif

#ifndef HAVE_EPOLL_CREATE1
int
epoll_create1 (int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_FACCESSAT
int
faccessat (int fd __attribute__ ((unused)),
	   const char *pathname __attribute__ ((unused)),
	   int mode __attribute__ ((unused)),
	   int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_FALLOCATE
int
fallocate (int fd __attribute__ ((unused)),
	   int mode __attribute__ ((unused)),
	   off_t offset __attribute __ ((unused)),
	   off_t len __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_FCHMODAT
int
fchmodat (int dirfd __attribute__ ((unused)),
	  const char *pathname __attribute__ ((unused)),
	  mode_t mode __attribute__ ((unused)),
	  int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_FCHOWNAT
int
fchownat (int dirfd __attribute__ ((unused)),
	  const char *pathname __attribute__ ((unused)),
	  uid_t owner __attribute__ ((unused)),
	  gid_t group __attribute__ ((unused)),
	  int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_FUTIMESAT
int
futimesat (int dirfd __attribute__ ((unused)),
	   const char *pathname __attribute__ ((unused)),
	   const struct timeval times[2] __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_INOTIFY_ADD_WATCH
int
inotify_add_watch (int fd __attribute__ ((unused)),
		   const char* pathname __attribute__ ((unused)),
		   uint32_t mask __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_INOTIFY_INIT
int
inotify_init (void)
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_INOTIFY_RM_WATCH
int
inotify_rm_watch (int fd __attribute__ ((unused)),
		  uint32_t wd __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_MKDIRAT
int
mkdirat (int dirfd __attribute__ ((unused)),
	 const char *pathname __attribute__ ((unused)),
	 mode_t mode __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_MKNODAT
int
mknodat (int dirfd __attribute__ ((unused)),
	 const char *pathname __attribute__ ((unused)),
	 mode_t mode __attribute__ ((unused)),
	 dev_t dev __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_OPENAT
int
openat (int dirfd __attribute__ ((unused)),
	const char *pathname __attribute__ ((unused)),
	int oflag __attribute__ ((unused)),
	...)
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_RENAMEAT
int
renameat (int olddirfd __attribute__ ((unused)),
	  const char *oldpath __attribute__ ((unused)),
	  int newdirfd __attribute__ ((unused)),
	  const char *newpath __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_SPLICE
int
splice (int fd __attribute__ ((unused)),
	loff_t *off_in __attribute__ ((unused)),
	int fd_out __attribute__ ((unused)),
	loff_t *off_out __attribute__ ((unused)),
	size_t len __attribute__ ((unused)),
	unsigned int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_TEE
int
tee (int fd_in __attribute__ ((unused)),
     int fd_out __attribute__ ((unused)),
     size_t len __attribute__ ((unused)),
     unsigned int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_UNLINKAT
int
unlinkat (int dirfd __attribute__ ((unused)),
	  const char *pathname __attribute__ ((unused)),
	  int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif

#ifndef HAVE_UNSHARE
int
unshare (int flags __attribute__ ((unused)))
{
  errno = ENOSYS;
  return -1;
}
#endif
