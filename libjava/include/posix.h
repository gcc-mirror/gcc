// posix.h -- Helper functions for POSIX-flavored OSs.

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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

extern int _Jv_select (int n, fd_set *, fd_set *, fd_set *, struct timeval *);
extern void _Jv_gettimeofday (struct timeval *);
