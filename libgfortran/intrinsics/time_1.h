/* Implementation of the CPU_TIME intrinsic.
   Copyright (C) 2003, 2007 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef LIBGFORTRAN_TIME_H
#define LIBGFORTRAN_TIME_H

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* The time related intrinsics (DTIME, ETIME, CPU_TIME) to "compare
   different algorithms on the same computer or discover which parts
   are the most expensive", need a way to get the CPU time with the
   finest resolution possible. We can only be accurate up to
   microseconds.

   As usual with UNIX systems, unfortunately no single way is
   available for all systems.  */

#ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#else
#  if HAVE_SYS_TIME_H
#    include <sys/time.h>
#  else
#    ifdef HAVE_TIME_H
#      include <time.h>
#    endif
#  endif
#endif

/* The most accurate way to get the CPU time is getrusage (). */
#if defined (HAVE_GETRUSAGE) && defined (HAVE_SYS_RESOURCE_H)
#  include <sys/resource.h>
#endif  /* HAVE_GETRUSAGE && HAVE_SYS_RESOURCE_H  */

#if defined (__GNUC__) && (__GNUC__ >= 3)
#  define ATTRIBUTE_ALWAYS_INLINE __attribute__ ((__always_inline__))
#else
#  define ATTRIBUTE_ALWAYS_INLINE
#endif

static inline int __time_1 (long *, long *, long *, long *) ATTRIBUTE_ALWAYS_INLINE;

/* Helper function for the actual implementation of the DTIME, ETIME and
   CPU_TIME intrinsics.  Returns a CPU time in microseconds or -1 if no
   CPU time could be computed.  */

#ifdef __MINGW32__

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static int
__time_1 (long *user_sec, long *user_usec, long *system_sec, long *system_usec)
{
  union {
    FILETIME ft;
    unsigned long long ulltime;
  } kernel_time,  user_time;

  FILETIME unused1, unused2;
  unsigned long long total_time;

  /* No support for Win9x.  The high order bit of the DWORD
     returned by GetVersion is 0 for NT and higher. */
  if (GetVersion () >= 0x80000000)
    {
      *user_sec = *system_sec = 0;
      *user_usec = *system_usec = 0;
      return -1;
    }

  /* The FILETIME structs filled in by GetProcessTimes represent
     time in 100 nanosecond units. */
  GetProcessTimes (GetCurrentProcess (), &unused1, &unused2,
              	   &kernel_time.ft, &user_time.ft);

  *user_sec = user_time.ulltime / 10000000;
  *user_usec = (user_time.ulltime % 10000000) / 10;

  *system_sec = kernel_time.ulltime / 10000000;
  *system_usec = (kernel_time.ulltime % 10000000) / 10;
  return 0;
}

#else

static inline int
__time_1 (long *user_sec, long *user_usec, long *system_sec, long *system_usec)
{
#if defined (HAVE_GETRUSAGE) && defined (HAVE_SYS_RESOURCE_H)
  struct rusage usage;
  getrusage (0, &usage);

  *user_sec = usage.ru_utime.tv_sec;
  *user_usec = usage.ru_utime.tv_usec;
  *system_sec = usage.ru_stime.tv_sec;
  *system_usec = usage.ru_stime.tv_usec;
  return 0;

#else /* ! HAVE_GETRUSAGE || ! HAVE_SYS_RESOURCE_H  */

  /* We have nothing to go on.  Return -1.  */
  *user_sec = *system_sec = 0;
  *user_usec = *system_usec = 0;
  return -1;

#endif
}

#endif


#endif /* LIBGFORTRAN_TIME_H */
