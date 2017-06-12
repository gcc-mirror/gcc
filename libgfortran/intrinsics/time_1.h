/* Wrappers for platform timing functions.
   Copyright (C) 2003-2017 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef LIBGFORTRAN_TIME_H
#define LIBGFORTRAN_TIME_H

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <errno.h>

/* The time related intrinsics (DTIME, ETIME, CPU_TIME) to "compare
   different algorithms on the same computer or discover which parts
   are the most expensive", need a way to get the CPU time with the
   finest resolution possible. We can only be accurate up to
   microseconds.

   As usual with UNIX systems, unfortunately no single way is
   available for all systems.  */

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include <time.h>

#ifdef HAVE_SYS_TYPES_H
     #include <sys/types.h>
#endif

/* The most accurate way to get the CPU time is getrusage (). */
#if defined (HAVE_GETRUSAGE) && defined (HAVE_SYS_RESOURCE_H)
#  include <sys/resource.h>
#endif  /* HAVE_GETRUSAGE && HAVE_SYS_RESOURCE_H  */

/* The most accurate way to get the CPU time is getrusage ().
   If we have times(), that's good enough, too.  */
#if !defined (HAVE_GETRUSAGE) || !defined (HAVE_SYS_RESOURCE_H)
/* For times(), we _must_ know the number of clock ticks per second.  */
#  if defined (HAVE_TIMES) && (defined (HZ) || defined (_SC_CLK_TCK) || defined (CLK_TCK))
#    ifdef HAVE_SYS_PARAM_H
#      include <sys/param.h>
#    endif
#    if defined (HAVE_SYS_TIMES_H)
#      include <sys/times.h>
#    endif
#    ifndef HZ
#      if defined _SC_CLK_TCK
#        define HZ  sysconf(_SC_CLK_TCK)
#      else
#        define HZ  CLK_TCK
#      endif
#    endif
#  endif  /* HAVE_TIMES etc.  */
#endif  /* !HAVE_GETRUSAGE || !HAVE_SYS_RESOURCE_H  */


/* If the re-entrant version of localtime is not available, provide a
   fallback implementation.  On some targets where the _r version is
   not available, localtime uses thread-local storage so it's
   threadsafe.  */

#ifndef HAVE_LOCALTIME_R
/* If _POSIX is defined localtime_r gets defined by mingw-w64 headers.  */
#ifdef localtime_r
#undef localtime_r
#endif

static inline struct tm *
localtime_r (const time_t * timep, struct tm * result)
{
  *result = *localtime (timep);
  return result;
}
#endif


/* Helper function for the actual implementation of the DTIME, ETIME and
   CPU_TIME intrinsics.  Returns 0 for success or -1 if no
   CPU time could be computed.  */

#if defined(__MINGW32__)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline int
gf_cputime (long *user_sec, long *user_usec, long *system_sec, long *system_usec)
{
  union {
    FILETIME ft;
    unsigned long long ulltime;
  } kernel_time,  user_time;

  FILETIME unused1, unused2;

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
gf_cputime (long *user_sec, long *user_usec, long *system_sec, long *system_usec)
{
#if defined (HAVE_GETRUSAGE) && defined (HAVE_SYS_RESOURCE_H)
  struct rusage usage;
  int err;
  err = getrusage (RUSAGE_SELF, &usage);

  *user_sec = usage.ru_utime.tv_sec;
  *user_usec = usage.ru_utime.tv_usec;
  *system_sec = usage.ru_stime.tv_sec;
  *system_usec = usage.ru_stime.tv_usec;
  return err;

#elif defined HAVE_TIMES
  struct tms buf;
  clock_t err;
  err = times (&buf);
  long hz = HZ;
  *user_sec = buf.tms_utime / hz;
  *user_usec = (buf.tms_utime % hz) * (1000000. / hz);
  *system_sec = buf.tms_stime / hz;
  *system_usec = (buf.tms_stime % hz) * (1000000. / hz);
  if ((err == (clock_t) -1) && errno != 0)
    return -1;
  return 0;

#elif defined(HAVE_CLOCK_GETTIME) && (defined(CLOCK_PROCESS_CPUTIME_ID) \
				      || defined(CLOCK_THREAD_CPUTIME_ID))
  /* Newer versions of VxWorks have CLOCK_THREAD_CPUTIME_ID giving
     per-thread CPU time.  CLOCK_PROCESS_CPUTIME_ID would be better
     but is not available.  */
#ifndef CLOCK_PROCESS_CPUTIME_ID
#define CLOCK_PROCESS_CPUTIME_ID CLOCK_THREAD_CPUTIME_ID
#endif
  struct timespec ts;
  int err = clock_gettime (CLOCK_PROCESS_CPUTIME_ID, &ts);
  *user_sec = ts.tv_sec;
  *user_usec = ts.tv_nsec / 1000;
  *system_sec = *system_usec = 0;
  return err;

#else 
  clock_t c = clock ();
  *user_sec = c / CLOCKS_PER_SEC;
  *user_usec = (c % CLOCKS_PER_SEC) * (1000000. / CLOCKS_PER_SEC);
  *system_sec = *system_usec = 0;
  if (c == (clock_t) -1)
    return -1;
  return 0;

#endif
}

#endif


/* Realtime clock with microsecond resolution, falling back to other
   functions if the target does not support gettimeofday().

   Arguments:
   secs     - OUTPUT, seconds
   usecs    - OUTPUT, microseconds

   The OUTPUT arguments shall represent the number of seconds and
   microseconds since the Epoch.

   Return value: 0 for success, -1 for error. In case of error, errno
   is set.
*/
static inline int
gf_gettime (time_t * secs, long * usecs)
{
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tv;
  int err;
  err = gettimeofday (&tv, NULL);
  *secs = tv.tv_sec;
  *usecs = tv.tv_usec;
  return err;
#elif defined(HAVE_CLOCK_GETTIME)
  struct timespec ts;
  int err = clock_gettime (CLOCK_REALTIME, &ts);
  *secs = ts.tv_sec;
  *usecs = ts.tv_nsec / 1000;
  return err;
#else
  time_t t = time (NULL);
  *secs = t;
  *usecs = 0;
  if (t == ((time_t)-1))
    return -1;
  return 0;
#endif
}


#endif /* LIBGFORTRAN_TIME_H */
