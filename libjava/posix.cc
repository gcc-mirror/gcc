// posix.cc -- Helper functions for POSIX-flavored OSs.

/* Copyright (C) 2000, 2001, 2002, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include "posix.h"

#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include <jvm.h>
#include <java-stack.h>
#include <java/lang/Thread.h>
#include <java/io/InterruptedIOException.h>
#include <java/util/Properties.h>

#if defined (ECOS)
extern "C" unsigned long long _clock (void);
#endif

#if defined(HAVE_PROC_SELF_EXE)
static char exec_name[20];
  // initialized in _Jv_platform_initialize()
#endif

const char *_Jv_ThisExecutable (void)
{
#if defined(DISABLE_MAIN_ARGS)
  return "[Embedded App]";
#elif defined(HAVE_PROC_SELF_EXE)
  return exec_name;
    // initialized in _Jv_platform_initialize()
#else
  return _Jv_GetSafeArg (0);
#endif
}

// gettimeofday implementation.
jlong
_Jv_platform_gettimeofday ()
{
#if defined (HAVE_GETTIMEOFDAY)
  timeval tv;
  gettimeofday (&tv, NULL);
  return (tv.tv_sec * 1000LL) + (tv.tv_usec / 1000LL);
#elif defined (HAVE_TIME)
  return time (NULL) * 1000LL;
#elif defined (HAVE_FTIME)
  struct timeb t;
  ftime (&t);
  return (t.time * 1000LL) + t.millitm;
#elif defined (ECOS)
  // FIXME.
  return _clock();
#else
  // In the absence of any function, time remains forever fixed.
  return 23000;
#endif
}

jlong
_Jv_platform_nanotime ()
{
#ifdef HAVE_CLOCK_GETTIME
  struct timespec now;
  clockid_t id;
#ifdef CLOCK_MONOTONIC
  id = CLOCK_MONOTONIC;
#elif defined (CLOCK_HIGHRES)
  id = CLOCK_HIGHRES;
#else
  id = CLOCK_REALTIME;
#endif
  if (clock_gettime (id, &now) == 0)
    {
      jlong result = (jlong) now.tv_sec;
      result = result * 1000000000LL + now.tv_nsec;
      return result;
    }
  // clock_gettime failed, but we can fall through.
#endif // HAVE_CLOCK_GETTIME
#if defined (HAVE_GETTIMEOFDAY)
 {
   timeval tv;
   gettimeofday (&tv, NULL);
   return (tv.tv_sec * 1000000000LL) + tv.tv_usec * 1000LL;
 }
#else
  return _Jv_platform_gettimeofday () * 1000000LL;
#endif
}

// Platform-specific VM initialization.
void
_Jv_platform_initialize (void)
{
#if defined (HAVE_SIGACTION)
  // We only want this on POSIX systems.
  struct sigaction act;
  act.sa_handler = SIG_IGN;
  sigemptyset (&act.sa_mask);
  act.sa_flags = 0;
  sigaction (SIGPIPE, &act, NULL);
#else
  signal (SIGPIPE, SIG_IGN);
#endif

#if defined (HAVE_PROC_SELF_EXE)
  // Compute our executable name
  sprintf (exec_name, "/proc/%d/exe", getpid ());
#endif
}

// Set platform-specific System properties.
void
_Jv_platform_initProperties (java::util::Properties* newprops)
{
  // A convenience define.
#define SET(Prop,Val) \
  newprops->put(JvNewStringLatin1 (Prop), JvNewStringLatin1 (Val))

  SET ("file.separator", "/");
  SET ("path.separator", ":");
  SET ("line.separator", "\n");
  const char *tmpdir = ::getenv("TMPDIR");
  if (! tmpdir)
    tmpdir = "/tmp";
  SET ("java.io.tmpdir", tmpdir);
  const char *zoneinfodir = ::getenv("TZDATA");
  if (! zoneinfodir)
    zoneinfodir = "/usr/share/zoneinfo";
  SET ("gnu.java.util.zoneinfo.dir", zoneinfodir);
}

static inline void
internal_gettimeofday (struct timeval *result)
{
#if defined (HAVE_GETTIMEOFDAY)
  gettimeofday (result, NULL);
#else
  jlong val = _Jv_platform_gettimeofday ();
  result->tv_sec = val / 1000;
  result->tv_usec = (val % 1000) * 1000;
#endif /* HAVE_GETTIMEOFDAY */
}

// A wrapper for select() which ignores EINTR.
int
_Jv_select (int n, fd_set *readfds, fd_set  *writefds,
	    fd_set *exceptfds, struct timeval *timeout)
{
#ifdef HAVE_SELECT
  // If we have a timeout, compute the absolute ending time.
  struct timeval end, delay;
  if (timeout)
    {
      internal_gettimeofday (&end);
      end.tv_usec += timeout->tv_usec;
      if (end.tv_usec >= 1000000)
	{
	  ++end.tv_sec;
	  end.tv_usec -= 1000000;
	}
      end.tv_sec += timeout->tv_sec;
      delay = *timeout;
    }
  else
    {
      // Placate compiler.
      delay.tv_sec = delay.tv_usec = 0;
    }

  while (1)
    {
      int r = select (n, readfds, writefds, exceptfds,
		      timeout ? &delay : NULL);
      if (r != -1 || errno != EINTR)
	return r;

      // Here we know we got EINTR.
      if (java::lang::Thread::interrupted ())
	throw new java::io::InterruptedIOException (JvNewStringLatin1 ("select interrupted"));

      struct timeval after;
      if (timeout)
	{
	  internal_gettimeofday (&after);
	  // Now compute new timeout argument.
	  delay.tv_usec = end.tv_usec - after.tv_usec;
	  delay.tv_sec = end.tv_sec - after.tv_sec;
	  if (delay.tv_usec < 0)
	    {
	      --delay.tv_sec;
	      delay.tv_usec += 1000000;
	    }
	  if (delay.tv_sec < 0)
	    {
	      // We assume that the user wants a valid select() call
	      // more than precise timing.  So if we get a series of
	      // EINTR we just keep trying with delay 0 until we get a
	      // valid result.
	      delay.tv_sec = 0;
	    }
	}
    }
#else /* HAVE_SELECT */
  return 0;
#endif
}

// Given an address, find the object that defines it and the nearest
// defined symbol to that address.  Returns 0 if no object defines this
// address.
int
_Jv_platform_dladdr (void *addr, _Jv_AddrInfo *info)
{
  int ret_val = 0;

#if defined (HAVE_DLFCN_H) && defined (HAVE_DLADDR)
  Dl_info addr_info;
  ret_val = dladdr (addr, &addr_info);
  if (ret_val != 0)
    {
      info->file_name = addr_info.dli_fname;
      info->base = addr_info.dli_fbase;
      info->sym_name = addr_info.dli_sname;
      info->sym_addr = addr_info.dli_saddr;
    }
#else
  info->file_name = NULL;
  info->base = NULL;
  info->sym_name = NULL;
  info->sym_addr = NULL;
#endif

  return ret_val;
}
