/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

// We want to make sure to pick up the POSIX ctime_r.  Some systems,
// such as Solaris 2.6, have their own version as well.
#ifdef HAVE_CTIME_R
#define _POSIX_PTHREAD_SEMANTICS
#endif

#include <cni.h>
#include <java/util/Date.h>
#include <java/lang/String.h>

#include <time.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if HAVE_CTIME_R
/* Use overload resolution to find out the signature of ctime_r.  */

  /* This is Posix ctime_r().  */
template <typename T_clock, typename T_buf, size_t buflen>
static inline char *
ctime_adaptor (char* (*ctime_r)(T_clock *clock, T_buf *buf),
	       time_t *clock, char (&buf)[buflen])
{
  return ctime_r(clock, buf);
}

/* This is an old-style ctime_r, used on IRIX 5.2.  */
template <typename T_clock, typename T_buf, typename T_buflen, size_t buflen>
static inline char *
ctime_adaptor (char* (*ctime_r)(T_clock *clock, T_buf *buf, T_buflen len),
	       time_t *clock, char (&buf)[buflen])
{
  return ctime_r(clock, buf, buflen);
}
#endif

jstring
java::util::Date::toString()
{
#ifdef HAVE_CTIME_R
  time_t t = millis / 1000;
  char buf[30];
  return JvNewStringLatin1 (ctime_adaptor (ctime_r, &t, buf));
#elif defined (HAVE_CTIME)
  // FIXME: this isn't thread-safe.
  time_t t = millis / 1000;
  return JvNewStringLatin1 (ctime (&t));
#else
  return NULL;
#endif
}
