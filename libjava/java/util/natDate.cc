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

jstring
java::util::Date::toString()
{
#ifdef HAVE_CTIME_R
  time_t t = millis / 1000;
  char buf[30];
  return JvNewStringLatin1 (ctime_r (&t, buf));
#elif defined (HAVE_CTIME)
  // FIXME: this isn't thread-safe.
  time_t t = millis / 1000;
  return JvNewStringLatin1 (ctime (&t));
#else
  return NULL;
#endif
}
