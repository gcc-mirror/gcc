// natVMThrowable.cc - native helper methods for Throwable

/* Copyright (C) 2000, 2002  Free Software Foundation, Inc

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Andrew Haley <aph@cygnus.com>
 * @author Mark Wielaard <mark@klomp.org>
 *
 * Native helper methods for VM specific Throwable support.
 */

#include <config.h>

#include <string.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/Object.h>
#include <java-threads.h>
#include <java/lang/Throwable.h>
#include <java/lang/VMThrowable.h>

#include <sys/types.h>

#include <stdlib.h>

#include <unistd.h>

#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

/* FIXME: size of the stack trace is limited to 128 elements.  It's
   undoubtedly sensible to limit the stack trace, but 128 is rather
   arbitrary.  It may be better to configure this.  */

java::lang::VMThrowable *
java::lang::VMThrowable::fillInStackTrace (java::lang::Throwable* t)
{
  if (! trace_enabled)
    return NULL;
#if defined (HAVE_BACKTRACE)
  VMThrowable* state = new VMThrowable;
  void *p[128];
  
  // We subtract 1 from the number of elements because we don't want
  // to include the calls to fillInStackTrace in the trace.
  int n = backtrace (p, 128) - 1;  

  void **addrs;
  if (n > 0)
    {
      state->length = n;
      addrs = (void **) _Jv_Malloc (n * sizeof p[0]);
      while (n--)
	addrs[n] = p[n];
    }
  else
    addrs = NULL;

  state->stackTraceAddrs = reinterpret_cast<gnu::gcj::RawData *> (addrs);

  return state;
#endif
  return NULL;
}
