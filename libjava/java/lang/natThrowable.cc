// natThrowable.cc - Superclass for all exceptions.

/* Copyright (C) 2000  Free Software Foundation, Inc

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Andrew Haley <aph@cygnus.com>
 * @date Jan 6  2000
 */

#include <config.h>

#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Object.h>
#include <java-threads.h>
#include <java/lang/Throwable.h>
#include <java/lang/StackTraceElement.h>
#include <java/io/PrintStream.h>
#include <java/io/PrintWriter.h>
#include <java/io/IOException.h>

#include <sys/types.h>

#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>

#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#include <name-finder.h>

/* FIXME: size of the stack trace is limited to 128 elements.  It's
   undoubtedly sensible to limit the stack trace, but 128 is rather
   arbitrary.  It may be better to configure this.  */

java::lang::Throwable *
java::lang::Throwable::fillInStackTrace (void)
{
  if (! trace_enabled)
    return this;
#if defined (HAVE_BACKTRACE)
  void *p[128];
  
  // We subtract 1 from the number of elements because we don't want
  // to include the call to fillInStackTrace in the trace.
  int n = backtrace (p, 128) - 1;  

  if (n > 0)
    {
      // We copy the array below to deal with alignment issues.
      stackTraceBytes = JvNewByteArray (n * sizeof p[0]);
      memcpy (elements (stackTraceBytes), p+1, (n * sizeof p[0]));
    }

#endif

  return this;
}

JArray<java::lang::StackTraceElement*> *
java::lang::Throwable::getStackTrace0 ()
{
#ifdef HAVE_BACKTRACE
  if (!stackTraceBytes)
    return NULL;

  int depth = stackTraceBytes->length / sizeof (void *);
  void *p[depth];
  // This memcpy is esential; it ensures that the array of void* is
  // correctly aligned.
  memcpy (p, elements (stackTraceBytes), sizeof p);

  JArray<java::lang::StackTraceElement*> *result;
  java::lang::StackTraceElement** el;
  result = reinterpret_cast <JArray<java::lang::StackTraceElement *>*>
    (JvNewObjectArray (depth, &java::lang::StackTraceElement::class$, NULL));
  el = elements (result);

  _Jv_name_finder finder (_Jv_ThisExecutable ());

  for (int i = 0; i < depth; i++)
    el[i] = finder.lookup (p[i]);

  return result;
#else
  return NULL;
#endif /* HAVE_BACKTRACE */
}
