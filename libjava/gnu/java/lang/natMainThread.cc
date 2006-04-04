// natMainThread.cc - Implementation of MainThread native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2003, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-threads.h>

#include <gnu/java/lang/MainThread.h>
#include <java/lang/Runtime.h>
#include <java/lang/ThreadGroup.h>

typedef void main_func (jobject);

void
gnu::java::lang::MainThread::call_main (void)
{
  Utf8Const* main_signature = _Jv_makeUtf8Const ("([Ljava.lang.String;)V", 22);
  Utf8Const* main_name = _Jv_makeUtf8Const ("main", 4);

  _Jv_Method *meth = _Jv_LookupDeclaredMethod (klass, main_name,
					       main_signature);

  // Some checks from Java Spec section 12.1.4.
  const char *msg = NULL;
  if (meth == NULL)
    msg = "no suitable method `main' in class";
  else if (! ::java::lang::reflect::Modifier::isStatic(meth->accflags))
    msg = "`main' must be static";
  else if (! ::java::lang::reflect::Modifier::isPublic(meth->accflags))
    msg =  "`main' must be public";
  if (msg != NULL)
    {
      fprintf (stderr, "%s\n", msg);
      ::exit(1);
    }

  main_func *real_main = (main_func *) meth->ncode;
  (*real_main) (args);

  // Note that we do thread cleanup here.  We have to do this here and
  // not in _Jv_RunMain; if we do if after the main thread has exited,
  // our ThreadGroup will be null, and if Runtime.exit tries to create
  // a new Thread (which it does when running shutdown hooks), it will
  // eventually NPE due to this.
  _Jv_ThreadWait ();

  int status = (int) ::java::lang::ThreadGroup::had_uncaught_exception;
  ::java::lang::Runtime::exitNoChecksAccessor (status);
}
