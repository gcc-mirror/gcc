// natMainThread.cc - Implementation of MainThread native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/lang/MainThread.h>

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
}
