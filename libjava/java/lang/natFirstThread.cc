// natFirstThread.cc - Implementation of FirstThread native methods.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <cni.h>
#include <jvm.h>

#include <java/lang/FirstThread.h>
#include <java/lang/Class.h>
#include <java/lang/String.h>
#include <java/lang/System.h>
#include <java/lang/reflect/Modifier.h>
#include <java/io/PrintStream.h>

#define DIE(Message)  die (JvNewStringLatin1 (Message))

typedef void main_func (jobject);

void
java::lang::FirstThread::run0 (void)
{
  Utf8Const* main_signature = _Jv_makeUtf8Const ("([Ljava.lang.String;)V", 22);
  Utf8Const* main_name = _Jv_makeUtf8Const ("main", 4);

#if 0
  // Note: this turns out to be more painful than useful.  Apparently
  // many people rely on being able to have main in a non-public
  // class.
  // This is based on my reading of 12.3.3.
  if (! java::lang::reflect::Modifier::isPublic(klass->getModifiers()))
    DIE ("class must be public");
#endif

  if (klass == NULL)
    {
      klass = java::lang::Class::forName (klass_name);
      if (klass != NULL) _Jv_InitClass (klass);
    }

  _Jv_Method *meth = _Jv_GetMethodLocal (klass, main_name, main_signature);

  // Some checks from Java Spec section 12.1.4.
  if (meth == NULL)
    DIE ("no suitable method `main' in class");
  if (! java::lang::reflect::Modifier::isStatic(meth->accflags))
    DIE ("`main' must be static");
  if (! java::lang::reflect::Modifier::isPublic(meth->accflags))
    DIE ("`main' must be public");

  main_func *real_main = (main_func *) meth->ncode;
  (*real_main) (args);
}
