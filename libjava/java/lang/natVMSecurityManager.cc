/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@redhat.com>

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/VMSecurityManager.h>
#include <java/lang/SecurityManager.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Class.h>
#include <gnu/gcj/runtime/StackTrace.h>

JArray<jclass> *
java::lang::VMSecurityManager::getClassContext ()
{
  JArray<jclass> *result = NULL;
  gnu::gcj::runtime::StackTrace *t = new gnu::gcj::runtime::StackTrace();
  if (t)
    {
      int maxlen = t->length();

      int len = 0;
      for (int i=0; i<maxlen; i++)
	{
	  jclass klass = t->classAt(i);
	  if (klass != NULL && klass != &java::lang::VMSecurityManager::class$
	      && klass != &java::lang::SecurityManager::class$)
	    ++len;
	}

      result =
	(JArray<jclass> *) _Jv_NewObjectArray (len, &java::lang::Class::class$,
					       NULL);

      len = 0;
      for (int i=0; i<maxlen; i++)
	{
	  jclass klass = t->classAt(i);
	  if (klass != NULL && klass != &java::lang::VMSecurityManager::class$
	      && klass != &java::lang::SecurityManager::class$)
	    elements(result)[len++] = klass;
	}
    }

  return result;
}
