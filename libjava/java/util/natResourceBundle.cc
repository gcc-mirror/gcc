/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@redhat.com>

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/util/ResourceBundle.h>
#include <java/lang/SecurityManager.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Class.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <gnu/gcj/runtime/StackTrace.h>

java::lang::ClassLoader *
java::util::ResourceBundle::getCallingClassLoader ()
{
  gnu::gcj::runtime::StackTrace *t = new gnu::gcj::runtime::StackTrace(6);
  try
    {
      for (int i = 3; ; ++i)
	{
	  jclass klass = t->classAt(i);
	  if (klass != NULL)
	    return klass->getClassLoaderInternal();
	}
    }
  catch (::java::lang::ArrayIndexOutOfBoundsException *e)
    {
    }
  return NULL;
}
