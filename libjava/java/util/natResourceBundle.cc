// natResourceBundle.cc - Native code for ResourceBundle class.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/util/ResourceBundle.h>
#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>

JArray<jclass> *
java::util::ResourceBundle::getClassContext ()
{
  // FIXME: we currently lack the capability to correctly implement
  // this method.  So we fake it by telling ResourceBundle that we
  // only have the system class loader.
  jobjectArray a = JvNewObjectArray (2, &java::lang::Class::class$, NULL);
  jobject *elts = elements (a);
  elts[0] = java::lang::ClassLoader::getSystemClassLoader ();
  elts[1] = elts[0];

  return reinterpret_cast< JArray<jclass> *> (a);
}
