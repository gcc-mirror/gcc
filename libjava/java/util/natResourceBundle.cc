/* Copyright (C) 2002, 2003, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@redhat.com>

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-stack.h>
#include <java/util/ResourceBundle.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Class.h>

using namespace java::lang;

java::lang::ClassLoader *
java::util::ResourceBundle::getCallingClassLoader ()
{
  jclass caller = _Jv_StackTrace::GetCallingClass (&ResourceBundle::class$);
  if (caller)
    return caller->getClassLoaderInternal();
  return NULL;
}
