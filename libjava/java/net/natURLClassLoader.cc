// natURLClassLoader.cc -- Native part of the URLClassLoader class.

/* Copyright (C) 2006 Free Software Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/net/URLClassLoader.h>

jboolean
java::net::URLClassLoader::runtimeInitialized ()
{
  return gcj::runtimeInitialized;
}
