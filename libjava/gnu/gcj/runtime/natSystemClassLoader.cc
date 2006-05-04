// natSystemClassLoader.cc - native code for system class loader

/* Copyright (C) Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <execution.h>

#include <gnu/gcj/runtime/SystemClassLoader.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/util/HashMap.h>

jclass
gnu::gcj::runtime::SystemClassLoader::findClass (jstring name)
{
  jclass result = (jclass) nativeClasses->get(name);
  if (! result)
    return URLClassLoader::findClass(name);
  // Never return a class whose supers are not installed.
  _Jv_Linker::wait_for_state (result, JV_STATE_LOADING);
  return result;
}
