/* Copyright (C) 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@redhat.com>

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-stack.h>

#include <java/lang/VMSecurityManager.h>
#include <java/lang/SecurityManager.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Class.h>

JArray<jclass> *
java::lang::VMSecurityManager::getClassContext (jclass klass)
{
  JArray<jclass> *result = 
    _Jv_StackTrace::GetClassContext (klass);

  return result;
}
