// natVMAccessController.cc -- Native part of the VMAccessController class.

/* Copyright (C) 2006 Free Software Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-stack.h>

#include <java/security/VMAccessController.h>

jobjectArray
java::security::VMAccessController::getStack ()
{
  return _Jv_StackTrace::GetAccessControlStack ();
}
