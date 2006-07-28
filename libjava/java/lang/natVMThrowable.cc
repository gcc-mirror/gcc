// natVMThrowable.cc - Native part of VMThrowable class.

/* Copyright (C) 2003, 2006 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-stack.h>

#include <java/lang/Throwable.h>
#include <java/lang/VMThrowable.h>

using namespace gnu::gcj;

java::lang::VMThrowable *
java::lang::VMThrowable::fillInStackTrace (java::lang::Throwable *)
{
  using namespace java::lang;

  // Don't trace stack during initialization of the runtime.
  if (! gcj::runtimeInitialized)
    return NULL;
  
  _Jv_StackTrace *trace = _Jv_StackTrace::GetStackTrace ();
  VMThrowable *vmthrowable = new VMThrowable ();
  vmthrowable->data = (RawDataManaged *) trace;
  return vmthrowable;
}


JArray< ::java::lang::StackTraceElement *> *
java::lang::VMThrowable::getStackTrace (java::lang::Throwable *throwable)
{
  _Jv_StackTrace *trace = reinterpret_cast <_Jv_StackTrace *> (data);
  return _Jv_StackTrace::GetStackTraceElements (trace, throwable);
}
