// natLogger.cc - Native part of Logger class.

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This Logger is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the Logger "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <string.h>

#pragma implementation "Logger.h"

#include <gcj/cni.h>
#include <jvm.h>
#include <java-stack.h>

#include <java/lang/Object.h>
#include <java/lang/Class.h>
#include <java/util/logging/Logger.h>
#include <java/lang/StackTraceElement.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>

using namespace java::util::logging;

java::lang::StackTraceElement* 
java::util::logging::Logger::getCallerStackFrame ()
{
  jclass klass = NULL;
  _Jv_Method *meth = NULL;
  _Jv_StackTrace::GetCallerInfo (&Logger::class$, &klass, &meth);

  jstring meth_name = NULL;
  jstring klass_name = NULL;
  if (klass != NULL)
    klass_name = klass->getName();
  if (meth != NULL)
    meth_name = _Jv_NewStringUtf8Const (meth->name);
  
  java::lang::StackTraceElement *e 
    = new java::lang::StackTraceElement
    (JvNewStringUTF (""), 0, klass_name, meth_name, false);

  return e;
}
