// natLogger.cc - Native part of Logger class.

/* Copyright (C) 1998, 1999, 2000, 2001, 2005  Free Software Foundation

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


#include <java/lang/Object.h>
#include <java/lang/Class.h>
#include <java/util/logging/Logger.h>
#include <java/lang/StackTraceElement.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>

java::lang::StackTraceElement* 
java::util::logging::Logger::getCallerStackFrame ()
{
  gnu::gcj::runtime::StackTrace *t 
    = new gnu::gcj::runtime::StackTrace(4);
  java::lang::Class *klass = NULL;
  int i = 1;
  try
    {
      // skip until this class
      while ((klass = t->classAt (i)) != getClass())
	i++;
      // skip the stackentries of this class
      while ((klass = t->classAt (i)) == getClass() || klass == NULL)
	i++;
    }
  catch (::java::lang::ArrayIndexOutOfBoundsException *e)
    {
      // FIXME: RuntimeError
    }

  java::lang::StackTraceElement *e 
    = new java::lang::StackTraceElement
    (JvNewStringUTF (""), 0, 
     klass->getName(), t->methodAt(i), false);

  return e;
}
