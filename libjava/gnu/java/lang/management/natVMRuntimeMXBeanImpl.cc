/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
/**
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 * @date Wed 21 Jun 2006 */
/* Written using online API docs for JDK 1.5 beta from http://java.sun.com.
 * Status:  Believed complete and correct.
 */
 
#include <config.h>

#include <jvm.h>

#include <unistd.h>

#include <gnu/java/lang/management/VMRuntimeMXBeanImpl.h>

JArray<jstring>* gnu::java::lang::management::VMRuntimeMXBeanImpl::getInputArguments()
{
  return ::gcj::vmArgs;
}

jlong gnu::java::lang::management::VMRuntimeMXBeanImpl::getStartTime()
{
  return ::gcj::startTime;
}

jlong gnu::java::lang::management::VMRuntimeMXBeanImpl::getPID()
{
  return getpid();
}


