/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
/**
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 * @date Sun 20 Aug 2006 */
/* Written using online API docs for JDK 1.5 beta from http://java.sun.com.
 * Status:  Believed complete and correct.
 */
 
#include <config.h>

#include <jvm.h>

#include <gnu/java/lang/management/VMClassLoadingMXBeanImpl.h>

jint
gnu::java::lang::management::VMClassLoadingMXBeanImpl::getLoadedClassCount ()
{
  /* FIXME: We never unload, so this never goes down again */
  return ::gcj::loadedClasses;
}


jlong
gnu::java::lang::management::VMClassLoadingMXBeanImpl::getUnloadedClassCount ()
{
  /* FIXME: We never unload, so this always == 0 */
  return ::gcj::unloadedClasses;
}


jboolean
gnu::java::lang::management::VMClassLoadingMXBeanImpl::isVerbose ()
{
  return ::gcj::verbose_class_flag;
}


void
gnu::java::lang::management::VMClassLoadingMXBeanImpl::setVerbose (jboolean b)
{
  ::gcj::verbose_class_flag = b;
}
