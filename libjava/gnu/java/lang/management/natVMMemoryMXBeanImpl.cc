/* Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
/**
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 * @date Tue 08 Aug 2006 */
/* 
 * Status:  Stubbed.
 */

#include <config.h>

#include <gcj/cni.h>
#include <gnu/java/lang/management/VMMemoryMXBeanImpl.h>
#include <java/lang/UnsupportedOperationException.h>

::java::lang::management::MemoryUsage *
gnu::java::lang::management::VMMemoryMXBeanImpl::getNonHeapMemoryUsage ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMMemoryMXBeanImpl::getNonHeapMemoryUsage () not implemented"));
}


jint
gnu::java::lang::management::VMMemoryMXBeanImpl::getObjectPendingFinalizationCount ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMMemoryMXBeanImpl::getObjectPendingFinalizationCount () not implemented"));
}


jboolean
gnu::java::lang::management::VMMemoryMXBeanImpl::isVerbose ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMMemoryMXBeanImpl::isVerbose () not implemented"));
}


void
gnu::java::lang::management::VMMemoryMXBeanImpl::setVerbose (jboolean)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMMemoryMXBeanImpl::setVerbose (jboolean) not implemented"));
}
