#include <config.h>

#include <gnu/java/lang/management/VMThreadMXBeanImpl.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

jlongArray
gnu::java::lang::management::VMThreadMXBeanImpl::findDeadlockedThreads ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::findDeadlockedThreads () not implemented"));
}

jlongArray
gnu::java::lang::management::VMThreadMXBeanImpl::findMonitorDeadlockedThreads ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::findMonitorDeadlockedThreads () not implemented"));
}

jlong
gnu::java::lang::management::VMThreadMXBeanImpl::getCurrentThreadCpuTime ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getCurrentThreadCpuTime () not implemented"));
}


jlong
gnu::java::lang::management::VMThreadMXBeanImpl::getCurrentThreadUserTime ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getCurrentThreadUserTime () not implemented"));
}

void
gnu::java::lang::management::VMThreadMXBeanImpl::getLockInfo (::java::lang::management::ThreadInfo *)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getLockInfo () not implemented"));
}

void
gnu::java::lang::management::VMThreadMXBeanImpl::getMonitorInfo (::java::lang::management::ThreadInfo *)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getMonitorInfo () not implemented"));
}

jint
gnu::java::lang::management::VMThreadMXBeanImpl::getPeakThreadCount ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getPeakThreadCount () not implemented"));
}


jlong
gnu::java::lang::management::VMThreadMXBeanImpl::getThreadCpuTime (jlong)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getThreadCpuTime (jlong) not implemented"));
}


::java::lang::management::ThreadInfo *
gnu::java::lang::management::VMThreadMXBeanImpl::getThreadInfoForId (jlong, jint)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getThreadInfoForId (jlong, jint) not implemented"));
}


jlong
gnu::java::lang::management::VMThreadMXBeanImpl::getThreadUserTime (jlong)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getThreadUserTime (jlong) not implemented"));
}


jlong
gnu::java::lang::management::VMThreadMXBeanImpl::getTotalStartedThreadCount ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::getTotalStartedThreadCount () not implemented"));
}


void
gnu::java::lang::management::VMThreadMXBeanImpl::resetPeakThreadCount ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMThreadMXBeanImpl::resetPeakThreadCount () not implemented"));
}
