#include <config.h>

#include <gnu/java/lang/management/VMGarbageCollectorMXBeanImpl.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

jlong
gnu::java::lang::management::VMGarbageCollectorMXBeanImpl::getCollectionCount (::java::lang::String *)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMGarbageCollectorMXBeanImpl::getCollectionCount (::java::lang::String *) not implemented"));
}


jlong
gnu::java::lang::management::VMGarbageCollectorMXBeanImpl::getCollectionTime (::java::lang::String *)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMGarbageCollectorMXBeanImpl::getCollectionTime (::java::lang::String *) not implemented"));
}
