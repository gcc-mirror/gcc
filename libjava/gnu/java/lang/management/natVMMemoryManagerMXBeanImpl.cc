#include <config.h>

#include <gnu/java/lang/management/VMMemoryManagerMXBeanImpl.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

jboolean
gnu::java::lang::management::VMMemoryManagerMXBeanImpl::isValid (::java::lang::String *)
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMMemoryManagerMXBeanImpl::isValid (::java::lang::String *) not implemented"));
}
