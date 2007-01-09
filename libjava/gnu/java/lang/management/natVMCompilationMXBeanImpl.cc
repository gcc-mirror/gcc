#include <config.h>

#include <gnu/java/lang/management/VMCompilationMXBeanImpl.h>
#include <gcj/cni.h>
#include <java/lang/UnsupportedOperationException.h>

jlong
gnu::java::lang::management::VMCompilationMXBeanImpl::getTotalCompilationTime ()
{
  throw new ::java::lang::UnsupportedOperationException (JvNewStringLatin1 ("gnu::java::lang::management::VMCompilationMXBeanImpl::getTotalCompilationTime () not implemented"));
}
