#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/FloatBufferImpl.h>

JArray<jfloat>*
gnu::java::nio::FloatBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

void
gnu::java::nio::FloatBufferImpl::nio_put_Byte(gnu::java::nio::FloatBufferImpl*, jint, jint, jbyte)
{
}

jbyte
gnu::java::nio::FloatBufferImpl::nio_get_Byte(gnu::java::nio::FloatBufferImpl*, jint, jint)
{
  return 0;
}
