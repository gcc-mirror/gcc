#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/IntBufferImpl.h>

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

void
gnu::java::nio::IntBufferImpl::nio_put_Byte(gnu::java::nio::IntBufferImpl*, jint, jint, jbyte)
{
}

jbyte
gnu::java::nio::IntBufferImpl::nio_get_Byte(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return 0;
}
