#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/CharBufferImpl.h>

JArray<jchar>*
gnu::java::nio::CharBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

void
gnu::java::nio::CharBufferImpl::nio_put_Byte(gnu::java::nio::CharBufferImpl*, jint, jint, jbyte)
{
}

jbyte
gnu::java::nio::CharBufferImpl::nio_get_Byte(gnu::java::nio::CharBufferImpl*, jint, jint)
{
  return 0;
}
