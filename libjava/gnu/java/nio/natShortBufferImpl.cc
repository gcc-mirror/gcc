#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/ShortBufferImpl.h>

JArray<jshort>*
gnu::java::nio::ShortBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

void
gnu::java::nio::ShortBufferImpl::nio_put_Byte(gnu::java::nio::ShortBufferImpl*, jint, jint, jbyte)
{
}

jbyte
gnu::java::nio::ShortBufferImpl::nio_get_Byte(gnu::java::nio::ShortBufferImpl*, jint, jint)
{
  return 0;
}
