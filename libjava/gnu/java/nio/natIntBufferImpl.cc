#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/IntBufferImpl.h>

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jshort>*)
{
  return NULL;
}

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jint>*)
{
  return NULL;
}

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jlong>*)
{
  return NULL;
}

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jchar>*)
{
  return NULL;
}

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jfloat>*)
{
  return NULL;
}

JArray<jint>*
gnu::java::nio::IntBufferImpl::nio_cast(JArray<jdouble>*)
{
  return NULL;
}

void
gnu::java::nio::IntBufferImpl::nio_put_Byte(gnu::java::nio::IntBufferImpl*, jint, jint, jbyte)
{
}

void
gnu::java::nio::IntBufferImpl::nio_put_Char(gnu::java::nio::IntBufferImpl*, jint, jint, jchar)
{
}

void
gnu::java::nio::IntBufferImpl::nio_put_Short(gnu::java::nio::IntBufferImpl*, jint, jint, jshort)
{
}

void
gnu::java::nio::IntBufferImpl::nio_put_Int(gnu::java::nio::IntBufferImpl*, jint, jint, jint)
{
}

void
gnu::java::nio::IntBufferImpl::nio_put_Long(gnu::java::nio::IntBufferImpl*, jint, jint, jlong)
{
}

void
gnu::java::nio::IntBufferImpl::nio_put_Float(gnu::java::nio::IntBufferImpl*, jint, jint, jfloat)
{
}

void
gnu::java::nio::IntBufferImpl::nio_put_Double(gnu::java::nio::IntBufferImpl*, jint, jint, jdouble)
{
}

jbyte
gnu::java::nio::IntBufferImpl::nio_get_Byte(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return 0;
}

jchar
gnu::java::nio::IntBufferImpl::nio_get_Char(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return ' ';
}

jshort
gnu::java::nio::IntBufferImpl::nio_get_Short(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return 0;
}

jint
gnu::java::nio::IntBufferImpl::nio_get_Int(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return 0;
}

jlong
gnu::java::nio::IntBufferImpl::nio_get_Long(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return 0;
}

jfloat
gnu::java::nio::IntBufferImpl::nio_get_Float(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return 0.0;
}

jdouble
gnu::java::nio::IntBufferImpl::nio_get_Double(gnu::java::nio::IntBufferImpl*, jint, jint)
{
  return 0.0;
}
