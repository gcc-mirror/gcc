#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/LongBufferImpl.h>

JArray<jlong>*
gnu::java::nio::LongBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

JArray<jlong>*
gnu::java::nio::LongBufferImpl::nio_cast(JArray<jshort>*)
{
  return NULL;
}

JArray<jlong>*
gnu::java::nio::LongBufferImpl::nio_cast(JArray<jint>*)
{
  return NULL;
}

JArray<jlong>*
gnu::java::nio::LongBufferImpl::nio_cast(JArray<jlong>*)
{
  return NULL;
}

JArray<jlong>*
gnu::java::nio::LongBufferImpl::nio_cast(JArray<jchar>*)
{
  return NULL;
}

JArray<jlong>*
gnu::java::nio::LongBufferImpl::nio_cast(JArray<jfloat>*)
{
  return NULL;
}

JArray<jlong>*
gnu::java::nio::LongBufferImpl::nio_cast(JArray<jdouble>*)
{
  return NULL;
}

void
gnu::java::nio::LongBufferImpl::nio_put_Byte(gnu::java::nio::LongBufferImpl*, jint, jint, jbyte)
{
}

void
gnu::java::nio::LongBufferImpl::nio_put_Char(gnu::java::nio::LongBufferImpl*, jint, jint, jchar)
{
}

void
gnu::java::nio::LongBufferImpl::nio_put_Short(gnu::java::nio::LongBufferImpl*, jint, jint, jshort)
{
}

void
gnu::java::nio::LongBufferImpl::nio_put_Int(gnu::java::nio::LongBufferImpl*, jint, jint, jint)
{
}

void
gnu::java::nio::LongBufferImpl::nio_put_Long(gnu::java::nio::LongBufferImpl*, jint, jint, jlong)
{
}

void
gnu::java::nio::LongBufferImpl::nio_put_Float(gnu::java::nio::LongBufferImpl*, jint, jint, jfloat)
{
}

void
gnu::java::nio::LongBufferImpl::nio_put_Double(gnu::java::nio::LongBufferImpl*, jint, jint, jdouble)
{
}

jbyte
gnu::java::nio::LongBufferImpl::nio_get_Byte(gnu::java::nio::LongBufferImpl*, jint, jint)
{
  return 0;
}

jchar
gnu::java::nio::LongBufferImpl::nio_get_Char(gnu::java::nio::LongBufferImpl*, jint, jint)
{
  return ' ';
}

jshort
gnu::java::nio::LongBufferImpl::nio_get_Short(gnu::java::nio::LongBufferImpl*, jint, jint)
{
  return 0;
}

jint
gnu::java::nio::LongBufferImpl::nio_get_Int(gnu::java::nio::LongBufferImpl*, jint, jint)
{
  return 0;
}

jlong
gnu::java::nio::LongBufferImpl::nio_get_Long(gnu::java::nio::LongBufferImpl*, jint, jint)
{
  return 0;
}

jfloat
gnu::java::nio::LongBufferImpl::nio_get_Float(gnu::java::nio::LongBufferImpl*, jint, jint)
{
  return 0.0;
}

jdouble
gnu::java::nio::LongBufferImpl::nio_get_Double(gnu::java::nio::LongBufferImpl*, jint, jint)
{
  return 0.0;
}
