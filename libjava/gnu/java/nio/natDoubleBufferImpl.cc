#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/DoubleBufferImpl.h>

JArray<jdouble>*
gnu::java::nio::DoubleBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

JArray<jdouble>*
gnu::java::nio::DoubleBufferImpl::nio_cast(JArray<jshort>*)
{
  return NULL;
}

JArray<jdouble>*
gnu::java::nio::DoubleBufferImpl::nio_cast(JArray<jint>*)
{
  return NULL;
}

JArray<jdouble>*
gnu::java::nio::DoubleBufferImpl::nio_cast(JArray<jlong>*)
{
  return NULL;
}

JArray<jdouble>*
gnu::java::nio::DoubleBufferImpl::nio_cast(JArray<jchar>*)
{
  return NULL;
}

JArray<jdouble>*
gnu::java::nio::DoubleBufferImpl::nio_cast(JArray<jfloat>*)
{
  return NULL;
}

JArray<jdouble>*
gnu::java::nio::DoubleBufferImpl::nio_cast(JArray<jdouble>*)
{
  return NULL;
}

void
gnu::java::nio::DoubleBufferImpl::nio_put_Byte(gnu::java::nio::DoubleBufferImpl*, jint, jint, jbyte)
{
}

void
gnu::java::nio::DoubleBufferImpl::nio_put_Char(gnu::java::nio::DoubleBufferImpl*, jint, jint, jchar)
{
}

void
gnu::java::nio::DoubleBufferImpl::nio_put_Short(gnu::java::nio::DoubleBufferImpl*, jint, jint, jshort)
{
}

void
gnu::java::nio::DoubleBufferImpl::nio_put_Int(gnu::java::nio::DoubleBufferImpl*, jint, jint, jint)
{
}

void
gnu::java::nio::DoubleBufferImpl::nio_put_Long(gnu::java::nio::DoubleBufferImpl*, jint, jint, jlong)
{
}

void
gnu::java::nio::DoubleBufferImpl::nio_put_Float(gnu::java::nio::DoubleBufferImpl*, jint, jint, jfloat)
{
}

void
gnu::java::nio::DoubleBufferImpl::nio_put_Double(gnu::java::nio::DoubleBufferImpl*, jint, jint, jdouble)
{
}

jbyte
gnu::java::nio::DoubleBufferImpl::nio_get_Byte(gnu::java::nio::DoubleBufferImpl*, jint, jint)
{
  return 0;
}

jchar
gnu::java::nio::DoubleBufferImpl::nio_get_Char(gnu::java::nio::DoubleBufferImpl*, jint, jint)
{
  return ' ';
}

jshort
gnu::java::nio::DoubleBufferImpl::nio_get_Short(gnu::java::nio::DoubleBufferImpl*, jint, jint)
{
  return 0;
}

jint
gnu::java::nio::DoubleBufferImpl::nio_get_Int(gnu::java::nio::DoubleBufferImpl*, jint, jint)
{
  return 0;
}

jlong
gnu::java::nio::DoubleBufferImpl::nio_get_Long(gnu::java::nio::DoubleBufferImpl*, jint, jint)
{
  return 0;
}

jfloat
gnu::java::nio::DoubleBufferImpl::nio_get_Float(gnu::java::nio::DoubleBufferImpl*, jint, jint)
{
  return 0.0;
}

jdouble
gnu::java::nio::DoubleBufferImpl::nio_get_Double(gnu::java::nio::DoubleBufferImpl*, jint, jint)
{
  return 0.0;
}
