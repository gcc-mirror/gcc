// natByteBufferImpl.cc

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/ByteBufferImpl.h>

JArray<jbyte>*
gnu::java::nio::ByteBufferImpl::nio_cast(JArray<jbyte>*)
{
  return NULL;
}

JArray<jbyte>*
gnu::java::nio::ByteBufferImpl::nio_cast(JArray<jshort>*)
{
  return NULL;
}

JArray<jbyte>*
gnu::java::nio::ByteBufferImpl::nio_cast(JArray<jint>*)
{
  return NULL;
}

JArray<jbyte>*
gnu::java::nio::ByteBufferImpl::nio_cast(JArray<jlong>*)
{
  return NULL;
}

JArray<jbyte>*
gnu::java::nio::ByteBufferImpl::nio_cast(JArray<jchar>*)
{
  return NULL;
}

JArray<jbyte>*
gnu::java::nio::ByteBufferImpl::nio_cast(JArray<jfloat>*)
{
  return NULL;
}

JArray<jbyte>*
gnu::java::nio::ByteBufferImpl::nio_cast(JArray<jdouble>*)
{
  return NULL;
}

void
gnu::java::nio::ByteBufferImpl::nio_put_Byte(gnu::java::nio::ByteBufferImpl*, jint, jint, jbyte)
{
}

void
gnu::java::nio::ByteBufferImpl::nio_put_Char(gnu::java::nio::ByteBufferImpl*, jint, jint, jchar)
{
}

void
gnu::java::nio::ByteBufferImpl::nio_put_Short(gnu::java::nio::ByteBufferImpl*, jint, jint, jshort)
{
}

void
gnu::java::nio::ByteBufferImpl::nio_put_Int(gnu::java::nio::ByteBufferImpl*, jint, jint, jint)
{
}

void
gnu::java::nio::ByteBufferImpl::nio_put_Long(gnu::java::nio::ByteBufferImpl*, jint, jint, jlong)
{
}

void
gnu::java::nio::ByteBufferImpl::nio_put_Float(gnu::java::nio::ByteBufferImpl*, jint, jint, jfloat)
{
}

void
gnu::java::nio::ByteBufferImpl::nio_put_Double(gnu::java::nio::ByteBufferImpl*, jint, jint, jdouble)
{
}

jbyte
gnu::java::nio::ByteBufferImpl::nio_get_Byte(gnu::java::nio::ByteBufferImpl*, jint, jint)
{
  return 0;
}

jchar
gnu::java::nio::ByteBufferImpl::nio_get_Char(gnu::java::nio::ByteBufferImpl*, jint, jint)
{
  return ' ';
}

jshort
gnu::java::nio::ByteBufferImpl::nio_get_Short(gnu::java::nio::ByteBufferImpl*, jint, jint)
{
  return 0;
}

jint
gnu::java::nio::ByteBufferImpl::nio_get_Int(gnu::java::nio::ByteBufferImpl*, jint, jint)
{
  return 0;
}

jlong
gnu::java::nio::ByteBufferImpl::nio_get_Long(gnu::java::nio::ByteBufferImpl*, jint, jint)
{
  return 0;
}

jfloat
gnu::java::nio::ByteBufferImpl::nio_get_Float(gnu::java::nio::ByteBufferImpl*, jint, jint)
{
  return 0.0;
}

jdouble
gnu::java::nio::ByteBufferImpl::nio_get_Double(gnu::java::nio::ByteBufferImpl*, jint, jint)
{
  return 0.0;
}
