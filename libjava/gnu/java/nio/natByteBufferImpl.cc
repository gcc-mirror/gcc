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
