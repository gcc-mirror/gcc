// natFloatBufferImpl.cc

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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
