// natCharBufferImpl.cc

/* Copyright (C) 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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
