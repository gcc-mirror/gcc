// natDirectByteBufferImpl.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/java/nio/DirectByteBufferImpl.h>

jlong
gnu::java::nio::DirectByteBufferImpl::allocateImpl (jint capacity)
{
  // FIXME: implement this
  return 0;
}

void
gnu::java::nio::DirectByteBufferImpl::freeImpl (jlong address)
{
  // FIXME: implement this
}

jbyte
gnu::java::nio::DirectByteBufferImpl::getImpl (jint index)
{
  // FIXME: implement this
  // Dont forget: add offset to index
  return 0;
}

void
gnu::java::nio::DirectByteBufferImpl::putImpl (jint index, jbyte value)
{
  // FIXME: implement this
  // Dont forget: add offset to index
}
