// natDirectByteBufferImpl.cc

/* Copyright (C) 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <stdlib.h>

#include <gnu/gcj/RawData.h>
#include <java/nio/DirectByteBufferImpl.h>

gnu::gcj::RawData*
java::nio::DirectByteBufferImpl::allocateImpl (jint capacity)
{
  return reinterpret_cast<gnu::gcj::RawData*> (::malloc (capacity));
}

void
java::nio::DirectByteBufferImpl::freeImpl (gnu::gcj::RawData* address)
{
  ::free (reinterpret_cast<void*> (address));
}

jbyte
java::nio::DirectByteBufferImpl::getImpl (jint index)
{
  jbyte* pointer = reinterpret_cast<jbyte*> (address) + offset + index;
  return *pointer;
}

void
java::nio::DirectByteBufferImpl::putImpl (jint index, jbyte value)
{
  jbyte* pointer = reinterpret_cast<jbyte*> (address) + offset + index;
  *pointer = value;
}

void
java::nio::DirectByteBufferImpl::shiftDown
(jint dst_offset, jint src_offset, jint count)
{
  jbyte* dst = reinterpret_cast<jbyte*> (address) + offset + dst_offset;
  jbyte* src = reinterpret_cast<jbyte*> (address) + offset + src_offset;
  ::memmove(dst, src, count);
}
