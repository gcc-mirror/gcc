// natDirectByteBufferImpl.cc

/* Copyright (C) 2003, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <stdlib.h>

#include <gnu/gcj/RawData.h>
#include <java/nio/VMDirectByteBuffer.h>

using gnu::gcj::RawData;

RawData*
java::nio::VMDirectByteBuffer::allocate (jint capacity)
{
  return reinterpret_cast<gnu::gcj::RawData*> (::malloc (capacity));
}

void
java::nio::VMDirectByteBuffer::free (gnu::gcj::RawData* address)
{
  ::free (reinterpret_cast<void*> (address));
}

jbyte
java::nio::VMDirectByteBuffer::get (RawData* address, jint index)
{
  jbyte* pointer = reinterpret_cast<jbyte*> (address) + index;
  return *pointer;
}

void
java::nio::VMDirectByteBuffer::get (RawData* address, jint index,
				    jbyteArray dst, jint offset, jint length)
{
  jbyte* src = reinterpret_cast<jbyte*> (address) + index;
  memcpy (elements (dst) + offset, src, length);
}

void
java::nio::VMDirectByteBuffer::put (gnu::gcj::RawData* address,
				    jint index, jbyte value)
{
  jbyte* pointer = reinterpret_cast<jbyte*> (address) + index;
  *pointer = value;
}

RawData*
java::nio::VMDirectByteBuffer::adjustAddress (RawData* address, jint offset)
{
  jbyte* start = reinterpret_cast<jbyte*> (address) + offset;
  return reinterpret_cast<RawData*>(start);
}

void
java::nio::VMDirectByteBuffer::shiftDown (RawData* address, jint dst_offset,
					  jint src_offset, jint count)
{
  jbyte* dst = reinterpret_cast<jbyte*> (address) + dst_offset;
  jbyte* src = reinterpret_cast<jbyte*> (address) + src_offset;
  ::memmove(dst, src, count);
}
