// natStringBuffer.cc - Implementation of java.lang.StringBuffer native methods.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <gnu/gcj/runtime/StringBuffer.h>
#include <java/lang/String.h>

gnu::gcj::runtime::StringBuffer *
gnu::gcj::runtime::StringBuffer::append (jint num)
{
  // Use an array large enough for "-2147483648"; i.e. 11 chars.
  jchar buffer[11];
  int i = _Jv_FormatInt (buffer+11, num);
  jint needed = count + i;
  ensureCapacity_unsynchronized (needed);
  jchar* dst = elements (value) + count;
  jchar* src = buffer+11-i;
  while (--i >= 0)
    *dst++ = *src++;
  count = needed;
  return this;
}

java::lang::String *
gnu::gcj::runtime::StringBuffer::toString ()
{
  return new java::lang::String (this);
}
