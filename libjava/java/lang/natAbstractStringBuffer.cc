// natStringBuffer.cc - Implementation of java.lang.StringBuffer native methods.

/* Copyright (C) 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <java/lang/AbstractStringBuffer.h>

java::lang::AbstractStringBuffer*
java::lang::AbstractStringBuffer::append (jint num)
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

jboolean
java::lang::AbstractStringBuffer::regionMatches(jint toffset, jstring other)
{
  jint len = other->count;
  jchar *tptr = elements(value) + toffset;
  jchar *optr = JvGetStringChars(other);
  while (--len >= 0)
    if (*tptr++ != *optr++)
      return false;
  return true;
}
