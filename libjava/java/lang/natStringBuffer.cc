// natStringBuffer.cc - Implementation of java.lang.StringBuffer native methods.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <java/lang/StringBuffer.h>

java::lang::StringBuffer*
java::lang::StringBuffer::append (jint num)
{
  // Use an array large enough for "-2147483648"; i.e. 11 chars.
  jchar buffer[11];
  int i = _Jv_FormatInt (buffer+11, num);
  JvSynchronize dummy (this);
  jint needed = count + i;
  ensureCapacity_unsynchronized (needed);
  jchar* dst = elements (value) + count;
  jchar* src = buffer+11-i;
  while (--i >= 0)
    *dst++ = *src++;
  count = needed;
  return this;
}
