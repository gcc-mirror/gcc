// natFloat.cc - Implementation of java.lang.Float native methods.

/* Copyright (C) 1998, 1999, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <java/lang/Float.h>
#include <jvm.h>

union u
{
  jint l;
  jfloat d;
};

jint 
java::lang::Float::floatToIntBits(jfloat value)
{
  union u u;
  u.d = value;
  jint e = u.l & 0x7f800000;
  jint f = u.l & 0x007fffff;

  if (e == 0x7f800000 && f != 0)
    u.l = 0x7fc00000;

  return u.l;
}

jint 
java::lang::Float::floatToRawIntBits(jfloat value)
{
  union u u;
  u.d = value;  
  return u.l;
}

jfloat 
java::lang::Float::intBitsToFloat(jint bits)
{
  union u u;
  u.l = bits;
  return u.d;
}

