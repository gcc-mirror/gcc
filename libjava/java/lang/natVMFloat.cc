// natVMFloat.cc - Implementation of java.lang.VMFloat native methods.

/* Copyright (C) 1998, 1999, 2001, 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <java/lang/Float.h>
#include <java/lang/VMFloat.h>
#include <jvm.h>

union u
{
  jint l;
  jfloat d;
};

jint 
java::lang::VMFloat::floatToIntBits(jfloat value)
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
java::lang::VMFloat::floatToRawIntBits(jfloat value)
{
  union u u;
  u.d = value;  
  return u.l;
}

jfloat 
java::lang::VMFloat::intBitsToFloat(jint bits)
{
  union u u;
  u.l = bits;
  return u.d;
}

