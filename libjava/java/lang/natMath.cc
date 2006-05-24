/* Copyright (C) 1998, 1999, 2000, 2002, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
/**
 * @author Andrew Haley <aph@cygnus.com>
 * @date Tue Sep 22 1998 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
#include <config.h>

#include <java/lang/String.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/Integer.h>
#include <java/lang/Long.h>
#include <java/lang/Math.h>
#include <gcj/array.h>

#include "fdlibm.h"

jdouble java::lang::Math::cos(jdouble x)
{
  return (jdouble)::cos((double)x);
}  

jdouble java::lang::Math::sin(jdouble x)
{
  return (jdouble)::sin((double)x);
}  

jdouble java::lang::Math::tan(jdouble x)
{
  return (jdouble)::tan((double)x);
}  

jdouble java::lang::Math::asin(jdouble x)
{
  return (jdouble)::asin((double)x);
}  

jdouble java::lang::Math::acos(jdouble x)
{
  return (jdouble)::acos((double)x);
}  

jdouble java::lang::Math::atan(jdouble x)
{
  return (jdouble)::atan((double)x);
}  

jdouble java::lang::Math::atan2(jdouble y, jdouble x)
{
  return (jdouble)::atan2((double)y, (double)x);
}  

jdouble java::lang::Math::log(jdouble x)
{
  return (jdouble)::log((double)x);
}  

jdouble java::lang::Math::exp(jdouble x)
{
  return (jdouble)::exp((double)x);
}  

jdouble java::lang::Math::sqrt(jdouble x)
{
  return (jdouble)::sqrt((double)x);
}  

jdouble java::lang::Math::pow(jdouble y, jdouble x)
{
  return (jdouble)::pow((double)y, (double)x);
}  

jdouble java::lang::Math::IEEEremainder(jdouble y, jdouble x)
{
  return (jdouble)::__ieee754_remainder((double)y, (double)x);
}  

jdouble java::lang::Math::rint(jdouble x)
{
  return (jdouble)::rint((double)x);
}  

jdouble java::lang::Math::floor(jdouble x)
{
  return (jdouble)::floor((double)x);
}  

jdouble java::lang::Math::ceil(jdouble x)
{
  return (jdouble)::ceil((double)x);
}  

jdouble java::lang::Math::log10(jdouble x)
{
  return (jdouble)::log10((double)x);
}  

jdouble java::lang::Math::cbrt(jdouble x)
{
  return (jdouble)::cbrt((double)x);
}

jdouble java::lang::Math::cosh(jdouble x)
{
  return (jdouble)::cosh((double)x);
}

jdouble java::lang::Math::expm1(jdouble x)
{
  return (jdouble)::expm1((double)x);
}

jdouble java::lang::Math::hypot(jdouble x, jdouble y)
{
  return (jdouble)::hypot((double)x, (double)y);
}

jdouble java::lang::Math::log1p(jdouble x)
{
  return (jdouble)::log1p((double)x);
}

jdouble java::lang::Math::sinh(jdouble x)
{
  return (jdouble)::sinh((double)x);
}

jdouble java::lang::Math::tanh(jdouble x)
{
  return (jdouble)::tanh((double)x);
}

static inline int
floatToIntBits (jfloat value)
{
  union {
    jint l;
    jfloat d;
  } u;
  u.d = value;
  return u.l;
}

static inline bool
isNaN (jint bits)
{
  jint e = bits & 0x7f800000;
  jint f = bits & 0x007fffff;

  return e == 0x7f800000 && f != 0;
}

static inline jlong
doubleToLongBits (jdouble value)
{
  union {
    jlong l;
    jdouble d;
  } u;
  u.d = value;
  return u.l;
}

static inline bool 
isNaN (jlong bits)
{
  jlong e = bits & 0x7ff0000000000000LL;
  jlong f = bits & 0x000fffffffffffffLL;
  
  return e == 0x7ff0000000000000LL && f != 0LL;
}

