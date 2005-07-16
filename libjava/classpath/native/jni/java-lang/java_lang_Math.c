/* Math.c - java.lang.Math native functions
   Copyright (C) 1998, 1999, 2004 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


#include <config.h>
#include <java_lang_Math.h>
#include <fdlibm.h>

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_sin
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return sin (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_cos
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return cos (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_tan
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return tan (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_asin
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return asin (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_acos
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return acos (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_atan
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return atan (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_atan2
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble y, jdouble x)
{
  return atan2 (y, x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_exp
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return exp (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_log
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return log (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_sqrt
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return sqrt (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_pow
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x, jdouble y)
{
  return pow (x, y);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_IEEEremainder
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x, jdouble y)
{
  return remainder (x, y);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_ceil
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return ceil (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_floor
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return floor (x);
}

JNIEXPORT jdouble JNICALL
Java_java_lang_Math_rint
  (JNIEnv * env __attribute__ ((__unused__)),
   jclass cls __attribute__ ((__unused__)), jdouble x)
{
  return rint (x);
}
