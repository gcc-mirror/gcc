/* primlib.h
   Copyright (C) 1998 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

#ifndef __PRIMLIB_H__
#define __PRIMLIB_H__

#include <jni.h>

#define PRIMLIB_UNKNOWN  0
#define PRIMLIB_OBJECT   1
#define PRIMLIB_BOOLEAN  2
#define PRIMLIB_BYTE     3
#define PRIMLIB_CHAR     4
#define PRIMLIB_SHORT    5
#define PRIMLIB_INT      6
#define PRIMLIB_LONG     7
#define PRIMLIB_FLOAT    8
#define PRIMLIB_DOUBLE   9
#define PRIMLIB_VOID     10
#define PRIMLIB_NULL     11
#define PRIMLIB_NUMTYPES 12

/* Low-level primitive class accessor functions. */
JNIEXPORT jclass JNICALL PRIMLIB_GetNativeWrapClass(JNIEnv * env, int reflectType);
JNIEXPORT jclass JNICALL PRIMLIB_GetNativeTypeClass(JNIEnv * env, int reflectType);
JNIEXPORT jmethodID JNICALL PRIMLIB_GetNativeWrapClassConstructor(JNIEnv * env, int reflectType);
JNIEXPORT jmethodID JNICALL PRIMLIB_GetNativeWrapClassAccessor(JNIEnv * env, int reflectType);

/* Type discovery functions: WrapperType finds out j.l.Boolean/Byte/etc., and
   Type finds out j.l.Boolean.TYPE, etc.
*/
JNIEXPORT jint JNICALL PRIMLIB_GetReflectiveWrapperType(JNIEnv * env, jobject obj);
JNIEXPORT jint JNICALL PRIMLIB_GetReflectiveType(JNIEnv * env, jclass returnType);

/* Constructor functions. */
JNIEXPORT jobject JNICALL PRIMLIB_WrapBoolean(JNIEnv * env, jboolean b);
JNIEXPORT jobject JNICALL PRIMLIB_WrapByte   (JNIEnv * env, jbyte b);
JNIEXPORT jobject JNICALL PRIMLIB_WrapChar   (JNIEnv * env, jchar c);
JNIEXPORT jobject JNICALL PRIMLIB_WrapShort  (JNIEnv * env, jshort s);
JNIEXPORT jobject JNICALL PRIMLIB_WrapInt    (JNIEnv * env, jint i);
JNIEXPORT jobject JNICALL PRIMLIB_WrapLong   (JNIEnv * env, jlong l);
JNIEXPORT jobject JNICALL PRIMLIB_WrapFloat  (JNIEnv * env, jfloat f);
JNIEXPORT jobject JNICALL PRIMLIB_WrapDouble (JNIEnv * env, jdouble d);

/* Widening conversion unwrapping functions. */
JNIEXPORT jboolean JNICALL PRIMLIB_UnwrapBoolean(JNIEnv * env, jobject obj);
JNIEXPORT jbyte    JNICALL PRIMLIB_UnwrapByte   (JNIEnv * env, jobject obj);
JNIEXPORT jshort   JNICALL PRIMLIB_UnwrapShort  (JNIEnv * env, jobject obj);
JNIEXPORT jchar    JNICALL PRIMLIB_UnwrapChar   (JNIEnv * env, jobject obj);
JNIEXPORT jint     JNICALL PRIMLIB_UnwrapInt    (JNIEnv * env, jobject obj);
JNIEXPORT jlong    JNICALL PRIMLIB_UnwrapLong   (JNIEnv * env, jobject obj);
JNIEXPORT jfloat   JNICALL PRIMLIB_UnwrapFloat  (JNIEnv * env, jobject obj);
JNIEXPORT jdouble  JNICALL PRIMLIB_UnwrapDouble (JNIEnv * env, jobject obj);

/* Simple unwrapping functions. Objects *must* be of correct type. */
JNIEXPORT jboolean JNICALL PRIMLIB_GetBooleanObjectValue(JNIEnv * env, jobject obj);
JNIEXPORT jbyte    JNICALL PRIMLIB_GetByteObjectValue   (JNIEnv * env, jobject obj);
JNIEXPORT jshort   JNICALL PRIMLIB_GetShortObjectValue  (JNIEnv * env, jobject obj);
JNIEXPORT jchar    JNICALL PRIMLIB_GetCharObjectValue   (JNIEnv * env, jobject obj);
JNIEXPORT jint     JNICALL PRIMLIB_GetIntObjectValue    (JNIEnv * env, jobject obj);
JNIEXPORT jlong    JNICALL PRIMLIB_GetLongObjectValue   (JNIEnv * env, jobject obj);
JNIEXPORT jfloat   JNICALL PRIMLIB_GetFloatObjectValue  (JNIEnv * env, jobject obj);
JNIEXPORT jdouble  JNICALL PRIMLIB_GetDoubleObjectValue (JNIEnv * env, jobject obj);

/* jvalue conversion: Unwrap obj to the type of classType, with widening conversion. */
JNIEXPORT jvalue JNICALL PRIMLIB_UnwrapJValue(JNIEnv* env, jobject obj, jclass classType);

#endif
