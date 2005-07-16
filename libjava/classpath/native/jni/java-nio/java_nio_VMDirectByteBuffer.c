/* java_nio_VMDirectByteBuffer.c - Native methods for VMDirectByteBuffer
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <jni.h>
#include <jcl.h>

#include "java_nio_VMDirectByteBuffer.h"

static jclass classRawData;
static jmethodID methodRawDataInit;
static jfieldID fieldNativePointer;

static void *NIOGetPointer (JNIEnv *, jobject);
static jobject NIOGetRawData (JNIEnv *, void *pointer);

static void *
NIOGetPointer (JNIEnv * env, jobject rawdata)
{
#if SIZEOF_VOID_P == 4
  return (void *) (*env)->GetIntField (env, rawdata, fieldNativePointer);
#elif SIZEOF_VOID_P == 8
  return (void *) (*env)->GetLongField (env, rawdata, fieldNativePointer);
#else
#error unsupported pointer size
#endif
}

static jobject
NIOGetRawData (JNIEnv * env, void *pointer)
{
#if SIZEOF_VOID_P == 4
  return (*env)->NewObject (env, classRawData, methodRawDataInit,
			    (jint) pointer);
#elif SIZEOF_VOID_P == 8
  return (*env)->NewObject (env, classRawData, methodRawDataInit,
			    (jlong) pointer);
#else
#error unsupported pointer size
#endif
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_init
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)))
{
#if SIZEOF_VOID_P == 4
  classRawData = (*env)->FindClass (env, "gnu/classpath/RawData32");
  if (classRawData == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
			  "unable to find internal class");
      return;
    }

  methodRawDataInit = (*env)->GetMethodID (env, classRawData,
					   "<init>", "(I)V");
  if (methodRawDataInit == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
			  "unable to find internal constructor");
      return;
    }

  fieldNativePointer = (*env)->GetFieldID (env, classRawData, "data", "I");
  if (fieldNativePointer == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
			  "unable to find internal field");
      return;
    }
#elif SIZEOF_VOID_P == 8
  classRawData = (*env)->FindClass (env, "gnu/classpath/RawData64");
  if (classRawData == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
			  "unable to find internal class");
      return;
    }

  methodRawDataInit = (*env)->GetMethodID (env, classRawData,
					   "<init>", "(J)V");
  if (methodRawDataInit == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
			  "unable to find internal constructor");
      return;
    }

  fieldNativePointer = (*env)->GetFieldID (env, classRawData, "data", "J");
  if (fieldNativePointer == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
			  "unable to find internal field");
      return;
    }
#else
#error unsupported pointer size
#endif

  /* We need to wrap the jclass in global reference to make it persistent */
  if ((classRawData = (*env)->NewGlobalRef (env, classRawData)) == NULL)
    {
      JCL_ThrowException (env, "java/lang/InternalError",
			  "failed to create global reference");
      return;
    }
}

JNIEXPORT jobject JNICALL
Java_java_nio_VMDirectByteBuffer_allocate
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)), jint capacity)
{
  void *buffer;

  buffer = malloc (capacity);

  if (buffer == NULL)
    {
      JCL_ThrowException (env, "java/lang/OutOfMemoryError",
			  "unable to allocate memory for direct byte buffer");
      return 0;
    }

  return NIOGetRawData (env, buffer);
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_free
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)), jobject address)
{
  free (NIOGetPointer (env, address));
}

JNIEXPORT jbyte JNICALL
Java_java_nio_VMDirectByteBuffer_get__Lgnu_classpath_RawData_2I
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint index)
{
  return ((jbyte *) NIOGetPointer (env, address))[index];
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_put
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint index, jbyte value)
{
  jbyte *pointer = (jbyte *) NIOGetPointer (env, address) + index;
  *pointer = value;
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_get__Lgnu_classpath_RawData_2I_3BII
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint index, jbyteArray dst, jint dst_offset, jint dst_len)
{
  jbyte *src = (jbyte *) NIOGetPointer (env, address) + index;
  memcpy ((*env)->GetByteArrayElements (env, dst, NULL) + dst_offset, src,
	  dst_len);
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_shiftDown
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint dst_offset, jint src_offset, jint count)
{
  jbyte *dst = (jbyte *) NIOGetPointer (env, address) + dst_offset;
  jbyte *src = (jbyte *) NIOGetPointer (env, address) + src_offset;
  memmove (dst, src, count);
}

JNIEXPORT jobject JNICALL
Java_java_nio_VMDirectByteBuffer_adjustAddress
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint offset)
{
  return NIOGetRawData (env, (jbyte *) NIOGetPointer (env, address) + offset);
}
