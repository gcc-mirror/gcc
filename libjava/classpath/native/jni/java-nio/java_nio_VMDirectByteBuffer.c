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

JNIEXPORT jobject JNICALL
Java_java_nio_VMDirectByteBuffer_allocate
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)), jint capacity)
{
  void *buffer;

  if (capacity < 0)
    {
      JCL_ThrowException (env, "java/lang/IllegalArgumentException",
			  "negative capacity");
      return 0;
    }

  buffer = malloc (capacity);

  if (buffer == NULL)
    {
      JCL_ThrowException (env, "java/lang/OutOfMemoryError",
			  "unable to allocate memory for direct byte buffer");
      return 0;
    }

  memset (buffer, 0, capacity);

  return JCL_NewRawDataObject (env, buffer);
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_free
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)), jobject address)
{
  free (JCL_GetRawData (env, address));
}

JNIEXPORT jbyte JNICALL
Java_java_nio_VMDirectByteBuffer_get__Lgnu_classpath_Pointer_2I
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint index)
{
  return ((jbyte *) JCL_GetRawData (env, address))[index];
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_put__Lgnu_classpath_Pointer_2IB
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint index, jbyte value)
{
  jbyte *pointer = (jbyte *) JCL_GetRawData (env, address) + index;
  *pointer = value;
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_get__Lgnu_classpath_Pointer_2I_3BII
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint index, jbyteArray dst, jint dst_offset, jint dst_len)
{
  jbyte *src = (jbyte *) JCL_GetRawData (env, address) + index;
  jbyte *_dst = (*env)->GetByteArrayElements (env, dst, NULL);
  memcpy (_dst + dst_offset, src, dst_len);
  (*env)->ReleaseByteArrayElements (env, dst, _dst, 0);
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_put__Lgnu_classpath_Pointer_2I_3BII
  (JNIEnv *env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint index, jbyteArray src, jint src_offset, jint src_len)
{
  jbyte *_src = (*env)->GetByteArrayElements (env, src, NULL);
  jbyte *dst = (jbyte *)JCL_GetRawData (env, address);
  memcpy (dst + index, _src + src_offset, src_len);
  (*env)->ReleaseByteArrayElements (env, src, _src, 0);
}

JNIEXPORT void JNICALL
Java_java_nio_VMDirectByteBuffer_shiftDown
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint dst_offset, jint src_offset, jint count)
{
  jbyte *dst = (jbyte *) JCL_GetRawData (env, address) + dst_offset;
  jbyte *src = (jbyte *) JCL_GetRawData (env, address) + src_offset;
  memmove (dst, src, count);
}

JNIEXPORT jobject JNICALL
Java_java_nio_VMDirectByteBuffer_adjustAddress
  (JNIEnv * env, jclass clazz __attribute__ ((__unused__)),
   jobject address, jint offset)
{
  return JCL_NewRawDataObject (env, (jbyte *) JCL_GetRawData (env, address) + offset);
}
