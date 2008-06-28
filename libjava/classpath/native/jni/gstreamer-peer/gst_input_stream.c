/*GstInputStream.c - Header file for the GstClasspathPlugin
 Copyright (C) 2007 Free Software Foundation, Inc.

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

#include <jni.h>
#include <jcl.h>

#include <string.h>
#include <stdlib.h>

#include <gdk/gdk.h>

#include <glib.h>

#include "gst_peer.h"

#include "gnu_javax_sound_sampled_gstreamer_io_GstInputStream.h"
#include "gst_input_stream.h"

/* for caching */
static jmethodID readID = NULL;
static jmethodID pointerConstructorID = NULL;
static jmethodID availableID = NULL;

static jfieldID streamID = NULL;
static jfieldID pointerDataID = NULL;

struct _GstInputStreamPrivate
{
  JavaVM *vm;
  jclass readerClass;
  jclass pointerClass;
  
  jobject reader;
};

/* ************************************************************************** */

static void init_pointer_IDs (JNIEnv* env);

/* ************************************************************************** */

/* JNI Methods */

JNIEXPORT void JNICALL
Java_gnu_javax_sound_sampled_gstreamer_io_GstInputStream_init_1id_1cache
  (JNIEnv *env, jclass clazz)
{
  readID = (*env)->GetMethodID(env, clazz, "read", "([BII)I");
  availableID = (*env)->GetMethodID(env, clazz, "available", "()I");
  
  streamID = (*env)->GetFieldID(env, clazz, "gstInputStream",
                                "Lgnu/classpath/Pointer;");
  init_pointer_IDs(env);
}

JNIEXPORT void JNICALL
Java_gnu_javax_sound_sampled_gstreamer_io_GstInputStream_init_1instance
  (JNIEnv *env, jobject reader)
{
  GstInputStream *istream = NULL;
  
  jclass localReader = NULL;
  jclass localPointer = NULL;
  jobject _pointer = NULL;
  
  istream = (GstInputStream *) JCL_malloc (env, sizeof (GstInputStream));
  if (istream == NULL)
    return;
   
  istream->priv = (GstInputStreamPrivate *)
      JCL_malloc (env, sizeof (GstInputStreamPrivate));
  if (istream->priv == NULL)
    {
      JCL_free (env, istream);
      return;
    }
  
  /* get a local references first */
  localReader = (*env)->GetObjectClass(env, reader);
  if (localReader == NULL)
    {
      JCL_free (env, istream->priv);
      JCL_free (env, istream);
      JCL_ThrowException (env, "java/lang/InternalError",
                               "Class Initialization failed.");
      
      return;
    }
  
#if SIZEOF_VOID_P == 8
  localPointer = JCL_FindClass (env, "gnu/classpath/Pointer64");
#else
# if SIZEOF_VOID_P == 4
  localPointer = JCL_FindClass (env, "gnu/classpath/Pointer32");
# else
#   error "Pointer size is not supported."
# endif /* SIZEOF_VOID_P == 4 */
#endif /* SIZEOF_VOID_P == 8 */

  if (localReader == NULL || localPointer == NULL)
    { 
      JCL_free (env, istream->priv);
      JCL_free (env, istream);
      JCL_ThrowException (env, "java/lang/InternalError",
                               "Class Initialization failed.");
      return;
    }

  /* fill out our structure */
  istream->priv->readerClass = (*env)->NewGlobalRef(env, localReader);
  istream->priv->pointerClass = (*env)->NewGlobalRef(env, localPointer);
  (*env)->GetJavaVM(env, &istream->priv->vm);
  istream->priv->reader = (*env)->NewGlobalRef(env, reader);
  
  _pointer = (*env)->GetObjectField(env, reader, streamID);
  
   /* this should be always null */
  if (_pointer == NULL)
    {
#if SIZEOF_VOID_P == 8
      _pointer = (*env)->NewObject(env, istream->priv->pointerClass,
                                   pointerConstructorID, (jlong) istream);
#else
      _pointer = (*env)->NewObject(env, istream->priv->pointerClass,
                                   pointerConstructorID, (jint) istream);
#endif
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, reader, streamID, (jlong) istream);
#else
      (*env)->SetIntField(env, reader, streamID, (jint) istream);
#endif
    }
    
    /* store back our pointer into the calling class */
    (*env)->SetObjectField(env, reader, streamID, _pointer);
}

/* exported library functions */

void
gst_input_stream_clean (GstInputStream *self)
{
  JNIEnv *env = NULL;
  
  env = gst_get_jenv (self->priv->vm);
  
  (*env)->DeleteGlobalRef (env, self->priv->reader);
  (*env)->DeleteGlobalRef (env, self->priv->readerClass);
  (*env)->DeleteGlobalRef (env, self->priv->pointerClass);
  
  JCL_free (env, self->priv);
  JCL_free (env, self);
}

int
gst_input_stream_available (GstInputStream *self)
{
  JNIEnv *env = NULL;
  
  if (self == NULL || self->priv == NULL ||
      self->priv->vm == NULL || self->priv->reader == NULL)
    {
      return -1;
    }
  
  env = gst_get_jenv (self->priv->vm);
  if (env == NULL)
    {
      g_warning("GstInputStream::gst_input_stream_available " \
                "failed to get java env");
      return -1;
    }
  
  return (*env)->CallIntMethod (env, self->priv->reader, availableID); 
}

int
gst_input_stream_read (GstInputStream *self, int *data, int offset,
                       int length)
{
  JNIEnv *env = NULL;
  
  int ret = -1;
  jbyteArray buffer;
  jbyte *bytes = NULL;
  
  if (self == NULL || self->priv == NULL ||
      self->priv->vm == NULL || self->priv->reader == NULL)
    {
      return -1;
    }

  env = gst_get_jenv (self->priv->vm);
  if (env == NULL)
    {
      g_warning("GstInputStream::gst_input_stream_read failed to get java env");
      return -1;
    }
   
  buffer = (*env)->NewByteArray (env, length);   
  if (buffer == NULL)
    {
      g_warning ("GstInputStream::gst_input_stream_read called, failed");
      return -1;
    }

  ret = (*env)->CallIntMethod (env, self->priv->reader, readID, buffer, 0,
                               length);
  if (ret < 0)
    {
      (*env)->DeleteLocalRef(env, buffer);
      return ret;
    }
  
  bytes = (*env)->GetByteArrayElements (env, buffer, NULL);
  
  /* copy bytes and release */
  memcpy (data + offset, bytes, ret);
  
  (*env)->ReleaseByteArrayElements (env, buffer, bytes, 0);
  (*env)->DeleteLocalRef (env, buffer);

  return ret;
}

/* private functions */

static void init_pointer_IDs (JNIEnv* env)
{
  jclass pointerClass = NULL;
  
#if SIZEOF_VOID_P == 8
  pointerClass = JCL_FindClass (env, "gnu/classpath/Pointer64");
  if (pointerClass != NULL)
    {
      pointerDataID = (*env)->GetFieldID (env, pointerClass, "data", "J");
      pointerConstructorID = (*env)->GetMethodID (env, pointerClass, "<init>",
                                                  "(J)V");
    }
#else
# if SIZEOF_VOID_P == 4
  pointerClass = JCL_FindClass (env, "gnu/classpath/Pointer32"); 
  if (pointerClass != NULL)
    { 
      pointerDataID = (*env)->GetFieldID(env, pointerClass, "data", "I");
      pointerConstructorID = (*env)->GetMethodID(env, pointerClass,
                                                 "<init>", "(I)V");
    }
# else
#   error "Pointer size is not supported."
# endif /* SIZEOF_VOID_P == 4 */
#endif /* SIZEOF_VOID_P == 8 */
}
