/*gst_native_pipeline.c - Header file for the GstClasspathPlugin
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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <unistd.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */

#if defined(HAVE_SYS_IOCTL_H)
#define BSD_COMP /* Get FIONREAD on Solaris2 */
#include <sys/ioctl.h>
#endif
#if defined(HAVE_SYS_FILIO_H) /* Get FIONREAD on Solaris 2.5 */
#include <sys/filio.h>
#endif

#include <gdk/gdk.h>
#include <glib.h>

#include <gst/gst.h>

#include "cpio.h"
#include "gst_peer.h"

#include "gnu_javax_sound_sampled_gstreamer_lines_GstPipeline.h"
#include "gst_native_pipeline.h"

static jmethodID pointerConstructorMID = NULL;

static jfieldID pipelineFID = NULL;
static jfieldID pointerDataFID = NULL;
static jfieldID nameFID = NULL;
static jfieldID capacityFID = NULL;

/*
 * Needed to compute the size of the data still available for processing in the
 * pipeline. We give a default here but this will be overwritten by the
 * detection routines.
 */
static long GST_DETECTED_PIPE_CAPACITY = 65536;

/*
 * Note: the Java code uses enum classes, these are not mapped into constants
 * by the javah tool, changes to these values should be reflected in the Java
 * side.
 */
enum
{
  PLAY,
  PAUSE,
  STOP
};

/*
 * Defined as constants in the Java code, hence mapped by javah.
 */
enum
{
  READ = gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_READ,
  WRITE = gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_WRITE
};

struct _GstNativePipelinePrivate
{
  JavaVM *vm;
  jclass GstPipelineClass;
  jclass PointerClass;
  
  jobject jni_pipeline;

  char *name;
  int fd;
 
  GstElement *pipeline;
};

/* ************************************************************************** */
/*
static void gst_native_pipeline_clean (GstNativePipeline *self);*/
static char *create_name (void);
static void init_pointer_IDs (JNIEnv* env);
static jint get_free_space (int fd);
static void detect_pipe_max (void);

/* ************************************************************************** */

/* JNI Methods */

JNIEXPORT void JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_init_1id_1cache
  (JNIEnv *env, jclass clazz)
{
  pipelineFID = (*env)->GetFieldID (env, clazz, "pipeline",
                                    "Lgnu/classpath/Pointer;");
  nameFID = (*env)->GetFieldID (env, clazz, "name", "Ljava/lang/String;");
  capacityFID = (*env)->GetFieldID (env, clazz, "capacity", "J");

  init_pointer_IDs (env);
}

JNIEXPORT void JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_init_1instance
  (JNIEnv *env, jobject pipeline)
{
  GstNativePipeline *_pipeline = NULL;
  
  jclass localGstPipelineClass = NULL;
  jclass localPointerClass = NULL;
  jobject _pointer = NULL;
  
  _pipeline =
    (GstNativePipeline *) JCL_malloc (env, sizeof (GstNativePipeline));
  if (_pipeline == NULL)
    return;
  
  _pipeline->priv = (GstNativePipelinePrivate *)
    JCL_malloc (env, sizeof (GstNativePipelinePrivate));
  if (_pipeline->priv == NULL)
    {
      JCL_free (env, _pipeline);
      return;
    }
  
#if SIZEOF_VOID_P == 8
  localPointerClass = JCL_FindClass (env, "gnu/classpath/Pointer64");
#else
# if SIZEOF_VOID_P == 4
  localPointerClass = JCL_FindClass (env, "gnu/classpath/Pointer32");
# else
#   error "Pointer size is not supported."
# endif /* SIZEOF_VOID_P == 4 */
#endif /* SIZEOF_VOID_P == 8 */

  localGstPipelineClass = (*env)->GetObjectClass(env, pipeline);
  if (localGstPipelineClass == NULL || localGstPipelineClass == NULL)
    {
      JCL_free (env, _pipeline->priv);
      JCL_free (env, _pipeline);
      JCL_ThrowException (env, "java/lang/InternalError",
                               "Class Initialization failed.");
      return;
    }

  GST_DETECTED_PIPE_CAPACITY = (long) (*env)->GetLongField(env, pipeline,
                                                           capacityFID);
  
  /* fill the object */
  (*env)->GetJavaVM(env, &_pipeline->priv->vm);
  _pipeline->priv->jni_pipeline = (*env)->NewGlobalRef(env, pipeline);
  _pipeline->priv->GstPipelineClass =
    (*env)->NewGlobalRef(env, localGstPipelineClass);
  _pipeline->priv->PointerClass = (*env)->NewGlobalRef(env, localPointerClass);
  _pipeline->priv->pipeline = NULL;
  
  _pointer = (*env)->GetObjectField(env, pipeline, pipelineFID);
  
  if (_pointer == NULL)
    {
#if SIZEOF_VOID_P == 8
      _pointer = (*env)->NewObject(env, _pipeline->priv->PointerClass,
                                   pointerConstructorMID, (jlong) _pipeline);
#else
      _pointer = (*env)->NewObject(env, _pipeline->priv->PointerClass,
                                   pointerConstructorMID, (jint) _pipeline);
#endif
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, pipeline, pipelineFID, (jlong) _pipeline);
#else
      (*env)->SetIntField(env, pipeline, pipelineFID, (jint) _pipeline);
#endif
    }
      
  /* store back our pointer into the calling class */
  (*env)->SetObjectField(env, pipeline, pipelineFID, _pointer);
}

JNIEXPORT jboolean JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_set_1state
  (JNIEnv *env, jclass clazz, jobject pointer, jint state)
{
  GstNativePipeline *jpipeline = NULL;
  jboolean result = JNI_FALSE;
  
  if (pointer == NULL)
    {
      JCL_ThrowException (env, "javax/sound/sampled/LineUnavailableException",
                               "Can't change pipeline state: " \
                               "pipeline not initialized");
      return result;
    }
    
  jpipeline = (GstNativePipeline *) get_object_from_pointer (env, pointer,
                                                             pointerDataFID);
  if (jpipeline == NULL)
    return JNI_FALSE;
                                                         
  switch (state)
    {
      case (PLAY):
        gst_element_set_state(GST_ELEMENT(jpipeline->priv->pipeline),
                              GST_STATE_PLAYING);
        result = JNI_TRUE;
        break;
        
      case (PAUSE):
        gst_element_set_state(GST_ELEMENT(jpipeline->priv->pipeline),
                              GST_STATE_PAUSED);
        result = JNI_TRUE;
        break;
        
      case (STOP):
#ifndef WITHOUT_FILESYSTEM
        /* clean the pipeline and kill named pipe */
        if (jpipeline->priv->name)
          {
            cpio_removeFile (jpipeline->priv->name);
            g_free (jpipeline->priv->name);
            jpipeline->priv->name = NULL;
          }
#endif /* WITHOUT_FILESYSTEM */
  
        if (jpipeline->priv->pipeline != NULL)
          gst_object_unref (GST_OBJECT(jpipeline->priv->pipeline));
        result = JNI_TRUE;
        break;
        
      default:
        /* nothing */
        result = JNI_FALSE;
        break; 
    }
    
  return result;
}

JNIEXPORT void JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_open_1native_1pipe
  (JNIEnv *env, jclass clazz, jobject pointer, jint mode)
{
  GstNativePipeline *jpipeline = NULL;
  
  jpipeline = (GstNativePipeline *) get_object_from_pointer (env, pointer,
                                                             pointerDataFID);
  switch (mode)
    {
      case (READ):
        jpipeline->priv->fd =
            open (jpipeline->priv->name, O_RDONLY | O_NONBLOCK);
        break;
      
      case (WRITE):
        /* TODO: no-op currently */
        break;
    }
}

JNIEXPORT void JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_close_1native_1pipe
  (JNIEnv *env, jclass clazz, jobject pointer)
{
#ifndef WITHOUT_FILESYSTEM
  GstNativePipeline *jpipeline = NULL;
  jpipeline = (GstNativePipeline *) get_object_from_pointer (env, pointer,
                                                             pointerDataFID);
  /* kill the named pipe */
  if (jpipeline->priv->name)
    {
      cpio_removeFile (jpipeline->priv->name);
      g_free (jpipeline->priv->name);
      jpipeline->priv->name = NULL;
    }
#endif /* WITHOUT_FILESYSTEM */
}

JNIEXPORT jboolean JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_create_1named_1pipe
  (JNIEnv *env, jobject GstPipeline, jobject pointer)
{
#ifndef WITHOUT_FILESYSTEM
  /*
   * We get a temp name for the named pipe, create the named pipe and then
   * set the relative field in the java class.
   */
  GstNativePipeline *jpipeline = NULL;
  jstring *name = NULL;
  
  jpipeline = (GstNativePipeline *) get_object_from_pointer (env, pointer,
                                                             pointerDataFID);
  if (jpipeline == NULL)
    return JNI_FALSE;                                                        
  
  jpipeline->priv->name = create_name ();
  if (jpipeline->priv->name == NULL)
    return JNI_FALSE;
   
  if (mkfifo (jpipeline->priv->name, 0600) < 0)
    {
      if (jpipeline->priv->name != NULL)
        free (jpipeline->priv->name);
      return JNI_FALSE;
    }
  
  /* now set the String field */
  name = (*env)->NewStringUTF(env, jpipeline->priv->name);
  if (name == NULL)
    {
      cpio_removeFile (jpipeline->priv->name);
      if (jpipeline->priv->name != NULL)
        free (jpipeline->priv->name);
      
      return JNI_FALSE;
    }
  
  (*env)->SetObjectField(env, GstPipeline, nameFID, name);
    
  return JNI_TRUE;
  
#else /* not WITHOUT_FILESYSTEM */
  return JNI_FALSE;
#endif /* not WITHOUT_FILESYSTEM */
}

JNIEXPORT jint JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_available
  (JNIEnv *env, jclass clazz, jobject pointer, jint mode)
{
  jint result = -1;
 
#ifndef WITHOUT_FILESYSTEM
  
  GstNativePipeline *jpipeline = NULL;
  jpipeline = (GstNativePipeline *) get_object_from_pointer (env, pointer,
                                                             pointerDataFID);
                                                                                                         
  if (mode == READ)
    {
      result = get_free_space (jpipeline->priv->fd);
    }
  else
    {
# if defined (FIONREAD)      
      if (ioctl (jpipeline->priv->fd, FIONREAD, &result) == -1)
        g_warning("IMPLEMENT ME: ioctl failed");
        
# else /* not defined (FIONREAD) */
      g_warning("IMPLEMENT ME: !defined (FIONREAD");
# endif /* defined (FIONREAD) */
    
    } /* if (mode == READ) */
    
#endif  /* not WITHOUT_FILESYSTEM */

  return result;
}

JNIEXPORT jlong JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstPipeline_detect_1pipe_1size
  (JNIEnv *env, jobject GstPipeline)
{
  detect_pipe_max ();
  
  return GST_DETECTED_PIPE_CAPACITY;
}

/* exported library functions */
/*
static void gst_native_pipeline_clean (GstNativePipeline *self)
{
  JNIEnv *env = NULL;
  
  env = gst_get_jenv (self->priv->vm);
  
  (*env)->DeleteGlobalRef (env, self->priv->jni_pipeline);
  (*env)->DeleteGlobalRef (env, self->priv->GstPipelineClass);
  (*env)->DeleteGlobalRef (env, self->priv->PointerClass);
  
  if (self->priv->pipeline != NULL)
    gst_object_unref (GST_OBJECT (self->priv->pipeline));
  
  if (self->priv->name)
    {
      cpio_removeFile (self->priv->name);
      g_free (self->priv->name);
      self->priv->name = NULL;
    }
    
  JCL_free (env, self->priv);
  JCL_free (env, self);
}
*/
void gst_native_pipeline_set_pipeline (GstNativePipeline *self,
                                       GstElement *pipeline)
{
  if (self->priv->pipeline != NULL)
    gst_object_unref (GST_OBJECT (self->priv->pipeline));
    
  self->priv->pipeline = pipeline;
}

GstElement *gst_native_pipeline_get_pipeline (GstNativePipeline *self)
{
  return self->priv->pipeline;
}

char *gst_native_pipeline_get_pipeline_name (GstNativePipeline *self)
{
  return self->priv->name;
}

int gst_native_pipeline_get_pipeline_fd (GstNativePipeline *self)
{
  return self->priv->fd;
}

/* private functions */

static void init_pointer_IDs (JNIEnv* env)
{
  jclass PointerClass = NULL;
  
#if SIZEOF_VOID_P == 8
  PointerClass = JCL_FindClass (env, "gnu/classpath/Pointer64");
  if (PointerClass != NULL)
    {
      pointerDataFID = (*env)->GetFieldID (env, PointerClass, "data", "J");
      pointerConstructorMID = (*env)->GetMethodID (env, PointerClass, "<init>",
                                                   "(J)V");
    }
#else
# if SIZEOF_VOID_P == 4
  PointerClass = JCL_FindClass (env, "gnu/classpath/Pointer32"); 
  if (PointerClass != NULL)
    { 
      pointerDataFID = (*env)->GetFieldID(env, PointerClass, "data", "I");
      pointerConstructorMID = (*env)->GetMethodID(env, PointerClass,
                                                  "<init>", "(I)V");
    }
# else
#   error "Pointer size is not supported."
# endif /* SIZEOF_VOID_P == 4 */
#endif /* SIZEOF_VOID_P == 8 */
}

static jint get_free_space (int fd)
{
  jint result = -1;
  
#if defined (FIONSPACE)

  if (ioctl (fd, FIONSPACE, &result) == -1)
    {
      g_warning("IMPLEMENT ME: ioctl failed");
    }
    
#elif defined (FIONREAD)

  if (ioctl (fd, FIONREAD, &result) == -1)
    {
      g_warning("IMPLEMENT ME: ioctl failed");
    }

  result = GST_DETECTED_PIPE_CAPACITY - result;
  
#else
   g_warning("IMPLEMENT ME!!! - !defined (FIONSPACE), !defined (FIONREAD");
 
#endif

  return result;
}

static char *create_name (void)
{
  char *buffer = NULL;
  char *tmp = NULL;
  
  buffer = (char *) g_malloc0 (_GST_MALLOC_SIZE_);
  if (buffer == NULL)
    {
      /* huston, we have a problem... */
      return NULL;
    }
    
  tmp = tempnam (NULL, _GST_PIPELINE_PREFIX_);
  if (tmp == NULL)
    {
      g_free (buffer);
      return NULL;
    }
  
  g_snprintf (buffer, _GST_MALLOC_SIZE_, "%s%s", tmp, _GST_PIPELINE_SUFFIX_);
  g_free (tmp);
  
  return buffer;
}

static void detect_pipe_max (void)
{
  int read_fd;
  int write_fd;
  
  /* can be anything! */
  char *character = "a";
  char *pipe = NULL;
  
  gboolean available = TRUE;
  int w = 0;
  long wrote = 0;
  
  pipe = create_name ();
  if (pipe == NULL)
    {
      g_warning ("can't create test pipe name");
      return;
    }
 
  if (mkfifo (pipe, 0600) < 0)
    {
      g_warning ("unable to create test pipe...");
      g_free (pipe);
      
      return;
    }
 
  /* open both end of the pipe */
  read_fd = open (pipe, O_RDONLY | O_NONBLOCK);
  if (read_fd < 0)
    {
      cpio_removeFile (pipe);
      g_free (pipe);
      
      return;
    }
    
  write_fd = open (pipe, O_WRONLY | O_NONBLOCK);
  if (write_fd < 0)
    {
      cpio_closeFile (write_fd);
      cpio_removeFile (pipe);
      g_free (pipe);
      
      return;
    }

  while (available)
    {
      w = 0;
          
      cpio_write (write_fd, character, 1, &w);
      if (w < 0)
        available = FALSE;
      else
        wrote += w;
    }
    
  GST_DETECTED_PIPE_CAPACITY = wrote;
    
  cpio_closeFile (write_fd);    
  cpio_closeFile (read_fd);
  cpio_removeFile (pipe);
      
  g_free (pipe);  
}
