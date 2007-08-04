/*gstinputstream.c - Header file for the GstClasspathPlugin
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
#include <glib/gprintf.h>

#include "gstinputstream.h"

struct _GstInputStreamPrivate
{
  JavaVM *vm;
  jobject *reader;
  
  gboolean eof;
  guint8 *buffer;
  long size;
  long length;
  
  gboolean disposed;
};

#define INPUT_STREAM_GET_PRIVATE(o)  \
  (G_TYPE_INSTANCE_GET_PRIVATE ((o), GST_TYPE_INPUT_STREAM, GstInputStreamPrivate))

/* properties */

enum
{
  ARG_0,
  ARG_JVM,
  ARG_READER
};

/* ***** */

static JNIEnv *gst_input_stream_get_jenv(GstInputStream *self);

static void gst_input_stream_set_property (GObject *object,
                                           guint property_id,
                                           const GValue *value,
                                           GParamSpec *pspec);

static void gst_input_stream_get_property (GObject  *object,
                                           guint property_id,
                                           GValue *value,
                                           GParamSpec *pspec);
                               
static void gst_input_stream_instance_init (GTypeInstance *instance,
                                            gpointer g_class);

static void gst_input_stream_class_init (gpointer g_class,
                                         gpointer g_class_data);

static GObject *
gst_input_stream_constructor (GType type, guint n_construct_properties,
                              GObjectConstructParam *construct_properties);

static void
gst_input_stream_dispose (GObject *obj);

static void
gst_input_stream_finalize (GObject *obj);

/* ************************************************************************** */

/* class methods */

int
gst_input_stream_read (GstInputStream *self, int *data, int offset,
                       int length)
{
  /* TODO: cache */
  jmethodID _readID = NULL;
  jclass InputStream = NULL;
  
  JNIEnv *env = NULL;
  
  int ret = -1;
  jbyteArray buffer;
  jbyte *bytes = NULL;
  
  if (self->priv->disposed || self->priv->vm == NULL ||
      self->priv->reader == NULL)
    {
      return -1;
    }

  env = gst_input_stream_get_jenv (self);
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
  
  InputStream = (*env)->GetObjectClass(env, self->priv->reader);
  _readID = (*env)->GetMethodID(env, InputStream, "read", "([BII)I");
  if (_readID == NULL)
    {
      (*env)->DeleteLocalRef(env, buffer);
      return -1;
    }
 
  ret = (*env)->CallIntMethod (env, self->priv->reader, _readID, buffer, 0,
                               length);
  if (ret == -1)
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

gboolean
gst_input_stream_available (GstInputStream *self, guint64 *size)
{
  /* TODO: caching */

  jmethodID _availableID = NULL;
  jclass InputStream = NULL;
  JNIEnv *env = NULL;

  if (self->priv->disposed || self->priv->vm == NULL ||
      self->priv->reader == NULL)
    {
      return FALSE;
    }
 
  env = gst_input_stream_get_jenv(self);
  if (env == NULL)
    {
      g_warning("GstInputStream::gst_input_stream_available failed to get java env");
      return FALSE;
    }
  
  InputStream = (*env)->GetObjectClass(env, self->priv->reader);
  _availableID = (*env)->GetMethodID(env, InputStream, "available", "()I");
  if (_availableID == NULL)
    {
      return FALSE;
    }
  
  *size = (*env)->CallIntMethod (env, self->priv->reader, _availableID); 
  
  return TRUE;
}

void gst_input_stream_reset (GstInputStream *self)
{
  jmethodID _resetID = NULL;
  jclass InputStream = NULL;
  JNIEnv *env = NULL;

  if (self->priv->disposed || self->priv->vm == NULL ||
      self->priv->reader == NULL)
    {
      return;
    }
 
  env = gst_input_stream_get_jenv(self);
  if (env == NULL)
    {
      g_warning("GstInputStream::gst_input_stream_reset failed to get java env");
      return;
    }
  
  InputStream = (*env)->GetObjectClass(env, self->priv->reader);
  _resetID = (*env)->GetMethodID(env, InputStream, "reset", "()V");
  if (_resetID == NULL)
    {
      return;
    }
  
  (*env)->CallVoidMethod (env, self->priv->reader, _resetID);
}

long gst_input_stream_skip (GstInputStream *self, long size)
{
  jmethodID _seekID = NULL;
  jclass InputStream = NULL;
  JNIEnv *env = NULL;

  long skipped = -1;

  if (self->priv->disposed || self->priv->vm == NULL ||
      self->priv->reader == NULL)
    {
      return skipped;
    }
 
  env = gst_input_stream_get_jenv(self);
  if (env == NULL)
    {
      g_warning("GstInputStream::gst_input_stream_skip failed to get java env");
      return size;
    }
  
  InputStream = (*env)->GetObjectClass(env, self->priv->reader);
  _seekID = (*env)->GetMethodID(env, InputStream, "skip", "(J)J");
  if (_seekID == NULL)
    {
      return skipped;
    }
  
  size = (*env)->CallIntMethod (env, self->priv->reader, _seekID, size); 
  if (size != 0)
    {
      return skipped;
    }
    
  return skipped;
}

gboolean gst_input_stream_can_seek (GstInputStream *self)
{
  if (gst_input_stream_skip(self, 0) != 0)
    {
      g_warning ("GstInputStream::gst_input_stream_can_seek CANNOT seek");
      return FALSE;
    }
  
  return TRUE;
}

/* ************************************************************************** */

/* getter and setter */

static void
gst_input_stream_set_property (GObject *object,
                               guint property_id,
                               const GValue *value,
                               GParamSpec *pspec)
{
  GstInputStream *self = GST_INPUT_STREAM (object);

  switch (property_id)
    {
      case ARG_JVM:
        {
          self->priv->vm = g_value_get_pointer(value);
        }
        break;

      case ARG_READER:
        {
          self->priv->reader = g_value_get_pointer(value);
        }
        break;
      
      default:
        /* We don't have any other property... */
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
        break;
    } /* switch */
}

static void
gst_input_stream_get_property (GObject  *object,
                               guint property_id,
                               GValue *value,
                               GParamSpec *pspec)
{
  GstInputStream *self = GST_INPUT_STREAM (object);

  switch (property_id)
    {
      case ARG_JVM:
        {
          g_value_set_pointer (value, self->priv->vm);
        }
        break;

      case ARG_READER:
        {
          g_value_set_pointer (value, self->priv->reader);
        }
        break;
      
      default:
        /* We don't have any other property... */
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
        break;
    } /* switch */
}

/* ************************************************************************** */

static void
gst_input_stream_instance_init (GTypeInstance *instance,
                                gpointer g_class __attribute__ ((unused)))
{
  GstInputStream *self = GST_INPUT_STREAM (instance);
  
  self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, GST_TYPE_INPUT_STREAM,
                                            GstInputStreamPrivate);

  self->priv->vm = NULL;
  self->priv->reader = NULL;
  self->priv->disposed = FALSE;
  self->priv->eof = FALSE;
  self->priv->buffer = NULL;
  self->priv->size = 0;
  self->priv->length = 0;
}

static void
gst_input_stream_class_init (gpointer g_class,
                             gpointer g_class_data __attribute__ ((unused)))
{
  GObjectClass *gobject_class;
  GstInputStreamClass *klass;
  GObjectClass *parent_class;
  
  GParamSpec *pspec;
  
  gobject_class = G_OBJECT_CLASS (g_class);
  klass = GST_INPUT_STREAM_CLASS (g_class);
  gobject_class = G_OBJECT_CLASS (g_class);
  
  g_type_class_add_private (klass, sizeof (GstInputStreamPrivate));
  
  gobject_class->set_property = gst_input_stream_set_property;
  gobject_class->get_property = gst_input_stream_get_property;
  gobject_class->dispose = gst_input_stream_dispose;
  gobject_class->finalize = gst_input_stream_finalize;
  gobject_class->constructor = gst_input_stream_constructor;

  parent_class = g_type_class_peek_parent (klass);

  /* register properties */
  pspec = g_param_spec_pointer (GST_ISTREAM_JVM,
                                "Set the java environment property",
                                "Set the java environment property",
                                G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class, ARG_JVM, pspec);
  
  pspec = g_param_spec_pointer (GST_ISTREAM_READER,
                                "Set the java reader property",
                                "Set the java reader property",
                                G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class, ARG_READER, pspec);

}

/* class constructors */

static GObject *
gst_input_stream_constructor (GType type, guint n_construct_properties,
                              GObjectConstructParam *construct_properties)
{
  GObject *obj;
  GObjectClass *parent_class;
  
  /* parent */
  GstInputStreamClass *klass;
  klass = GST_INPUT_STREAM_CLASS (g_type_class_peek (GST_TYPE_INPUT_STREAM));
  parent_class = g_type_class_peek_parent (klass);
  obj = parent_class->constructor (type, n_construct_properties,
                                   construct_properties);               
  return obj;
}

static void
gst_input_stream_dispose (GObject *obj)
{
  GObjectClass *parent_class;
  GstInputStream *self = GST_INPUT_STREAM (obj);
  if (self->priv->disposed)
    {
      /* If dispose did already run, return. */
      return;
    }

  /* Make sure dispose does not run twice. */
  self->priv->disposed = TRUE;

  if (self->priv->buffer != NULL)
    g_free(self->priv->buffer); 

  /* Chain up to the parent class */
  parent_class = g_type_class_peek_parent (GST_INPUT_STREAM_CLASS (obj));
  G_OBJECT_CLASS (parent_class)->dispose (obj);
}

static void
gst_input_stream_finalize (GObject *obj)
{
  /* nothing else to do */
  GObjectClass *parent_class =
    g_type_class_peek_parent (GST_INPUT_STREAM_CLASS (obj));
  G_OBJECT_CLASS (parent_class)->finalize (obj);
}

static JNIEnv *
gst_input_stream_get_jenv(GstInputStream *self)
{
    void *env = NULL;
    
    if ((*self->priv->vm)->GetEnv(self->priv->vm, &env, JNI_VERSION_1_2) != JNI_OK)
      {
        if ((*self->priv->vm)->AttachCurrentThreadAsDaemon(self->priv->vm,
                                                           &env, NULL) < 0)
          {
            g_warning ("GstInputStream:- env not attached");     
            return NULL;
          }
      }
       
    return (JNIEnv *) env;
}

GType gst_input_stream_get_type (void)
{
  static GType type = 0;
  
  if (type == 0)
    {
      static const GTypeInfo info = {
        sizeof (GstInputStreamClass),
        NULL,   /* base_init */
        NULL,   /* base_finalize */
        gst_input_stream_class_init,   /* class_init */
        NULL,   /* class_finalize */
        NULL,   /* class_data */
        sizeof (GstInputStream),
        0,      /* n_preallocs */
        gst_input_stream_instance_init    /* instance_init */
      };
      
      type = g_type_register_static (G_TYPE_OBJECT,
                                     "GstInputStreamType",
                                     &info, 0);
    }
    
    return type;
}
