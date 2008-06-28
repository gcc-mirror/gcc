/*gst_native_data_line.c - Implements the native methods of GstNativeDataLine 
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

#include <gst/gst.h>

#include "jcl.h"
#include "gnu_javax_sound_sampled_gstreamer_lines_GstNativeDataLine.h"

#include "gst_peer.h"
#include "gst_classpath_src.h"
#include "gst_native_pipeline.h"

static jfieldID pointerDataFID = NULL;

/* ************************************************************************** */

static GstElement *setup_pipeline (GstNativePipeline *jpipeline, int fd);
static void
gst_newpad (GstElement *decodebin, GstPad *pad, gboolean last, gpointer data);

/* ************************************************************************** */

JNIEXPORT void JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstNativeDataLine_init_1id_1cache
  (JNIEnv *env __attribute__ ((unused)), jclass clazz __attribute__ ((unused)))
{
  jclass pointerClass = NULL;
  
#if SIZEOF_VOID_P == 8
  pointerClass = JCL_FindClass (env, "gnu/classpath/Pointer64");
  if (pointerClass != NULL)
    {
      pointerDataFID = (*env)->GetFieldID (env, pointerClass, "data", "J");
    }
#else
# if SIZEOF_VOID_P == 4
  pointerClass = JCL_FindClass (env, "gnu/classpath/Pointer32"); 
  if (pointerClass != NULL)
    { 
      pointerDataFID = (*env)->GetFieldID(env, pointerClass, "data", "I");
    }
# else
#   error "Pointer size is not supported."
# endif /* SIZEOF_VOID_P == 4 */
#endif /* SIZEOF_VOID_P == 8 */
}

JNIEXPORT jboolean JNICALL
Java_gnu_javax_sound_sampled_gstreamer_lines_GstNativeDataLine_setup_1sink_1pipeline
  (JNIEnv *env, jclass clazz __attribute__ ((unused)),
   jobject pointer)
{
  GstNativePipeline *jpipeline = NULL;
  
  GstElement *pipeline = NULL;
  GstElement *sink = NULL;
  GstElement *audioconv= NULL;
  GstElement *resample = NULL;
  GstElement *audio = NULL;
  GstElement *decodebin = NULL;
  
  GstPad *audiopad = NULL;
  
  gst_init (NULL, NULL);
  
  /* get the pipeline from the pointer, then create it if needed */
  jpipeline = (GstNativePipeline *) get_object_from_pointer (env, pointer,
                                                            pointerDataFID);
  if (jpipeline == NULL)
    return JNI_FALSE;
 
  pipeline = setup_pipeline (jpipeline,
                             gst_native_pipeline_get_pipeline_fd (jpipeline));
  if (pipeline == NULL)
    return JNI_FALSE;
   
  /* add the audio sink to the pipeline */
  /* TODO: hardcoded values */
  sink = gst_element_factory_make ("autoaudiosink", "alsa-output");
  if (sink == NULL)
    {
      gst_object_unref(GST_OBJECT(pipeline));
      gst_object_unref(GST_OBJECT(sink));

      g_warning ("unable to create sink\n");
      return JNI_FALSE;
    }
  
  audioconv = gst_element_factory_make ("audioconvert", "aconv");
  if (audioconv == NULL)
    {
      gst_object_unref(GST_OBJECT(pipeline));
      gst_object_unref(GST_OBJECT(sink));
      gst_object_unref(GST_OBJECT(decodebin));

      g_warning ("unable to create audioconv\n");
      return JNI_FALSE;
    }
  
  audio = gst_bin_new ("audiobin");
  if (audio == NULL)
    {
      gst_object_unref(GST_OBJECT(pipeline));
      gst_object_unref(GST_OBJECT(sink));
      gst_object_unref(GST_OBJECT(decodebin));

      g_warning ("unable to create audioconv\n");
      return JNI_FALSE;
    }
  
  resample = gst_element_factory_make ("audioresample", "audioresample");
  if (audioconv == NULL)
    {
      gst_object_unref(GST_OBJECT(pipeline));
      gst_object_unref(GST_OBJECT(sink));
      gst_object_unref(GST_OBJECT(decodebin));
      gst_object_unref(GST_OBJECT(audio));

      g_warning ("unable to create resample\n");
      return JNI_FALSE;
    }
  
  audiopad = gst_element_get_pad (audioconv, "sink");
  gst_bin_add_many (GST_BIN (audio), audioconv, resample, sink, NULL);
  gst_element_link (audioconv, sink);

  gst_element_add_pad (audio, gst_ghost_pad_new ("sink", audiopad));

  gst_object_unref (audiopad);
  gst_bin_add (GST_BIN (pipeline), audio);
    
  decodebin = gst_bin_get_by_name (GST_BIN (pipeline), "decodebin");
  g_signal_connect (decodebin, "new-decoded-pad", G_CALLBACK (gst_newpad),
                    audio);
  
  gst_native_pipeline_set_pipeline (jpipeline, pipeline);
  
  return JNI_TRUE;
}

/* ************************************************************************** */

static GstElement *setup_pipeline (GstNativePipeline *jpipeline, int fd)
{
  GstElement *decodebin = NULL;
  GstElement *source = NULL;
  
  GstElement *pipeline = NULL;
  
  if (fd < 0)
    return NULL;
  
  pipeline = gst_pipeline_new ("java sound pipeline");
  if (pipeline == NULL)
    return NULL;
    
  decodebin = gst_element_factory_make ("decodebin", "decodebin");
  if (decodebin == NULL)
    {
      gst_object_unref(GST_OBJECT(pipeline));
      gst_object_unref(GST_OBJECT(source));

      g_warning ("unable to create decodebin\n");
      return NULL;
    }
    
  source = gst_element_factory_make ("fdsrc", "source");
  if (source == NULL)
    {
      gst_object_unref(GST_OBJECT(pipeline));
      gst_object_unref(GST_OBJECT(source));
      gst_object_unref(GST_OBJECT(decodebin));

      g_warning ("unable to create a source");
      return JNI_FALSE;
    }
  g_object_set (G_OBJECT (source), "fd", fd, NULL);
  
  gst_bin_add_many (GST_BIN (pipeline), source, decodebin, NULL);
  gst_element_link (source, decodebin);
  
  return pipeline;
}

static void
gst_newpad (GstElement *decodebin, GstPad *pad, gboolean last, gpointer data)
{
  GstCaps *caps;
  GstStructure *str;
  GstPad *audiopad;
    
  GstElement *audio = (GstElement *) data;

  /* only link once */
  audiopad = gst_element_get_pad (audio, "sink");
  if (GST_PAD_IS_LINKED (audiopad))
    {
      g_object_unref (audiopad);
      return;
    } 
  
  /* check media type */
  caps = gst_pad_get_caps (pad);
  str = gst_caps_get_structure (caps, 0);
  if (!g_strrstr (gst_structure_get_name (str), "audio"))
    {
      gst_caps_unref (caps);
      gst_object_unref (audiopad);
      return;
    } 
  gst_caps_unref (caps);
  
  /* link'n'play */
  gst_pad_link (pad, audiopad);
}
