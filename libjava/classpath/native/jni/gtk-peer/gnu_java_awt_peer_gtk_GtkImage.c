/* gtkimage.c
   Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.

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

#include "jcl.h"
#include "gtkpeer.h"

#include "gnu_java_awt_peer_gtk_GtkImage.h"

/* The constant fields in java.awt.Image */   
#define SCALE_DEFAULT      1
#define SCALE_FAST         2
#define SCALE_SMOOTH       4
#define SCALE_REPLICATE    8 
#define SCALE_AREA_AVERAGING  16

/* local stuff */
static GdkInterpType mapHints(jint hints);
static void createRawData (JNIEnv * env, jobject obj, void *ptr);
static void setWidthHeight (JNIEnv * env, jobject obj, int width, int height);

/**
 * Loads a pixbuf from a file.
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_loadPixbuf
  (JNIEnv *env, jobject obj, jstring name)
{
  const char *filename;
  int width, height;
  GdkPixbuf *pixbuf;

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);

  if (filename == NULL)
    return JNI_FALSE;

  pixbuf = gdk_pixbuf_new_from_file (filename, NULL);
  if (pixbuf == NULL)
    {
      (*env)->ReleaseStringUTFChars (env, name, filename);
      return JNI_FALSE;
    }

  width =  gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  
  createRawData (env, obj, pixbuf);
  setWidthHeight(env, obj, width, height);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  return JNI_TRUE;
}

/*
 * Creates the image from an array of java bytes.
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_loadImageFromData
  (JNIEnv *env, jobject obj, jbyteArray data)
{
  jbyte *src;
  GdkPixbuf* pixbuf;
  GdkPixbufLoader* loader;
  int len;
  int width;
  int height;

  src = (*env)->GetByteArrayElements (env, data, NULL);
  len = (*env)->GetArrayLength (env, data);

  loader = gdk_pixbuf_loader_new ();

  gdk_pixbuf_loader_write (loader, (guchar *)src, len, NULL);
  gdk_pixbuf_loader_close (loader, NULL);

  (*env)->ReleaseByteArrayElements (env, data, src, 0);

  pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);

  if (pixbuf == NULL)
    {
      g_object_unref (loader);
      createRawData (env, obj, NULL);
      return JNI_FALSE;
    }

  g_object_ref (pixbuf);
  g_object_unref (loader);

  width =  gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  createRawData (env, obj, pixbuf);
  setWidthHeight(env, obj, width, height);

  return JNI_TRUE;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_createFromPixbuf
(JNIEnv *env, jobject obj)
{
  int width, heigth;
  GdkPixbuf *pixbuf = cp_gtk_image_get_pixbuf (env, obj);
  width =  gdk_pixbuf_get_width (pixbuf);
  heigth = gdk_pixbuf_get_height (pixbuf);
  setWidthHeight(env, obj, width, heigth);
}

/**
 * Returns a copy of the pixel data as a java array.
 */
JNIEXPORT jintArray JNICALL 
Java_gnu_java_awt_peer_gtk_GtkImage_getPixels(JNIEnv *env, jobject obj)
{
  GdkPixbuf *pixbuf;
  int width, height, rowstride;
  guchar *pixeldata;
  jintArray result_array;
  jint *result_array_iter, *dst;
  int i,j;

  gdk_threads_enter ();

  pixbuf = cp_gtk_image_get_pixbuf (env, obj);
  width =  gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  rowstride = gdk_pixbuf_get_rowstride (pixbuf);

  result_array = (*env)->NewIntArray (env, (width * height));
  if (result_array == NULL)
    {
      gdk_threads_leave ();
      return NULL;
    }

  dst = result_array_iter = 
    (*env)->GetIntArrayElements (env, result_array, NULL);


  pixeldata = gdk_pixbuf_get_pixels (pixbuf);

  g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);

  if (gdk_pixbuf_get_has_alpha (pixbuf))
    {
      for(i = 0 ; i < height; i++)
	{
	  memcpy(dst, (void *)pixeldata, width * 4);
	  dst += width;
	  pixeldata += rowstride;
	}
    } else {

    /* Add a default alpha value of 0xFF to the pixeldata without alpha
       information and keep it in the same format as the pixeldata with alpha
       information. On Little Endian systems: AABBGGRR and on Big Endian
       systems: RRGGBBAA.  */

      for(i = 0; i < height; i++)
	{
	  for(j = 0; j < width; j++)

#ifndef WORDS_BIGENDIAN
	    dst[j] = 0xFF000000
	      | (pixeldata[j*3 + 2] & 0xFF) << 16
	      | (pixeldata[j*3 + 1] & 0xFF) << 8
	      | (pixeldata[j*3] & 0xFF);
#else
	    dst[j] = (pixeldata[j*3] & 0xFF) << 24
	      | (pixeldata[j*3 + 1] & 0xFF) << 16
	      | (pixeldata[j*3 + 2] & 0xFF) << 8
	      | 0xFF;
#endif
	  dst += width;
	  pixeldata += rowstride;
	}
    }
  
  (*env)->ReleaseIntArrayElements (env, result_array, result_array_iter, 0);
    
  gdk_threads_leave ();
  return result_array;
}

/**
 * Returns a copy of the pixel data as a java array.
 * (GdkPixbuf only)
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkImage_setPixels(JNIEnv *env, jobject obj,
					      jintArray pixels)
{
  GdkPixbuf *pixbuf = cp_gtk_image_get_pixbuf (env, obj);
  int width, height, rowstride;
  guchar *pixeldata;
  jint *src_array_iter, *src;
  int i;

  width =  gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  rowstride = gdk_pixbuf_get_rowstride (pixbuf);

  src = src_array_iter = 
    (*env)->GetIntArrayElements (env, pixels, NULL);

  pixeldata = gdk_pixbuf_get_pixels (pixbuf);
  for(i = 0 ; i < height; i++)
    {
      memcpy((void *)pixeldata, (void *)src, width * 4);
      src += width;
      pixeldata += rowstride;
    }

  (*env)->ReleaseIntArrayElements (env, pixels, src_array_iter, 0);
}

/**
 * Allocates a Gtk Pixbuf 
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_createPixbuf(JNIEnv *env, jobject obj)
{
  int width, height;
  jclass cls;
  jfieldID field;

  cls = (*env)->GetObjectClass (env, obj);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  width = (*env)->GetIntField (env, obj, field);

  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  height = (*env)->GetIntField (env, obj, field);

  createRawData (env, obj, gdk_pixbuf_new (GDK_COLORSPACE_RGB, 
					   TRUE,
					   8,
					   width,
					   height));
}

/**
 * Allocates a Gtk Pixbuf 
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_initFromBuffer(JNIEnv *env, jobject obj,
						   jlong bufferPointer)
{
  int width, height;
  jclass cls;
  jfieldID field;
  GdkPixbuf *pixbuf;
  const guchar *bp = JLONG_TO_PTR(const guchar, bufferPointer);

  g_assert(bp != NULL);
  cls = (*env)->GetObjectClass( env, obj );
  field = (*env)->GetFieldID( env, cls, "width", "I" );
  g_assert( field != 0 );
  width = (*env)->GetIntField( env, obj, field );

  field = (*env)->GetFieldID( env, cls, "height", "I" );
  g_assert( field != 0 );
  height = (*env)->GetIntField( env, obj, field );

  pixbuf = gdk_pixbuf_new_from_data( bp,
				     GDK_COLORSPACE_RGB, TRUE, 8,
				     width, height, width * 4, NULL, NULL );
  g_assert( pixbuf != NULL );
  createRawData( env, obj, pixbuf );
}

/**
 * Frees the Gtk Pixbuf.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_freePixbuf(JNIEnv *env, jobject obj)
{
  gdk_pixbuf_unref (cp_gtk_image_get_pixbuf (env, obj));
}

/**
 * Sets this to a scaled version of the original pixbuf
 * width and height of the destination GtkImage must be set.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkImage_createScaledPixbuf(JNIEnv *env, 
						       jobject destination, 
						       jobject source,
						       jint hints)
{
  GdkPixbuf* dst;
  int width, height;
  jclass cls;
  jfieldID field;

  GdkPixbuf *pixbuf;

  cls = (*env)->GetObjectClass (env, destination);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  width = (*env)->GetIntField (env, destination, field);

  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  height = (*env)->GetIntField (env, destination, field);

  pixbuf = cp_gtk_image_get_pixbuf (env, source);

  dst = gdk_pixbuf_scale_simple(pixbuf,
				width, height,
				mapHints(hints));

  createRawData (env, destination, (void *)dst);
}

/**
 * Used by GtkFramePeer
 */
GdkPixbuf *cp_gtk_image_get_pixbuf (JNIEnv *env, jobject obj)
{
  jclass cls;
  jfieldID data_fid;
  jobject data;

  cls = (*env)->GetObjectClass (env, obj);
  data_fid = (*env)->GetFieldID (env, cls, "pixbuf", 
				 "Lgnu/classpath/Pointer;");
  g_assert (data_fid != 0);
  data = (*env)->GetObjectField (env, obj, data_fid);

  if (data == NULL)
    return NULL;

  return (GdkPixbuf *)JCL_GetRawData (env, data);
}

/**
 * Maps java.awt.Image scaling hints to the native GDK ones.
 */
static GdkInterpType mapHints(jint hints)
{
  switch ( hints ) 
    {
      /* For FAST, we use the nearest-neighbor. Fastest and lowest quality. */
    case SCALE_FAST:
    case SCALE_REPLICATE:
      return GDK_INTERP_NEAREST;

      /* Hyperbolic for smooth. Slowest too. */
    case SCALE_SMOOTH:
      return GDK_INTERP_HYPER;
      
      /* the inbetweenish method */
    case SCALE_AREA_AVERAGING:
      return GDK_INTERP_TILES;

      /* default to bilinear */
    }
  return GDK_INTERP_BILINEAR;
}

/* Sets the width and height fields of a GtkImage object. */
static void setWidthHeight (JNIEnv * env, jobject obj, int width, int height)
{
  jclass cls;
  jfieldID field;
  
  cls = (*env)->GetObjectClass (env, obj);
  g_assert (cls != 0);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  (*env)->SetIntField (env, obj, field, (jint)width);
   
  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  (*env)->SetIntField (env, obj, field, (jint)height);
}

/* Store and get the pixbuf pointer */
static void
createRawData (JNIEnv * env, jobject obj, void *ptr)
{
  jclass cls;
  jobject data;
  jfieldID data_fid;

  cls = (*env)->GetObjectClass (env, obj);
  data_fid = (*env)->GetFieldID (env, cls, "pixbuf", 
				 "Lgnu/classpath/Pointer;");
  g_assert (data_fid != 0);

  data = JCL_NewRawDataObject (env, ptr);

  (*env)->SetObjectField (env, obj, data_fid, data);
}

