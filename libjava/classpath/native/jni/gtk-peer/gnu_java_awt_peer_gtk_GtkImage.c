/* gtkimage.c
   Copyright (C) 2005 Free Software Foundation, Inc.

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
#include <gdk-pixbuf/gdk-pixbuf.h>

/* The constant fields in java.awt.Image */   
#define SCALE_DEFAULT      1
#define SCALE_FAST         2
#define SCALE_SMOOTH       4
#define SCALE_REPLICATE    8 
#define SCALE_AREA_AVERAGING  16

/* local stuff */
static GdkInterpType mapHints(jint hints);
static jboolean offScreen (JNIEnv * env, jobject obj);
static void *getData (JNIEnv * env, jobject obj);
static void createRawData (JNIEnv * env, jobject obj, void *ptr);
static void setWidthHeight (JNIEnv * env, jobject obj, int width, int height);

/**
 * Loads a pixmap from a file.
 */
JNIEXPORT jboolean JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_loadPixbuf
  (JNIEnv *env, jobject obj, jstring name)
{
  const char *filename;
  int width, height;
  GdkPixbuf *pixbuf;

  gdk_threads_enter ();

  /* Don't use the JCL convert function because it throws an exception
     on failure */
  filename = (*env)->GetStringUTFChars (env, name, 0);

  if (filename == NULL)
    {
      gdk_threads_leave ();
      return JNI_FALSE;
    }

  pixbuf = gdk_pixbuf_new_from_file (filename, NULL);
  if (pixbuf == NULL)
    {
      (*env)->ReleaseStringUTFChars (env, name, filename);
      gdk_threads_leave ();
      return JNI_FALSE;
    }

  width =  gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  
  createRawData (env, obj, pixbuf);
  setWidthHeight(env, obj, width, height);
  (*env)->ReleaseStringUTFChars (env, name, filename);

  gdk_threads_leave ();

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

  gdk_threads_enter ();

  src = (*env)->GetByteArrayElements (env, data, NULL);
  len = (*env)->GetArrayLength (env, data);

  loader = gdk_pixbuf_loader_new ();

  gdk_pixbuf_loader_write (loader, (guchar *)src, len, NULL);
  gdk_pixbuf_loader_close (loader, NULL);

  (*env)->ReleaseByteArrayElements (env, data, src, 0);

  pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);

  if (pixbuf == NULL)
    {
      createRawData (env, obj, NULL);

      gdk_threads_leave ();

      return JNI_FALSE;
    }

  width =  gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  createRawData (env, obj, pixbuf);
  setWidthHeight(env, obj, width, height);

  gdk_threads_leave ();

  return JNI_TRUE;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_createFromPixbuf
(JNIEnv *env, jobject obj)
{
  int width, heigth;
  GdkPixbuf *pixbuf = (GdkPixbuf *) getData (env, obj);
  gdk_threads_enter ();
  width =  gdk_pixbuf_get_width (pixbuf);
  heigth = gdk_pixbuf_get_height (pixbuf);
  gdk_threads_leave ();
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
      for(i = 0; i < height; i++)
	{
	  for(j = 0; j < width; j++)
	    dst[j] = 0xFF000000 |
	      (pixeldata[j*3 + 2] & 0xFF) << 16 |
	      (pixeldata[j*3 + 1] & 0xFF) << 8 |
	      (pixeldata[j*3] & 0xFF);
	  dst += width;
	  pixeldata += rowstride;
	}
    }
  
  if (offScreen (env, obj) == JNI_TRUE)
    gdk_pixbuf_unref (pixbuf);

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
  GdkPixbuf *pixbuf = (GdkPixbuf *)getData (env, obj);
  int width, height, rowstride;
  guchar *pixeldata;
  jint *src_array_iter, *src;
  int i;

  gdk_threads_enter ();

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

  gdk_threads_leave ();
}

/**
 * Allocates a Gtk Pixbuf or Pixmap.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_createPixmap(JNIEnv *env, jobject obj)
{
  int width, height;
  jclass cls;
  jfieldID field;

  gdk_threads_enter ();

  cls = (*env)->GetObjectClass (env, obj);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  width = (*env)->GetIntField (env, obj, field);

  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  height = (*env)->GetIntField (env, obj, field);

  if (offScreen (env, obj) == JNI_FALSE)
    createRawData (env, obj, gdk_pixbuf_new (GDK_COLORSPACE_RGB, 
					     TRUE,
					     8,
					     width,
					     height));
  else
    createRawData (env, obj, gdk_pixmap_new (NULL, width, height,
					     gdk_rgb_get_visual ()->depth));

  gdk_threads_leave ();
}

/**
 * Frees the Gtk Pixmap.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_freePixmap(JNIEnv *env, jobject obj)
{
  gdk_threads_enter ();
  if (offScreen (env, obj) == JNI_FALSE)
    gdk_pixbuf_unref ((GdkPixbuf *)getData (env, obj));
  else
    g_object_unref ((GdkPixmap *)getData (env, obj));

  gdk_threads_leave ();
}

/**
 * Sets this pixmap to a scaled version of the source pixmap.
 * width and height of the destination GtkImage must be set.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkImage_createScaledPixmap(JNIEnv *env, 
						       jobject destination, 
						       jobject source,
						       jint hints)
{
  GdkPixbuf* dst;
  int width, height;
  jclass cls;
  jfieldID field;

  GdkPixbuf *pixbuf;

  gdk_threads_enter ();

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

  if (offScreen (env, source) == JNI_TRUE)
      gdk_pixbuf_unref (pixbuf);

  createRawData (env, destination, (void *)dst);

  gdk_threads_leave ();
}

/**
 * Draws the pixbuf at x, y, scaled to width and height and 
 * optionally composited with a given background color.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkImage_drawPixelsScaled 
  (JNIEnv *env, jobject obj, jobject gc_obj,
   jint bg_red, jint bg_green, jint bg_blue, 
   jint x, jint y, jint width, jint height, jboolean composite)
{
  GdkPixbuf* dst;
  struct graphics *g;
  guint32 bgColor;

  gdk_threads_enter ();
  
  if (width <= 0 || height <= 0)
    {
      gdk_threads_leave ();
      return;
    }

  bgColor = ((bg_red & 0xFF) << 16) |
    ((bg_green & 0xFF) << 8) | (bg_blue & 0xFF);
    
  g = (struct graphics *) NSA_GET_G_PTR (env, gc_obj);
  
  if (!g || !GDK_IS_DRAWABLE (g->drawable))
    {
      gdk_threads_leave ();
      return;
    }

  if (offScreen (env, obj) == JNI_FALSE)
    {
      GdkPixbuf* pixbuf = (GdkPixbuf *)getData (env, obj);

      /* Scale and composite the image */
      if (composite == JNI_TRUE)
	dst = gdk_pixbuf_composite_color_simple (pixbuf,
						 width,
						 height,
						 GDK_INTERP_BILINEAR,
						 255,
						 width,
						 bgColor,
						 bgColor);
      else
	dst = gdk_pixbuf_scale_simple(pixbuf,
				      width, height,
				      GDK_INTERP_BILINEAR);

      gdk_draw_pixbuf (g->drawable,
		       g->gc,
		       dst,
		       0, 0,
		       x + g->x_offset, y + g->y_offset, 
		       width, height,
		       GDK_RGB_DITHER_NORMAL, 0, 0);
      gdk_pixbuf_unref (dst);

    } else {
      /* Get a pixmap */
      GdkPixmap* pixmap = (GdkPixmap *)getData (env, obj);
      gdk_draw_drawable (g->drawable,
			 g->gc,
			 pixmap,
			 0, 0, /* src x,y */
			 x + g->x_offset, y + g->y_offset, 
			 width, height);
    }
    
  gdk_threads_leave ();
}

/**
 * Draws the pixbuf at x, y, scaled to width and height and 
 * optionally composited and/or flipped with a given background color.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkImage_drawPixelsScaledFlipped 
(JNIEnv *env, jobject obj, jobject gc_obj,
 jint bg_red, jint bg_green, jint bg_blue, 
#if GTK_MINOR_VERSION > 4
 jboolean flipx, jboolean flipy,
#else
 jboolean flipx __attribute__((unused)),
 jboolean flipy __attribute__((unused)),
#endif
 jint srcx, jint srcy, jint srcwidth, jint srcheight, 
 jint dstx, jint dsty, jint dstwidth, jint dstheight, 
 jboolean composite)
{
  GdkPixbuf *pixbuf;
  GdkPixbuf *tmp, *dst;
  struct graphics *g;
  guint32 bgColor;

  gdk_threads_enter ();
  
  if (srcwidth <= 0 || srcheight <= 0
      || dstwidth <= 0 || dstheight <= 0)
    {
      gdk_threads_leave ();
      return;
    }

  bgColor = ((bg_red & 0xFF) << 16) |
    ((bg_green & 0xFF) << 8) | (bg_blue & 0xFF);
    
  g = (struct graphics *) NSA_GET_G_PTR (env, gc_obj);
  
  if (!g || !GDK_IS_DRAWABLE (g->drawable))
    {
      gdk_threads_leave ();
      return;
    }

  if (offScreen (env, obj) == JNI_FALSE)
    {
      pixbuf = (GdkPixbuf *)getData (env, obj);

      /* Get the source area */
      tmp = gdk_pixbuf_new (GDK_COLORSPACE_RGB, 
			    TRUE,
			    8,
			    srcwidth,
			    srcheight);

      gdk_pixbuf_copy_area (pixbuf, 
			    srcx, srcy,
			    srcwidth, srcheight,
			    tmp, 
			    0, 0); /* dst x , dst y */
    } else {
      /* Get a pixbuf from the pixmap */
      GdkDrawable *pixmap = (GdkDrawable *)getData(env, obj);
      tmp = gdk_pixbuf_get_from_drawable (NULL,
					  pixmap,
					  gdk_drawable_get_colormap( pixmap ),
					  srcx, srcy,
					  0, 0, /* dst x , dst y */
					  srcwidth, srcheight);
    }

  /* FIXME: This #if should be discarded once I feel comfortable about
     GTK 2.6 dependence */
#if GTK_MINOR_VERSION > 4
  /* Flip it if necessary. */
  if (flipx == JNI_TRUE)
    {
      GdkPixbuf *tmp2 = gdk_pixbuf_flip (tmp, TRUE);
      gdk_pixbuf_unref (tmp);
      tmp = tmp2;
    }
  if (flipy == JNI_TRUE)
    {
      GdkPixbuf *tmp2 = gdk_pixbuf_flip (tmp, FALSE);
      gdk_pixbuf_unref (tmp);
      tmp = tmp2;
    }
#endif
  
  /* Scale and composite the image */
  if (composite == JNI_TRUE)
    dst = gdk_pixbuf_composite_color_simple (tmp,
					     dstwidth,
					     dstheight,
					     GDK_INTERP_BILINEAR,
					     255,
					     dstwidth,
					     bgColor,
					     bgColor);
  else
    dst = gdk_pixbuf_scale_simple(tmp,
				  dstwidth, dstheight,
				  GDK_INTERP_BILINEAR);
  gdk_pixbuf_unref (tmp);
    
  gdk_draw_pixbuf (g->drawable,
		   g->gc,
		   dst,
		   0, 0,
		   dstx + g->x_offset, dsty + g->y_offset, 
		   dstwidth, dstheight,
		   GDK_RGB_DITHER_NORMAL, 0, 0);
  
  gdk_pixbuf_unref (dst);

  gdk_threads_leave ();
}

/**
 * Used by GtkFramePeer
 */
GdkPixbuf *cp_gtk_image_get_pixbuf (JNIEnv *env, jobject obj)
{
  int width, height;
  GdkPixbuf *pixbuf;
  GdkPixmap* pixmap;
  jclass cls;
  jfieldID field;

  if (offScreen (env, obj) == JNI_FALSE)
    return (GdkPixbuf *)getData (env, obj);

  cls = (*env)->GetObjectClass (env, obj);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  width = (*env)->GetIntField (env, obj, field);
   
  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  height = (*env)->GetIntField (env, obj, field);

  /* Get a pixmap */
  pixmap = (GdkPixmap *)getData (env, obj);
  pixbuf = gdk_pixbuf_get_from_drawable (NULL,
					 pixmap,
					 gdk_drawable_get_colormap( pixmap ),
					 0, 0, /* src x , src y */
					 0, 0, /* dst x , dst y */
					 width, height);
  return pixbuf;
}

/**
 * Used by GdkGraphics
 */
GdkPixmap *cp_gtk_image_get_pixmap (JNIEnv *env, jobject obj)
{
  if (offScreen (env, obj) == JNI_FALSE)
    return NULL;
  return (GdkPixmap *)getData (env, obj);
}

jboolean cp_gtk_image_is_offscreen (JNIEnv *env, jobject obj)
{
  return offScreen(env, obj);
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

/* Returns the value of the offScreen field. */
static jboolean offScreen (JNIEnv *env, jobject obj)
{
  jclass cls;
  jfieldID field;

  cls = (*env)->GetObjectClass (env, obj);
  field = (*env)->GetFieldID (env, cls, "offScreen", "Z");
  g_assert (field != 0);
  return (*env)->GetBooleanField (env, obj, field);
}

/* Store and get the pixbuf pointer */
static void
createRawData (JNIEnv * env, jobject obj, void *ptr)
{
  jclass cls;
  jobject data;
  jfieldID data_fid;

  cls = (*env)->GetObjectClass (env, obj);
  data_fid = (*env)->GetFieldID (env, cls, "pixmap", 
				 "Lgnu/classpath/Pointer;");
  g_assert (data_fid != 0);

  data = JCL_NewRawDataObject (env, ptr);

  (*env)->SetObjectField (env, obj, data_fid, data);
}

static void *
getData (JNIEnv * env, jobject obj)
{
  jclass cls;
  jfieldID data_fid;
  jobject data;

  cls = (*env)->GetObjectClass (env, obj);
  data_fid = (*env)->GetFieldID (env, cls, "pixmap", 
				 "Lgnu/classpath/Pointer;");
  g_assert (data_fid != 0);
  data = (*env)->GetObjectField (env, obj, data_fid);

  return JCL_GetRawData (env, data);
}
