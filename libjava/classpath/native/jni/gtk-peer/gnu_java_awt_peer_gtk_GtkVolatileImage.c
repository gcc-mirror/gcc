/* gnu_java_awt_peer_gtk_VolatileImage.c
   Copyright (C)  2006 Free Software Foundation, Inc.

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
#include <gdk/gdkx.h>
#include <gdk/gdktypes.h>
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixdata.h>

#include "gnu_java_awt_peer_gtk_GtkVolatileImage.h"
#include "cairographics2d.h"


/**
 * Creates a cairo surface, ARGB32, native ordering, premultiplied alpha.
 */
JNIEXPORT jlong JNICALL 
Java_gnu_java_awt_peer_gtk_GtkVolatileImage_init (JNIEnv *env, 
						  jobject obj __attribute__ ((__unused__)), 
						  jobject peer,
						  jint width, jint height)
{
  GtkWidget *widget = NULL;
  GdkPixmap* pixmap;
  void *ptr = NULL;

  gdk_threads_enter();

  if( peer != NULL )
    {
      ptr = gtkpeer_get_widget (env, peer);
      g_assert (ptr != NULL);
      
      widget = GTK_WIDGET (ptr);
      g_assert (widget != NULL);
      pixmap = gdk_pixmap_new( widget->window, width, height, -1 );
    }
  else
    pixmap = gdk_pixmap_new( NULL, width, height, 
			     gdk_rgb_get_visual()->depth );

  gdk_threads_leave();

  g_assert( pixmap != NULL );

  return PTR_TO_JLONG( pixmap );
}

/**
 * Destroy the surface
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkVolatileImage_destroy
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  GdkPixmap* pixmap = JLONG_TO_PTR(GdkPixmap, pointer);
  if( pixmap != NULL )
    {
      gdk_threads_enter();
      g_object_unref( pixmap );
      gdk_threads_leave();
    }
}

/**
 * Gets all pixels in an array
 */
JNIEXPORT jintArray JNICALL 
Java_gnu_java_awt_peer_gtk_GtkVolatileImage_nativeGetPixels
(JNIEnv *env, jobject obj, jlong pointer)
{
  /* jint *pixeldata, *jpixdata; */
  jint *jpixdata;
  GdkPixmap *pixmap;
  GdkPixbuf *pixbuf;
  jintArray jpixels;
  int width, height, depth, size;
  jclass cls;
  jfieldID field;
  guchar *pixels;

  cls = (*env)->GetObjectClass (env, obj);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  width = (*env)->GetIntField (env, obj, field);

  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  height = (*env)->GetIntField (env, obj, field);

  pixmap = JLONG_TO_PTR(GdkPixmap, pointer);
  g_assert(pixmap != NULL);

  gdk_threads_enter();

  /* get depth in bytes */
  depth = gdk_drawable_get_depth( pixmap ) >> 3;
  size = width * height;
  jpixels = (*env)->NewIntArray ( env, size );
  jpixdata = (*env)->GetIntArrayElements (env, jpixels, NULL);
  
  pixbuf = gdk_pixbuf_new( GDK_COLORSPACE_RGB, TRUE, 8, width, height );
  gdk_pixbuf_get_from_drawable( pixbuf, pixmap, NULL, 0, 0, 0, 0, width, height );
  
  if (pixbuf != NULL)
    {
      pixels = gdk_pixbuf_get_pixels(pixbuf);
      memcpy (jpixdata, pixels, size * sizeof(jint));
    }
    
  (*env)->ReleaseIntArrayElements (env, jpixels, jpixdata, 0);

  gdk_threads_leave();

  return jpixels;
}

/**
 * Copy area
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkVolatileImage_nativeCopyArea
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jint x, jint y, jint w, jint h, jint dx, jint dy)
{
  GdkPixbuf *pixbuf;
  GdkPixmap* pixmap = JLONG_TO_PTR(GdkPixmap, pointer);

  g_assert (pixmap != NULL);

  gdk_threads_enter();
  
  pixbuf = gdk_pixbuf_new( GDK_COLORSPACE_RGB, TRUE, 8, w, h );
  gdk_pixbuf_get_from_drawable( pixbuf, pixmap, NULL, x, y, 0, 0, w, h );
  gdk_draw_pixbuf (pixmap, NULL, pixbuf,
		   0, 0, x + dx, y + dy, 
		   w, h, 
		   GDK_RGB_DITHER_NORMAL, 0, 0);
  gdk_threads_leave();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkVolatileImage_nativeDrawVolatile
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jlong srcptr, jint x, jint y, jint w, jint h)
{
  GdkPixmap *dst, *src;
  GdkGC *gc;

  src = JLONG_TO_PTR(GdkPixmap, srcptr);
  dst = JLONG_TO_PTR(GdkPixmap, pointer);
  g_assert (src != NULL);
  g_assert (dst != NULL);

  gdk_threads_enter();
 
  gc = gdk_gc_new( dst );
  gdk_draw_drawable(dst,
		    gc,
		    src,
		    0, 0,
		    x, y,
		    w, h);
  g_object_unref( gc );

  gdk_threads_leave();
}

