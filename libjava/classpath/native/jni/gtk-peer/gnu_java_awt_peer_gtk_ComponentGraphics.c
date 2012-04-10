/* gnu_java_awt_peer_gtk_ComponentGraphics.c
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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
#include <gdk/gdktypes.h>
#include <gdk/gdkprivate.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixdata.h>

#include <cairo-ft.h>

#include <stdio.h>
#include <stdlib.h>

#if HAVE_XRENDER
#include <gdk/gdkx.h>
#include <X11/extensions/Xrender.h>
#endif

#include "gnu_java_awt_peer_gtk_ComponentGraphics.h"

#include "cairographics2d.h"

static short flush_scheduled = 0;

static gboolean flush (gpointer data __attribute__((unused)))
{
  gdk_threads_enter ();

  gdk_display_flush (gdk_display_get_default ());
  flush_scheduled = 0;

  gdk_threads_leave ();

  return FALSE;
}

/* The minimum time period between calls to XFlush, in
   milliseconds. */
#define MINIMUM_FLUSH_PERIOD 20

/* schedule_flush must be called with the GDK lock held. */
static void
schedule_flush ()
{
  if (!flush_scheduled)
    {
      g_timeout_add (MINIMUM_FLUSH_PERIOD, flush, NULL);
      flush_scheduled = 1;
    }
}

void cp_gtk_grab_current_drawable(GtkWidget *widget, GdkDrawable **draw,
				  GdkWindow **win)
{
  g_assert (widget != NULL);
  g_assert (draw != NULL);
  g_assert (win != NULL);

  *win = widget->window;

  *draw = *win;
  gdk_window_get_internal_paint_info (*win, draw, 0, 0); 
}

/**
 * Returns whether the XRender extension is supported
 */
JNIEXPORT jboolean JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_hasXRender
  (JNIEnv *env __attribute__ ((unused)), jclass cls __attribute__ ((unused)))
{
#if HAVE_XRENDER
  int ev = 0, err = 0; 
  if( XRenderQueryExtension (GDK_DISPLAY(), &ev, &err) )
    return JNI_TRUE;
#endif
  return JNI_FALSE;
}


JNIEXPORT jlong JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_initState
  (JNIEnv *env, jobject obj __attribute__ ((unused)), jobject peer)
{
  GdkDrawable *drawable;
  GtkWidget *widget;
  cairo_t *cr;
  void *ptr;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, peer);
  g_assert (ptr != NULL);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  drawable = widget->window;
  g_assert (drawable != NULL);

  cr = gdk_cairo_create(drawable);

  g_assert(cr != NULL);

  gdk_threads_leave();

  return PTR_TO_JLONG(cr);
}

JNIEXPORT jlong JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_initFromVolatile
  (JNIEnv *env  __attribute__ ((unused)), jobject obj __attribute__ ((unused)),
   jlong ptr)
{
  GdkDrawable *drawable;
  cairo_t *cr;

  gdk_threads_enter();

  drawable = JLONG_TO_PTR(GdkDrawable, ptr);
  g_assert (drawable != NULL);

  cr = gdk_cairo_create (drawable);
  g_assert(cr != NULL);

  gdk_threads_leave();

  return PTR_TO_JLONG(cr);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_start_1gdk_1drawing
  (JNIEnv *env __attribute__ ((unused)), jobject obj __attribute__ ((unused)))
{
  gdk_threads_enter();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_end_1gdk_1drawing
  (JNIEnv *env __attribute__ ((unused)), jobject obj __attribute__ ((unused)))
{
  schedule_flush ();
  gdk_threads_leave();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_copyAreaNative
  (JNIEnv *env, jobject obj __attribute__((unused)), jobject peer,
   jint x, jint y, jint w, jint h, jint dx, jint dy)
{
  GdkPixbuf *pixbuf;
  GdkDrawable *drawable;
  GdkWindow *win;
  GtkWidget *widget = NULL;
  void *ptr = NULL;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, peer);
  g_assert (ptr != NULL);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  cp_gtk_grab_current_drawable (widget, &drawable, &win);
  g_assert (drawable != NULL);

  pixbuf = gdk_pixbuf_new( GDK_COLORSPACE_RGB, TRUE, 8, w, h );
  gdk_pixbuf_get_from_drawable( pixbuf, drawable, NULL, x, y, 0, 0, w, h );
  gdk_draw_pixbuf (drawable, NULL, pixbuf,
		   0, 0, x + dx, y + dy, 
		   w, h, 
		   GDK_RGB_DITHER_NORMAL, 0, 0);
  gdk_threads_leave();
}

JNIEXPORT jobject JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_nativeGrab
(JNIEnv *env, jclass cls __attribute__((unused)), jobject peer )
{
  GdkPixbuf *pixbuf;
  GdkDrawable *drawable;
  GdkWindow *win;
  gint w,h;
  GtkWidget *widget = NULL;
  void *ptr = NULL;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, peer);
  g_assert (ptr != NULL);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  cp_gtk_grab_current_drawable (widget, &drawable, &win);
  g_assert (drawable != NULL);

  gdk_drawable_get_size ( drawable, &w, &h );

  pixbuf = gdk_pixbuf_new( GDK_COLORSPACE_RGB, TRUE, 8, w, h );
  gdk_pixbuf_get_from_drawable( pixbuf, drawable, NULL, 0, 0, 0, 0, w, h );
  g_object_ref( pixbuf );
  gdk_draw_pixbuf (drawable, NULL, pixbuf,
		   0, 0, 0, 0, 
		   w, h, 
		   GDK_RGB_DITHER_NORMAL, 0, 0);
  gdk_threads_leave();

  return JCL_NewRawDataObject (env, pixbuf);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphics_drawVolatile
(JNIEnv *env, jobject obj __attribute__ ((unused)), jobject peer, 
 jlong img, jint x, jint y, jint w, jint h, jint cx, jint cy, jint cw, jint ch)
{
  GdkPixmap *pixmap;
  GtkWidget *widget = NULL;
  GdkGC *gc;
  GdkRectangle clip;
  void *ptr;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, peer);
  g_assert (ptr != NULL);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  pixmap = JLONG_TO_PTR(GdkPixmap, img);
 
  gc = gdk_gc_new(widget->window);

  clip.x = cx;
  clip.y = cy;
  clip.width = cw;
  clip.height = ch;
  gdk_gc_set_clip_rectangle(gc, &clip);

  gdk_draw_drawable(widget->window,
		    gc,
		    pixmap,
		    0, 0,
		    x, y,
		    w, h);

  g_object_unref( gc );

  schedule_flush ();

  gdk_threads_leave();
}
