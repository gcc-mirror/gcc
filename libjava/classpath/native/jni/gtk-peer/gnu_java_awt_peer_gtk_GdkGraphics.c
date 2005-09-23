/* gdkgraphics.c
   Copyright (C) 1999 Free Software Foundation, Inc.

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

#include "gtkpeer.h"
#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_GdkGraphics.h"
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>

static jmethodID initComponentGraphicsUnlockedID;

void
cp_gtk_graphics_init_jni (void)
{
  jclass gdkgraphics;

  gdkgraphics = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                         "gnu/java/awt/peer/gtk/GdkGraphics");

  initComponentGraphicsUnlockedID =
    (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gdkgraphics,
                                      "initComponentGraphicsUnlocked",
                                      "()V");
}

struct state_table *cp_gtk_native_graphics_state_table;

static struct state_table *native_graphics_global_ref_table;

#define NSA_GLOBAL_G_INIT(env, clazz) \
  native_graphics_global_ref_table = cp_gtk_init_state_table (env, clazz)

#define NSA_GET_GLOBAL_G_REF(env, obj) \
  cp_gtk_get_state (env, obj, native_graphics_global_ref_table)

#define NSA_SET_GLOBAL_G_REF(env, obj) \
  do {jobject *globRefPtr; \
    globRefPtr = (jobject *) malloc (sizeof (jobject)); \
    *globRefPtr = (*env)->NewGlobalRef (env, obj); \
    cp_gtk_set_state (env, obj, native_graphics_global_ref_table, (void *)globRefPtr);} while (0)

#define NSA_DEL_GLOBAL_G_REF(env, obj) \
  do {jobject *globRefPtr = cp_gtk_get_state (env, obj, native_graphics_global_ref_table); \
    cp_gtk_remove_state_slot (env, obj, native_graphics_global_ref_table); \
    (*env)->DeleteGlobalRef (env, *globRefPtr); \
    free (globRefPtr);} while (0)

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_initStaticState
  (JNIEnv *env, jclass clazz)
{
   gdk_threads_enter();

   NSA_G_INIT (env, clazz);
   NSA_GLOBAL_G_INIT (env, clazz);

   gdk_threads_leave();
}

#define GDK_STABLE_IS_PIXMAP(d) (GDK_IS_PIXMAP(d))

static GdkPoint *translate_points (JNIEnv *env, jintArray xpoints,
                                   jintArray ypoints, jint npoints,
                                   jint x_offset, jint y_offset);
static void realize_cb (GtkWidget *widget, jobject jgraphics);

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_copyState
  (JNIEnv *env, jobject obj, jobject old)
{
  struct graphics *g = NULL;
  struct graphics *g_old = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) g_malloc (sizeof (struct graphics));
  g_old = (struct graphics *) NSA_GET_G_PTR (env, old);

  *g = *g_old;

  g->gc = gdk_gc_new (g->drawable);
  gdk_gc_copy (g->gc, g_old->gc);

  if (GDK_STABLE_IS_PIXMAP (g->drawable))
    gdk_pixmap_ref (g->drawable);
  else /* GDK_IS_WINDOW (g->drawable) */
    gdk_window_ref (g->drawable);

  gdk_colormap_ref (g->cm);

  NSA_SET_G_PTR (env, obj, g);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_initState__II
  (JNIEnv *env, jobject obj, jint width, jint height)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) g_malloc (sizeof (struct graphics));
  g->x_offset = g->y_offset = 0;

  g->drawable = (GdkDrawable *) gdk_pixmap_new (NULL, width, height, 
						gdk_rgb_get_visual ()->depth);
  g->cm = gdk_rgb_get_cmap ();
  gdk_colormap_ref (g->cm);
  g->gc = gdk_gc_new (g->drawable);

  NSA_SET_G_PTR (env, obj, g);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_initFromImage
   (JNIEnv *env, jobject obj, jobject source)
{
  struct graphics *g = NULL;
  GdkPixmap *pixmap = NULL;

  gdk_threads_enter ();

  pixmap = cp_gtk_image_get_pixmap (env, source);
  g_assert(pixmap != NULL);
  gdk_pixmap_ref (pixmap);

  g = (struct graphics *) g_malloc (sizeof (struct graphics));
  g->x_offset = g->y_offset = 0;

  g->drawable = (GdkDrawable *)pixmap;

  g->cm = gdk_drawable_get_colormap (g->drawable);
  gdk_colormap_ref (g->cm);
  g->gc = gdk_gc_new (g->drawable);

  NSA_SET_G_PTR (env, obj, g);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_initStateUnlocked
  (JNIEnv *env, jobject obj, jobject peer)
{
  struct graphics *g = NULL;
  void *ptr = NULL;
  GtkWidget *widget = NULL;
  GdkColor color;

  g = (struct graphics *) g_malloc (sizeof (struct graphics));
  ptr = NSA_GET_PTR (env, peer);
  g->x_offset = 0;
  g->y_offset = 0;

  widget = GTK_WIDGET (ptr);
  g->drawable = (GdkDrawable *) widget->window;

  gdk_window_ref (g->drawable);
  g->cm = gtk_widget_get_colormap (widget);
  gdk_colormap_ref (g->cm);
  g->gc = gdk_gc_new (g->drawable);
  gdk_gc_copy (g->gc, widget->style->fg_gc[GTK_STATE_NORMAL]);
  color = widget->style->fg[GTK_STATE_NORMAL];

  NSA_SET_G_PTR (env, obj, g);
}

/* copy the native state of the peer (GtkWidget *) to the native state
   of the graphics object */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_initState__Lgnu_java_awt_peer_gtk_GtkComponentPeer_2
  (JNIEnv *env, jobject obj, jobject peer)
{
  gdk_threads_enter ();
  Java_gnu_java_awt_peer_gtk_GdkGraphics_initStateUnlocked
    (env, obj, peer);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_connectSignals
  (JNIEnv *env, jobject obj, jobject peer)
{
  void *ptr = NULL;
  jobject *gref = NULL;

  gdk_threads_enter ();

  NSA_SET_GLOBAL_G_REF (env, obj);
  gref = NSA_GET_GLOBAL_G_REF (env, obj);

  ptr = NSA_GET_PTR (env, peer);

  g_signal_connect_after (G_OBJECT (ptr), "realize",
                          G_CALLBACK (realize_cb), *gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_dispose
  (JNIEnv *env, jobject obj)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_DEL_G_PTR (env, obj);

  /* check if dispose has been called already */
  if (!g)
    {
      gdk_threads_leave ();
      return;
    }

  XFlush (GDK_DISPLAY ());

  gdk_gc_destroy (g->gc);

  if (GDK_STABLE_IS_PIXMAP (g->drawable))
    gdk_pixmap_unref (g->drawable);
  else /* GDK_IS_WINDOW (g->drawable) */
    gdk_window_unref (g->drawable);

  gdk_colormap_unref (g->cm);

  g_free (g);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_translateNative
  (JNIEnv *env, jobject obj, jint x, jint y)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  g->x_offset += x;
  g->y_offset += y;

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawString
  (JNIEnv *env, jobject obj, jobject font, jstring str, jint x, jint y)
{
  struct peerfont *pfont = NULL;
  struct graphics *g = NULL;
  const char *cstr = NULL;
  int baseline_y = 0;
  PangoLayoutIter *iter = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);
  g_assert (g != NULL);

  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, font);
  g_assert (pfont != NULL);

  cstr = (*env)->GetStringUTFChars (env, str, NULL);

  pango_layout_set_font_description (pfont->layout, pfont->desc);
  pango_layout_set_text (pfont->layout, cstr, -1);
  iter = pango_layout_get_iter (pfont->layout);

  baseline_y = pango_layout_iter_get_baseline (iter);

  gdk_draw_layout (g->drawable, g->gc,
                   x + g->x_offset,
                   y + g->y_offset - PANGO_PIXELS (baseline_y),
                   pfont->layout);

  pango_layout_iter_free (iter);
  pango_layout_set_text (pfont->layout, "", -1);

  gdk_flush ();

  (*env)->ReleaseStringUTFChars (env, str, cstr);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawLine
  (JNIEnv *env, jobject obj, jint x, jint y, jint x2, jint y2)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_draw_line (g->drawable, g->gc, 
		 x + g->x_offset, y + g->y_offset, 
		 x2 + g->x_offset, y2 + g->y_offset);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_fillRect
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_draw_rectangle (g->drawable, g->gc, TRUE, 
		      x + g->x_offset, y + g->y_offset, width, height);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawRect
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_draw_rectangle (g->drawable, g->gc, FALSE, 
		      x + g->x_offset, y + g->y_offset, width, height);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_copyArea
  (JNIEnv *env, jobject obj, jint x, jint y, 
   jint width, jint height, jint dx, jint dy)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_window_copy_area ((GdkWindow *)g->drawable,
			g->gc,
			x + g->x_offset + dx, y + g->y_offset + dy,
			(GdkWindow *)g->drawable,
			x + g->x_offset, y + g->y_offset,
			width, height);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_clearRect
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g = NULL;
  GdkGCValues saved;
  GtkWidget *widget = NULL;
  union widget_union w;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  if (!g)
    {
      gdk_threads_leave ();
      return;
    }

  if (GDK_IS_WINDOW (g->drawable))
    {
      w.widget = &widget;
      gdk_window_get_user_data (GDK_WINDOW (g->drawable), w.void_widget);
      if (widget == NULL || !GTK_IS_EVENT_BOX (widget))
        gdk_window_clear_area ((GdkWindow *) g->drawable,
                               x + g->x_offset, y + g->y_offset,
                               width, height);
    }
  else
    {
      gdk_gc_get_values (g->gc, &saved);
      gdk_gc_set_foreground (g->gc, &(saved.background));
      gdk_draw_rectangle (g->drawable, g->gc, TRUE, 
			  x + g->x_offset, y + g->y_offset, width, height);
      gdk_gc_set_foreground (g->gc, &(saved.foreground));
    }

  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_setFunction
  (JNIEnv *env, jobject obj, jint func)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_gc_set_function (g->gc, func);

  gdk_threads_leave ();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_setFGColor
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  struct graphics *g = NULL;

  gdk_threads_enter ();

  color.red = red << 8;
  color.green = green << 8;
  color.blue = blue << 8;

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);
  
  gdk_color_alloc (g->cm, &color);
  gdk_gc_set_foreground (g->gc, &color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawArc
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height, 
   jint angle1, jint angle2)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_draw_arc (g->drawable, g->gc, FALSE, 
		x + g->x_offset, y + g->y_offset, 
		width, height, angle1 << 6, angle2 << 6);
  gdk_flush ();

  gdk_threads_leave ();
}  

static GdkPoint *
translate_points (JNIEnv *env, jintArray xpoints, jintArray ypoints, 
		  jint npoints, jint x_offset, jint y_offset)
{
  GdkPoint *points;
  jint *x, *y;
  int i;

  /* allocate one more point than necessary, in case we need to tack
     on an extra due to the semantics of Java polygons. */
  points = g_malloc (sizeof (GdkPoint) * (npoints + 1));
  
  x = (*env)->GetIntArrayElements (env, xpoints, NULL);
  y = (*env)->GetIntArrayElements (env, ypoints, NULL);

  for (i = 0; i < npoints; i++)
    {
      points[i].x = x[i] + x_offset;
      points[i].y = y[i] + y_offset;
    }

  (*env)->ReleaseIntArrayElements (env, xpoints, x, JNI_ABORT);
  (*env)->ReleaseIntArrayElements (env, ypoints, y, JNI_ABORT);

  return points;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawPolyline
  (JNIEnv *env, jobject obj, jintArray xpoints, jintArray ypoints, 
   jint npoints)
{
  struct graphics *g = NULL;
  GdkPoint *points = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);
  points = translate_points (env, xpoints, ypoints, npoints,
			     g->x_offset, g->y_offset);

  gdk_draw_lines (g->drawable, g->gc, points, npoints);
  gdk_flush ();

  g_free (points);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawPolygon
  (JNIEnv *env, jobject obj, jintArray xpoints, jintArray ypoints, 
   jint npoints)
{
  struct graphics *g = NULL;
  GdkPoint *points = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);
  points = translate_points (env, xpoints, ypoints, npoints,
			     g->x_offset, g->y_offset);

  /* make sure the polygon is closed, per Java semantics.
     if it's not, we close it. */
  if (points[0].x != points[npoints-1].x || points[0].y != points[npoints-1].y)
    points[npoints++] = points[0];

  gdk_draw_lines (g->drawable, g->gc, points, npoints);
  gdk_flush ();

  g_free (points);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_fillPolygon
  (JNIEnv *env, jobject obj, jintArray xpoints, jintArray ypoints, 
   jint npoints)
{
  struct graphics *g = NULL;
  GdkPoint *points = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);
  points = translate_points (env, xpoints, ypoints, npoints,
			     g->x_offset, g->y_offset);
  gdk_draw_polygon (g->drawable, g->gc, TRUE, points, npoints);
  gdk_flush ();

  g_free (points);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_fillArc
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height, 
   jint angle1, jint angle2)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_draw_arc (g->drawable, g->gc, TRUE, 
		x + g->x_offset, y + g->y_offset, 
		width, height, angle1 << 6, angle2 << 6);
  gdk_flush ();

  gdk_threads_leave ();
}  

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawOval
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_draw_arc (g->drawable, g->gc, FALSE, 
		x + g->x_offset, y + g->y_offset, 
		width, height, 0, 23040);
  gdk_flush ();

  gdk_threads_leave ();
}  

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_fillOval
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g = NULL;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  gdk_draw_arc (g->drawable, g->gc, TRUE, 
		x + g->x_offset, y + g->y_offset, 
		width, height, 0, 23040);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_setClipRectangle
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g = NULL;
  GdkRectangle rectangle;

  gdk_threads_enter ();

  g = (struct graphics *) NSA_GET_G_PTR (env, obj);

  rectangle.x = x + g->x_offset;
  rectangle.y = y + g->y_offset;
  rectangle.width = width;
  rectangle.height = height;

  gdk_gc_set_clip_rectangle (g->gc, &rectangle);

  gdk_threads_leave ();
}

static void
realize_cb (GtkWidget *widget __attribute__ ((unused)), jobject jgraphics)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(),
                                       jgraphics,
                                       initComponentGraphicsUnlockedID);

  NSA_DEL_GLOBAL_G_REF (cp_gtk_gdk_env(), jgraphics);
}
