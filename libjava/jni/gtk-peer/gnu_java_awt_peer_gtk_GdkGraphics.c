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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

#define GDK_STABLE_IS_PIXMAP(d) (GDK_IS_PIXMAP(d))

GdkPoint *
translate_points (JNIEnv *env, jintArray xpoints, jintArray ypoints, 
		  jint npoints, jint x_offset, jint y_offset);
static void realize_cb (GtkWidget *widget, jobject peer);

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_copyState
  (JNIEnv *env, jobject obj, jobject old)
{
  struct graphics *g, *g_old;

  g = (struct graphics *) malloc (sizeof (struct graphics));
  g_old = (struct graphics *) NSA_GET_PTR (env, old);

  *g = *g_old;

  gdk_threads_enter ();

  g->gc = gdk_gc_new (g->drawable);
  gdk_gc_copy (g->gc, g_old->gc);

  if (GDK_STABLE_IS_PIXMAP (g->drawable))
    gdk_pixmap_ref (g->drawable);
  else /* GDK_IS_WINDOW (g->drawable) */
    gdk_window_ref (g->drawable);

  gdk_colormap_ref (g->cm);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, g);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_initState__II
  (JNIEnv *env, jobject obj, jint width, jint height)
{
  struct graphics *g;

  g = (struct graphics *) malloc (sizeof (struct graphics));
  g->x_offset = g->y_offset = 0;

  gdk_threads_enter ();
  g->drawable = (GdkDrawable *) gdk_pixmap_new (NULL, width, height, 
						gdk_rgb_get_visual ()->depth);
  g->cm = gdk_rgb_get_cmap ();
  gdk_colormap_ref (g->cm);
  g->gc = gdk_gc_new (g->drawable);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, g);
}

/* copy the native state of the peer (GtkWidget *) to the native state
   of the graphics object */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_initState__Lgnu_java_awt_peer_gtk_GtkComponentPeer_2
  (JNIEnv *env, jobject obj, jobject peer)
{
  struct graphics *g = (struct graphics *) malloc (sizeof (struct graphics));
  void *ptr;
  GtkWidget *widget;
  GdkColor color;

  ptr = NSA_GET_PTR (env, peer);
  g->x_offset = g->y_offset = 0;

  gdk_threads_enter ();

  widget = GTK_WIDGET (ptr);
  g->drawable = (GdkDrawable *) widget->window;

  gdk_window_ref (g->drawable);
  g->cm = gtk_widget_get_colormap (widget);
  gdk_colormap_ref (g->cm);
  g->gc = gdk_gc_new (g->drawable);
  gdk_gc_copy (g->gc, widget->style->fg_gc[GTK_STATE_NORMAL]);
  color = widget->style->fg[GTK_STATE_NORMAL];

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, g);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_connectSignals
  (JNIEnv *env, jobject obj, jobject peer)
{
  void *ptr;
  jobject *gref;

  NSA_SET_GLOBAL_REF (env, obj);
  gref = NSA_GET_GLOBAL_REF (env, obj);

  ptr = NSA_GET_PTR (env, peer);

  gdk_threads_enter ();

  g_signal_connect_after (G_OBJECT (ptr), "realize",
                          G_CALLBACK (realize_cb), *gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_dispose
  (JNIEnv *env, jobject obj)
{
  struct graphics *g;

  
  g = (struct graphics *) NSA_DEL_PTR (env, obj);

  if (!g) return;		/* dispose has been called more than once */
  
  gdk_threads_enter ();
  XFlush (GDK_DISPLAY ());

  gdk_gc_destroy (g->gc);

  if (GDK_STABLE_IS_PIXMAP (g->drawable))
    gdk_pixmap_unref (g->drawable);
  else /* GDK_IS_WINDOW (g->drawable) */
    gdk_window_unref (g->drawable);

  gdk_colormap_unref (g->cm);

  gdk_threads_leave ();


  free (g);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_translateNative
  (JNIEnv *env, jobject obj, jint x, jint y)
{
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  g->x_offset += x;
  g->y_offset += y;

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawString
  (JNIEnv *env, jobject obj, jobject font, jstring str, jint x, jint y)
{
  struct peerfont *pfont = NULL;
  struct graphics *g;
  const char *cstr;
  int baseline_y;
  PangoLayoutIter *iter;

  g = (struct graphics *) NSA_GET_PTR (env, obj);
  g_assert (g != NULL);

  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, font);
  g_assert (pfont != NULL);

  cstr = (*env)->GetStringUTFChars (env, str, NULL);

  gdk_threads_enter ();

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
  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, str, cstr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawLine
  (JNIEnv *env, jobject obj, jint x, jint y, jint x2, jint y2)
{
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
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
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gdk_draw_rectangle (g->drawable, g->gc, TRUE, 
		      x + g->x_offset, y + g->y_offset, width, height);
  gdk_flush ();
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawRect
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
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
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
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
Java_gnu_java_awt_peer_gtk_GdkGraphics_copyPixmap
  (JNIEnv *env, jobject obj, jobject offscreen, 
   jint x, jint y, jint width, jint height)
{
  struct graphics *g1, *g2;

  g1 = (struct graphics *) NSA_GET_PTR (env, obj);
  g2 = (struct graphics *) NSA_GET_PTR (env, offscreen);

  gdk_threads_enter ();
  gdk_window_copy_area ((GdkWindow *)g1->drawable,
			g1->gc,
			x + g1->x_offset, y + g1->y_offset,
			(GdkWindow *)g2->drawable,
			0 + g2->x_offset, 0 + g2->y_offset, 
			width, height);
  gdk_flush ();
  gdk_threads_leave ();
}

static void flip_pixbuf (GdkPixbuf *pixbuf,
                         jboolean flip_x,
                         jboolean flip_y,
                         jint width,
                         jint height)
{
  gint src_rs;
  guchar *src_pix;

  src_rs = gdk_pixbuf_get_rowstride (pixbuf);
  src_pix = gdk_pixbuf_get_pixels (pixbuf);

  if (flip_x) 
    {
      gint i, channels;
      guchar buf[4];

      channels = gdk_pixbuf_get_has_alpha (pixbuf) ? 4 : 3;

      for (i = 0; i < height; i++) 
        {
          guchar *left = src_pix + i * src_rs;
          guchar *right = left + channels * (width - 1);
          while (left < right)
            { 
              g_memmove (buf, left, channels);
              g_memmove (left, right, channels);
              g_memmove (right, buf, channels);
              left += channels;
              right -= channels;
            }
        }
    }

  if (flip_y) 
    {
      guchar *top = src_pix;
      guchar *bottom = top + (height - 1) * src_rs;
      gpointer buf = g_malloc (src_rs);
      
      while (top < bottom)
        {
          g_memmove (buf, top, src_rs);
          g_memmove (top, bottom, src_rs);
          g_memmove (bottom, buf, src_rs); 
          top += src_rs;
          bottom -= src_rs;
        }

      g_free (buf);
    }
}
  
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_copyAndScalePixmap
  (JNIEnv *env, jobject obj, jobject offscreen, jboolean flip_x, jboolean flip_y,
   jint src_x, jint src_y, jint src_width, jint src_height,
   jint dest_x, jint dest_y, jint dest_width, jint dest_height)
{
  struct graphics *g1, *g2;
  GdkPixbuf *buf_src, *buf_dest;

  g1 = (struct graphics *) NSA_GET_PTR (env, obj);
  g2 = (struct graphics *) NSA_GET_PTR (env, offscreen);

  gdk_threads_enter ();

  buf_src = gdk_pixbuf_get_from_drawable (NULL,
                                          g2->drawable,
                                          g2->cm,
                                          src_x,
                                          src_y,
                                          0,
                                          0,
                                          src_width,
                                          src_height);

  buf_dest = gdk_pixbuf_scale_simple (buf_src, 
                                      dest_width, 
                                      dest_height, 
                                      GDK_INTERP_BILINEAR);

  if (flip_x || flip_y)
    {
      flip_pixbuf (buf_dest, flip_x, flip_y, dest_width, dest_height);
    }

  gdk_pixbuf_render_to_drawable (buf_dest,
                                 g1->drawable,
                                 g1->gc,
                                 0,
                                 0,
                                 dest_x,
                                 dest_y,
                                 dest_width,
                                 dest_height,
                                 GDK_RGB_DITHER_NORMAL,
                                 0,
                                 0);

  g_object_unref (G_OBJECT (buf_src));
  g_object_unref (G_OBJECT (buf_dest));

  gdk_threads_leave ();
}




JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_clearRect
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  struct graphics *g;
  GdkGCValues saved;
  GtkWidget *widget;
  union widget_union w;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

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
  struct graphics *g;
  g = (struct graphics *) NSA_GET_PTR (env, obj);
  
  gdk_threads_enter ();
  gdk_gc_set_function (g->gc, func);
  gdk_threads_leave ();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_setFGColor
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  struct graphics *g;

  color.red = red << 8;
  color.green = green << 8;
  color.blue = blue << 8;

  g = (struct graphics *) NSA_GET_PTR (env, obj);
  
  gdk_threads_enter ();
  gdk_color_alloc (g->cm, &color);
  gdk_gc_set_foreground (g->gc, &color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawArc
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height, 
   jint angle1, jint angle2)
{
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  gdk_draw_arc (g->drawable, g->gc, FALSE, 
		x + g->x_offset, y + g->y_offset, 
		width, height, angle1 << 6, angle2 << 6);
  gdk_flush ();
  gdk_threads_leave ();
}  

GdkPoint *
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
  struct graphics *g;
  GdkPoint *points;

  g = (struct graphics *) NSA_GET_PTR (env, obj);
  points = translate_points (env, xpoints, ypoints, npoints,
			     g->x_offset, g->y_offset);

  gdk_threads_enter ();
  gdk_draw_lines (g->drawable, g->gc, points, npoints);
  gdk_flush ();
  gdk_threads_leave ();

  g_free (points);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_drawPolygon
  (JNIEnv *env, jobject obj, jintArray xpoints, jintArray ypoints, 
   jint npoints)
{
  struct graphics *g;
  GdkPoint *points;

  g = (struct graphics *) NSA_GET_PTR (env, obj);
  points = translate_points (env, xpoints, ypoints, npoints,
			     g->x_offset, g->y_offset);

  /* make sure the polygon is closed, per Java semantics.
     if it's not, we close it. */
  if (points[0].x != points[npoints-1].x || points[0].y != points[npoints-1].y)
    points[npoints++] = points[0];

  gdk_threads_enter ();
  gdk_draw_lines (g->drawable, g->gc, points, npoints);
  gdk_flush ();
  gdk_threads_leave ();

  g_free (points);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_fillPolygon
  (JNIEnv *env, jobject obj, jintArray xpoints, jintArray ypoints, 
   jint npoints)
{
  struct graphics *g;
  GdkPoint *points;

  g = (struct graphics *) NSA_GET_PTR (env, obj);
  points = translate_points (env, xpoints, ypoints, npoints,
			     g->x_offset, g->y_offset);
  gdk_threads_enter ();
  gdk_draw_polygon (g->drawable, g->gc, TRUE, points, npoints);
  gdk_flush ();
  gdk_threads_leave ();

  g_free (points);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics_fillArc
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height, 
   jint angle1, jint angle2)
{
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
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
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
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
  struct graphics *g;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
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
  struct graphics *g;
  GdkRectangle rectangle;

  g = (struct graphics *) NSA_GET_PTR (env, obj);

  rectangle.x = x + g->x_offset;
  rectangle.y = y + g->y_offset;
  rectangle.width = width;
  rectangle.height = height;

  gdk_threads_enter ();
  gdk_gc_set_clip_rectangle (g->gc, &rectangle);
  gdk_threads_leave ();
}

static void realize_cb (GtkWidget *widget __attribute__ ((unused)), 
			jobject peer)
{
  gdk_threads_leave ();

  (*gdk_env())->CallVoidMethod (gdk_env(), peer, initComponentGraphicsID);

  NSA_DEL_GLOBAL_REF (gdk_env(), peer);

  gdk_threads_enter ();
}
