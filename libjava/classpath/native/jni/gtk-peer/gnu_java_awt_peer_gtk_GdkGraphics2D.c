/* gnu_java_awt_peer_gtk_GdkGraphics2d.c
   Copyright (C) 2003, 2005  Free Software Foundation, Inc.

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

#include "gtkcairopeer.h"
#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_GdkGraphics2D.h"
#include <gdk/gdktypes.h>
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>
#include <X11/extensions/Xrender.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixdata.h>

#include <cairo-ft.h>
#include <cairo-xlib.h>

#include <stdio.h>
#include <stdlib.h>

static jmethodID initComponentGraphics2DUnlockedID;

void
cp_gtk_graphics2d_init_jni (void)
{
  jclass gdkgraphics2d;
  JNIEnv *env = cp_gtk_gdk_env();

  gdkgraphics2d = (*env)->FindClass (env,
				     "gnu/java/awt/peer/gtk/GdkGraphics2D");
  if ((*env)->ExceptionOccurred(env))
    return;

  initComponentGraphics2DUnlockedID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gdkgraphics2d,
                                                         "initComponentGraphics2DUnlocked",
                                                         "()V");
}

static struct state_table *native_graphics2d_state_table;

#define NSA_G2D_INIT(env, clazz) \
  native_graphics2d_state_table = cp_gtk_init_state_table (env, clazz)

#define NSA_GET_G2D_PTR(env, obj) \
  cp_gtk_get_state (env, obj, native_graphics2d_state_table)

#define NSA_SET_G2D_PTR(env, obj, ptr) \
  cp_gtk_set_state (env, obj, native_graphics2d_state_table, (void *)ptr)

#define NSA_DEL_G2D_PTR(env, obj) \
  cp_gtk_remove_state_slot (env, obj, native_graphics2d_state_table)

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_initStaticState 
  (JNIEnv *env, jclass clazz)
{
   gdk_threads_enter();

   NSA_G2D_INIT (env, clazz);

   gdk_threads_leave();
}

/* these public final constants are part of the java2d public API, so we
   write them explicitly here to save fetching them from the constant pool
   all the time. */

#ifndef min
#define min(x,y) ((x) < (y) ? (x) : (y))
#endif

enum java_awt_alpha_composite_rule
  {
    java_awt_alpha_composite_CLEAR = 1,
    java_awt_alpha_composite_SRC = 2,
    java_awt_alpha_composite_SRC_OVER = 3,
    java_awt_alpha_composite_DST_OVER = 4,
    java_awt_alpha_composite_SRC_IN = 5,
    java_awt_alpha_composite_DST_IN = 6,
    java_awt_alpha_composite_SRC_OUT = 7,
    java_awt_alpha_composite_DST_OUT = 8,
    java_awt_alpha_composite_DST = 9,
    java_awt_alpha_composite_SRC_ATOP = 10,
    java_awt_alpha_composite_DST_ATOP = 11,
    java_awt_alpha_composite_XOR = 12
  };

enum java_awt_basic_stroke_join_rule
  {
    java_awt_basic_stroke_JOIN_MITER = 0,
    java_awt_basic_stroke_JOIN_ROUND = 1,
    java_awt_basic_stroke_JOIN_BEVEL = 2
  };

enum java_awt_basic_stroke_cap_rule
  {
    java_awt_basic_stroke_CAP_BUTT = 0,
    java_awt_basic_stroke_CAP_ROUND = 1,
    java_awt_basic_stroke_CAP_SQUARE = 2
  };

enum java_awt_geom_path_iterator_winding_rule
  {
    java_awt_geom_path_iterator_WIND_EVEN_ODD = 0,
    java_awt_geom_path_iterator_WIND_NON_ZERO = 1
  };

enum java_awt_rendering_hints_filter
  {
    java_awt_rendering_hints_VALUE_INTERPOLATION_NEAREST_NEIGHBOR = 0,    
    java_awt_rendering_hints_VALUE_INTERPOLATION_BILINEAR = 1,
    java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_SPEED = 2,
    java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_QUALITY = 3,
    java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_DEFAULT = 4
 
  };

static int
peer_is_disposed(JNIEnv *env, jobject obj)
{
  static jfieldID fid = NULL;
  jclass cls;
  jobject peer;

  return 0;

  if (fid == NULL)
    {
      cls = (*env)->GetObjectClass(env, obj);
      fid = (*env)->GetFieldID(env, cls, "component",
			       "Lgnu/java/awt/peer/gtk/GtkComponentPeer;");
    }
  g_assert(fid != NULL);
  peer = (*env)->GetObjectField(env, obj, fid);
  if (peer == NULL || NSA_GET_PTR (env, peer) != NULL)
    return 0;
  else
    {
      return 1;
    }
}


static void 
grab_current_drawable (GtkWidget *widget, GdkDrawable **draw, GdkWindow **win)
{  
  g_assert (widget != NULL);
  g_assert (draw != NULL);
  g_assert (win != NULL);

  *win = widget->window;

  *draw = *win;
  gdk_window_get_internal_paint_info (*win, draw, 0, 0); 
  g_object_ref (*draw);
}


static int
x_server_has_render_extension (void)
{
  int ev = 0, err = 0; 
  return (int) XRenderQueryExtension (GDK_DISPLAY (), &ev, &err);
}

static void
init_graphics2d_as_pixbuf (struct graphics2d *gr)
{
  gint width, height;
  gint bits_per_sample = 8;
  gint total_channels = 4;
  gboolean has_alpha = TRUE;
  
  g_assert (gr != NULL);
  g_assert (gr->drawable != NULL);

  if (gr->debug) printf ("initializing graphics2d as pixbuf\n");
  gdk_drawable_get_size (gr->drawable, &width, &height);
  gr->drawbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, 
				has_alpha, bits_per_sample,
				width, height);
  g_assert (gr->drawbuf != NULL);
  g_assert (gdk_pixbuf_get_bits_per_sample (gr->drawbuf) == bits_per_sample);
  g_assert (gdk_pixbuf_get_n_channels (gr->drawbuf) == total_channels);
  
  gr->surface = cairo_image_surface_create_for_data (gdk_pixbuf_get_pixels (gr->drawbuf), 
						CAIRO_FORMAT_ARGB32, 
						gdk_pixbuf_get_width (gr->drawbuf), 
						gdk_pixbuf_get_height (gr->drawbuf), 
						gdk_pixbuf_get_rowstride (gr->drawbuf));      
  g_assert (gr->surface != NULL);
  gr->mode = MODE_DRAWABLE_NO_RENDER;
  if (gr->cr != NULL)
    cairo_destroy (gr->cr);
  gr->cr = cairo_create (gr->surface);
}

static void
init_graphics2d_as_renderable (struct graphics2d *gr)
{
  Drawable draw;
  Display * dpy;
  Visual * vis;
  
  g_assert (gr != NULL);
  g_assert (gr->drawable != NULL);

  gr->drawbuf = NULL;
  
  if (gr->debug) printf ("initializing graphics2d as renderable\n");
  draw = gdk_x11_drawable_get_xid (gr->drawable);
  
  dpy = gdk_x11_drawable_get_xdisplay (gr->drawable);
  g_assert (dpy != NULL);
  
  vis = gdk_x11_visual_get_xvisual (gdk_drawable_get_visual (gr->drawable));
  g_assert (vis != NULL);
  
  gr->surface = cairo_xlib_surface_create (dpy, draw, vis, gr->width, gr->height);
  g_assert (gr->surface != NULL);
  gr->mode = MODE_DRAWABLE_WITH_RENDER;
  if (gr->cr != NULL)
    cairo_destroy (gr->cr);
  gr->cr = cairo_create (gr->surface);
}

static void
begin_drawing_operation (JNIEnv *env, struct graphics2d * gr)
{  
  g_assert(cairo_status (gr->cr) == CAIRO_STATUS_SUCCESS);

  switch (gr->mode)
    {
    case MODE_DRAWABLE_WITH_RENDER:
      break;

    case MODE_DRAWABLE_NO_RENDER:
      {
	
	gint drawable_width, drawable_height;
	gint pixbuf_width, pixbuf_height;
	gint width, height;
	
	gdk_drawable_get_size (gr->drawable, &drawable_width, &drawable_height);
	pixbuf_width = gdk_pixbuf_get_width (gr->drawbuf);
	pixbuf_height = gdk_pixbuf_get_height (gr->drawbuf);
	width = min (drawable_width, pixbuf_width);
	height = min (drawable_height, pixbuf_height);
	
	gdk_pixbuf_get_from_drawable (gr->drawbuf, /* destination pixbuf */
				      gr->drawable, 
				      NULL, /* colormap */
				      0, 0, 0, 0,
				      width, height); 
	
	if (gr->debug) printf ("copied (%d, %d) pixels from GDK drawable to pixbuf\n",
			       width, height);      
      }
      break;

    case MODE_JAVA_ARRAY:
      {
        jboolean isCopy;
        gr->javabuf = (*env)->GetPrimitiveArrayCritical (env, gr->jarray, &isCopy);
        gr->isCopy |= isCopy;
        if (gr->isCopy)
          {
	    /* Make sure that the pixel buffer copy is already initalized,
	       i.e. we already failed to get direct access in initState. */
	    g_assert (gr->javabuf_copy != NULL);
	    memcpy (gr->javabuf_copy, gr->javabuf, gr->width * gr->height * 4);
	  }
      }
      break;
    }
}

static void
end_drawing_operation (JNIEnv *env, struct graphics2d * gr)
{
  g_assert(cairo_status (gr->cr) == CAIRO_STATUS_SUCCESS);

  switch (gr->mode)
    {
    case MODE_DRAWABLE_WITH_RENDER:
      break;

    case MODE_DRAWABLE_NO_RENDER:
      {

	gint drawable_width, drawable_height;
	gint pixbuf_width, pixbuf_height;
	gint width, height;
	
	gdk_drawable_get_size (gr->drawable, &drawable_width, &drawable_height);
	pixbuf_width = gdk_pixbuf_get_width (gr->drawbuf);
	pixbuf_height = gdk_pixbuf_get_height (gr->drawbuf);
	width = min (drawable_width, pixbuf_width);
	height = min (drawable_height, pixbuf_height);
	
	gdk_draw_pixbuf (gr->drawable, NULL, gr->drawbuf,
			 0, 0, 0, 0, 
			 width, height, 
			 GDK_RGB_DITHER_NORMAL, 0, 0);
	
	if (gr->debug) printf ("copied (%d, %d) pixels from pixbuf to GDK drawable\n",
			       width, height);
      }
      break;
      
    case MODE_JAVA_ARRAY:
      if (gr->isCopy)
	memcpy (gr->javabuf, gr->javabuf_copy, gr->width * gr->height * 4);
      (*env)->ReleasePrimitiveArrayCritical (env, gr->jarray, gr->javabuf, JNI_COMMIT);
    }
}


static void 
update_pattern_transform (struct graphics2d *gr)
{
  cairo_matrix_t mat;

  g_assert (gr != NULL);
  if (gr->pattern == NULL)
    return;

  cairo_get_matrix (gr->cr, &mat);
  cairo_pattern_set_matrix (gr->pattern, &mat);
}

static void
check_for_debug (struct graphics2d *gr)
{
  gr->debug = (gboolean)(getenv("DEBUGJ2D") != NULL);
}

static void
realize_cb (GtkWidget *widget __attribute__ ((unused)), jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(),
                                       peer,
                                       initComponentGraphics2DUnlockedID);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_copyState
  (JNIEnv *env, jobject obj, jobject old)
{
  struct graphics2d *g = NULL, *g_old = NULL;

  gdk_threads_enter();

  g = (struct graphics2d *) g_malloc (sizeof (struct graphics2d));
  g_assert (g != NULL);
  memset (g, 0, sizeof(struct graphics2d));

  g_old = (struct graphics2d *) NSA_GET_G2D_PTR (env, old);
  g_assert (g_old != NULL);

  if (g_old->debug) printf ("copying state from existing graphics2d\n");

  g->debug = g_old->debug; 
  g->mode = g_old->mode;

  g->width = g_old->width;
  g->height = g_old->height;

  if (g_old->mode == MODE_JAVA_ARRAY)
    {
      jint size = g->width * g->height * 4;
      
      g->jarray = (*env)->NewGlobalRef (env, g_old->jarray);
      g->javabuf = (*env)->GetIntArrayElements (env, g->jarray, &g->isCopy);
      g->isCopy = JNI_TRUE;
      g->javabuf_copy = (jint *) g_malloc (size);
      memcpy (g->javabuf_copy, g->javabuf, size);
      g->surface = cairo_image_surface_create_for_data ((unsigned char *) g->javabuf,
						         CAIRO_FORMAT_ARGB32,
						         g->width,
						         g->height,
						         g->width * 4);
      g_assert (g->surface != NULL);
      g->cr = cairo_create (g->surface);
      g_assert (g->cr != NULL);
      (*env)->ReleaseIntArrayElements (env, g->jarray, g->javabuf, JNI_ABORT);
    }
  else
    {
      g->drawable = g_old->drawable;
      g_object_ref (g->drawable);

      if (x_server_has_render_extension ())
	init_graphics2d_as_renderable (g);
      else
	init_graphics2d_as_pixbuf (g);
    }

  if (g->pattern)
    cairo_pattern_set_filter (g->pattern, CAIRO_FILTER_FAST);

  NSA_SET_G2D_PTR (env, obj, g);

  gdk_threads_leave();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_initState___3III
(JNIEnv *env, jobject obj, jintArray jarr, jint width, jint height)
{
  struct graphics2d *gr = NULL;
  jint *cairobuf = NULL;

  gdk_threads_enter();

  gr = (struct graphics2d *) g_malloc (sizeof (struct graphics2d));
  g_assert (gr != NULL);
  memset (gr, 0, sizeof(struct graphics2d));

  check_for_debug (gr);  

  if (gr->debug) printf ("constructing java-backed image of size (%d,%d)\n",
			 width, height);

  gr->width = width;
  gr->height = height;
  gr->jarray = (*env)->NewGlobalRef(env, jarr);
  gr->javabuf = (*env)->GetPrimitiveArrayCritical (env, gr->jarray, &gr->isCopy);
  if (gr->isCopy)
    {
      /* We didn't get direct access to the pixel buffer, so we'll have to
         maintain a separate copy for Cairo. */
      jint size = gr->width * gr->height * 4;
      gr->javabuf_copy = (jint *) g_malloc (size);
      memcpy (gr->javabuf_copy, gr->javabuf, size);
      cairobuf = gr->javabuf_copy;
    }
  else
    {
      /* Have Cairo write directly to the Java array. */
      cairobuf = gr->javabuf;
    }
  gr->surface = cairo_image_surface_create_for_data ((unsigned char *) cairobuf,
						     CAIRO_FORMAT_ARGB32,
						     gr->width,
						     gr->height,
						     gr->width * 4);
  g_assert (gr->surface != NULL);
  gr->cr = cairo_create (gr->surface);
  g_assert (gr->cr != NULL);
  (*env)->ReleasePrimitiveArrayCritical (env, gr->jarray, gr->javabuf, JNI_COMMIT);
  
  gr->mode = MODE_JAVA_ARRAY;

  if (gr->debug) printf ("constructed java-backed image of size (%d,%d)\n",
			 width, height);

  NSA_SET_G2D_PTR (env, obj, gr);

  gdk_threads_leave();  
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_initState__II
  (JNIEnv *env, jobject obj, jint width, jint height)
{
  struct graphics2d *gr = NULL;
  
  gdk_threads_enter();

  gr = (struct graphics2d *) g_malloc (sizeof (struct graphics2d));
  g_assert (gr != NULL);
  memset (gr, 0, sizeof(struct graphics2d));

  check_for_debug (gr);  

  if (gr->debug) printf ("constructing offscreen drawable of size (%d,%d)\n",
			 width, height);

  gr->drawable = (GdkDrawable *) gdk_pixmap_new (NULL, width, height, 
						 gdk_rgb_get_visual ()->depth);
  g_assert (gr->drawable != NULL);

  gr->width = width;
  gr->height = height;

  if (x_server_has_render_extension ())
    init_graphics2d_as_renderable (gr);
  else
    init_graphics2d_as_pixbuf (gr);

  if (gr->debug) printf ("constructed offscreen drawable of size (%d,%d)\n",
			 width, height);
  NSA_SET_G2D_PTR (env, obj, gr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_gdkDrawDrawable
  (JNIEnv *env, jobject self, jobject other, jint x, jint y)
{
  struct graphics2d *src = NULL;
  struct graphics2d *dst = NULL;
  gint s_height;
  gint s_width;
  gint d_height;
  gint d_width;
  gint height;
  gint width;
  cairo_matrix_t matrix;
  cairo_operator_t tmp_op;

  gdk_threads_enter();
  
  if (peer_is_disposed(env, self))
    {
      gdk_threads_leave();
      return;
    }

  src = (struct graphics2d *)NSA_GET_G2D_PTR (env, other);
  dst = (struct graphics2d *)NSA_GET_G2D_PTR (env, self);
  g_assert (src != NULL);
  g_assert (dst != NULL);  

  if (src->debug) printf ("copying from offscreen drawable\n");

  begin_drawing_operation(env, dst); 

  /* gdk_flush(); */

  if (!GDK_IS_DRAWABLE (src->drawable) || 
   !GDK_IS_DRAWABLE (dst->drawable))
    {
      gdk_threads_leave ();
      return;
    }

  gdk_drawable_get_size (src->drawable, &s_width, &s_height);
  gdk_drawable_get_size (dst->drawable, &d_width, &d_height);
  width = min (s_width, d_width);
  height = min (s_height, d_height);

  cairo_get_matrix (src->cr, &matrix);
  cairo_matrix_translate (&matrix, (double)-x, (double)-y); 
  if (src->pattern)
    cairo_pattern_set_matrix (src->pattern, &matrix); 
  tmp_op = cairo_get_operator (dst->cr); 
  cairo_set_operator(dst->cr, CAIRO_OPERATOR_SOURCE); 
  cairo_set_source_surface (dst->cr, src->surface, 0, 0);
  cairo_paint (dst->cr);
  cairo_set_operator(dst->cr, tmp_op);

  cairo_matrix_translate (&matrix, (double)x, (double)y);
  if (src->pattern)
    cairo_pattern_set_matrix (src->pattern, &matrix);

  gdk_flush();

  end_drawing_operation(env, dst);

  if (src->debug) printf ("copied %d x %d pixels from offscreen drawable\n", width, height);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_initStateUnlocked
  (JNIEnv *env, jobject obj, jobject peer)
{
  struct graphics2d *gr = NULL;
  GtkWidget *widget = NULL;
  void *ptr = NULL;

  if (peer_is_disposed(env, obj))
    return;

  ptr = NSA_GET_PTR (env, peer);
  g_assert (ptr != NULL);

  gr = (struct graphics2d *) g_malloc (sizeof (struct graphics2d));
  g_assert (gr != NULL);
  memset (gr, 0, sizeof(struct graphics2d));

  check_for_debug (gr);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  grab_current_drawable (widget, &(gr->drawable), &(gr->win));
  g_assert (gr->drawable != NULL);

  gr->width = widget->allocation.width;
  gr->height = widget->allocation.height;

  if (x_server_has_render_extension ())
    init_graphics2d_as_renderable (gr);
  else
    init_graphics2d_as_pixbuf (gr);

  NSA_SET_G2D_PTR (env, obj, gr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_initState__Lgnu_java_awt_peer_gtk_GtkComponentPeer_2
  (JNIEnv *env, jobject obj, jobject peer)
{
  struct graphics2d *gr = NULL;
  GtkWidget *widget = NULL;
  void *ptr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave ();
      return;
    }

  ptr = NSA_GET_PTR (env, peer);
  g_assert (ptr != NULL);

  gr = (struct graphics2d *) g_malloc (sizeof (struct graphics2d));
  g_assert (gr != NULL);
  memset (gr, 0, sizeof(struct graphics2d));

  check_for_debug (gr);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  grab_current_drawable (widget, &(gr->drawable), &(gr->win));
  g_assert (gr->drawable != NULL);

  gr->width = widget->allocation.width;
  gr->height = widget->allocation.height;

  if (x_server_has_render_extension ())
    init_graphics2d_as_renderable (gr);
  else
    init_graphics2d_as_pixbuf (gr);

  NSA_SET_G2D_PTR (env, obj, gr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_connectSignals
  (JNIEnv *env, jobject obj, jobject peer)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, peer);

  g_signal_connect_after (G_OBJECT (ptr), "realize",
                          G_CALLBACK (realize_cb), obj);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_dispose
  (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  gr = (struct graphics2d *) NSA_DEL_G2D_PTR (env, obj);

  if (gr == NULL) 
    {
      gdk_threads_leave();
      return; /* dispose has been called more than once */
    }

  if (gr->surface)
    cairo_surface_destroy (gr->surface);

  cairo_destroy (gr->cr);

  if (gr->drawbuf)
    g_object_unref (gr->drawbuf); 

  if (gr->drawable)
    g_object_unref (gr->drawable);

  if (gr->pattern)
    cairo_pattern_destroy (gr->pattern);

  if (gr->pattern_surface)
    cairo_surface_destroy (gr->pattern_surface);

  if (gr->pattern_pixels)
    g_free (gr->pattern_pixels);

  if (gr->mode == MODE_JAVA_ARRAY)
    {
      (*env)->DeleteGlobalRef (env, gr->jarray);
      if (gr->javabuf_copy)
        g_free (gr->javabuf_copy);
    }

  if (gr->debug) printf ("disposed of graphics2d\n");

  g_free (gr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_setGradient 
  (JNIEnv *env, jobject obj, 
   jdouble x1, jdouble y1, 
   jdouble x2, jdouble y2,
   jint r1, jint g1, jint b1, jint a1,
   jint r2, jint g2, jint b2, jint a2,
   jboolean cyclic)
{
  gdk_threads_enter();
  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_setGradientUnlocked
    (env, obj,
     x1, y1, x2, y2,
     r1, g1, b1, a1,
     r2, g2, b2, a2,
     cyclic);

  gdk_threads_leave();
}
  
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_setGradientUnlocked
  (JNIEnv *env, jobject obj, 
   jdouble x1, jdouble y1, 
   jdouble x2, jdouble y2,
   jint r1, jint g1, jint b1, jint a1,
   jint r2, jint g2, jint b2, jint a2,
   jboolean cyclic)
{
  struct graphics2d *gr = NULL;
  cairo_surface_t *surf = NULL;
  cairo_t *cr2 = NULL;
  cairo_matrix_t mat;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);

  if (peer_is_disposed(env, obj))
    return;

  if (gr->debug)
    printf ("setGradientUnlocked (%f,%f) -> (%f,%f); (%d,%d,%d,%d) -> (%d,%d,%d,%d)\n",
	    x1, y1,
	    x2, y2,
	    r1, g1, b1, a1,
	    r2, g2, b2, a2);

  if (cyclic)
    surf = cairo_surface_create_similar (gr->surface, CAIRO_FORMAT_ARGB32, 3, 2);
  else
    surf = cairo_surface_create_similar (gr->surface, CAIRO_FORMAT_ARGB32, 2, 2);      
  g_assert (surf != NULL);

  cr2 = cairo_create (surf);
  
  cairo_identity_matrix (cr2);

  cairo_set_source_rgba (cr2, r1 / 255.0, g1 / 255.0, b1 / 255.0, a1 / 255.0);
  cairo_rectangle (cr2, 0, 0, 1, 2);
  cairo_fill (cr2);
    
  cairo_set_source_rgba (cr2, r2 / 255.0, g2 / 255.0, b2 / 255.0, a2 / 255.0);
  cairo_rectangle (cr2, 1, 0, 1, 2);
  cairo_fill (cr2);

  if (cyclic)
    {
      cairo_set_source_rgba (cr2, r1 / 255.0, g1 / 255.0, b1 / 255.0, a1 / 255.0);
      cairo_rectangle (cr2, 2, 0, 1, 2);
      cairo_fill (cr2);
    }

  cairo_matrix_init_identity (&mat);

  /* 
     consider the vector [x2 - x1, y2 - y1] = [p,q]

     this is a line in space starting at an 'origin' x1, y1.

     it can also be thought of as a "transformed" unit vector in either the
     x or y directions. we have just *drawn* our gradient as a unit vector
     (well, a 2-3x unit vector) in the x dimension. so what we want to know
     is which transformation turns our existing unit vector into [p,q].

     which means solving for M in 
 
     [p,q] = M[1,0]

     [p,q] = |a b| [1,0]
             |c d|      

     [p,q] = [a,c], with b = d = 0.

     what does this mean? it means that our gradient is 1-dimensional; as
     you move through the x axis of our 2 or 3 pixel gradient from logical
     x positions 0 to 1, the transformation of your x coordinate under the
     matrix M causes you to accumulate both x and y values in fill
     space. the y value of a gradient coordinate is ignored, since the
     gradient is one dimensional. which is correct.

     unfortunately we want the opposite transformation, it seems, because of
     the way cairo is going to use this transformation. I'm a bit confused by
     that, but it seems to work right, so we take reciprocals of values and
     negate offsets. oh well.
     
   */
  {
    double a = (x2 - x1 == 0.) ? 0. : ((cyclic ? 3.0 : 2.0) / (x2 - x1));
    double c = (y2 - y1 == 0.) ? 0. : (1. / (y2 - y1));
    double dx = (x1 == 0.) ? 0. : 1. / x1;
    double dy = (y1 == 0.) ? 0. : 1. / y1;
    cairo_pattern_t *p;
    
    cairo_matrix_init (&mat,
                       a, 0.,
                       c, 0.,
                       dx, dy);
    
    p = cairo_pattern_create_for_surface (surf);
    cairo_pattern_set_matrix (p, &mat);
    cairo_pattern_set_filter (p, CAIRO_FILTER_BILINEAR);
  }

  /* FIXME: repeating gradients (not to mention hold gradients) don't seem to work. */
  /*   cairo_surface_set_repeat (surf, cyclic ? 1 : 0); */

  if (gr->pattern)
    cairo_pattern_destroy (gr->pattern);
  
  if (gr->pattern_surface)
    cairo_surface_destroy (gr->pattern_surface);

  if (gr->pattern_pixels)
    g_free (gr->pattern_pixels);
  
  gr->pattern_pixels = NULL;  
  gr->pattern_surface = surf;  
  gr->pattern = cairo_pattern_create_for_surface(surf);

  cairo_set_source (gr->cr, gr->pattern);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_setTexturePixels 
  (JNIEnv *env, jobject obj, jintArray jarr, jint w, jint h, jint stride)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_setTexturePixelsUnlocked
    (env, obj, jarr, w, h, stride);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_setTexturePixelsUnlocked
  (JNIEnv *env, jobject obj, jintArray jarr, jint w, jint h, jint stride)
{
  struct graphics2d *gr = NULL;
  jint *jpixels = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);

  if (gr->debug)
    printf ("setTexturePixelsUnlocked (%d pixels, %dx%d, stride: %d)\n",
	    (*env)->GetArrayLength (env, jarr), w, h, stride);

  if (gr->pattern)
    cairo_pattern_destroy (gr->pattern);

  if (gr->pattern_surface)
    cairo_surface_destroy (gr->pattern_surface);

  if (gr->pattern_pixels)
    g_free (gr->pattern_pixels);

  gr->pattern = NULL;
  gr->pattern_surface = NULL;
  gr->pattern_pixels = NULL;

  gr->pattern_pixels = (char *) g_malloc (h * stride * 4);
  g_assert (gr->pattern_pixels != NULL);

  jpixels = (*env)->GetIntArrayElements (env, jarr, NULL);
  g_assert (jpixels != NULL);
  memcpy (gr->pattern_pixels, jpixels, h * stride * 4);
  (*env)->ReleaseIntArrayElements (env, jarr, jpixels, 0);

  gr->pattern_surface = cairo_image_surface_create_for_data ((unsigned char *)gr->pattern_pixels, 
							CAIRO_FORMAT_ARGB32, 
							w, h, stride * 4);
  g_assert (gr->pattern_surface != NULL);
  gr->pattern = cairo_pattern_create_for_surface (gr->pattern_surface);
  g_assert (gr->pattern != NULL);
  cairo_pattern_set_extend (gr->pattern, CAIRO_EXTEND_REPEAT);
  cairo_set_source (gr->cr, gr->pattern);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_drawPixels 
  (JNIEnv *env, jobject obj, jintArray java_pixels, 
   jint w, jint h, jint stride, jdoubleArray java_matrix)
{
  struct graphics2d *gr = NULL;
  jint *native_pixels = NULL;
  jdouble *native_matrix = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);

  if (gr->debug)
    printf ("drawPixels (%d pixels, %dx%d, stride: %d)\n",
	    (*env)->GetArrayLength (env, java_pixels), w, h, stride);

  native_pixels = (*env)->GetIntArrayElements (env, java_pixels, NULL);
  native_matrix = (*env)->GetDoubleArrayElements (env, java_matrix, NULL);
  g_assert (native_pixels != NULL);
  g_assert (native_matrix != NULL);
  g_assert ((*env)->GetArrayLength (env, java_matrix) == 6);

  begin_drawing_operation (env, gr);
  
 {
   cairo_matrix_t mat;
   cairo_pattern_t *p;
   cairo_surface_t *surf = cairo_image_surface_create_for_data ((unsigned char *)native_pixels, 
							   CAIRO_FORMAT_ARGB32, 
							   w, h, stride * 4);   
   cairo_matrix_init_identity (&mat);
   cairo_matrix_init (&mat, 
                      native_matrix[0], native_matrix[1],
                      native_matrix[2], native_matrix[3],
                      native_matrix[4], native_matrix[5]);

   p = cairo_pattern_create_for_surface (surf);
   cairo_pattern_set_matrix (p, &mat);
   if (gr->pattern)
     cairo_pattern_set_filter (p, cairo_pattern_get_filter (gr->pattern));
   cairo_set_source (gr->cr, p);
   cairo_paint (gr->cr);
   cairo_pattern_destroy (p);
   cairo_surface_destroy (surf);
 }
  
 end_drawing_operation (env, gr);
 
 (*env)->ReleaseIntArrayElements (env, java_pixels, native_pixels, 0);
 (*env)->ReleaseDoubleArrayElements (env, java_matrix, native_matrix, 0);
 
  gdk_threads_leave();
}

/* passthrough methods to cairo */

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSave 
   (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_save\n");
  cairo_save (gr->cr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoRestore 
   (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_restore\n");
  cairo_restore (gr->cr);
  update_pattern_transform (gr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetMatrix 
   (JNIEnv *env, jobject obj, jdoubleArray java_matrix)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetMatrixUnlocked
    (env, obj, java_matrix);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetMatrixUnlocked
   (JNIEnv *env, jobject obj, jdoubleArray java_matrix)
{
  struct graphics2d *gr = NULL;
  jdouble *native_matrix = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);

  /* cairoSetMatrix was called before this graphics object's component
     was realized. */
  if (gr == NULL)
    return;

  native_matrix = (*env)->GetDoubleArrayElements (env, java_matrix, NULL);  
  g_assert (native_matrix != NULL);
  g_assert ((*env)->GetArrayLength (env, java_matrix) == 6);

  if (gr->debug)
    printf ("cairo_matrix_init [ %f, %f, %f, %f, %f, %f ]\n",
	    native_matrix[0], native_matrix[1],
	    native_matrix[2], native_matrix[3],
	    native_matrix[4], native_matrix[5]);

  {
    cairo_matrix_t mat;

    cairo_matrix_init_identity (&mat);
    cairo_matrix_init (&mat, 
                       native_matrix[0], native_matrix[1],
                       native_matrix[2], native_matrix[3],
                       native_matrix[4], native_matrix[5]);
    cairo_set_matrix (gr->cr, &mat);
  }

  (*env)->ReleaseDoubleArrayElements (env, java_matrix, native_matrix, 0);
  update_pattern_transform (gr);
}

static void
install_font_peer(cairo_t *cr,
		  struct peerfont *pfont,
		  int debug)
{
  cairo_font_face_t *ft;
  FT_Face face = NULL;

  g_assert(cr != NULL);
  g_assert(pfont != NULL);
  
  if (pfont->graphics_resource == NULL)
    {
      face = pango_ft2_font_get_face (pfont->font);
      g_assert (face != NULL);
      
      ft = cairo_ft_font_face_create_for_ft_face (face, 0);
      g_assert (ft != NULL);
      
      if (debug) printf ("install_font_peer made new cairo font for '%s' at %f\n", 
			 face->family_name,
			 (pango_font_description_get_size (pfont->desc) / 
			  (double)PANGO_SCALE));
    
      cairo_set_font_face (cr, ft); 
      cairo_font_face_destroy (ft);
      cairo_set_font_size (cr, 
			(pango_font_description_get_size (pfont->desc) / 
			 (double)PANGO_SCALE));
      ft = cairo_get_font_face (cr);
      pfont->graphics_resource = ft;
    }
  else
    {
      if (debug) printf ("install_font_peer reused existing font resource\n");
      ft = (cairo_font_face_t *) pfont->graphics_resource;
      cairo_set_font_face (cr, ft);       
    }
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_releasePeerGraphicsResource
   (JNIEnv *env, jclass clazz __attribute__ ((unused)), jobject java_font)
{
  struct peerfont *pfont = NULL;

  gdk_threads_enter();

  g_assert(java_font != NULL);

  pfont = (struct peerfont *) NSA_GET_FONT_PTR (env, java_font);
  g_assert (pfont != NULL);
  if (pfont->graphics_resource != NULL)
    {
      cairo_font_face_destroy ((cairo_font_face_t *) pfont->graphics_resource);
      pfont->graphics_resource = NULL;
    }

  gdk_threads_leave();
}

static void
paint_glyph_run(JNIEnv *env,
		struct graphics2d *gr,
		cairo_glyph_t **glyphs,
		gint *n_glyphs,
		PangoLayoutRun *run)
{
  gint i = 0;
  gint x = 0, y = 0;

  g_assert (gr != NULL);
  g_assert (glyphs != NULL);
  g_assert (n_glyphs != NULL);
  g_assert (run != NULL);

  if (run->glyphs != NULL && run->glyphs->num_glyphs > 0)
    {
      if (*n_glyphs < run->glyphs->num_glyphs)
	{
	  *glyphs = g_realloc(*glyphs, 
			      (sizeof(cairo_glyph_t) 
			       * run->glyphs->num_glyphs));
	  *n_glyphs = run->glyphs->num_glyphs;
	}
      
      g_assert (*glyphs != NULL);

      if (gr->debug) printf ("painting %d glyphs: ", run->glyphs->num_glyphs);

      for (i = 0; i < run->glyphs->num_glyphs; ++i)
	{	  
	  (*glyphs)[i].index = run->glyphs->glyphs[i].glyph;

	  (*glyphs)[i].x =
	    ((double) (x + run->glyphs->glyphs[i].geometry.x_offset)) 
	    / ((double) PANGO_SCALE);

	  (*glyphs)[i].y =
	    ((double) (y + run->glyphs->glyphs[i].geometry.y_offset)) 
	    / ((double) PANGO_SCALE);
	  
	  if (gr->debug) printf(" (%ld @ %f,%f)",  
				(*glyphs)[i].index,  
				(*glyphs)[i].x, 
				(*glyphs)[i].y);
	    
	  x += run->glyphs->glyphs[i].geometry.width;
	}

      if (gr->debug) printf("\n");
      begin_drawing_operation (env, gr);
      cairo_show_glyphs (gr->cr, *glyphs, run->glyphs->num_glyphs);
      end_drawing_operation (env, gr);      
    }
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoDrawGlyphVector
   (JNIEnv *env, jobject self, 
    jobject font,
    jfloat x, jfloat y, jint n,
    jintArray java_codes,
    jfloatArray java_positions)
{
  
  struct graphics2d *gr = NULL;
  struct peerfont *pfont = NULL;
  cairo_glyph_t *glyphs = NULL;
  int *native_codes;
  float *native_positions;
  jint i = 0;

  gdk_threads_enter ();

  g_assert (self != NULL);
  g_assert (java_codes != NULL);
  g_assert (java_positions != NULL);

  if (peer_is_disposed(env, self))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *)NSA_GET_G2D_PTR (env, self);
  g_assert (gr != NULL);

  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, font);
  g_assert (pfont != NULL);

  install_font_peer(gr->cr, pfont, gr->debug);

  glyphs = g_malloc( sizeof(cairo_glyph_t) * n);
  g_assert (glyphs != NULL);

  native_codes = (*env)->GetIntArrayElements (env, java_codes, NULL);
  native_positions = (*env)->GetFloatArrayElements (env, java_positions, NULL);
  
  for (i = 0; i < n; ++i)
    {
      glyphs[i].index = native_codes[i];
      glyphs[i].x = x + native_positions[ 2*i ];
      glyphs[i].y = y + native_positions[ 2*i + 1];
    }

  (*env)->ReleaseFloatArrayElements (env, java_positions, native_positions, 0);
  (*env)->ReleaseIntArrayElements (env, java_codes, native_codes, 0);

  begin_drawing_operation (env, gr);
  cairo_show_glyphs (gr->cr, glyphs, n);
  end_drawing_operation (env, gr);

  g_free(glyphs);

  gdk_threads_leave ();  
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoDrawGdkTextLayout
   (JNIEnv *env, jobject self, jobject java_layout, jfloat x, jfloat y)
{
  /* 
   * FIXME: Some day we expect either cairo or pango will know how to make
   * a pango layout paint to a cairo surface. that day is not yet here.
   */

  struct graphics2d *gr = NULL;
  struct textlayout *tl = NULL;
  PangoLayoutIter *i = NULL;
  PangoLayoutRun *run = NULL;
  cairo_glyph_t *glyphs = NULL;
  gint n_glyphs = 0;

  gdk_threads_enter ();

  g_assert (self != NULL);
  g_assert (java_layout != NULL);

  gr = (struct graphics2d *)NSA_GET_G2D_PTR (env, self);
  tl = (struct textlayout *)NSA_GET_TEXT_LAYOUT_PTR (env, java_layout);

  g_assert (gr != NULL);
  g_assert (tl != NULL);
  g_assert (tl->pango_layout != NULL);

  if (gr->debug) printf ("painting pango layout\n");

  if (peer_is_disposed(env, self))
    {
      gdk_threads_leave();
      return;
    }

  i = pango_layout_get_iter (tl->pango_layout);
  g_assert (i != NULL);

  cairo_translate (gr->cr, x, y);

  do 
    {
      run = pango_layout_iter_get_run (i);
      if (run != NULL)
	paint_glyph_run (env, gr, &glyphs, &n_glyphs, run);
    } 
  while (pango_layout_iter_next_run (i));
  
  if (glyphs != NULL)
    g_free (glyphs);

  cairo_translate (gr->cr, -x, -y);
  
  pango_layout_iter_free (i);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetOperator 
   (JNIEnv *env, jobject obj, jint op)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_set_operator %d\n", op);
  switch ((enum java_awt_alpha_composite_rule) op)
    {
    case java_awt_alpha_composite_CLEAR: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_CLEAR);
      break;
      
    case java_awt_alpha_composite_SRC: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_SOURCE);
      break;
      
    case java_awt_alpha_composite_SRC_OVER: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_OVER);
      break;

    case java_awt_alpha_composite_DST_OVER: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_DEST_OVER);
      break;

    case java_awt_alpha_composite_SRC_IN: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_IN);
      break;

    case java_awt_alpha_composite_DST_IN: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_DEST_IN);
      break;

    case java_awt_alpha_composite_SRC_OUT: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_OUT);
      break;

    case java_awt_alpha_composite_DST_OUT: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_DEST_OUT);
      break;

    case java_awt_alpha_composite_DST: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_DEST);
      break;

    case java_awt_alpha_composite_SRC_ATOP: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_ATOP);
      break;

    case java_awt_alpha_composite_DST_ATOP: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_DEST_ATOP);
      break;

    case java_awt_alpha_composite_XOR: 
      cairo_set_operator (gr->cr, CAIRO_OPERATOR_XOR);
      break;
    }

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetRGBAColor 
   (JNIEnv *env, jobject obj, jdouble r, jdouble g, jdouble b, jdouble a)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetRGBAColorUnlocked
    (env, obj, r, g, b, a);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetRGBAColorUnlocked
   (JNIEnv *env, jobject obj, jdouble r, jdouble g, jdouble b, jdouble a)
{
  struct graphics2d *gr = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);

  /* this is a very weird fact: GDK Pixbufs and RENDER drawables consider
     colors in opposite pixel order. I have no idea why.  thus when you
     draw to a PixBuf, you must exchange the R and B components of your
     color. */

  if (gr->debug)
    printf ("cairo_set_source_rgba (%f, %f, %f, %f)\n", r, g, b, a);

  if (gr->drawbuf)
    cairo_set_source_rgba (gr->cr, b, g, r, a);
  else
    cairo_set_source_rgba (gr->cr, r, g, b, a);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetFillRule 
   (JNIEnv *env, jobject obj, jint rule)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  if (gr->debug) printf ("cairo_set_fill_rule %d\n", rule);
  g_assert (gr != NULL);
  switch ((enum java_awt_geom_path_iterator_winding_rule) rule)
    {
    case java_awt_geom_path_iterator_WIND_NON_ZERO:
      cairo_set_fill_rule (gr->cr, CAIRO_FILL_RULE_WINDING);
      break;
    case java_awt_geom_path_iterator_WIND_EVEN_ODD:
      cairo_set_fill_rule (gr->cr, CAIRO_FILL_RULE_EVEN_ODD);
      break;
    }  

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineWidth 
   (JNIEnv *env, jobject obj, jdouble width)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineWidthUnlocked
    (env, obj, width);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineWidthUnlocked
   (JNIEnv *env, jobject obj, jdouble width)
{
  struct graphics2d *gr = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_set_line_width %f\n", width);
  cairo_set_line_width (gr->cr, width);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineCap 
   (JNIEnv *env, jobject obj, jint cap)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineCapUnlocked
    (env, obj, cap);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineCapUnlocked
   (JNIEnv *env, jobject obj, jint cap)
{
  struct graphics2d *gr = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_set_line_cap %d\n", cap);
  switch ((enum java_awt_basic_stroke_cap_rule) cap)
    {
    case java_awt_basic_stroke_CAP_BUTT: 
      cairo_set_line_cap (gr->cr, CAIRO_LINE_CAP_BUTT);
      break;

    case java_awt_basic_stroke_CAP_ROUND: 
      cairo_set_line_cap (gr->cr, CAIRO_LINE_CAP_ROUND);
      break;

    case java_awt_basic_stroke_CAP_SQUARE: 
      cairo_set_line_cap (gr->cr, CAIRO_LINE_CAP_SQUARE);
      break;
    }
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineJoin
   (JNIEnv *env, jobject obj, jint join)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineJoinUnlocked
    (env, obj, join);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetLineJoinUnlocked
   (JNIEnv *env, jobject obj, jint join)
{
  struct graphics2d *gr = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_set_line_join %d\n", join);
  switch ((enum java_awt_basic_stroke_join_rule) join)
    {
    case java_awt_basic_stroke_JOIN_MITER:
      cairo_set_line_join (gr->cr, CAIRO_LINE_JOIN_MITER);
      break;

    case java_awt_basic_stroke_JOIN_ROUND:
      cairo_set_line_join (gr->cr, CAIRO_LINE_JOIN_ROUND);
      break;

    case java_awt_basic_stroke_JOIN_BEVEL:
      cairo_set_line_join (gr->cr, CAIRO_LINE_JOIN_BEVEL);
      break;
    }
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetDash
   (JNIEnv *env, jobject obj, jdoubleArray dashes, jint ndash, jdouble offset)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetDashUnlocked
    (env, obj, dashes, ndash, offset);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetDashUnlocked
   (JNIEnv *env, jobject obj, jdoubleArray dashes, jint ndash, jdouble offset)
{
  struct graphics2d *gr = NULL;
  jdouble *dasharr = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_set_dash\n");
  dasharr = (*env)->GetDoubleArrayElements (env, dashes, NULL);  
  g_assert (dasharr != NULL);
  cairo_set_dash (gr->cr, dasharr, ndash, offset);
  (*env)->ReleaseDoubleArrayElements (env, dashes, dasharr, 0);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetMiterLimit 
   (JNIEnv *env, jobject obj, jdouble miter)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetMiterLimitUnlocked
    (env, obj, miter);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSetMiterLimitUnlocked
   (JNIEnv *env, jobject obj, jdouble miter)
{
  struct graphics2d *gr = NULL;

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_set_miter_limit %f\n", miter);
  cairo_set_miter_limit (gr->cr, miter);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoNewPath 
   (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);

  if (gr == NULL)
    {
      gdk_threads_leave ();
      return;
    }

  if (gr->debug) printf ("cairo_new_path\n");
  cairo_new_path (gr->cr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoMoveTo 
   (JNIEnv *env, jobject obj, jdouble x, jdouble y)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_move_to (%f, %f)\n", x, y);
  cairo_move_to (gr->cr, x, y);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoLineTo 
   (JNIEnv *env, jobject obj, jdouble x, jdouble y)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_line_to (%f, %f)\n", x, y);
  cairo_line_to (gr->cr, x, y);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoCurveTo 
   (JNIEnv *env, jobject obj, jdouble x1, jdouble y1, jdouble x2, jdouble y2, jdouble x3, jdouble y3)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug)
    printf ("cairo_curve_to (%f, %f), (%f, %f), (%f, %f)\n",
	    x1, y1, x2, y2, x3, y3);
  cairo_curve_to (gr->cr, x1, y1, x2, y2, x3, y3);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoRelMoveTo 
   (JNIEnv *env, jobject obj, jdouble dx, jdouble dy)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_rel_move_to (%f, %f)\n", dx, dy);
  cairo_rel_move_to (gr->cr, dx, dy);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoRelLineTo 
   (JNIEnv *env, jobject obj, jdouble dx, jdouble dy)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_rel_line_to (%f, %f)\n", dx, dy);
  cairo_rel_line_to (gr->cr, dx, dy);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoRelCurveTo 
   (JNIEnv *env, jobject obj, jdouble dx1, jdouble dy1, jdouble dx2, jdouble dy2, jdouble dx3, jdouble dy3)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug)
    printf ("cairo_rel_curve_to (%f, %f), (%f, %f), (%f, %f)\n",
	    dx1, dy1, dx2, dy2, dx3, dy3);
  cairo_rel_curve_to (gr->cr, dx1, dy1, dx2, dy2, dx3, dy3);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoRectangle 
   (JNIEnv *env, jobject obj, jdouble x, jdouble y, jdouble width, jdouble height)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);

  if (gr == NULL)
    {
      gdk_threads_leave ();
      return;
    }

  if (gr->debug)
    printf ("cairo_rectangle (%f, %f) (%f, %f)\n", x, y, width, height);
  cairo_rectangle (gr->cr, x, y, width, height);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoClosePath 
   (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_close_path\n");
  cairo_close_path (gr->cr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoStroke 
   (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_stroke\n");
  begin_drawing_operation (env, gr);
  cairo_stroke (gr->cr);
  end_drawing_operation (env, gr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoFill 
   (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_fill\n");
  begin_drawing_operation (env, gr);
  cairo_fill (gr->cr);
  end_drawing_operation (env, gr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoClip 
   (JNIEnv *env, jobject obj)
{
  struct graphics2d *gr = NULL;

  gdk_threads_enter();

  if (peer_is_disposed(env, obj))
    {
      gdk_threads_leave();
      return;
    }

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);

  if (gr == NULL)
    {
      gdk_threads_leave ();
      return;
    }

  if (gr->debug) printf ("cairo_clip\n");
  begin_drawing_operation (env, gr);
  cairo_reset_clip (gr->cr);
  cairo_clip (gr->cr);
  end_drawing_operation (env, gr);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSurfaceSetFilter
   (JNIEnv *env, jobject obj, jint filter)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSurfaceSetFilterUnlocked
    (env, obj, filter);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphics2D_cairoSurfaceSetFilterUnlocked
   (JNIEnv *env, jobject obj, jint filter)
{
  struct graphics2d *gr = NULL;   

  if (peer_is_disposed(env, obj))
    return;

  gr = (struct graphics2d *) NSA_GET_G2D_PTR (env, obj);
  g_assert (gr != NULL);
  if (gr->debug) printf ("cairo_pattern_set_filter %d\n", filter);
  switch ((enum java_awt_rendering_hints_filter) filter)
    {
    case java_awt_rendering_hints_VALUE_INTERPOLATION_NEAREST_NEIGHBOR:
      cairo_pattern_set_filter (gr->pattern, CAIRO_FILTER_NEAREST);
      break;
    case java_awt_rendering_hints_VALUE_INTERPOLATION_BILINEAR:
      cairo_pattern_set_filter (gr->pattern, CAIRO_FILTER_BILINEAR);
      break; 
    case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_SPEED:
      cairo_pattern_set_filter (gr->pattern, CAIRO_FILTER_FAST);
      break;
    case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_DEFAULT:
      cairo_pattern_set_filter (gr->pattern, CAIRO_FILTER_NEAREST);
      break;
    case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_QUALITY:
      cairo_pattern_set_filter (gr->pattern, CAIRO_FILTER_BEST);
      break;
    }
}
