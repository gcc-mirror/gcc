/* gnu_java_awt_peer_gtk_CairoGraphics2d.c
   Copyright (C) 2006  Free Software Foundation, Inc.

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
#include "gdkfont.h"
#include "cairographics2d.h"
#include "gnu_java_awt_peer_gtk_CairoGraphics2D.h"
#include <gdk/gdktypes.h>
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>

#include <cairo-ft.h>
#include <cairo-xlib.h>

#include <stdio.h>
#include <stdlib.h>

static void install_font_peer(cairo_t *cr, struct peerfont *pfont);
static void update_pattern_transform (struct cairographics2d *gr);
static struct cairographics2d *getPointer(JNIEnv *env, jobject obj);

static struct cairographics2d *
getPointer(JNIEnv *env, jobject obj)
{
  jclass cls;
  jlong value;
  jfieldID nofid;
  cls = (*env)->GetObjectClass( env, obj );
  nofid = (*env)->GetFieldID( env, cls, "nativePointer", "J" );
  value = (*env)->GetLongField( env, obj, nofid );
  (*env)->DeleteLocalRef( env, cls );

  return JLONG_TO_PTR(struct cairographics2d, value);
}

/**
 * Returns the cairo_t * associated with a CairoGraphics2D object,
 * This is used by GdkTextLayout.
 */
cairo_t *cp_gtk_get_cairo_t(JNIEnv *env,
			    jobject cairographics2dobj)
{
  struct cairographics2d *gr = getPointer(env, cairographics2dobj);
  g_assert(gr != NULL);
  return gr->cr;
}

/**
 * Allocates the cairographics2d structure.
 */
JNIEXPORT jlong JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_init
  (JNIEnv *env __attribute__ ((unused)),
   jobject obj __attribute__ ((unused)),
   jlong cairo_t_pointer)
{
  struct cairographics2d *g = NULL;
  cairo_t *cr = JLONG_TO_PTR(cairo_t, cairo_t_pointer);
  g_assert(cr != NULL);

  g = (struct cairographics2d *) g_malloc (sizeof (struct cairographics2d));

  g_assert (g != NULL);
  memset (g, 0, sizeof(struct cairographics2d));
  g->cr = cr;
  
  return PTR_TO_JLONG(g);
}

/**
 * Disposes of the cairographics2d structure.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_disposeNative
  (JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer(env, obj);

  if (gr == NULL)
    return;

  if (gr->cr)
    cairo_destroy (gr->cr);

  if (gr->pattern)
    cairo_pattern_destroy (gr->pattern);
  gr->pattern = NULL;
  
  if (gr->pattern_surface)
    cairo_surface_destroy (gr->pattern_surface);
  gr->pattern_surface = NULL;

  if (gr->pattern_pixels)
    g_free(gr->pattern_pixels);
  gr->pattern_pixels = NULL;

  g_free( gr );
}

/**
 * Set the gradient.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_setGradient
  (JNIEnv *env, jobject obj, 
   jdouble x1, jdouble y1, 
   jdouble x2, jdouble y2,
   jint r1, jint g1, jint b1, jint a1,
   jint r2, jint g2, jint b2, jint a2,
   jboolean cyclic)
{
  struct cairographics2d *gr = NULL;
  cairo_pattern_t* pattern;
  cairo_extend_t extend;

  gr = getPointer (env, obj);
  g_assert( gr != NULL );

  pattern = cairo_pattern_create_linear(x1, y1, x2, y2);
  g_assert( pattern != NULL );

  cairo_pattern_add_color_stop_rgba(pattern, 0.0, r1 / 255.0, g1 / 255.0, 
				    b1 / 255.0, a1 / 255.0);

  cairo_pattern_add_color_stop_rgba(pattern, 1.0, r2 / 255.0, g2 / 255.0, 
				    b2 / 255.0, a2 / 255.0);

  extend = (cyclic == JNI_TRUE) ? CAIRO_EXTEND_REFLECT : CAIRO_EXTEND_NONE;

  cairo_pattern_set_extend( pattern, extend );

  gr->pattern = pattern;
  cairo_set_source(gr->cr, gr->pattern);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_setTexturePixels
  (JNIEnv *env, jobject obj, jintArray jarr, jint w, jint h, jint stride)
{
  struct cairographics2d *gr = NULL;
  jint *jpixels = NULL;

  gr = getPointer (env, obj);
  g_assert (gr != NULL);

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
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_drawPixels 
  (JNIEnv *env, jobject obj, jintArray java_pixels, 
   jint w, jint h, jint stride, jdoubleArray java_matrix)
{
  jint *native_pixels = NULL;
  jdouble *native_matrix = NULL;
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  native_pixels = (*env)->GetIntArrayElements (env, java_pixels, NULL);
  native_matrix = (*env)->GetDoubleArrayElements (env, java_matrix, NULL);
  g_assert (native_pixels != NULL);
  g_assert (native_matrix != NULL);
  g_assert ((*env)->GetArrayLength (env, java_matrix) == 6);

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
  
 (*env)->ReleaseIntArrayElements (env, java_pixels, native_pixels, 0);
 (*env)->ReleaseDoubleArrayElements (env, java_matrix, native_matrix, 0);
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetMatrix
   (JNIEnv *env, jobject obj, jdoubleArray java_matrix)
{
  jdouble *native_matrix = NULL;
  struct cairographics2d *gr = getPointer (env, obj);

  native_matrix = (*env)->GetDoubleArrayElements (env, java_matrix, NULL);  
  g_assert (native_matrix != NULL);
  g_assert ((*env)->GetArrayLength (env, java_matrix) == 6);

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

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoDrawGlyphVector
   (JNIEnv *env, jobject obj, 
    jobject font,
    jfloat x, jfloat y, jint n,
    jintArray java_codes,
    jfloatArray java_positions)
{
  
  struct cairographics2d *gr = NULL;
  struct peerfont *pfont = NULL;
  cairo_glyph_t *glyphs = NULL;
  int *native_codes;
  float *native_positions;
  jint i = 0;

  g_assert (obj != NULL);
  g_assert (java_codes != NULL);
  g_assert (java_positions != NULL);

  gr = getPointer (env, obj);
  g_assert (gr != NULL);

  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, font);
  g_assert (pfont != NULL);

  install_font_peer(gr->cr, pfont);

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

  cairo_show_glyphs (gr->cr, glyphs, n);

  g_free(glyphs);
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetOperator 
   (JNIEnv *env, jobject obj, jint op)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

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
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetRGBAColor
   (JNIEnv *env, jobject obj, jdouble r, jdouble g, jdouble b, jdouble a)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_set_source_rgba (gr->cr, r, g, b, a);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetFillRule 
   (JNIEnv *env, jobject obj, jint rule)
{
  struct cairographics2d *gr = getPointer (env, obj);
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
}

/**
 * Set the line style, except for dashes.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetLine
  (JNIEnv *env, jobject obj, jdouble width, int cap, int join, double miterLimit)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  /* set width */
  cairo_set_line_width (gr->cr, width);

  /* set cap */
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

  /* set join */
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

  /* set miter */
  cairo_set_miter_limit (gr->cr, miterLimit);
}

/**
 * Set the line dashes
 */ 
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetDash
   (JNIEnv *env, jobject obj, jdoubleArray dashes, jint ndash, jdouble offset)
{
  jdouble *dasharr = NULL;
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  dasharr = (*env)->GetDoubleArrayElements (env, dashes, NULL);  
  g_assert (dasharr != NULL);

  cairo_set_dash (gr->cr, dasharr, ndash, offset);

  (*env)->ReleaseDoubleArrayElements (env, dashes, dasharr, 0);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoNewPath 
   (JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_new_path (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoMoveTo 
   (JNIEnv *env, jobject obj, jdouble x, jdouble y)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_move_to (gr->cr, x, y);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoLineTo 
   (JNIEnv *env, jobject obj, jdouble x, jdouble y)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_line_to (gr->cr, x, y);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoCurveTo 
   (JNIEnv *env, jobject obj, jdouble x1, jdouble y1, jdouble x2, jdouble y2, jdouble x3, jdouble y3)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);
  cairo_curve_to (gr->cr, x1, y1, x2, y2, x3, y3);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoRelMoveTo 
   (JNIEnv *env, jobject obj, jdouble dx, jdouble dy)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_rel_move_to (gr->cr, dx, dy);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoRelLineTo 
   (JNIEnv *env, jobject obj, jdouble dx, jdouble dy)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_rel_line_to (gr->cr, dx, dy);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoRelCurveTo 
   (JNIEnv *env, jobject obj, jdouble dx1, jdouble dy1, jdouble dx2, jdouble dy2, jdouble dx3, jdouble dy3)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_rel_curve_to (gr->cr, dx1, dy1, dx2, dy2, dx3, dy3);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoRectangle 
   (JNIEnv *env, jobject obj, jdouble x, jdouble y, jdouble width, jdouble height)
{
  struct cairographics2d *gr = getPointer (env, obj);

  cairo_rectangle (gr->cr, x, y, width, height);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoClosePath 
   (JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_close_path (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoStroke 
   (JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_stroke (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoFill 
   (JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);

  cairo_fill (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoClip 
   (JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer( env, obj );
  g_assert( gr != NULL );

  cairo_clip( gr->cr );
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoResetClip 
  (JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer( env, obj );
  g_assert (gr != NULL);

  cairo_reset_clip( gr->cr );
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoPreserveClip 
(JNIEnv *env, jobject obj)
{
  struct cairographics2d *gr = getPointer( env, obj );
  g_assert (gr != NULL);

  cairo_clip_preserve( gr->cr );
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSurfaceSetFilter
   (JNIEnv *env, jobject obj, jint filter)
{
  struct cairographics2d *gr = getPointer (env, obj);
  g_assert (gr != NULL);
  
  if (gr->pattern == NULL)
    return;
  
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

/************************** FONT STUFF ****************************/
static void
install_font_peer(cairo_t *cr,
		  struct peerfont *pfont)
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
      ft = (cairo_font_face_t *) pfont->graphics_resource;
      cairo_set_font_face (cr, ft);
      cairo_set_font_size (cr,
                           (pango_font_description_get_size (pfont->desc) /
                            (double)PANGO_SCALE));
    }
}

static void 
update_pattern_transform (struct cairographics2d *gr)
{
  cairo_matrix_t mat;

  g_assert (gr != NULL);
  if (gr->pattern == NULL)
    return;

  cairo_get_matrix (gr->cr, &mat);
  cairo_pattern_set_matrix (gr->pattern, &mat);
}


