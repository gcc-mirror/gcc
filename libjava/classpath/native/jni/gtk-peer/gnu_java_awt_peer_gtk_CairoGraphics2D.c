/* gnu_java_awt_peer_gtk_CairoGraphics2d.c
   Copyright (C) 2006, 2008  Free Software Foundation, Inc.

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

#include <cairo-ft.h>

#include <stdio.h>
#include <stdlib.h>

static void update_pattern_transform (struct cairographics2d *gr);

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
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
  jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);

  if (gr == NULL)
    return;

  if (gr->cr)
    {
      gdk_threads_enter();
      cairo_destroy (gr->cr);
      gdk_threads_leave();
    }

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
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
   jlong pointer,
   jdouble x1, jdouble y1, 
   jdouble x2, jdouble y2,
   jint r1, jint g1, jint b1, jint a1,
   jint r2, jint g2, jint b2, jint a2,
   jboolean cyclic)
{
  struct cairographics2d *gr = NULL;
  cairo_pattern_t* pattern;
  cairo_extend_t extend;

  gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert( gr != NULL );

  pattern = cairo_pattern_create_linear(x1, y1, x2, y2);
  g_assert( pattern != NULL );

  cairo_pattern_add_color_stop_rgba(pattern, 0.0, r1 / 255.0, g1 / 255.0, 
				    b1 / 255.0, a1 / 255.0);

  cairo_pattern_add_color_stop_rgba(pattern, 1.0, r2 / 255.0, g2 / 255.0, 
				    b2 / 255.0, a2 / 255.0);

  #if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 2, 0)
  	extend = (cyclic == JNI_TRUE) ? CAIRO_EXTEND_REFLECT : CAIRO_EXTEND_PAD;
  #else
  	extend = (cyclic == JNI_TRUE) ? CAIRO_EXTEND_REFLECT : CAIRO_EXTEND_NONE;
  #endif

  cairo_pattern_set_extend( pattern, extend );

  gr->pattern = pattern;
  cairo_set_source(gr->cr, gr->pattern);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_setPaintPixels
 (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
  jlong pointer, jintArray jarr, jint w, jint h, jint stride, jboolean repeat,
  jint x, jint y)
{
  struct cairographics2d *gr = NULL;
  jint *jpixels = NULL;

  gr = JLONG_TO_PTR(struct cairographics2d, pointer);
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
  
  cairo_set_source_surface(gr->cr, gr->pattern_surface, x, y);
  
  if (repeat)
  	cairo_pattern_set_extend(cairo_get_source(gr->cr), CAIRO_EXTEND_REPEAT);
  else
  	cairo_pattern_set_extend(cairo_get_source(gr->cr), CAIRO_EXTEND_NONE);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_drawPixels 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jintArray java_pixels, 
 jint w, jint h, jint stride, jdoubleArray java_matrix, jdouble alpha,
 jint interpolation)
{
  jint *native_pixels = NULL;
  jdouble *native_matrix = NULL;
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
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
   switch ((enum java_awt_rendering_hints_filter) interpolation)
     {
     case java_awt_rendering_hints_VALUE_INTERPOLATION_NEAREST_NEIGHBOR:
       cairo_pattern_set_filter (p, CAIRO_FILTER_NEAREST);
       break;
     case java_awt_rendering_hints_VALUE_INTERPOLATION_BILINEAR:
       cairo_pattern_set_filter (p, CAIRO_FILTER_BILINEAR);
       break; 
     case java_awt_rendering_hints_VALUE_INTERPOLATION_BICUBIC:
       cairo_pattern_set_filter (p, CAIRO_FILTER_GAUSSIAN);
       break; 
     case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_SPEED:
       cairo_pattern_set_filter (p, CAIRO_FILTER_FAST);
       break;
     case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_DEFAULT:
       cairo_pattern_set_filter (p, CAIRO_FILTER_NEAREST);
       break;
     case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_QUALITY:
       cairo_pattern_set_filter (p, CAIRO_FILTER_BEST);
       break;
     }
   
   cairo_set_source (gr->cr, p);
   if (alpha == 1.)
     cairo_paint (gr->cr);
   else
     cairo_paint_with_alpha(gr->cr, alpha);

   cairo_pattern_destroy (p);
   cairo_surface_destroy (surf);
 }
  
 (*env)->ReleaseIntArrayElements (env, java_pixels, native_pixels, 0);
 (*env)->ReleaseDoubleArrayElements (env, java_matrix, native_matrix, 0);
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetMatrix
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdoubleArray java_matrix)
{
  jdouble *native_matrix = NULL;
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

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
    g_assert (gr != NULL);
    cairo_set_matrix (gr->cr, &mat);
  }

  (*env)->ReleaseDoubleArrayElements (env, java_matrix, native_matrix, 0);
  update_pattern_transform (gr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoScale
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble x, jdouble y)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_scale (gr->cr, x, y);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoDrawGlyphVector
(JNIEnv *env, jobject obj __attribute__((unused)), jlong pointer,
 jobject font,
 jfloat x, jfloat y, jint n,
 jintArray java_codes,
 jfloatArray java_positions, jlongArray java_fontset)
{
  struct cairographics2d *gr = NULL;
  struct peerfont *pfont = NULL;
  cairo_glyph_t *glyphs = NULL;
  int *native_codes;
  float *native_positions;
  jint i = 0;

  g_assert (java_codes != NULL);
  g_assert (java_positions != NULL);

  gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  pfont = (struct peerfont *) gtkpeer_get_font(env, font);
  g_assert (pfont != NULL);

  glyphs = g_malloc( sizeof(cairo_glyph_t) * n);
  g_assert (glyphs != NULL);

  native_codes = (*env)->GetIntArrayElements (env, java_codes, NULL);
  native_positions = (*env)->GetFloatArrayElements (env, java_positions, NULL);
  
  /* Set up glyphs and layout */
  for (i = 0; i < n; ++i)
    {
      glyphs[i].index = native_codes[i];
      glyphs[i].x = x + native_positions[ 2*i ];
      glyphs[i].y = y + native_positions[ 2*i + 1];
    }

  (*env)->ReleaseFloatArrayElements (env, java_positions, native_positions, 0);
  (*env)->ReleaseIntArrayElements (env, java_codes, native_codes, 0);

  /* Iterate through glyphs and draw */
  jlong* fonts = (*env)->GetLongArrayElements (env, java_fontset, NULL);
  gdk_threads_enter();
  for (i = 0; i < n; i++)
    {
      PangoFcFont *font = JLONG_TO_PTR(PangoFcFont, fonts[i]);

      /* Draw as many glyphs as possible with the current font */
      int length = 0;
      while (i < n-1 && fonts[i] == fonts[i+1])
        {
          length++;
          i++;
        }
    
      FT_Face face = pango_fc_font_lock_face( font );
      cairo_font_face_t *ft = cairo_ft_font_face_create_for_ft_face (face, 0);
      g_assert (ft != NULL);

      cairo_set_font_face (gr->cr, ft);
      cairo_show_glyphs (gr->cr, &glyphs[i-length], length+1);
                       
      cairo_font_face_destroy (ft);
      pango_fc_font_unlock_face(font);
    }
  gdk_threads_leave();
    
  (*env)->ReleaseLongArrayElements (env, java_fontset, fonts, 0);
  g_free(glyphs);
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetFont
(JNIEnv *env __attribute__ ((unused)), jobject obj __attribute__ ((unused)),
 jlong pointer, jobject font)
{
  struct cairographics2d *gr = NULL;
  struct peerfont *pfont = NULL;
  FT_Face face = NULL;
  cairo_font_face_t *ft = NULL;

  gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);
  
  pfont = (struct peerfont *) gtkpeer_get_font(env, font);
  g_assert (pfont != NULL);

  gdk_threads_enter();
  face = pango_fc_font_lock_face( (PangoFcFont *)pfont->font );
  g_assert (face != NULL);

  ft = cairo_ft_font_face_create_for_ft_face (face, 0);
  g_assert (ft != NULL);

  cairo_set_font_face (gr->cr, ft);
  cairo_set_font_size (gr->cr,
                       (pango_font_description_get_size (pfont->desc) /
                       (double)PANGO_SCALE));
                       
  cairo_font_face_destroy (ft);
  pango_fc_font_unlock_face((PangoFcFont *)pfont->font);
  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetOperator 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jint op)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
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
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble r, jdouble g, jdouble b, jdouble a)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_set_source_rgba (gr->cr, r, g, b, a);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetFillRule 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jint rule)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
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
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble width, jint cap, jint join, jdouble miterLimit)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
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
(JNIEnv *env, jobject obj __attribute__((unused)),
 jlong pointer, jdoubleArray dashes, jint ndash, jdouble offset)
{
  jdouble *dasharr = NULL;
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  dasharr = (*env)->GetDoubleArrayElements (env, dashes, NULL);  
  g_assert (dasharr != NULL);

  cairo_set_dash (gr->cr, dasharr, ndash, offset);

  (*env)->ReleaseDoubleArrayElements (env, dashes, dasharr, 0);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSave
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_save (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoRestore
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_restore (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoNewPath 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_new_path (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoMoveTo 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble x, jdouble y)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_move_to (gr->cr, x, y);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoLineTo 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble x, jdouble y)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_line_to (gr->cr, x, y);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoCurveTo 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble x1, jdouble y1,
 jdouble x2, jdouble y2, jdouble x3, jdouble y3)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);
  cairo_curve_to (gr->cr, x1, y1, x2, y2, x3, y3);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoRectangle 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble x, jdouble y, jdouble width, jdouble height)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);

  cairo_rectangle (gr->cr, x, y, width, height);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoArc 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble x, jdouble y, jdouble radius, jdouble angle1,
 jdouble angle2)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);

  cairo_arc (gr->cr, x, y, radius, angle1, angle2);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoClosePath 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_close_path (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoStroke 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_stroke (gr->cr);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoFill 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer, jdouble alpha)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  if (alpha == 1.0)
    cairo_fill (gr->cr);
  else
    {
      cairo_save(gr->cr);
      cairo_clip(gr->cr);
      cairo_paint_with_alpha(gr->cr, alpha);
      cairo_restore(gr->cr);
    }
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoClip 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert( gr != NULL );

  cairo_clip( gr->cr );
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoResetClip 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong pointer)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  cairo_reset_clip( gr->cr );
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoGraphics2D_cairoSetAntialias
(JNIEnv *env __attribute__ ((unused)), jobject obj __attribute__ ((unused)),
 jlong pointer, jboolean aa)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, pointer);
  g_assert (gr != NULL);

  if (aa)
    cairo_set_antialias(gr->cr, CAIRO_ANTIALIAS_GRAY);
  else
    cairo_set_antialias(gr->cr, CAIRO_ANTIALIAS_NONE);
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

