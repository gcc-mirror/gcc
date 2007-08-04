/* gnu_java_awt_GdkFont.c
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   
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

#define PANGO_ENABLE_ENGINE
#include <pango/pango.h>
#include <pango/pangoft2.h>
#include <pango/pangofc-font.h>
#include <freetype/ftglyph.h>
#include <freetype/ftoutln.h>
#include <freetype/fttypes.h>
#include <freetype/tttables.h>
#include "gdkfont.h"
#include "gtkpeer.h"
#include "gnu_java_awt_peer_gtk_GdkFontPeer.h"

enum java_awt_font_style {
  java_awt_font_PLAIN = 0,
  java_awt_font_BOLD = 1,
  java_awt_font_ITALIC = 2
};

enum java_awt_font_baseline {
  java_awt_font_ROMAN_BASELINE = 0,
  java_awt_font_CENTER_BASELINE = 1,
  java_awt_font_HANGING_BASELINE = 2
};

static PangoFT2FontMap *ft2_map = NULL;

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_initStaticState 
  (JNIEnv *env, jclass clazz __attribute__((unused)))
{
  gtkpeer_init_font_IDs(env);
  ft2_map = PANGO_FT2_FONT_MAP(pango_ft2_font_map_new());
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_initState
  (JNIEnv *env, jobject self)
{
  struct peerfont *pfont = NULL;

  gdk_threads_enter ();

  g_assert (self != NULL);
  pfont = (struct peerfont *) g_malloc0 (sizeof (struct peerfont));
  g_assert (pfont != NULL);
  gtkpeer_set_font (env, self, pfont);

  gdk_threads_leave ();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_dispose
  (JNIEnv *env, jobject self)
{
  struct peerfont *pfont = NULL;

  gdk_threads_enter ();

  pfont = (struct peerfont *) gtkpeer_get_font (env, self);
  g_assert (pfont != NULL);
  if (pfont->layout != NULL)
    g_object_unref (pfont->layout);
  if (pfont->font != NULL)
    g_object_unref (pfont->font);
  if (pfont->set != NULL)
    g_object_unref (pfont->set);
  if (pfont->ctx != NULL)
    g_object_unref (pfont->ctx);
  if (pfont->desc != NULL)
    pango_font_description_free (pfont->desc);
  g_free (pfont);

  gdk_threads_leave ();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_releasePeerGraphicsResource
   (JNIEnv *env, jobject java_font)
{
  struct peerfont *pfont = NULL;

  gdk_threads_enter();

  pfont = (struct peerfont *) gtkpeer_get_font (env, java_font);
  g_assert (pfont != NULL);
  if (pfont->graphics_resource != NULL)
    {
      cairo_font_face_destroy ((cairo_font_face_t *) pfont->graphics_resource);
      pfont->graphics_resource = NULL;
    }

  gdk_threads_leave();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_getFontMetrics
(JNIEnv *env, jobject java_font, jdoubleArray java_metrics)
{
  FT_Face face;
  struct peerfont *pfont = NULL;
  jdouble *native_metrics = NULL;
  short x_ppem;
  short y_ppem;
  short units_per_em;
  double factorx;
  double factory;

  gdk_threads_enter();

  pfont = (struct peerfont *) gtkpeer_get_font (env, java_font);
  g_assert (pfont != NULL);
  face = pango_fc_font_lock_face ((PangoFcFont *)pfont->font);

  native_metrics 
    = (*env)->GetDoubleArrayElements (env, java_metrics, NULL);

  g_assert (native_metrics != NULL);

  x_ppem = face->size->metrics.x_ppem;
  y_ppem = face->size->metrics.y_ppem;
  units_per_em = face->units_per_EM;
  factorx = units_per_em / x_ppem;
  factory = units_per_em / y_ppem;
  native_metrics[FONT_METRICS_ASCENT] = face->ascender / factory;
  native_metrics[FONT_METRICS_MAX_ASCENT] = face->bbox.yMax / factory;
  native_metrics[FONT_METRICS_DESCENT] = - face->descender / factory;
  native_metrics[FONT_METRICS_MAX_DESCENT] = - face->bbox.yMin / factory;
  native_metrics[FONT_METRICS_MAX_ADVANCE] = face->max_advance_width / factorx;
  native_metrics[FONT_METRICS_HEIGHT] = face->height / factory;
  native_metrics[FONT_METRICS_UNDERLINE_OFFSET] =
    face->underline_position / factory;
  native_metrics[FONT_METRICS_UNDERLINE_THICKNESS] =
    face->underline_thickness / factory;
    
  pango_fc_font_unlock_face((PangoFcFont *)pfont->font);

  (*env)->ReleaseDoubleArrayElements (env, 
				      java_metrics, 
				      native_metrics, 0);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_getTextMetrics
   (JNIEnv *env, jobject java_font, jstring str, jdoubleArray java_metrics)
{
  struct peerfont *pfont = NULL;
  const char *cstr = NULL;
  jdouble *native_metrics = NULL;  
  PangoRectangle log;
  PangoRectangle log2;
  int line_count = 0;
  int i = 0;
  int width = 0;

  gdk_threads_enter();

  pfont = (struct peerfont *) gtkpeer_get_font(env, java_font);
  g_assert (pfont != NULL);

  cstr = (*env)->GetStringUTFChars (env, str, NULL);
  g_assert(cstr != NULL);
  
  pango_layout_set_text (pfont->layout, cstr, -1);
  pango_layout_get_extents (pfont->layout, NULL, &log);
  
  line_count = pango_layout_get_line_count (pfont->layout);
  for (i = 0; i < line_count; i++)
   {
     pango_layout_line_get_extents (pango_layout_get_line (pfont->layout, i), 
       NULL, &log2);
     width += log2.width;
   }

  (*env)->ReleaseStringUTFChars (env, str, cstr);  
  pango_layout_set_text (pfont->layout, "", -1);

  native_metrics = (*env)->GetDoubleArrayElements (env, java_metrics, NULL);
  g_assert (native_metrics != NULL);

  native_metrics[TEXT_METRICS_X_BEARING] 
    = PANGO_PIXELS( ((double)log.x) );

  native_metrics[TEXT_METRICS_Y_BEARING] 
    = PANGO_PIXELS( ((double)log.y) );

  native_metrics[TEXT_METRICS_HEIGHT] 
    = PANGO_PIXELS( ((double)log.height) );

  native_metrics[TEXT_METRICS_WIDTH]
    = PANGO_PIXELS( ((double)width) );

  native_metrics[TEXT_METRICS_X_ADVANCE] 
    = PANGO_PIXELS( ((double) (log.x + log.width)) );

  native_metrics[TEXT_METRICS_Y_ADVANCE] 
    = PANGO_PIXELS( ((double) (log.y + log.height)) );
	 
  (*env)->ReleaseDoubleArrayElements (env, java_metrics, native_metrics, 0);

  gdk_threads_leave();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_setFont
  (JNIEnv *env, jobject self, jstring family_name_str, jint style_int, jint size)
{
  struct peerfont *pfont = NULL;
  char const *family_name = NULL;
  enum java_awt_font_style style;

  gdk_threads_enter ();

  style = (enum java_awt_font_style) style_int;

  g_assert (self != NULL);
  pfont = (struct peerfont *) gtkpeer_get_font(env, self);
  g_assert (pfont != NULL);

  /* Clear old font information */
  if (pfont->ctx != NULL)
    g_object_unref (pfont->ctx);
  if (pfont->font != NULL)
    g_object_unref (pfont->font);
  if (pfont->set != NULL)
    g_object_unref (pfont->set);
  if (pfont->desc != NULL)
    pango_font_description_free (pfont->desc);

  /* Set new description information */
  pfont->desc = pango_font_description_new ();
  g_assert (pfont->desc != NULL);

  family_name = (*env)->GetStringUTFChars(env, family_name_str, 0);
  g_assert (family_name != NULL);
  pango_font_description_set_family (pfont->desc, family_name);
  (*env)->ReleaseStringUTFChars(env, family_name_str, family_name);

  if (style & java_awt_font_BOLD)
    pango_font_description_set_weight (pfont->desc, PANGO_WEIGHT_BOLD);

  if (style & java_awt_font_ITALIC)
    pango_font_description_set_style (pfont->desc, PANGO_STYLE_ITALIC);

  pango_font_description_set_size (pfont->desc, size * PANGO_SCALE);
  
  /* Create new context */
  pfont->ctx = pango_ft2_font_map_create_context (ft2_map);
  g_assert (pfont->ctx != NULL);
  
  pango_context_set_font_description (pfont->ctx, pfont->desc);
  pango_context_set_language (pfont->ctx, gtk_get_default_language());
  
  /* Create new fontset and default font */
  pfont->set = pango_context_load_fontset(pfont->ctx, pfont->desc,
  										  gtk_get_default_language());
  pfont->font = pango_context_load_font (pfont->ctx, pfont->desc);
  g_assert (pfont->font != NULL);

  if (pfont->layout == NULL)
    pfont->layout = pango_layout_new (pfont->ctx);
  g_assert (pfont->layout != NULL);

  gdk_threads_leave ();
}


JNIEXPORT jbyteArray JNICALL 
Java_gnu_java_awt_peer_gtk_GdkFontPeer_getTrueTypeTable 
  (JNIEnv *env, jobject self, jbyte n, jbyte a, jbyte m, jbyte e)
{
  struct peerfont *pfont = NULL;
  FT_Face face;
  FT_ULong length = 0;
  FT_ULong tag;
  int error;
  FT_Byte *buffer;
  jbyteArray result_array;
  jbyte *rbuf;

  pfont = (struct peerfont *) gtkpeer_get_font(env, self);
  if(pfont == NULL)
    return NULL;

  gdk_threads_enter ();
  face = pango_fc_font_lock_face ((PangoFcFont *)pfont->font);
  tag = FT_MAKE_TAG( n, a, m, e );

  /* Get the length of the table requested */
  error = FT_Load_Sfnt_Table( face, tag, 0, NULL, &length );
  if ( error ) 
    {
      pango_fc_font_unlock_face ((PangoFcFont *)pfont->font);
      gdk_threads_leave ();
      return NULL;
    }

  buffer = (FT_Byte *)g_malloc0( length );
  if ( buffer == NULL ) 
    {
      pango_fc_font_unlock_face ((PangoFcFont *)pfont->font);
      gdk_threads_leave ();
      return NULL;
    }
  /* get the table data */
  error = FT_Load_Sfnt_Table( face, tag, 0, buffer, &length );
  if ( error ) 
    {
      pango_fc_font_unlock_face ((PangoFcFont *)pfont->font);
      g_free(buffer);
      gdk_threads_leave ();
      return NULL;
    }

  /* copy to a jbytearray */
  result_array = (*env)->NewByteArray (env, length);

  rbuf = (*env)->GetByteArrayElements (env, result_array, NULL);
  memcpy(rbuf, buffer, length);
  (*env)->ReleaseByteArrayElements (env, result_array, rbuf, 0);

  g_free(buffer);
  pango_fc_font_unlock_face ((PangoFcFont *)pfont->font);
  gdk_threads_leave ();

  /* done */
  return result_array;
}
