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

#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_GdkFontPeer.h"

struct state_table *cp_gtk_native_font_state_table;

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

static jmethodID glyphVector_ctor;
static jclass glyphVector_class;
static PangoAttrList *attrs = NULL;

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_initStaticState 
  (JNIEnv *env, jclass clazz)
{
  NSA_FONT_INIT (env, clazz);

  glyphVector_class = (*env)->FindClass 
    (env, "gnu/java/awt/peer/gtk/GdkGlyphVector");

  glyphVector_ctor = (*env)->GetMethodID 
    (env, glyphVector_class, "<init>", 
     "([D[ILjava/awt/Font;Ljava/awt/font/FontRenderContext;)V");
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
  NSA_SET_FONT_PTR (env, self, pfont);

  gdk_threads_leave ();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_dispose
  (JNIEnv *env, jobject self)
{
  struct peerfont *pfont = NULL;

  gdk_threads_enter ();

  pfont = (struct peerfont *)NSA_DEL_FONT_PTR (env, self);
  g_assert (pfont != NULL);
  if (pfont->layout != NULL)
    g_object_unref (pfont->font);
  if (pfont->font != NULL)
    g_object_unref (pfont->font);
  if (pfont->ctx != NULL)
    g_object_unref (pfont->ctx);
  if (pfont->desc != NULL)
    pango_font_description_free (pfont->desc);
  g_free (pfont);

  gdk_threads_leave ();
}


JNIEXPORT jobject JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_getGlyphVector
  (JNIEnv *env, jobject self, 
   jstring chars,
   jobject font, 
   jobject fontRenderContext)
{
  struct peerfont *pfont = NULL;
  GList *items = NULL;
  GList *i = NULL;
  gchar *str = NULL;
  int len = 0;
  int j = 0;
  double *native_extents = NULL;
  int *native_codes = NULL;
  jintArray java_codes = NULL;
  jdoubleArray java_extents = NULL;

  gdk_threads_enter ();

  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, self);
  g_assert (pfont != NULL);

  len = (*cp_gtk_gdk_env())->GetStringUTFLength (env, chars);  
  str = (gchar *)(*env)->GetStringUTFChars (env, chars, NULL);
  g_assert (str != NULL);

  if (attrs == NULL)
    attrs = pango_attr_list_new ();

  if (len > 0 && str[len-1] == '\0')
    len--;
  
  items = pango_itemize (pfont->ctx, str, 0, len, attrs, NULL);

  i = g_list_first (items);

  if (i == NULL)       
    {
      java_extents = (*env)->NewDoubleArray (env, 0);
      java_codes = (*env)->NewIntArray (env, 0);
    }
  else
    { 
      PangoGlyphString *glyphs;
      PangoItem *item = (PangoItem *)i->data;

      pango_context_set_font_description (pfont->ctx, pfont->desc);
      pango_context_set_language (pfont->ctx, gtk_get_default_language());
      pango_context_load_font (pfont->ctx, pfont->desc);

      glyphs = pango_glyph_string_new ();
      g_assert (glyphs != NULL);

      pango_shape (str + item->offset, item->length, 
		   &(item->analysis), glyphs);

      if (glyphs->num_glyphs > 0)
	{
	  int x = 0;
	  double scale = ((double) PANGO_SCALE);

	  java_extents = (*env)->NewDoubleArray (env, glyphs->num_glyphs * NUM_GLYPH_METRICS);
	  java_codes = (*env)->NewIntArray (env, glyphs->num_glyphs);

	  native_extents = (*env)->GetDoubleArrayElements (env, java_extents, NULL);
	  native_codes = (*env)->GetIntArrayElements (env, java_codes, NULL);

	  for (j = 0; j < glyphs->num_glyphs; ++j)
	    {
	      PangoRectangle ink;
	      PangoRectangle logical;
	      PangoGlyphGeometry *geom = &glyphs->glyphs[j].geometry;

	      pango_font_get_glyph_extents (pfont->font, 
					    glyphs->glyphs[j].glyph,
					    &ink, &logical);

	      native_codes[j] = glyphs->glyphs[j].glyph;

	      native_extents[ GLYPH_LOG_X(j)      ] = (logical.x)      / scale;
	      native_extents[ GLYPH_LOG_Y(j)      ] = (- logical.y)    / scale;
	      native_extents[ GLYPH_LOG_WIDTH(j)  ] = (logical.width)  / scale;
	      native_extents[ GLYPH_LOG_HEIGHT(j) ] = (logical.height) / scale;

	      native_extents[ GLYPH_INK_X(j)      ] = (ink.x)       / scale;
	      native_extents[ GLYPH_INK_Y(j)      ] = (- ink.y)     / scale;
	      native_extents[ GLYPH_INK_WIDTH(j)  ] = (ink.width)   / scale;
	      native_extents[ GLYPH_INK_HEIGHT(j) ] = (ink.height)  / scale;

	      native_extents[ GLYPH_POS_X(j)      ] = (x + geom->x_offset)  / scale;
	      native_extents[ GLYPH_POS_Y(j)      ] = (  - geom->y_offset)  / scale;

	      x += geom->width;
	    }
	  (*env)->ReleaseDoubleArrayElements (env, java_extents, native_extents, 0);
	  (*env)->ReleaseIntArrayElements (env, java_codes, native_codes, 0);
	}

      pango_glyph_string_free (glyphs);
    }

  (*env)->ReleaseStringUTFChars (env, chars, str);
  
  for (i = g_list_first (items); i != NULL; i = g_list_next (i))
    g_free (i->data);
  
  g_list_free (items);

  gdk_threads_leave ();

  return (*env)->NewObject (env, 
			    glyphVector_class, 
			    glyphVector_ctor, 
			    java_extents, java_codes,
			    font, fontRenderContext);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_getFontMetrics
   (JNIEnv *env, jobject java_font, jdoubleArray java_metrics)
{
  struct peerfont *pfont = NULL;
  jdouble *native_metrics = NULL;
  PangoFontMetrics *pango_metrics = NULL;

  gdk_threads_enter();

  pfont = (struct peerfont *) NSA_GET_FONT_PTR (env, java_font);
  g_assert (pfont != NULL);

  pango_metrics 
    = pango_context_get_metrics (pfont->ctx, pfont->desc,
				 gtk_get_default_language ());

  native_metrics 
    = (*env)->GetDoubleArrayElements (env, java_metrics, NULL);

  g_assert (native_metrics != NULL);

  native_metrics[FONT_METRICS_ASCENT] 
    = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics));

  native_metrics[FONT_METRICS_MAX_ASCENT] 
    = native_metrics[FONT_METRICS_ASCENT];

  native_metrics[FONT_METRICS_DESCENT] 
    = PANGO_PIXELS (pango_font_metrics_get_descent (pango_metrics));

  if (native_metrics[FONT_METRICS_DESCENT] < 0)
    native_metrics[FONT_METRICS_DESCENT] 
      = - native_metrics[FONT_METRICS_DESCENT];

  native_metrics[FONT_METRICS_MAX_DESCENT] 
    = native_metrics[FONT_METRICS_DESCENT];

  native_metrics[FONT_METRICS_MAX_ADVANCE] 
    = PANGO_PIXELS (pango_font_metrics_get_approximate_char_width 
		    (pango_metrics));
	 
  (*env)->ReleaseDoubleArrayElements (env, 
				      java_metrics, 
				      native_metrics, 0);

  pango_font_metrics_unref (pango_metrics);

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

  gdk_threads_enter();

  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, java_font);
  g_assert (pfont != NULL);

  cstr = (*env)->GetStringUTFChars (env, str, NULL);
  g_assert(cstr != NULL);

  pango_layout_set_text (pfont->layout, cstr, -1);
  pango_layout_get_extents (pfont->layout, NULL, &log);

  (*env)->ReleaseStringUTFChars (env, str, cstr);  
  pango_layout_set_text (pfont->layout, "", -1);

  native_metrics = (*env)->GetDoubleArrayElements (env, java_metrics, NULL);
  g_assert (native_metrics != NULL);

  native_metrics[TEXT_METRICS_X_BEARING] 
    = PANGO_PIXELS( ((double)log.x) );

  native_metrics[TEXT_METRICS_Y_BEARING] 
    = PANGO_PIXELS( ((double)log.y) );

  native_metrics[TEXT_METRICS_WIDTH] 
    = PANGO_PIXELS( ((double)log.width) );

  native_metrics[TEXT_METRICS_HEIGHT] 
    = PANGO_PIXELS( ((double)log.height) );

  native_metrics[TEXT_METRICS_X_ADVANCE] 
    = PANGO_PIXELS( ((double) (log.x + log.width)) );

  native_metrics[TEXT_METRICS_Y_ADVANCE] 
    = PANGO_PIXELS( ((double) (log.y + log.height)) );
	 
  (*env)->ReleaseDoubleArrayElements (env, java_metrics, native_metrics, 0);

  gdk_threads_leave();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_setFont
  (JNIEnv *env, jobject self, jstring family_name_str, jint style_int, jint size, jboolean useGraphics2D)
{
  struct peerfont *pfont = NULL;
  char const *family_name = NULL;
  enum java_awt_font_style style;
  PangoFT2FontMap *ft2_map = NULL;

  gdk_threads_enter ();

  style = (enum java_awt_font_style) style_int;

  g_assert (self != NULL);
  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, self);
  g_assert (pfont != NULL);

  if (pfont->ctx != NULL)
    g_object_unref (pfont->ctx);
  if (pfont->font != NULL)
    g_object_unref (pfont->font);
  if (pfont->desc != NULL)
    pango_font_description_free (pfont->desc);

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

  if (useGraphics2D)
    {
      pango_font_description_set_size (pfont->desc, size * PANGO_SCALE);
      if (pfont->ctx == NULL)
	{
	  ft2_map = PANGO_FT2_FONT_MAP(pango_ft2_font_map_for_display ());
	  pfont->ctx = pango_ft2_font_map_create_context (ft2_map);
	}
    }
  else
    {
      /* GDK uses a slightly different DPI setting. */
      pango_font_description_set_size (pfont->desc,
				   size * cp_gtk_dpi_conversion_factor);
      if (pfont->ctx == NULL)
	pfont->ctx = gdk_pango_context_get();
    }

  g_assert (pfont->ctx != NULL);
  
  if (pfont->font != NULL)
    {
      g_object_unref (pfont->font);
      pfont->font = NULL;
    }
  
  pango_context_set_font_description (pfont->ctx, pfont->desc);
  pango_context_set_language (pfont->ctx, gtk_get_default_language());
  pfont->font = pango_context_load_font (pfont->ctx, pfont->desc);
  g_assert (pfont->font != NULL);

  if (pfont->layout == NULL)
    pfont->layout = pango_layout_new (pfont->ctx);
  g_assert (pfont->layout != NULL);

  gdk_threads_leave ();
}


