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

#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_GdkFontPeer.h"

struct state_table *native_font_state_table;

/*
rough sketch of the mapping between java and 
pango text objects:
  
  Font              <->    - PangoFont
                           - PangoFontDescription
                           - PangoContext
                           - PangoLayout (for rendering and measuring)

  GlyphVector       <->    - GList of PangoGlyphItem
                           - PangoFontDescription
                           - PangoContext

  FontRenderContext <->    stays in plain java

*/

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

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_initStaticState 
  (JNIEnv *env, jclass clazz)
{
  NSA_FONT_INIT (env, clazz);
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

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontPeer_setFont
  (JNIEnv *env, jobject self, jstring family_name_str, jint style_int, jint size, jboolean useGraphics2D)
{
  struct peerfont *pfont = NULL;
  char const *family_name = NULL;
  enum java_awt_font_style style;
  PangoFT2FontMap *ft2_map;

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

  pango_font_description_set_size (pfont->desc, size * dpi_conversion_factor);

  if (style & java_awt_font_BOLD)
    pango_font_description_set_weight (pfont->desc, PANGO_WEIGHT_BOLD);

  if (style & java_awt_font_ITALIC)
    pango_font_description_set_style (pfont->desc, PANGO_STYLE_ITALIC);

  if (useGraphics2D)
    {
      if (pfont->ctx == NULL)
	{
	  ft2_map = PANGO_FT2_FONT_MAP(pango_ft2_font_map_for_display ());
	  pfont->ctx = pango_ft2_font_map_create_context (ft2_map);
	}
    }
  else
    {
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


