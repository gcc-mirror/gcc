/* gdkfontmetrics.c
   Copyright (C) 1999, 2003 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GdkFontMetrics.h"
#include <gdk/gdkx.h>

#define ASCENT      0
#define MAX_ASCENT  1
#define DESCENT     2
#define MAX_DESCENT 3
#define MAX_ADVANCE 4
#define NUM_METRICS 5

JNIEXPORT jintArray JNICALL Java_gnu_java_awt_peer_gtk_GdkFontMetrics_initState
  (JNIEnv *env, jobject obj __attribute__((unused)),
   jstring fname, jint style, jint size)
{
  jintArray array;
  jint *metrics;
  const char *font_name;
  PangoFontDescription *font_desc;
  PangoContext *context;
  PangoFontMetrics *pango_metrics;

  array = (*env)->NewIntArray (env, NUM_METRICS);

  metrics = (*env)->GetIntArrayElements (env, array, NULL);
  font_name = (*env)->GetStringUTFChars (env, fname, NULL);

  gdk_threads_enter ();

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc, size * dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  context = gdk_pango_context_get();
  pango_context_set_font_description (context, font_desc);

  pango_metrics = pango_context_get_metrics (context, font_desc,
                                             gtk_get_default_language ());

  metrics[ASCENT] =
    PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics));
  metrics[MAX_ASCENT]  = metrics[ASCENT];
  metrics[DESCENT] =
    PANGO_PIXELS (pango_font_metrics_get_descent (pango_metrics));
  metrics[MAX_DESCENT] = metrics[DESCENT];
  metrics[MAX_ADVANCE] =
    PANGO_PIXELS (pango_font_metrics_get_approximate_char_width (pango_metrics));

  pango_font_metrics_unref (pango_metrics);

  pango_font_description_free (font_desc);

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, fname, font_name);
  (*env)->ReleaseIntArrayElements (env, array, metrics, 0);

  return array;
}

JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_gtk_GdkFontMetrics_stringWidth
  (JNIEnv *env, jobject obj __attribute__((unused)),
   jstring fname, jint style, jint size, jstring str)
{
  PangoFontDescription *font_desc;
  PangoContext *context;
  PangoLayout *layout;
  int width = 0;
  const char *cstr;
  const char *font_name;

  cstr = (*env)->GetStringUTFChars (env, str, NULL);
  font_name = (*env)->GetStringUTFChars (env, fname, NULL);

  gdk_threads_enter ();

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc, size * dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  context = gdk_pango_context_get();
  pango_context_set_font_description (context, font_desc);

  layout = pango_layout_new (context);

  pango_layout_set_text (layout, cstr, -1);

  pango_layout_get_pixel_size (layout, &width, NULL);

  pango_font_description_free (font_desc);

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, fname, font_name);
  (*env)->ReleaseStringUTFChars (env, str, cstr);

  return width;
}
