/* gdkfontmetrics.c
   Copyright (C) 1999, 2003, 2004 Free Software Foundation, Inc.

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

#include "gnu_java_awt_peer_gtk_GdkFontMetrics.h"
#include <gdk/gdkx.h>

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontMetrics_getPeerFontMetrics
   (JNIEnv *env, jclass clazz __attribute__ ((unused)), jobject java_font, jdoubleArray java_metrics)
{
  struct peerfont *pfont = NULL;
  jdouble *native_metrics = NULL;
  PangoFontMetrics *pango_metrics;

  gdk_threads_enter();

  pfont = (struct peerfont *) NSA_GET_FONT_PTR (env, java_font);
  g_assert (pfont != NULL);

  pango_metrics = pango_context_get_metrics (pfont->ctx, pfont->desc,
                                             gtk_get_default_language ());

  native_metrics = (*env)->GetDoubleArrayElements (env, java_metrics, NULL);
  g_assert (native_metrics != NULL);

  native_metrics[FONT_METRICS_ASCENT] = PANGO_PIXELS (pango_font_metrics_get_ascent (pango_metrics));
  native_metrics[FONT_METRICS_MAX_ASCENT] = native_metrics[FONT_METRICS_ASCENT];
  native_metrics[FONT_METRICS_DESCENT] = PANGO_PIXELS (pango_font_metrics_get_descent (pango_metrics));
  if (native_metrics[FONT_METRICS_DESCENT] < 0)
    native_metrics[FONT_METRICS_DESCENT] = - native_metrics[FONT_METRICS_DESCENT];
  native_metrics[FONT_METRICS_MAX_DESCENT] = native_metrics[FONT_METRICS_DESCENT];
  native_metrics[FONT_METRICS_MAX_ADVANCE] = PANGO_PIXELS (pango_font_metrics_get_approximate_char_width (pango_metrics));
	 
  (*env)->ReleaseDoubleArrayElements (env, java_metrics, native_metrics, 0);

  pango_font_metrics_unref (pango_metrics);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkFontMetrics_getPeerTextMetrics
   (JNIEnv *env, jclass clazz __attribute__ ((unused)), jobject java_font, jstring str, jdoubleArray java_metrics)
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

  native_metrics[TEXT_METRICS_X_BEARING] = PANGO_PIXELS(log.x);
  native_metrics[TEXT_METRICS_Y_BEARING] = PANGO_PIXELS(log.y);
  native_metrics[TEXT_METRICS_WIDTH] = PANGO_PIXELS(log.width);
  native_metrics[TEXT_METRICS_HEIGHT] = PANGO_PIXELS(log.height);
  native_metrics[TEXT_METRICS_X_ADVANCE] = PANGO_PIXELS(log.x + log.width);
  native_metrics[TEXT_METRICS_Y_ADVANCE] = PANGO_PIXELS(log.y + log.height);
	 
  (*env)->ReleaseDoubleArrayElements (env, java_metrics, native_metrics, 0);

  gdk_threads_leave();
}

