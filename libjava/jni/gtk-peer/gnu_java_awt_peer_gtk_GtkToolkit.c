/* gtktoolkit.c -- Native portion of GtkToolkit
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkToolkit.h"

static jint gdk_color_to_java_color (GdkColor color);

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_beep
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  gdk_threads_enter ();
  gdk_beep ();
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_sync
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  gdk_threads_enter ();
  gdk_flush ();
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_getScreenSizeDimensions
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
   jintArray jdims)
{
  jint *dims = (*env)->GetIntArrayElements (env, jdims, 0);  

  gdk_threads_enter ();

  dims[0] = gdk_screen_width ();
  dims[1] = gdk_screen_height ();

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements(env, jdims, dims, 0);
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_getScreenResolution
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  jint res;

  gdk_threads_enter ();

  res = gdk_screen_width () / (gdk_screen_width_mm () / 25.4);

  gdk_threads_leave ();
  return res;
}

#define CONVERT(type, state) \
  gdk_color_to_java_color (style->type[GTK_STATE_ ## state])

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkToolkit_loadSystemColors
  (JNIEnv *env, jobject obj __attribute__((unused)),
   jintArray jcolors)
{
  jint *colors;
  GtkStyle *style;

  colors = (*env)->GetIntArrayElements (env, jcolors, 0);

  gdk_threads_enter ();

  style = gtk_widget_get_default_style ();

  colors[AWT_DESKTOP]                 = CONVERT (bg, SELECTED);
  colors[AWT_ACTIVE_CAPTION]          = CONVERT (bg, SELECTED);
  colors[AWT_ACTIVE_CAPTION_TEXT]     = CONVERT (text, SELECTED);
  colors[AWT_ACTIVE_CAPTION_BORDER]   = CONVERT (fg, NORMAL);
  colors[AWT_INACTIVE_CAPTION]        = CONVERT (base, INSENSITIVE);
  colors[AWT_INACTIVE_CAPTION_TEXT]   = CONVERT (fg, INSENSITIVE);
  colors[AWT_INACTIVE_CAPTION_BORDER] = CONVERT (fg, INSENSITIVE);
  colors[AWT_WINDOW]                  = CONVERT (bg, NORMAL);
  colors[AWT_WINDOW_BORDER]           = CONVERT (fg, NORMAL);
  colors[AWT_WINDOW_TEXT]             = CONVERT (fg, NORMAL);
  colors[AWT_MENU]                    = CONVERT (bg, NORMAL);
  colors[AWT_MENU_TEXT]               = CONVERT (fg, NORMAL);
  colors[AWT_TEXT]                    = CONVERT (bg, NORMAL);
  colors[AWT_TEXT_TEXT]               = CONVERT (fg, NORMAL);
  colors[AWT_TEXT_HIGHLIGHT]          = CONVERT (bg, SELECTED);
  colors[AWT_TEXT_HIGHLIGHT_TEXT]     = CONVERT (fg, SELECTED);
  colors[AWT_TEXT_INACTIVE_TEXT]      = CONVERT (bg, INSENSITIVE);
  colors[AWT_CONTROL]                 = CONVERT (bg, NORMAL);
  colors[AWT_CONTROL_TEXT]            = CONVERT (fg, NORMAL);
  colors[AWT_CONTROL_HIGHLIGHT]       = CONVERT (base, ACTIVE);
  colors[AWT_CONTROL_LT_HIGHLIGHT]    = CONVERT (bg, PRELIGHT);
  colors[AWT_CONTROL_SHADOW]          = CONVERT (bg, ACTIVE);
  colors[AWT_CONTROL_DK_SHADOW]       = CONVERT (fg, INSENSITIVE);
  colors[AWT_SCROLLBAR]               = CONVERT (base, INSENSITIVE);
  colors[AWT_INFO]                    = CONVERT (bg, NORMAL);
  colors[AWT_INFO_TEXT]               = CONVERT (fg, NORMAL);

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements(env, jcolors, colors, 0);
}

#undef CONVERT

static jint
gdk_color_to_java_color (GdkColor gdk_color)
{
  guchar red;
  guchar green;
  guchar blue;
  float factor;

  factor = 255.0 / 65535.0;

  red   = (float) gdk_color.red   * factor;
  green = (float) gdk_color.green * factor;
  blue  = (float) gdk_color.blue  * factor;

  return (jint) (0xff000000 | (red << 16) | (green << 8) | blue);
}
