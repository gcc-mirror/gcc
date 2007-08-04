/* gtkgenericpeer.c -- Native implementation of GtkGenericPeer
   Copyright (C) 1998, 1999, 2002, 2004 Free Software Foundation, Inc.

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


#include "gtkpeer.h"
#include "gnu_java_awt_peer_gtk_GtkGenericPeer.h"

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkGenericPeer_initIDs
(JNIEnv *env, jclass clz __attribute__((unused)))
{
  gtkpeer_init_widget_IDs(env);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkGenericPeer_dispose
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  /* For now the native state for any object must be a widget.
     However, a subclass could override dispose() if required.  */
  gtk_widget_destroy (GTK_WIDGET (ptr));

  /* Delete global reference. */
  gtkpeer_del_global_ref(env, obj);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkGenericPeer_gtkWidgetModifyFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  PangoFontDescription *font_desc;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, obj);

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc,
                                   size * cp_gtk_dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET(ptr), font_desc);

  pango_font_description_free (font_desc);

  (*env)->ReleaseStringUTFChars (env, name, font_name);

  gdk_threads_leave();
}
