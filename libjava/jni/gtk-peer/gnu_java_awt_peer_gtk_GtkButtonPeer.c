/* gtkbuttonpeer.c -- Native implementation of GtkButtonPeer
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
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"
#include "gnu_java_awt_peer_gtk_GtkButtonPeer.h"

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkButtonPeer_create
  (JNIEnv *env, jobject obj, jstring label)
{
  const char *c_label;
  GtkWidget *button;

  NSA_SET_GLOBAL_REF (env, obj);

  c_label = (*env)->GetStringUTFChars (env, label, NULL);

  gdk_threads_enter ();

  button = gtk_button_new_with_label (c_label);
  gtk_widget_show (button);

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, label, c_label);
  NSA_SET_PTR (env, obj, button);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkButtonPeer_connectJObject
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_widget_realize (GTK_WIDGET (ptr));

  connect_awt_hook (env, obj, 1, GTK_BUTTON(ptr)->event_window);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkButtonPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  /* FIXME: Do we need to connect any signals here? Otherwise just do not
     override parent method. */

  /* Connect the superclass signals.  */
  Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals (env, obj);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkButtonPeer_gtkSetLabel
  (JNIEnv *env, jobject obj, jstring jtext)
{
  const char *text;
  GtkWidget *label;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  text = (*env)->GetStringUTFChars (env, jtext, NULL);

  gdk_threads_enter ();

  label = gtk_bin_get_child (GTK_BIN (ptr));
  gtk_label_set_text (GTK_LABEL (label), text);

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, jtext, text);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkButtonPeer_gtkSetFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  GtkWidget *label;
  PangoFontDescription *font_desc;

  ptr = NSA_GET_PTR (env, obj);

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  gdk_threads_enter();

  label = gtk_bin_get_child (GTK_BIN (ptr));

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc, size * dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET(label), font_desc);

  pango_font_description_free (font_desc);

  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, name, font_name);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkButtonPeer_gtkWidgetSetForeground
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  GtkWidget *label;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  color.red = (red / 255.0) * 65535;
  color.green = (green / 255.0) * 65535;
  color.blue = (blue / 255.0) * 65535;

  gdk_threads_enter ();

  label = gtk_bin_get_child (GTK_BIN(ptr));

  gtk_widget_modify_fg (label, GTK_STATE_NORMAL, &color);
  gtk_widget_modify_fg (label, GTK_STATE_ACTIVE, &color);
  gtk_widget_modify_fg (label, GTK_STATE_PRELIGHT, &color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkButtonPeer_gtkActivate
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_widget_activate (GTK_WIDGET (ptr));

  gdk_threads_leave ();
}
