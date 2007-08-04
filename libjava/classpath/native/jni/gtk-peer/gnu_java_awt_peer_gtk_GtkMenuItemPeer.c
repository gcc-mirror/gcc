/* gtkmenuitempeer.c -- Native implementation of GtkMenuItemPeer
   Copyright (C) 1999 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkMenuItemPeer.h"
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"

static jmethodID postMenuActionEventID;

void
cp_gtk_menuitem_init_jni (void)
{
  jclass gtkmenuitempeer;

  gtkmenuitempeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                      "gnu/java/awt/peer/gtk/GtkMenuItemPeer");

  postMenuActionEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                                     gtkmenuitempeer,
                                                     "postMenuActionEvent",
                                                     "()V");
}

static void item_activate_cb (GtkMenuItem *item __attribute__((unused)),
                              jobject peer_obj);

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkMenuItemPeer_create
  (JNIEnv *env, jobject obj, jstring label)
{
  GtkWidget *widget;
  const char *str;

  gdk_threads_enter ();

  gtkpeer_set_global_ref (env, obj);

  str = (*env)->GetStringUTFChars (env, label, NULL);

  /* "-" signals that we need a separator. */
  if (strcmp (str, "-") == 0)
    widget = gtk_menu_item_new ();
  else
    widget = gtk_menu_item_new_with_label (str);

  gtk_widget_show (widget);

  (*env)->ReleaseStringUTFChars (env, label, str);

  gtkpeer_set_widget (env, obj, widget);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkMenuItemPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jobject gref;
  
  gdk_threads_enter ();
  
  ptr = gtkpeer_get_widget (env, obj);
  gref = gtkpeer_get_global_ref (env, obj);

  g_signal_connect (G_OBJECT (ptr), "activate",
                    G_CALLBACK (item_activate_cb), gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkMenuItemPeer_gtkWidgetModifyFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  GtkWidget *label;
  PangoFontDescription *font_desc;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, obj);

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  label = gtk_bin_get_child (GTK_BIN (ptr));

  if (label)
    {
      font_desc = pango_font_description_from_string (font_name);
      pango_font_description_set_size (font_desc,
                                   size * cp_gtk_dpi_conversion_factor);

      if (style & AWT_STYLE_BOLD)
        pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

      if (style & AWT_STYLE_ITALIC)
        pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

      gtk_widget_modify_font (GTK_WIDGET(label), font_desc);

      pango_font_description_free (font_desc);
    }

  (*env)->ReleaseStringUTFChars (env, name, font_name);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkMenuItemPeer_setEnabled
  (JNIEnv *env, jobject obj, jboolean enabled)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  gtk_widget_set_sensitive (GTK_WIDGET (ptr), enabled);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkMenuItemPeer_setLabel
  (JNIEnv *env, jobject obj, jstring label)
{
  void *ptr;
  const char *str;
  GtkAccelLabel *accel_label;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  str = (*env)->GetStringUTFChars (env, label, NULL);

  accel_label = GTK_ACCEL_LABEL (GTK_BIN (ptr)->child);

  gtk_label_set_text (GTK_LABEL (accel_label), str);
  gtk_accel_label_refetch (accel_label);

  (*env)->ReleaseStringUTFChars (env, label, str);

  gdk_threads_leave ();
}

static void
item_activate_cb (GtkMenuItem *item __attribute__((unused)), jobject peer_obj)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer_obj,
                                       postMenuActionEventID);
}
