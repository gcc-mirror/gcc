/* gtktextfieldpeer.c -- Native implementation of GtkTextFieldPeer
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkTextFieldPeer.h"

static jint
get_border_width (GtkWidget *entry);

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_create
  (JNIEnv *env, jobject obj, jint text_width)
{
  GtkWidget *entry;

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  gdk_threads_enter ();

  entry = gtk_entry_new ();
  gtk_widget_set_size_request (entry,
			       text_width + 2 * get_border_width (entry), -1);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, entry);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkWidgetSetBackground
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  color.red = (red / 255.0) * 65535;
  color.green = (green / 255.0) * 65535;
  color.blue = (blue / 255.0) * 65535;

  gdk_threads_enter ();

  gtk_widget_modify_base (GTK_WIDGET (ptr), GTK_STATE_NORMAL, &color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkWidgetSetForeground
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  color.red = (red / 255.0) * 65535;
  color.green = (green / 255.0) * 65535;
  color.blue = (blue / 255.0) * 65535;

  gdk_threads_enter ();

  gtk_widget_modify_text (GTK_WIDGET (ptr), GTK_STATE_NORMAL, &color);
  gtk_widget_modify_base (GTK_WIDGET (ptr), GTK_STATE_SELECTED, &color);

  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkEntryGetBorderWidth
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int border_width = 0;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  border_width = get_border_width (GTK_WIDGET (ptr));

  gdk_threads_leave ();

  return border_width;
}

/* GTK hard-codes this value.  It is the space between a GtkEntry's
   frame and its text. */
#define INNER_BORDER 2

static jint
get_border_width (GtkWidget *entry)
{
  gint focus_width;
  gboolean interior_focus;
  int x_border_width = INNER_BORDER;

  gtk_widget_style_get (entry,
			"interior-focus", &interior_focus,
			"focus-line-width", &focus_width,
			NULL);

  if (GTK_ENTRY (entry)->has_frame)
    x_border_width += entry->style->xthickness;

  if (!interior_focus)
    x_border_width += focus_width;

  return x_border_width;
}

#undef INNER_BORDER

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_setEchoChar
  (JNIEnv *env, jobject obj, jchar c)
{
  void *ptr;
  GtkEntry *entry;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  entry = GTK_ENTRY (ptr);

  if (c != 0)
    {
      /* FIXME: use gtk_entry_set_invisible_char (GtkEntry *entry,
	 gunichar ch) here.  That means we must convert from jchar
	 (utf16) to gunichar (ucs4). */
      gtk_entry_set_visibility (entry, FALSE);
    }
  else
    gtk_entry_set_visibility (entry, TRUE);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkSetFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  GtkWidget *entry;
  PangoFontDescription *font_desc;

  ptr = NSA_GET_PTR (env, obj);

  entry = GTK_WIDGET (ptr);
  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  gdk_threads_enter();

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc, size * dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET(entry), font_desc);

  pango_font_description_free (font_desc);

  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, name, font_name);
}
