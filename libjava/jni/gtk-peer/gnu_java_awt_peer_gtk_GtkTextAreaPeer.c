/* gtktextareapeer.c -- Native implementation of GtkTextAreaPeer
   Copyright (C) 1998, 1999, 2003 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkTextAreaPeer.h"

#define TEXT_FROM_SW(obj) (GTK_TEXT_VIEW(GTK_SCROLLED_WINDOW (obj)->container.child))
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_create
  (JNIEnv *env, jobject obj,
   jint textview_width, jint textview_height,  jint scroll)
{
  GtkWidget *text, *sw;

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  gdk_threads_enter ();

  text = gtk_text_view_new ();
  gtk_widget_set_size_request (text, textview_width, textview_height);
  gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW (text), TRUE);

  gtk_widget_show (text);

  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (sw), text);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
     /* horizontal scrollbar */
     (scroll == AWT_TEXTAREA_SCROLLBARS_BOTH
      || scroll == AWT_TEXTAREA_SCROLLBARS_HORIZONTAL_ONLY) ? 
       GTK_POLICY_ALWAYS : GTK_POLICY_NEVER,
     /* vertical scrollbar */
     (scroll == AWT_TEXTAREA_SCROLLBARS_BOTH
      || scroll == AWT_TEXTAREA_SCROLLBARS_VERTICAL_ONLY) ? 
       GTK_POLICY_ALWAYS : GTK_POLICY_NEVER);

  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (text),
			       (scroll == AWT_TEXTAREA_SCROLLBARS_BOTH
				|| scroll == AWT_TEXTAREA_SCROLLBARS_HORIZONTAL_ONLY)
			       ? GTK_WRAP_NONE : GTK_WRAP_WORD);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, sw);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_insert
  (JNIEnv *env, jobject obj, jstring contents, jint position)
{
  GtkTextBuffer *buf;
  GtkTextIter iter;
  GtkWidget *text;
  void *ptr;
  const char *str;
  int pos=position;

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  gdk_threads_enter ();

  text = GTK_WIDGET (TEXT_FROM_SW (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
  gtk_text_buffer_get_iter_at_offset (buf, &iter, pos);
  gtk_text_buffer_insert (buf, &iter, str, strlen (str));

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, contents, str);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_replaceRange
  (JNIEnv *env, jobject obj, jstring contents, jint start, jint end)
{
  GtkWidget *text;
  GtkTextBuffer *buf;
  GtkTextIter iter, startIter, endIter;
  void *ptr;
  const char *str;
  int mystart = start;
  int myend = end;

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  gdk_threads_enter ();
  
  text = GTK_WIDGET (TEXT_FROM_SW (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));

  gtk_text_buffer_get_iter_at_offset (buf, &startIter, mystart);
  gtk_text_buffer_get_iter_at_offset (buf, &endIter, myend);
  gtk_text_buffer_delete (buf, &startIter, &endIter);

  gtk_text_buffer_get_iter_at_offset (buf, &iter, mystart);
  gtk_text_buffer_insert(buf, &iter, str, strlen (str));

  gdk_threads_leave ();
  (*env)->ReleaseStringUTFChars (env, contents, str);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_gtkWidgetModifyFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  GtkWidget *text;
  PangoFontDescription *font_desc;

  ptr = NSA_GET_PTR (env, obj);

  text = GTK_WIDGET (TEXT_FROM_SW (ptr));

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  gdk_threads_enter();

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc, size * dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET (text), font_desc);

  pango_font_description_free (font_desc);

  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, name, font_name);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_gtkWidgetRequestFocus
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkWidget *text;

  ptr = NSA_GET_PTR (env, obj);
  
  gdk_threads_enter ();

  text = GTK_WIDGET (TEXT_FROM_SW (ptr));

  gtk_widget_grab_focus (text);

  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_getHScrollbarHeight
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkScrolledWindow *sw;
  GtkRequisition requisition;
  jint height = 0;
  jint spacing = 0;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  sw = GTK_SCROLLED_WINDOW (ptr);

  if (sw)
    {
      gtk_widget_size_request (sw->hscrollbar, &requisition);
      gtk_widget_style_get (GTK_WIDGET (sw), "scrollbar_spacing", &spacing, NULL);
      height = requisition.height + spacing;
    }

  gdk_threads_leave ();

  return height;
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_getVScrollbarWidth
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkScrolledWindow *sw;
  GtkRequisition requisition;
  jint width = 0;
  jint spacing = 0;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  sw = GTK_SCROLLED_WINDOW (ptr);

  if (sw)
    {
      gtk_widget_size_request (sw->vscrollbar, &requisition);
      gtk_widget_style_get (GTK_WIDGET (sw), "scrollbar_spacing", &spacing, NULL);
      width = requisition.width + spacing;
    }

  gdk_threads_leave ();

  return width;
}
