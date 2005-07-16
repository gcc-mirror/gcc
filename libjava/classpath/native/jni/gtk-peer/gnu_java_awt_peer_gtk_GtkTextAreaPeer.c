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
#include "gnu_java_awt_peer_gtk_GtkTextAreaPeer.h"

#define AWT_TEXTAREA_SCROLLBARS_BOTH 0
#define AWT_TEXTAREA_SCROLLBARS_VERTICAL_ONLY 1
#define AWT_TEXTAREA_SCROLLBARS_HORIZONTAL_ONLY 2

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_create
  (JNIEnv *env, jobject obj,
   jint textview_width, jint textview_height,  jint scroll)
{
  GtkWidget *text, *sw;

  gdk_threads_enter ();

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

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

  NSA_SET_PTR (env, obj, sw);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  void *ptr;
  jobject *gref;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  gref = NSA_GET_GLOBAL_REF (env, obj);

  /* Unwrap the text view from the scrolled window */
  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));

  /* TextComponent signals */
  cp_gtk_textcomponent_connect_signals (G_OBJECT (buf), gref);

  /* Component signals */
  cp_gtk_component_connect_signals (G_OBJECT (text), gref);

  gdk_threads_leave ();
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

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
  gtk_text_buffer_get_iter_at_offset (buf, &iter, position);
  gtk_text_buffer_insert (buf, &iter, str, strlen (str));

  (*env)->ReleaseStringUTFChars (env, contents, str);

  gdk_threads_leave ();
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

  gdk_threads_enter ();
  
  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));

  gtk_text_buffer_get_iter_at_offset (buf, &startIter, mystart);
  gtk_text_buffer_get_iter_at_offset (buf, &endIter, myend);
  gtk_text_buffer_delete (buf, &startIter, &endIter);

  gtk_text_buffer_get_iter_at_offset (buf, &iter, mystart);
  gtk_text_buffer_insert(buf, &iter, str, strlen (str));

  (*env)->ReleaseStringUTFChars (env, contents, str);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_gtkWidgetModifyFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  GtkWidget *text;
  PangoFontDescription *font_desc;

  gdk_threads_enter();

  ptr = NSA_GET_PTR (env, obj);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc,
                                   size * cp_gtk_dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET (text), font_desc);

  pango_font_description_free (font_desc);

  (*env)->ReleaseStringUTFChars (env, name, font_name);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_gtkWidgetRequestFocus
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkWidget *text;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  
  text = gtk_bin_get_child (GTK_BIN (ptr));

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

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

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

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

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

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_getCaretPosition
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextMark *mark;
  GtkTextIter iter;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
  mark = gtk_text_buffer_get_insert (buf);
  gtk_text_buffer_get_iter_at_mark (buf, &iter, mark);
  pos = gtk_text_iter_get_offset (&iter);

  gdk_threads_leave ();
  
  return pos;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_setCaretPosition
  (JNIEnv *env, jobject obj, jint pos)
{
  void *ptr;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter iter;
  GtkTextMark *oldmark;
  GtkTextIter olditer;
  int oldpos;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));

  /* Save old position. */
  oldmark = gtk_text_buffer_get_insert (buf);
  gtk_text_buffer_get_iter_at_mark (buf, &olditer, oldmark);
  oldpos = gtk_text_iter_get_offset (&olditer);

  /* Move to new position. */
  gtk_text_buffer_get_iter_at_offset (buf, &iter, pos);
  gtk_text_buffer_place_cursor (buf, &iter);

  /* Scroll to new position. Alignment is determined
     comparing the new position to the old position. */
  if (oldpos > pos)
    gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW (text),
                                  &iter, 0, TRUE, 0, 0);
  else if (oldpos < pos)
    gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW (text),
                                  &iter, 0, TRUE, 1, 1);

  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_getSelectionStart
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter start;
  GtkTextIter end;
  GtkTextMark *mark;
  GtkTextIter iter;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));

  if (gtk_text_buffer_get_selection_bounds (buf, &start, &end))
    {
      pos = gtk_text_iter_get_offset (&start);
    }
  else
    {
      mark = gtk_text_buffer_get_insert (buf);
      gtk_text_buffer_get_iter_at_mark (buf, &iter, mark);
      pos = gtk_text_iter_get_offset (&iter);
    }

  gdk_threads_leave ();

  return pos;
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_getSelectionEnd
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter start;
  GtkTextIter end;
  GtkTextMark *mark;
  GtkTextIter iter;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));

  if (gtk_text_buffer_get_selection_bounds (buf, &start, &end))
    {
      pos = gtk_text_iter_get_offset (&end);
    }
  else
    {
      mark = gtk_text_buffer_get_insert (buf);
      gtk_text_buffer_get_iter_at_mark (buf, &iter, mark);
      pos = gtk_text_iter_get_offset (&iter);
    }

  gdk_threads_leave ();

  return pos;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_select
  (JNIEnv *env, jobject obj, jint start, jint end)
{
  void *ptr;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter iter;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
  gtk_text_buffer_get_iter_at_offset (buf, &iter, start);
  /* quickly move both 'insert' and 'selection_bound' to the 
     same position */
  gtk_text_buffer_place_cursor (buf, &iter);  
  gtk_text_buffer_get_iter_at_offset (buf, &iter, end);
  gtk_text_buffer_move_mark_by_name (buf, "selection_bound", &iter);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_setEditable
  (JNIEnv *env, jobject obj, jboolean state)
{
  void *ptr;
  GtkWidget *text = NULL;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  gtk_text_view_set_editable (GTK_TEXT_VIEW (text), state);

  gdk_threads_leave ();
}

JNIEXPORT jstring JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_getText
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  char *contents = NULL;
  jstring jcontents;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter start, end;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  
  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
  gtk_text_buffer_get_start_iter (buf, &start);
  gtk_text_buffer_get_end_iter (buf, &end);
  contents = gtk_text_buffer_get_text (buf, &start, &end, FALSE);

  jcontents = (*env)->NewStringUTF (env, contents);
  g_free (contents);

  gdk_threads_leave ();

  return jcontents;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_setText
  (JNIEnv *env, jobject obj, jstring contents)
{
  void *ptr;
  const char *str;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);

  text = gtk_bin_get_child (GTK_BIN (ptr));

  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
  gtk_text_buffer_set_text (buf, str, strlen (str));

  (*env)->ReleaseStringUTFChars (env, contents, str);

  gdk_threads_leave ();
}
