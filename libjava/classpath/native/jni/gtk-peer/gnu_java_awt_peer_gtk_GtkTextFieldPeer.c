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
#include "gnu_java_awt_peer_gtk_GtkTextFieldPeer.h"

/* the color used for highlighting when the foreground is black,
   since black highlights aren't a Good Idea. */
#define BB_RED    16962
#define BB_GREEN  26985
#define BB_BLUE   31611

static jmethodID postTextEventID;

void
cp_gtk_textcomponent_init_jni (void)
{
  jclass gtkcomponentpeer;

  gtkcomponentpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                              "gnu/java/awt/peer/gtk/GtkComponentPeer");

  postTextEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkcomponentpeer,
					     "postTextEvent",
					     "()V");
}

static void textcomponent_changed_cb (GtkEditable *editable, jobject peer);

static jint get_border_width (GtkWidget *entry);

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_create
  (JNIEnv *env, jobject obj, jint text_width)
{
  GtkWidget *entry;

  gdk_threads_enter ();

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  entry = gtk_entry_new ();
  gtk_widget_set_size_request (entry,
			       text_width + 2 * get_border_width (entry), -1);

  NSA_SET_PTR (env, obj, entry);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jobject *gref;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  gref = NSA_GET_GLOBAL_REF (env, obj);

  /* TextComponent signals */
  cp_gtk_textcomponent_connect_signals (G_OBJECT (ptr), gref);

  /* Component signals */
  cp_gtk_component_connect_signals (G_OBJECT (ptr), gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkWidgetSetBackground
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  color.red = (red / 255.0) * 65535;
  color.green = (green / 255.0) * 65535;
  color.blue = (blue / 255.0) * 65535;

  gtk_widget_modify_base (GTK_WIDGET (ptr), GTK_STATE_NORMAL, &color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkWidgetSetForeground
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  color.red = (red / 255.0) * 65535;
  color.green = (green / 255.0) * 65535;
  color.blue = (blue / 255.0) * 65535;
  
  gtk_widget_modify_text (GTK_WIDGET (ptr), GTK_STATE_NORMAL, &color);

  if ( red == 0 && green == 0 && blue == 0)
    {
      color.red = BB_RED;
      color.green = BB_GREEN;
      color.blue = BB_BLUE;
    }
  gtk_widget_modify_base (GTK_WIDGET (ptr), GTK_STATE_SELECTED, &color);

  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkEntryGetBorderWidth
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int border_width = 0;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

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

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

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
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_gtkWidgetModifyFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  PangoFontDescription *font_desc;

  gdk_threads_enter();

  ptr = NSA_GET_PTR (env, obj);

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc,
                                   size * cp_gtk_dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET (ptr), font_desc);

  pango_font_description_free (font_desc);

  (*env)->ReleaseStringUTFChars (env, name, font_name);

  gdk_threads_leave();
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_getCaretPosition
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  pos = gtk_editable_get_position (GTK_EDITABLE (ptr));

  gdk_threads_leave ();
  
  return pos;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_setCaretPosition
  (JNIEnv *env, jobject obj, jint pos)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  gtk_editable_set_position (GTK_EDITABLE (ptr), pos);

  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_getSelectionStart
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  int starti, endi;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  if (gtk_editable_get_selection_bounds (GTK_EDITABLE (ptr), &starti, &endi))
    pos = starti;
  else
    pos = gtk_editable_get_position (GTK_EDITABLE (ptr));

  gdk_threads_leave ();

  return pos;
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_getSelectionEnd
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  int starti, endi;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  if (gtk_editable_get_selection_bounds (GTK_EDITABLE (ptr), &starti, &endi))
    pos = endi;
  else
    pos = gtk_editable_get_position (GTK_EDITABLE (ptr));

  gdk_threads_leave ();

  return pos;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_select
  (JNIEnv *env, jobject obj, jint start, jint end)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  gtk_editable_select_region (GTK_EDITABLE (ptr), start, end);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_setEditable
  (JNIEnv *env, jobject obj, jboolean state)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  gtk_editable_set_editable (GTK_EDITABLE (ptr), state);

  gdk_threads_leave ();
}

JNIEXPORT jstring JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_getText
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  char *contents = NULL;
  jstring jcontents;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  
  contents = gtk_editable_get_chars (GTK_EDITABLE (ptr), 0, -1);

  jcontents = (*env)->NewStringUTF (env, contents);

  g_free (contents);

  gdk_threads_leave ();

  return jcontents;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextFieldPeer_setText
  (JNIEnv *env, jobject obj, jstring contents)
{
  void *ptr;
  const char *str;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  gtk_entry_set_text (GTK_ENTRY (ptr), str);

  (*env)->ReleaseStringUTFChars (env, contents, str);

  gdk_threads_leave ();
}

void
cp_gtk_textcomponent_connect_signals (GObject *ptr, jobject *gref)
{
  g_signal_connect (G_OBJECT(ptr), "changed",
                    G_CALLBACK (textcomponent_changed_cb), *gref);
}

static void
textcomponent_changed_cb (GtkEditable *editable __attribute__((unused)),
			  jobject peer)
{
  gdk_threads_leave ();
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer, postTextEventID);
  gdk_threads_enter ();
}
