/* gtktextareapeer.c -- Native implementation of GtkTextAreaPeer
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
#include "gnu_java_awt_peer_gtk_GtkTextAreaPeer.h"

#define TEXT_FROM_SW(obj) (GTK_TEXT(GTK_SCROLLED_WINDOW (obj)->container.child))
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_create
  (JNIEnv *env, jobject obj, jint scroll)
{
  GtkWidget *text, *sw;

  gdk_threads_enter ();
  text = gtk_text_new (NULL, NULL);
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
  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, sw);
}


JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_old_create
  (JNIEnv *env, jobject obj, jobject parent_obj, 
   jstring contents, jint scroll)
{
  GtkWidget *text, *sw;
  const char *str;
  int pos=0;
  void *parent;

  parent = NSA_GET_PTR (env, parent_obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);

  gdk_threads_enter ();

  text = gtk_text_new (NULL, NULL);
  gtk_text_set_editable (GTK_TEXT (text), TRUE);

  gtk_editable_insert_text (GTK_EDITABLE (text), str,
			    strlen (str), &pos);

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

  set_visible (text, TRUE);
  set_parent (sw, GTK_CONTAINER (parent));

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, contents, str);

  NSA_SET_PTR (env, obj, sw);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_gtkTextGetSize
  (JNIEnv *env, jobject obj, jint rows, jint cols, jintArray jdims)
{
  void *ptr;
  jint *dims;
  GtkWidget *text;
  GtkScrolledWindow *sw;
  GtkRequisition myreq;

  ptr = NSA_GET_PTR (env, obj);

  dims = (*env)->GetIntArrayElements (env, jdims, 0);  
  dims[0] = dims[1] = 0;

  gdk_threads_enter ();

  text = GTK_WIDGET (TEXT_FROM_SW (ptr));
  sw = GTK_SCROLLED_WINDOW (ptr);

  gtk_signal_emit_by_name (GTK_OBJECT (GTK_SCROLLED_WINDOW(sw)->hscrollbar), 
			   "size_request", &myreq);
  //gtk_widget_size_request(GTK_WIDGET (GTK_SCROLLED_WINDOW(sw)->hscrollbar), 
  //				      &myreq);
  dims[0]=myreq.width+GTK_SCROLLED_WINDOW_CLASS 
    (GTK_OBJECT (sw)->klass)->scrollbar_spacing;

  gtk_signal_emit_by_name (GTK_OBJECT (GTK_SCROLLED_WINDOW(sw)->vscrollbar), 
			   "size_request", &myreq);
  //gtk_widget_size_request(GTK_WIDGET (GTK_SCROLLED_WINDOW(sw)->vscrollbar), 
  //				      &myreq);
  dims[1]=myreq.width+GTK_SCROLLED_WINDOW_CLASS 
    (GTK_OBJECT (sw)->klass)->scrollbar_spacing;
  
  /* The '1' in the following assignments is from 
     #define TEXT_BORDER_ROOM         1
     in gtktext.c */

  dims[0] += ((cols * gdk_char_width (text->style->font, 'W'))
	     + (2 * (text->style->klass->xthickness + 1)));
  dims[1] += ((rows * gdk_char_height (text->style->font, 'W'))
	     + (2 * (text->style->klass->ythickness + 1)));

  gdk_threads_leave ();
  
  (*env)->ReleaseIntArrayElements (env, jdims, dims, 0);
}


JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_insert
  (JNIEnv *env, jobject obj, jstring contents, jint position)
{
  void *ptr;
  const char *str;
  int pos=position;

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  gdk_threads_enter ();
  gtk_editable_insert_text (GTK_EDITABLE (TEXT_FROM_SW (ptr)), 
			    str, strlen (str), &pos);
  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, contents, str);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextAreaPeer_replaceRange
  (JNIEnv *env, jobject obj, jstring contents, jint start, jint end)
{
  void *ptr;
  GtkEditable *text;
  const char *str;
  int pos = start;

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  gdk_threads_enter ();
  
  text = GTK_EDITABLE (TEXT_FROM_SW (ptr));
  gtk_text_freeze (GTK_TEXT (text));
  gtk_editable_delete_text (text, start, end);
  gtk_editable_insert_text (text, str, strlen (str), &pos);
  gtk_text_thaw (GTK_TEXT (text));

  gdk_threads_leave ();
  (*env)->ReleaseStringUTFChars (env, contents, str);
}

