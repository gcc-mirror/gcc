/* gtkscrollpanepeer.c -- Native implementation of GtkScrollPanePeer
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
#include "gnu_java_awt_peer_gtk_GtkScrollPanePeer.h"

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_create 
  (JNIEnv *env, jobject obj)
{
  gpointer window;
  GtkWidget *layout;

  gdk_threads_enter ();
  window = gtk_scrolled_window_new (NULL, NULL);
  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, window);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_setScrollPosition
  (JNIEnv *env, jobject obj, jint x, jint y)
{
  GtkAdjustment *hadj, *vadj;
  GtkScrolledWindow *sw;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  sw = GTK_SCROLLED_WINDOW (ptr);

  hadj = gtk_scrolled_window_get_hadjustment (sw);
  vadj = gtk_scrolled_window_get_vadjustment (sw);

  gtk_adjustment_set_value (hadj, x);
  gtk_adjustment_set_value (vadj, y);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_gtkScrolledWindowSetHScrollIncrement
  (JNIEnv *env, jobject obj, jint u)
{
  GtkAdjustment *hadj;
  GtkScrolledWindow *sw;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  sw = GTK_SCROLLED_WINDOW(ptr);

  hadj = gtk_scrolled_window_get_hadjustment (sw);
  hadj->step_increment = u;

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_gtkScrolledWindowSetVScrollIncrement
  (JNIEnv *env, jobject obj, jint u)
{
  GtkAdjustment *vadj;
  GtkScrolledWindow *sw;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  sw = GTK_SCROLLED_WINDOW(ptr);

  vadj = gtk_scrolled_window_get_hadjustment (sw);
  vadj->step_increment = u;

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_childResized
  (JNIEnv *env, jobject obj, jint width, jint height)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  return;

  gdk_threads_enter ();
  gtk_widget_set_usize (GTK_BIN (ptr)->child, width, height);
  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_getHScrollbarHeight
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkScrolledWindow *sw;
  jint height;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  sw = GTK_SCROLLED_WINDOW (ptr);
  height = (sw->hscrollbar_visible) ? sw->hscrollbar->allocation.height : 0;
  gdk_threads_leave ();

  return height;
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_getVScrollbarWidth
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkScrolledWindow *sw;
  jint width;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  sw = GTK_SCROLLED_WINDOW (ptr);
  width = (sw->vscrollbar_visible) ? sw->vscrollbar->allocation.width : 0;
  gdk_threads_leave ();

  return width;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollPanePeer_setPolicy
  (JNIEnv *env, jobject obj, jint policy)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  switch (policy)
    {
    case AWT_SCROLLPANE_SCROLLBARS_AS_NEEDED:
      policy = GTK_POLICY_AUTOMATIC;
      break;
    case AWT_SCROLLPANE_SCROLLBARS_ALWAYS:
      policy = GTK_POLICY_ALWAYS;
      break;
    case AWT_SCROLLPANE_SCROLLBARS_NEVER:
      policy = GTK_POLICY_NEVER;
      break;
    }

  gdk_threads_enter ();
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (ptr), policy, policy);
  gdk_threads_leave ();
}
