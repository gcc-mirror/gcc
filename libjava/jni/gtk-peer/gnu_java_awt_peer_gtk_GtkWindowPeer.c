/* gtkwindowpeer.c -- Native implementation of GtkWindowPeer
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
#include "gnu_java_awt_peer_gtk_GtkWindowPeer.h"
#include "gnu_java_awt_peer_gtk_GtkFramePeer.h"
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>

/*
 * Make a new window.
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_create 
  (JNIEnv *env, jobject obj, jint type, jboolean decorated,
   jint width, jint height, jobject parent)
{
  GtkWidget *window_widget;
  GtkWindow *window;
  void *window_parent;
  GtkWidget *vbox, *layout;

  gdk_threads_enter ();
  window_widget = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  window = GTK_WINDOW (window_widget);

  // Avoid GTK runtime assertion failures.
  width = (width < 1) ? 1 : width;
  height = (height < 1) ? 1 : height;

  gtk_window_set_default_size (window, width, height);

  /* We must set this window's size requisition.  Otherwise when a
     resize is queued (when gtk_widget_queue_resize is called) the
     window will snap to its default requisition of 0x0.  If we omit
     this call, Frames and Dialogs shrink to degenerate 1x1 windows
     when their resizable property changes. */
  gtk_widget_set_size_request (window_widget, width, height);

  /* Keep this window in front of its parent, if it has one. */
  if (parent)
    {
      window_parent = NSA_GET_PTR (env, parent);
      gtk_window_set_transient_for (window, GTK_WINDOW(window_parent));
    }

  gtk_window_set_decorated (window, decorated);

  gtk_window_set_type_hint (window, type);

  gtk_window_group_add_window (global_gtk_window_group, window);

  vbox = gtk_vbox_new (0, 0);
  layout = gtk_layout_new (NULL, NULL);
  gtk_box_pack_end (GTK_BOX (vbox), layout, 1, 1, 0);
  gtk_container_add (GTK_CONTAINER (window_widget), vbox);

  gtk_widget_show (layout);
  gtk_widget_show (vbox);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, window_widget);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkWindowPeer_nativeSetVisible
  (JNIEnv *env, jobject obj, jboolean visible)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  if (visible)
    gtk_widget_show (GTK_WIDGET (ptr));
  else
    gtk_widget_hide (GTK_WIDGET (ptr));

  XFlush (GDK_DISPLAY ());

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkWindowPeer_connectHooks
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkWidget* vbox, *layout;
  GList* children;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  children = gtk_container_get_children(GTK_CONTAINER(ptr));
  vbox = children->data;

  if(!GTK_IS_VBOX(vbox))
    {
      printf("*** this is not a vbox\n");
    }
  children = gtk_container_get_children(GTK_CONTAINER(vbox));
  layout = children->data;

  if(!GTK_IS_LAYOUT(layout))
    {
      printf("*** widget is not a layout ***");
    }

  gtk_widget_realize (layout);

  connect_awt_hook (env, obj, 1, GTK_LAYOUT (layout)->bin_window);

  gtk_widget_realize (ptr);

  connect_awt_hook (env, obj, 1, GTK_WIDGET (ptr)->window);

  gdk_threads_leave ();
}

/*
 * Set a frame's title
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setTitle
  (JNIEnv *env, jobject obj, jstring title)
{
  void *ptr;
  const char *str;

  ptr = NSA_GET_PTR (env, obj);
  
  str = (*env)->GetStringUTFChars (env, title, NULL);
  
  gdk_threads_enter ();
  gtk_window_set_title (GTK_WINDOW (ptr), str);
  gdk_threads_leave ();
  
  (*env)->ReleaseStringUTFChars (env, title, str);
}

/*
 * Lower the z-level of a window. 
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_toBack (JNIEnv *env, 
    jobject obj)
{
  void *ptr;
  ptr = NSA_GET_PTR (env, obj);
    
  gdk_threads_enter ();
  gdk_window_lower (GTK_WIDGET (ptr)->window);

  XFlush (GDK_DISPLAY ());
  gdk_threads_leave ();
}

/*
 * Raise the z-level of a window.
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_toFront (JNIEnv *env, 
    jobject obj)
{
  void *ptr;
  ptr = NSA_GET_PTR (env, obj);
    
  gdk_threads_enter ();
  gdk_window_raise (GTK_WIDGET (ptr)->window);

  XFlush (GDK_DISPLAY ());
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setBoundsCallback
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
   jobject window, jint x, jint y, jint width, jint height)
{
  /* Circumvent package-private access to call Window's
     setBoundsCallback method. */
  (*gdk_env)->CallVoidMethod (gdk_env, window, setBoundsCallbackID,
			      x, y, width, height);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setSize
  (JNIEnv *env, jobject obj, jint width, jint height)
{
  void *ptr = NSA_GET_PTR (env, obj);

  // Avoid GTK runtime assertion failures.
  width = (width < 1) ? 1 : width;
  height = (height < 1) ? 1 : height;

  gdk_threads_enter ();
  gtk_widget_set_size_request (GTK_WIDGET(ptr), width, height);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_nativeSetBounds
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  void *ptr = NSA_GET_PTR (env, obj);

  // Avoid GTK runtime assertion failures.
  width = (width < 1) ? 1 : width;
  height = (height < 1) ? 1 : height;

  gdk_threads_enter ();
  gtk_window_move (GTK_WINDOW(ptr), x, y);
  /* Need to change the widget's request size. */
  gtk_widget_set_size_request (GTK_WIDGET(ptr), width, height);
  /* Also need to call gtk_window_resize.  If the resize is requested
     by the program and the window's "resizable" property is true then
     the size request will not be honoured. */
  gtk_window_resize (GTK_WINDOW (ptr), width, height);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkFramePeer_setMenuBarPeer
  (JNIEnv *env, jobject obj, jobject menubar)
{
  void *wptr, *mptr;
  GtkBox *box;

  if (!menubar) return;

  wptr = NSA_GET_PTR (env, obj);
  mptr = NSA_GET_PTR (env, menubar);

  if (!mptr) return; /* this case should remove a menu */

  gdk_threads_enter ();
  box = GTK_BOX (GTK_BIN (wptr)->child);
  gtk_box_pack_start (box, GTK_WIDGET (mptr), 0, 0, 0);
  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GtkFramePeer_getMenuBarHeight
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GList *children;
  jint height = 0;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  children = gtk_container_children (GTK_CONTAINER (GTK_BIN (ptr)->child));
  if (g_list_length (children) == 2)
    {
      GtkWidget *menubar = GTK_WIDGET (children->data);
      height = menubar->allocation.height;

    }
  gdk_threads_leave ();

  return height;
}
