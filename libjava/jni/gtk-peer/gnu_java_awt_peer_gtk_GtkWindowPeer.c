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

static void setBounds (GtkWidget *, jint, jint, jint, jint);

/*
 * Make a new window (any type)
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_create 
  (JNIEnv *env, jobject obj, jint type)
{
  gpointer window;
  GtkWidget *vbox, *layout;

  gdk_threads_enter ();
  window = gtk_window_new (type);

  vbox = gtk_vbox_new (0, 0);
  layout = gtk_layout_new (NULL, NULL);
  gtk_box_pack_end (GTK_BOX (vbox), layout, 1, 1, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);

  gtk_widget_show (layout);
  gtk_widget_show (vbox);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, window);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setVisible
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
  GtkWidget *layout;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  layout = GTK_WIDGET (gtk_container_children (GTK_CONTAINER (GTK_BIN (ptr)->child))->data);
  gtk_widget_realize (layout);
  connect_awt_hook (env, obj, 1, GTK_LAYOUT (layout)->bin_window);
  
  gtk_widget_realize (GTK_WIDGET (ptr));
  connect_awt_hook (env, obj, 1, GTK_WIDGET (ptr)->window);
  gdk_threads_leave ();
}

void
setup_window (JNIEnv *env, jobject obj, GtkWidget *window, jint width, 
	      jint height, jboolean visible)
{
  GtkWidget *layout, *vbox;
  gint x, y;

  gtk_window_set_policy (GTK_WINDOW (window), 1, 1, 0);
  gtk_widget_set_usize (window, width, height);

  vbox = gtk_vbox_new (0, 0);
  layout = gtk_layout_new (NULL, NULL);
  gtk_box_pack_end (GTK_BOX (vbox), layout, 1, 1, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_realize (layout);
  connect_awt_hook (env, obj, 1, GTK_LAYOUT(layout)->bin_window);
  gtk_widget_show (layout);
  gtk_widget_show (vbox);

  gtk_widget_realize (window);
/*    setBounds (window, x, y, width, height); */

  connect_awt_hook (env, obj, 1, window->window);
  set_visible (window, visible);
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
 * Set a window's resizing policy
 */

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setResizable
  (JNIEnv *env, jobject obj, jboolean resize)
{
  void *ptr;
  
  ptr = NSA_GET_PTR (env, obj);
  
  gdk_threads_enter ();
  gtk_window_set_policy (GTK_WINDOW (ptr), resize, resize, 0);
  gdk_threads_leave ();
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

static void
setBounds (GtkWidget *widget, jint x, jint y, jint width, jint height)
{
  gint current_x, current_y;
  gint origin_x, origin_y;

/*    gdk_window_get_root_origin (widget->window, &current_x, &current_y); */

/*    if (current_x != x || current_y != y) */
/*      { */
/*        gdk_window_set_hints (widget->window, x, y, 0, 0, 0, 0, GDK_HINT_POS); */
/*        gdk_window_move (widget->window, x, y); */
/*      } */

  gtk_widget_set_usize (widget, width, height);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setBounds
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  void *ptr;
  GtkWidget *widget;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  widget = GTK_WIDGET (ptr);
  setBounds (widget, x, y, width, height);

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


void
gdk_window_get_root_geometry (GdkWindow *window,
			      gint      *x,
			      gint      *y,
			      gint      *width,
			      gint      *height,
			      gint      *border,
			      gint      *depth)
{
  GdkWindowPrivate *private;
  Window xwindow;
  Window xparent;
  Window root;
  Window *children;
  unsigned int nchildren;
  
  g_return_if_fail (window != NULL);
  
  private = (GdkWindowPrivate*) window;
  if (x)
    *x = 0;
  if (y)
    *y = 0;
  if (width)
    *width = 0;
  if (height)
    *height = 0;
  if (border)
    *border = 0;
  if (depth)
    *depth = 0;

  if (private->destroyed)
    return;
  
  while (private->parent && ((GdkWindowPrivate*) private->parent)->parent)
    private = (GdkWindowPrivate*) private->parent;
  if (private->destroyed)
    return;
  
  xparent = private->xwindow;
  do
    {
      xwindow = xparent;
      if (!XQueryTree (private->xdisplay, xwindow,
		       &root, &xparent,
		       &children, &nchildren))
	return;
      
      if (children)
	XFree (children);
    }
  while (xparent != root);
  
  if (xparent == root)
    {
      unsigned int ww, wh, wb, wd;
      int wx, wy;
      
      if (XGetGeometry (private->xdisplay, xwindow, &root, &wx, &wy, &ww, &wh, &wb, &wd))
	{
	  if (x)
	    *x = wx;
	  if (y)
	    *y = wy;
	  if (width)
	    *width = ww;
	  if (height)
	    *height = wh;
	  if (border)
	    *border = wb;
	  if (depth)
	    *depth = wd;
	}
    }
}

