/* gtkpanelpeer.c -- Native implementation of GtkPanelPeer
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
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"
#include "gnu_java_awt_peer_gtk_GtkPanelPeer.h"

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkPanelPeer_create
  (JNIEnv *env, jobject obj)
{
  gpointer widget;

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  gdk_threads_enter ();
  
  widget = gtk_layout_new (NULL, NULL);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, widget);
}

typedef struct _GtkLayoutChild   GtkLayoutChild;

struct _GtkLayoutChild {
  GtkWidget *widget;
  gint x;
  gint y;
};

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkPanelPeer_connectJObject
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  gtk_widget_realize (GTK_WIDGET (ptr));
  connect_awt_hook (env, obj, 1, GTK_LAYOUT (ptr)->bin_window);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkPanelPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr = NSA_GET_PTR (env, obj);
  jobject *gref = NSA_GET_GLOBAL_REF (env, obj);
  g_assert (gref);

  gdk_threads_enter ();
  gtk_widget_realize (GTK_WIDGET (ptr));

  /* FIXME: If we don't need this then remove this method. */
/*    g_signal_connect (G_OBJECT (ptr), "size_request", GTK_SIGNAL_FUNC (sr), */
/*  		      NULL); */
  gdk_threads_leave ();

  /* Connect the superclass signals.  */
  Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals (env, obj);
}

/* FIXME: The following doesn't seem to be used.
   Is not declared as a native function in GtkPanelPeer.java */
/*
 * Make a new panel.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkPanelPeer_gtkPanelNew
    (JNIEnv *env, jobject obj, jobject parent_obj)
{
  GtkWidget *layout;
  void *parent;

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  parent = NSA_GET_PTR (env, parent_obj);

  gdk_threads_enter ();

  layout = gtk_layout_new (NULL, NULL);
  
  set_parent (layout, GTK_CONTAINER (parent));

  gtk_widget_realize (layout);

  connect_awt_hook (env, obj, 1, GTK_LAYOUT (layout)->bin_window);

  set_visible (layout, 1);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, layout);
}


