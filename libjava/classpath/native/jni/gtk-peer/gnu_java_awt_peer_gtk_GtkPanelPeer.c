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
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"
#include "gnu_java_awt_peer_gtk_GtkPanelPeer.h"

static gboolean panel_focus_in_cb (GtkWidget * widget,
                                   GdkEventFocus *event,
                                   jobject peer);
static gboolean panel_focus_out_cb (GtkWidget * widget,
                                    GdkEventFocus *event,
                                    jobject peer);

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkPanelPeer_create
  (JNIEnv *env, jobject obj)
{
  GtkWidget *widget;

  gdk_threads_enter ();

  gtkpeer_set_global_ref (env, obj);

  widget = gtk_fixed_new ();

  gtk_fixed_set_has_window (GTK_FIXED (widget), TRUE);

  GTK_WIDGET_SET_FLAGS (widget, GTK_CAN_FOCUS);

  gtkpeer_set_widget (env, obj, widget);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkPanelPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jobject gref;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  gref = gtkpeer_get_global_ref (env, obj);

  /* Panel signals.  These callbacks prevent expose events being
     delivered to the panel when it is focused. */
  g_signal_connect (G_OBJECT (ptr), "focus-in-event",
                    G_CALLBACK (panel_focus_in_cb), gref);

  g_signal_connect (G_OBJECT (ptr), "focus-out-event",
                    G_CALLBACK (panel_focus_out_cb), gref);

  /* Component signals.  Exclude focus signals. */
  cp_gtk_component_connect_expose_signals (ptr, gref);
  cp_gtk_component_connect_mouse_signals (ptr, gref);

  gdk_threads_leave ();
}

static gboolean
panel_focus_in_cb (GtkWidget * widget  __attribute__((unused)),
		    GdkEventFocus *event  __attribute__((unused)),
		    jobject peer __attribute__((unused)))
{
  return TRUE;
}

static gboolean
panel_focus_out_cb (GtkWidget * widget __attribute__((unused)),
		     GdkEventFocus *event __attribute__((unused)),
		     jobject peer __attribute__((unused)))
{
  return TRUE;
}
