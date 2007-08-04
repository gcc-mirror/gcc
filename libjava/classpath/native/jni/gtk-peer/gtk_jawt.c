/* gtk_jawt.c -- GTK implementation of classpath_jawt.h
   Copyright (C) 2005 Free Software Foundation, Inc.

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
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include "classpath_jawt.h"

jint
classpath_jawt_get_awt_version ()
{
  return CLASSPATH_JAWT_VERSION;
}

/* Does not require locking: meant to be called after the drawing
   surface is locked. */
Display*
classpath_jawt_get_default_display (JNIEnv* env, jobject canvas)
{
  GdkDisplay *display;
  Display *xdisplay;
  GtkWidget *widget;
  void *ptr;
  jobject peer;
  jclass class_id;
  jmethodID method_id;

  /* retrieve peer object */
  class_id = (*env)->GetObjectClass (env, canvas);

  method_id = (*env)->GetMethodID (env, class_id,
				   "getPeer",
				   "()Ljava/awt/peer/ComponentPeer;");

  peer = (*env)->CallObjectMethod (env, canvas, method_id);

  ptr = gtkpeer_get_widget (env, peer);

  widget = GTK_WIDGET (ptr);

  if (GTK_WIDGET_REALIZED (widget))
    {
      display = gtk_widget_get_display (widget);

      xdisplay = GDK_DISPLAY_XDISPLAY (display);

      return xdisplay;
    }
  else
    return NULL;
}

/* Does not require locking: meant to be called after the drawing
   surface is locked. */
VisualID
classpath_jawt_get_visualID (JNIEnv* env, jobject canvas)
{
  GtkWidget *widget;
  Visual *visual;
  void *ptr;
  jobject peer;
  jclass class_id;
  jmethodID method_id;

  class_id = (*env)->GetObjectClass (env, canvas);

  method_id = (*env)->GetMethodID (env, class_id,
				   "getPeer",
				   "()Ljava/awt/peer/ComponentPeer;");

  peer = (*env)->CallObjectMethod (env, canvas, method_id);

  ptr = gtkpeer_get_widget (env, peer);

  widget = GTK_WIDGET (ptr);

  if (GTK_WIDGET_REALIZED (widget))
    {
      visual = gdk_x11_visual_get_xvisual (gtk_widget_get_visual (widget));
      g_assert (visual != NULL);

      return visual->visualid;
    }
  else
    return (VisualID) NULL;
}

/* Does not require locking: meant to be called after the drawing
   surface is locked. */
int
classpath_jawt_get_depth (JNIEnv* env, jobject canvas)
{
  GtkWidget *widget;
  GdkVisual *visual;
  void *ptr;
  jobject peer;
  jclass class_id;
  jmethodID method_id;

  class_id = (*env)->GetObjectClass (env, canvas);

  method_id = (*env)->GetMethodID (env, class_id,
				   "getPeer",
				   "()Ljava/awt/peer/ComponentPeer;");

  peer = (*env)->CallObjectMethod (env, canvas, method_id);

  ptr = gtkpeer_get_widget (env, peer);

  widget = GTK_WIDGET (ptr);

  if (GTK_WIDGET_REALIZED (widget))
    {
      visual = gtk_widget_get_visual (widget);
      g_assert (visual != NULL);

      return visual->depth;
    }
  else
    return (VisualID) NULL;
}

/* Does not require locking: meant to be called after the drawing
   surface is locked. */
Drawable
classpath_jawt_get_drawable (JNIEnv* env, jobject canvas)
{
  GtkWidget *widget;
  int drawable;
  void *ptr;
  jobject peer;
  jclass class_id;
  jmethodID method_id;

  class_id = (*env)->GetObjectClass (env, canvas);

  method_id = (*env)->GetMethodID (env, class_id,
				   "getPeer",
				   "()Ljava/awt/peer/ComponentPeer;");

  peer = (*env)->CallObjectMethod (env, canvas, method_id);

  ptr = gtkpeer_get_widget (env, peer);

  widget = GTK_WIDGET (ptr);

  if (GTK_WIDGET_REALIZED (widget))
    {
      drawable = GDK_DRAWABLE_XID (widget->window);

      return drawable;
    }
  else
    return (Drawable) NULL;
}

jint
classpath_jawt_lock ()
{
  gdk_threads_enter ();
  return 0;
}

void
classpath_jawt_unlock ()
{
  gdk_threads_leave ();
}
