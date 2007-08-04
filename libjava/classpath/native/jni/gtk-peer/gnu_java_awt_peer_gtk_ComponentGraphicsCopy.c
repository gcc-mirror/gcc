/* gnu_java_awt_peer_gtk_ComponentGraphicsCopy.c
   Copyright (C) 2006 Free Software Foundation, Inc.

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

#include "jcl.h"
#include "gtkpeer.h"
#include <cairo-xlib.h>
#include <gdk/gdktypes.h>
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixdata.h>

#include <cairo-ft.h>
#include <cairo-xlib.h>

#include <stdio.h>
#include <stdlib.h>

#include "gnu_java_awt_peer_gtk_ComponentGraphicsCopy.h"

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphicsCopy_getPixbuf
   (JNIEnv *env, jobject obj __attribute__((unused)),
    jobject peer, jobject image)
{
  gint width, height;
  GdkPixbuf *pixbuf;
  GdkDrawable *drawable;
  GdkWindow *win;
  GtkWidget *widget = NULL;
  void *ptr = NULL;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, peer);
  g_assert (ptr != NULL);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  cp_gtk_grab_current_drawable (widget, &drawable, &win);
  g_assert (drawable != NULL);

  pixbuf = cp_gtk_image_get_pixbuf( env, image );
  g_assert( pixbuf != NULL);

  width = gdk_pixbuf_get_width( pixbuf );
  height = gdk_pixbuf_get_height( pixbuf );

  gdk_pixbuf_get_from_drawable( pixbuf, /* destination pixbuf */
				drawable, 
				NULL, /* colormap */
				0, 0, 0, 0,
				width, height );
  gdk_threads_leave();
}


JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_ComponentGraphicsCopy_copyPixbuf
  (JNIEnv *env, jobject obj __attribute__((unused)),
   jobject peer, jobject image,
   jint x __attribute__((unused)), jint y __attribute__((unused)),
   jint width __attribute__((unused)), jint height __attribute__((unused)))
{
  gint pwidth, pheight;
  GdkPixbuf *pixbuf;
  GdkDrawable *drawable;
  GdkWindow *win;
  GtkWidget *widget = NULL;
  void *ptr = NULL;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, peer);
  g_assert (ptr != NULL);

  widget = GTK_WIDGET (ptr);
  g_assert (widget != NULL);

  cp_gtk_grab_current_drawable (widget, &drawable, &win);
  g_assert (drawable != NULL);

  pixbuf = cp_gtk_image_get_pixbuf( env, image );
  g_assert( pixbuf != NULL);

  pwidth = gdk_pixbuf_get_width( pixbuf );
  pheight = gdk_pixbuf_get_height( pixbuf );

  gdk_draw_pixbuf (drawable, NULL, pixbuf,
		   0, 0, 0, 0, 
		   pwidth, pheight, 
		   GDK_RGB_DITHER_NORMAL, 0, 0);

  gdk_threads_leave();
}

