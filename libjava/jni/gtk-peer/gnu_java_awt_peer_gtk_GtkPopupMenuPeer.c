/* gtkpopupmenupeer.c -- Native implementation of GtkPopupMenuPeer
   Copyright (C) 1999, 2004 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkPopupMenuPeer.h"

struct pos
{
  gint x;
  gint y;
};

static void 
menu_pos (GtkMenu *menu __attribute__((unused)),
	  gint *x, gint *y,
	  gboolean *push_in,
	  gpointer user_data)
{
  struct pos *p = (struct pos *) user_data;

  *x = p->x;
  *y = p->y;
  *push_in = TRUE;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkPopupMenuPeer_show
  (JNIEnv *env, jobject obj, jint x, jint y, jlong time)
{
  void *ptr;
  struct pos *p;

  ptr = NSA_GET_PTR (env, obj);

  p = g_malloc (sizeof (struct pos));
  p->x = x;
  p->y = y;
  
  gdk_threads_enter ();
  gtk_menu_popup (GTK_MENU (GTK_MENU_ITEM (ptr)->submenu), 
		  NULL, NULL, menu_pos, p, 0, time);
  gdk_threads_leave ();

  g_free (p);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkPopupMenuPeer_setupAccelGroup
  (JNIEnv *env, jobject obj, jobject parent)
{
  void *ptr1, *ptr2;
  GtkMenu *menu;

  ptr1 = NSA_GET_PTR (env, obj);
  ptr2 = NSA_GET_PTR (env, parent);

  gdk_threads_enter ();
  menu = GTK_MENU (GTK_MENU_ITEM (ptr1)->submenu);
  gtk_menu_set_accel_group (menu, gtk_accel_group_new ());
  /* FIXME: update this to use GTK-2.4 GtkActions. */
#if 0
  _gtk_accel_group_attach (gtk_menu_get_accel_group (menu),
			   G_OBJECT (gtk_widget_get_toplevel (ptr2)));
#endif
  gdk_threads_leave ();
}
