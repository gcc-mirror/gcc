/* gtkchoicepeer.c -- Native implementation of GtkChoicePeer
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
#include "gnu_java_awt_peer_gtk_GtkChoicePeer.h"

static void connect_choice_item_selectable_hook (JNIEnv *env, 
						 jobject peer_obj, 
						 GtkItem *item, 
						 jobject item_obj);
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_create 
  (JNIEnv *env, jobject obj)
{
  GtkWidget *menu;
  GtkOptionMenu *option_menu;
  GtkRequisition child_requisition;

  gdk_threads_enter ();
  option_menu = GTK_OPTION_MENU (gtk_option_menu_new ());
  menu = gtk_menu_new ();
  gtk_widget_show (menu);

  gtk_option_menu_set_menu (GTK_OPTION_MENU (option_menu), menu);

  gtk_widget_size_request (gtk_menu_item_new_with_label (""), 
			   &child_requisition);
  option_menu->width = child_requisition.width;
  option_menu->height = child_requisition.height;

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, option_menu);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_append 
  (JNIEnv *env, jobject obj, jobjectArray items)
{
  gpointer ptr;
  GtkMenu *menu;
  jsize count, i;
  int need_set_history = 0;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  menu = GTK_MENU (gtk_option_menu_get_menu (GTK_OPTION_MENU (ptr)));

  if (!gtk_container_children (GTK_CONTAINER (menu)))
      need_set_history = 1;

  count = (*env)->GetArrayLength (env, items);

  for (i = 0; i < count; i++) 
    {
      jobject item;
      const char *label;
      GtkWidget *menuitem;

      item = (*env)->GetObjectArrayElement (env, items, i);
      label = (*env)->GetStringUTFChars (env, item, NULL);

      menuitem = gtk_menu_item_new_with_label (label);

      (*env)->ReleaseStringUTFChars (env, item, label);

      gtk_menu_append (menu, menuitem);
      gtk_widget_show (menuitem);

      connect_choice_item_selectable_hook (env, obj, 
					   GTK_ITEM (menuitem), item);
    }
  
  if (need_set_history)
    gtk_option_menu_set_history (GTK_OPTION_MENU (ptr), 0);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_add 
  (JNIEnv *env, jobject obj, jstring item, jint index)
{
  void *ptr;
  const char *label;
  GtkWidget *menu, *menuitem;
  int need_set_history = 0;

  ptr = NSA_GET_PTR (env, obj);
  
  label = (*env)->GetStringUTFChars (env, item, 0);      

  gdk_threads_enter ();
  menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (ptr));

  if (!gtk_container_children (GTK_CONTAINER (menu)))
      need_set_history = 1;

  menuitem = gtk_menu_item_new_with_label (label);
  gtk_menu_insert (GTK_MENU (menu), menuitem, index);
  gtk_widget_show (menuitem);
  connect_choice_item_selectable_hook (env, obj, GTK_ITEM (menuitem), item);

  if (need_set_history)
    gtk_option_menu_set_history (GTK_OPTION_MENU (ptr), 0);

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, item, label);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_remove 
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;
  GtkContainer *menu;
  GList *children;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  menu = GTK_CONTAINER (gtk_option_menu_get_menu (GTK_OPTION_MENU (ptr)));
  children = gtk_container_children (menu);
  gtk_container_remove (menu, GTK_WIDGET (g_list_nth (children, index)->data));
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_select 
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  gtk_option_menu_set_history (GTK_OPTION_MENU (ptr), index);
  gdk_threads_leave ();
}


static void
item_activate (GtkItem *item, struct item_event_hook_info *ie)
{
  gdk_threads_leave ();
  (*gdk_env)->CallVoidMethod (gdk_env, ie->peer_obj,
			      postItemEventID,
			      ie->item_obj,
			      (jint) AWT_ITEM_SELECTED);
  gdk_threads_enter ();
}

static void
connect_choice_item_selectable_hook (JNIEnv *env, jobject peer_obj, 
				     GtkItem *item, jobject item_obj)
{
  struct item_event_hook_info *ie;

  ie = (struct item_event_hook_info *) 
    malloc (sizeof (struct item_event_hook_info));

  ie->peer_obj = (*env)->NewGlobalRef (env, peer_obj);
  ie->item_obj = (*env)->NewGlobalRef (env, item_obj);

  gtk_signal_connect (GTK_OBJECT (item), "activate", 
		      GTK_SIGNAL_FUNC (item_activate), ie);
}
