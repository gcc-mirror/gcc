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
						 GtkItem *menuitem, 
						 const char *label);

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_create 
  (JNIEnv *env, jobject obj)
{
  GtkWidget *menu;
  GtkOptionMenu *option_menu;
  GtkRequisition child_requisition;

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

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

  /* Are we adding the first element? */
  if (gtk_option_menu_get_history (GTK_OPTION_MENU (ptr)) < 0)
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
      gtk_menu_append (menu, menuitem);
      gtk_widget_show (menuitem);

      connect_choice_item_selectable_hook (env, obj, 
					   GTK_ITEM (menuitem), label);

      (*env)->ReleaseStringUTFChars (env, item, label);
    }

  /* If we just added the first element select it. */  
  if (need_set_history)
    gtk_option_menu_set_history (GTK_OPTION_MENU (ptr), 0);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_nativeAdd 
  (JNIEnv *env, jobject obj, jstring item, jint index)
{
  void *ptr;
  const char *label;
  GtkWidget *menu, *menuitem;
  int current;
  int need_set_history = 0;

  ptr = NSA_GET_PTR (env, obj);
  
  label = (*env)->GetStringUTFChars (env, item, 0);      

  gdk_threads_enter ();
  
  current = gtk_option_menu_get_history (GTK_OPTION_MENU (ptr));

  /* Are we adding the first element or below or at the currently
     selected one? */
  if ((current < 0) || (current >= index))
      need_set_history = 1;

  menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (ptr));
  menuitem = gtk_menu_item_new_with_label (label);
  gtk_menu_insert (GTK_MENU (menu), menuitem, index);
  gtk_widget_show (menuitem);

  connect_choice_item_selectable_hook (env, obj, GTK_ITEM (menuitem), label);

  /* If we just added the first element select it.
     If we added at of below the currently selected position make
     the first item the selected one. */  
  if (need_set_history)
    gtk_option_menu_set_history (GTK_OPTION_MENU (ptr), 0);

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, item, label);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_nativeRemove 
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;
  GtkContainer *menu;
  GtkWidget *menuitem;
  GList *children;
  int need_set_history = 0;
  int i, from, to;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  menu = GTK_CONTAINER (gtk_option_menu_get_menu (GTK_OPTION_MENU (ptr)));
  children = gtk_container_children (menu);

  if (index == -1)
    {
      /* Remove all elements (removeAll) */
      from = g_list_length (children) - 1;
      to = 0;

      /* Select the first item to prevent spurious activate signals */
      gtk_option_menu_set_history (GTK_OPTION_MENU (ptr), 0);
    }
  else
    {
      /* Remove the specific index element */
      from = index;
      to = index;

      /* Are we removing the currently selected element? */
      if (gtk_option_menu_get_history (GTK_OPTION_MENU (ptr)) == index)
        need_set_history = 1;
    }

  for (i = from; i >= to; i--)
    {
      menuitem = GTK_WIDGET (g_list_nth (children, i)->data);
      gtk_container_remove (menu, menuitem);
      gtk_widget_destroy (menuitem);
    }

  /* If we just removed the currently selected element and there are
     still elements left in the list, make the first item the selected one. */  
  if (need_set_history && gtk_container_children (menu))
    gtk_option_menu_set_history (GTK_OPTION_MENU (ptr), 0);

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

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_getHistory 
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int index;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  index = gtk_option_menu_get_history (GTK_OPTION_MENU (ptr));

  gdk_threads_leave ();

  return index;
}

static void
item_activate (GtkItem *item __attribute__((unused)),
	       struct item_event_hook_info *ie)
{
  gdk_threads_leave ();

  jstring label = (*gdk_env)->NewStringUTF (gdk_env, ie->label);
  (*gdk_env)->CallVoidMethod (gdk_env, ie->peer_obj,
			      choicePostItemEventID,
			      label,
			      (jint) AWT_ITEM_SELECTED);
  gdk_threads_enter ();
}

static void
item_removed (gpointer data, 
	      GClosure gc __attribute__((unused)))
{
  struct item_event_hook_info *ie = data;

  free ((void *) ie->label);
  free (ie);
}

static void
connect_choice_item_selectable_hook (JNIEnv *env, 
				     jobject peer_obj, 
				     GtkItem *menuitem, 
				     const char *label)
{
  struct item_event_hook_info *ie;
  jobject *peer_objGlobPtr;

  ie = (struct item_event_hook_info *) 
    malloc (sizeof (struct item_event_hook_info));

  peer_objGlobPtr = NSA_GET_GLOBAL_REF (env, peer_obj);
  g_assert (peer_objGlobPtr);

  ie->peer_obj = *peer_objGlobPtr;
  ie->label = strdup (label);

  g_signal_connect_data (G_OBJECT (menuitem), "activate", 
		      GTK_SIGNAL_FUNC (item_activate), ie,
		      (GClosureNotify) item_removed, 0);
}
