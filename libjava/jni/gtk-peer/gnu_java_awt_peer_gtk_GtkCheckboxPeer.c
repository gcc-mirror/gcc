/* gtkcheckboxpeer.c -- Native implementation of GtkCheckboxPeer
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
#include "gnu_java_awt_peer_gtk_GtkCheckboxPeer.h"
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"

static void item_toggled (GtkToggleButton *item, jobject peer);

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer_dispose
  (JNIEnv *env, jobject obj)
{
  /* The actual underlying widget is owned by a different class.  So
     we just clean up the hash table here.  */
  NSA_DEL_PTR (env, obj);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer_remove
  (JNIEnv *env, jobject obj, jobject checkbox)
{
  GtkRadioButton *button;
  void *ptr;
  GSList *list;

  ptr = NSA_GET_PTR (env, checkbox);
  gdk_threads_enter ();
  button = GTK_RADIO_BUTTON (ptr);

  /* Update the group to point to some other widget in the group.  We
     have to do this because Gtk doesn't have a separate object to
     represent a radio button's group.  */
  for (list = gtk_radio_button_group (button); list != NULL;
       list = list->next)
    {
      if (list->data != button)
	break;
    }

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, list ? list->data : NULL);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_nativeCreate
  (JNIEnv *env, jobject obj, jobject group)
{
  GtkWidget *button;

  gdk_threads_enter ();

  if (group == NULL)
    button = gtk_check_button_new_with_label ("");
  else
    {
      void *native_group = NSA_GET_PTR (env, group);
      button = gtk_radio_button_new_with_label_from_widget (native_group, "");
      if (native_group == NULL)
	{
	  /* Set the native group so we can use the correct value the
	     next time around.  FIXME: this doesn't work!  */
	  NSA_SET_PTR (env, group, button);
	}
    }

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, button);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_connectHooks
  (JNIEnv *env, jobject obj)
{
  void *ptr = NSA_GET_PTR (env, obj);
  jobject peer;

  gdk_threads_enter ();

  peer = (*env)->NewGlobalRef (env, obj);

  /* FIXME: when the widget goes away, we should get rid of the global
     reference.  */
  gtk_signal_connect (GTK_OBJECT (ptr), "toggled",
		      GTK_SIGNAL_FUNC (item_toggled), peer);

  gdk_threads_leave ();

  /* Connect the superclass hooks.  */
  Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectHooks (env, obj);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_nativeSetCheckboxGroup
  (JNIEnv *env, jobject obj, jobject group)
{
  GtkRadioButton *button;
  void *native_group, *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  /* FIXME: we can't yet switch between a checkbutton and a
     radiobutton.  However, AWT requires this.  For now we just
     crash.  */

  button = GTK_RADIO_BUTTON (ptr);

  native_group = NSA_GET_PTR (env, group);
  if (native_group == NULL)
    gtk_radio_button_set_group (button, NULL);
  else
    gtk_radio_button_set_group (button,
				gtk_radio_button_group 
				(GTK_RADIO_BUTTON (native_group)));

  gdk_threads_leave ();

  /* If the native group wasn't set on the new CheckboxGroup, then set
     it now so that the right thing will happen with the next
     radiobutton.  The native state for a CheckboxGroup is a pointer
     to one of the widgets in the group.  We are careful to keep this
     always pointing at a live widget; whenever a widget is destroyed
     (or otherwise removed from the group), the CheckboxGroup peer is
     notified.  */
  if (native_group == NULL)
    NSA_SET_PTR (env, group, native_group);
}

static void
item_toggled (GtkToggleButton *item, jobject peer)
{
  (*gdk_env)->CallVoidMethod (gdk_env, peer,
			      postItemEventID,
			      peer,
			      item->active ?
			      (jint) AWT_ITEM_SELECTED :
			      (jint) AWT_ITEM_DESELECTED);
}
