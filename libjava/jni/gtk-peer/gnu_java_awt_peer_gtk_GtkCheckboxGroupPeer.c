/* gtkcheckboxgrouppeer.c -- Native implementation of GtkCheckboxGroupPeer
   Copyright (C) 2004  Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer.h"

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer_dispose
  (JNIEnv *env, jobject obj)
{
  /* The actual underlying widget is owned by a different class.  So
     we just clean up the hash table here.  */
  NSA_DEL_PTR (env, obj);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxGroupPeer_remove
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

