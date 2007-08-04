/* gtkchoicepeer.c -- Native implementation of GtkChoicePeer
   Copyright (C) 1998, 1999, 2006 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkChoicePeer.h"

static jmethodID postChoiceItemEventID;
static GtkWidget *choice_get_widget (GtkWidget *widget);

void
cp_gtk_choice_init_jni (void)
{
  jclass gtkchoicepeer;

  gtkchoicepeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                        "gnu/java/awt/peer/gtk/GtkChoicePeer");

  postChoiceItemEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkchoicepeer,
                                               "postChoiceItemEvent",
                                               "(I)V");
}

static void selection_changed_cb (GtkComboBox *combobox, jobject peer);

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_create 
  (JNIEnv *env, jobject obj)
{
  GtkWidget *combobox;
  GtkWidget *eventbox;

  gdk_threads_enter ();
  
  gtkpeer_set_global_ref (env, obj);
  
  eventbox = gtk_event_box_new ();
  combobox = gtk_combo_box_new_text ();
  gtk_container_add (GTK_CONTAINER (eventbox), combobox);
  gtk_widget_show (combobox);  

  gtkpeer_set_widget (env, obj, eventbox);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr = NULL;
  jobject gref;
  GtkWidget *bin;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  gref = gtkpeer_get_global_ref (env, obj);

  bin = choice_get_widget (GTK_WIDGET (ptr));

  /* Choice signals */
  g_signal_connect (G_OBJECT (bin), "changed",
                    G_CALLBACK (selection_changed_cb), gref);

  /* Component signals */
  cp_gtk_component_connect_signals (G_OBJECT (bin), gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_add 
  (JNIEnv *env, jobject obj, jstring item, jint index)
{
  void *ptr;
  const char *label;
  GtkWidget *bin;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  bin = choice_get_widget (GTK_WIDGET (ptr));
    
  label = (*env)->GetStringUTFChars (env, item, 0);      

  gtk_combo_box_insert_text (GTK_COMBO_BOX (bin), index, label);

  (*env)->ReleaseStringUTFChars (env, item, label);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_nativeRemove 
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;
  GtkWidget *bin;
  
  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  bin = choice_get_widget (GTK_WIDGET (ptr));

  /* First, unselect everything, to avoid problems when removing items. */
  gtk_combo_box_set_active (GTK_COMBO_BOX (bin), -1);
  gtk_combo_box_remove_text (GTK_COMBO_BOX (bin), index);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_nativeRemoveAll
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkTreeModel *model;
  GtkWidget *bin;
  gint count, i;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  bin = choice_get_widget (GTK_WIDGET (ptr));
  
  model = gtk_combo_box_get_model (GTK_COMBO_BOX (bin));
  count = gtk_tree_model_iter_n_children (model, NULL);

  /* First, unselect everything, to avoid problems when removing items. */
  gtk_combo_box_set_active (GTK_COMBO_BOX (bin), -1);

  for (i = count - 1; i >= 0; i--) {
    gtk_combo_box_remove_text (GTK_COMBO_BOX (bin), i);
  }

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_selectNative
  (JNIEnv *env, jobject obj, jint index)
{
  gdk_threads_enter ();

  Java_gnu_java_awt_peer_gtk_GtkChoicePeer_selectNativeUnlocked
    (env, obj, index);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_selectNativeUnlocked
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;
  GtkWidget *bin;
  
  ptr = gtkpeer_get_widget (env, obj);
  bin = choice_get_widget (GTK_WIDGET (ptr));
  gtk_combo_box_set_active (GTK_COMBO_BOX (bin), (gint)index);
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkChoicePeer_nativeGetSelected 
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int index;
  GtkWidget *bin;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  bin = choice_get_widget (GTK_WIDGET (ptr));
  
  index = gtk_combo_box_get_active (GTK_COMBO_BOX (bin));

  gdk_threads_leave ();

  return index;
}

static void
selection_changed_cb (GtkComboBox *combobox, jobject peer)
{
  gint index = gtk_combo_box_get_active(combobox);

  if (index >= 0)
    (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
					 postChoiceItemEventID, (jint)index );
}

static GtkWidget *
choice_get_widget (GtkWidget *widget)
{
  GtkWidget *wid;

  g_assert (GTK_IS_EVENT_BOX (widget));
  wid = gtk_bin_get_child (GTK_BIN(widget));

  return wid;
}
