/* gtklistpeer.c -- Native implementation of GtkListPeer
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
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"
#include "gnu_java_awt_peer_gtk_GtkListPeer.h"

static void item_select (GtkCList *list __attribute__((unused)),
	                 int row, int col __attribute__((unused)),
	                 GdkEventButton *event __attribute__((unused)), 
	                 jobject peer_obj);
static void item_unselect (GtkCList *list __attribute__((unused)),
	                   int row,
	                   int col __attribute__((unused)),
	                   GdkEventButton *event __attribute__((unused)),
	                   jobject peer_obj);

#define CLIST_FROM_SW(obj) (GTK_CLIST(GTK_SCROLLED_WINDOW (obj)->container.child))

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkListPeer_create
  (JNIEnv *env, jobject obj)
{
  GtkWidget *list, *sw;

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  gdk_threads_enter ();
  
  list = gtk_clist_new (1);
  gtk_widget_show (list);
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw), 
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (sw), list);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, sw);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkListPeer_connectJObject
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_widget_realize (GTK_WIDGET (ptr));

  connect_awt_hook (env, obj, 1, GTK_WIDGET (ptr)->window);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkListPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  GtkCList *list;
  void *ptr = NSA_GET_PTR (env, obj);
  jobject *gref = NSA_GET_GLOBAL_REF (env, obj);
  g_assert (gref);

  gdk_threads_enter ();

  gtk_widget_realize (GTK_WIDGET (ptr));

  /* connect selectable hook */
  
  list = CLIST_FROM_SW (ptr);

  g_signal_connect (G_OBJECT (list), "select_row", 
		      GTK_SIGNAL_FUNC (item_select), *gref);

  g_signal_connect (G_OBJECT (list), "unselect_row", 
		      GTK_SIGNAL_FUNC (item_unselect), *gref);

  /* Connect the superclass signals.  */
  /* FIXME: Cannot do that here or it will get the sw and not the list.
     We must a generic way of doing this. */
  /* Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals (env, peer_obj); */
  g_signal_connect (GTK_OBJECT (list), "event", 
                    G_CALLBACK (pre_event_handler), *gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkListPeer_append 
  (JNIEnv *env, jobject obj, jobjectArray items)
{
  void *ptr;
  GtkCList *list;
  jint count, i;

  ptr = NSA_GET_PTR (env, obj);

  count = (*env)->GetArrayLength (env, items);

  gdk_threads_enter ();
  list = CLIST_FROM_SW (ptr);
  for (i = 0; i < count; i++) 
    {
      const char *text;
      jobject item;

      item = (*env)->GetObjectArrayElement (env, items, i);

      text = (*env)->GetStringUTFChars (env, item, NULL);
      gtk_clist_append (list, (char **)&text);
      (*env)->ReleaseStringUTFChars (env, item, text);
    }

  gtk_clist_columns_autosize (list);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_add
  (JNIEnv *env, jobject obj, jstring text, jint index)
{
  void *ptr;
  const char *str;
    
  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, text, NULL);

  gdk_threads_enter ();
  gtk_clist_insert (CLIST_FROM_SW (ptr), index, (char **)&str);
  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, text, str);
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_delItems
  (JNIEnv *env, jobject obj, jint start, jint end)
{
  void *ptr;
  GtkCList *list;
  jint i;
    
  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  list = CLIST_FROM_SW (ptr);

  if (end == -1)		/* special case for removing all rows */
    gtk_clist_clear (list);
  else
    {
      gtk_clist_freeze (list);
      for (i = end; i >= start; i--)
        gtk_clist_remove (list, i);
      gtk_clist_thaw (list);
    }

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_select
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;
    
  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  gtk_clist_select_row (CLIST_FROM_SW (ptr), index, 0);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_deselect
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  gtk_clist_unselect_row (CLIST_FROM_SW (ptr), index, 0);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_getSize
  (JNIEnv *env, jobject obj, jint rows, jintArray jdims)
{
  void *ptr;
  jint *dims;
  GtkWidget *list;
  GtkScrolledWindow *sw;
  GtkRequisition myreq;

  dims = (*env)->GetIntArrayElements (env, jdims, NULL);
  dims[0] = dims[1] = 0;

  if (rows < 3)
    rows = 3;

  ptr = NSA_GET_PTR (env, obj);
  gdk_threads_enter ();

  list = GTK_WIDGET (CLIST_FROM_SW (ptr));
  sw = GTK_SCROLLED_WINDOW (ptr);

  gtk_widget_size_request(GTK_WIDGET(sw), &myreq);
  dims[1]=myreq.height;
  dims[0]=myreq.width;
  
  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements (env, jdims, dims, 0);
}


JNIEXPORT jintArray JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_getSelectedIndexes
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkCList *list;
  jintArray selection;
  jint *sel;
  GList *child;
  jint count, i;

  ptr = NSA_GET_PTR (env, obj);
  gdk_threads_enter ();

  list = CLIST_FROM_SW (ptr);
  count = g_list_length (list->selection);

  selection = (*env)->NewIntArray (env, count);
  sel = (*env)->GetIntArrayElements (env, selection, NULL);  

  for (i = 0, child = list->selection; i < count; i++)
    {
      sel[i] = GPOINTER_TO_INT (child->data);
      child = g_list_next (child);
    }
  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements (env, selection, sel, 0);

  return selection;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_makeVisible
  (JNIEnv *env, jobject obj, jint index)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  gtk_clist_moveto (CLIST_FROM_SW (ptr), index, 0, 0.5, 0.5);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkListPeer_setMultipleMode
  (JNIEnv *env, jobject obj, jboolean mode)
{
  void *ptr;
    
  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  gtk_clist_set_selection_mode (CLIST_FROM_SW (ptr),
				mode ? GTK_SELECTION_MULTIPLE : 
				       GTK_SELECTION_SINGLE);
  gdk_threads_leave ();
}

static void
item_select (GtkCList *list __attribute__((unused)),
	     int row, int col __attribute__((unused)),
	     GdkEventButton *event __attribute__((unused)), 
	     jobject peer_obj)
{
  //g_print ("select_row\n");
  (*gdk_env)->CallVoidMethod (gdk_env, peer_obj,
			      postListItemEventID,
			      row,
			      (jint) AWT_ITEM_SELECTED);
}

static void
item_unselect (GtkCList *list __attribute__((unused)),
	       int row,
	       int col __attribute__((unused)),
	       GdkEventButton *event __attribute__((unused)),
	       jobject peer_obj)
{
  //g_print ("unselect_row\n");
  (*gdk_env)->CallVoidMethod (gdk_env, peer_obj,
			      postListItemEventID,
			      row,
	   		      (jint) AWT_ITEM_DESELECTED);
}

