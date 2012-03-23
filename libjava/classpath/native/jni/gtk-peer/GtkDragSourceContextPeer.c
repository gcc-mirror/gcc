/* gtkdragsourcecontextpeer.c -- Native implementation of GtkDragSourceContextPeer
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

#include "gtkpeer.h"
#include "gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer.h"

#include <jni.h>
#include <gtk/gtk.h>

static GtkWidget * get_widget (GtkWidget *widget);
static void connect_signals_for_widget (GtkWidget *widget);
                                         
#define ACTION_COPY 1
#define ACTION_MOVE 2
#define ACTION_COPY_OR_MOVE  3
#define ACTION_LINK 1073741824

#define AWT_DEFAULT_CURSOR 0
#define AWT_CROSSHAIR_CURSOR 1
#define AWT_TEXT_CURSOR 2
#define AWT_WAIT_CURSOR 3
#define AWT_SW_RESIZE_CURSOR 4
#define AWT_SE_RESIZE_CURSOR 5
#define AWT_NW_RESIZE_CURSOR 6
#define AWT_NE_RESIZE_CURSOR 7
#define AWT_N_RESIZE_CURSOR 8
#define AWT_S_RESIZE_CURSOR 9
#define AWT_W_RESIZE_CURSOR 10
#define AWT_E_RESIZE_CURSOR 11
#define AWT_HAND_CURSOR 12
#define AWT_MOVE_CURSOR 13

static jmethodID dragEnterID;
static jmethodID dragExitID;
static jmethodID dragDropEndID;
static jmethodID dragMouseMovedID;
static jmethodID dragOverID;
static jmethodID dragActionChangedID;
static jmethodID acceptDragID;
static jmethodID rejectDragID;
static jmethodID acceptDropID;
static jmethodID rejectDropID;
static jmethodID dropCompleteID;

GtkWidget *widget;
GtkWidget *tgt;
jobject gref;
jobject javaObj;

JNIEXPORT void JNICALL 
Java_gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer_create 
  (JNIEnv *env, jobject obj, jobject comp)
{  
  gdk_threads_enter ();
 
  javaObj = obj;
  gtkpeer_set_global_ref (env, obj);  
  gtkpeer_set_global_ref (env, comp);
  
  gref = gtkpeer_get_widget (env, comp);
  widget = get_widget (GTK_WIDGET (gref));

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer_nativeSetCursor
  (JNIEnv *env __attribute__((unused)), jobject obj, jint type)
{
  GdkWindow *win;
  GdkCursorType gdk_cursor_type;
  GdkCursor *gdk_cursor;

  gdk_threads_enter ();

  javaObj = obj;
  
  switch (type)
    {
    case AWT_CROSSHAIR_CURSOR:
      gdk_cursor_type = GDK_CROSSHAIR;
      break;
    case AWT_TEXT_CURSOR:
      gdk_cursor_type = GDK_XTERM;
      break;
    case AWT_WAIT_CURSOR:
      gdk_cursor_type = GDK_WATCH;
      break;
    case AWT_SW_RESIZE_CURSOR:
      gdk_cursor_type = GDK_BOTTOM_LEFT_CORNER;
      break;
    case AWT_SE_RESIZE_CURSOR:
      gdk_cursor_type = GDK_BOTTOM_RIGHT_CORNER;
      break;
    case AWT_NW_RESIZE_CURSOR:
      gdk_cursor_type = GDK_TOP_LEFT_CORNER;
      break;
    case AWT_NE_RESIZE_CURSOR:
      gdk_cursor_type = GDK_TOP_RIGHT_CORNER;
      break;
    case AWT_N_RESIZE_CURSOR:
      gdk_cursor_type = GDK_TOP_SIDE;
      break;
    case AWT_S_RESIZE_CURSOR:
      gdk_cursor_type = GDK_BOTTOM_SIDE;
      break;
    case AWT_W_RESIZE_CURSOR:
      gdk_cursor_type = GDK_LEFT_SIDE;
      break;
    case AWT_E_RESIZE_CURSOR:
      gdk_cursor_type = GDK_RIGHT_SIDE;
      break;
    case AWT_HAND_CURSOR:
      gdk_cursor_type = GDK_HAND2;
      break;
    case AWT_MOVE_CURSOR:
      gdk_cursor_type = GDK_FLEUR;
      break;
    default:
      gdk_cursor_type = GDK_LEFT_PTR;
    }
  
  win = widget->window;
  if ((widget->window) == NULL)
    win = widget->window;
    
  gdk_cursor = gdk_cursor_new (gdk_cursor_type);

  gdk_window_set_cursor (win, gdk_cursor);
  gdk_cursor_unref (gdk_cursor);

  gdk_flush();
  
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer_connectSignals 
  (JNIEnv *env, jobject obj, jobject comp)
{
  jclass gtkdragsourcecontextpeer;
  jclass gtkdroptargetcontextpeer;
  
  gdk_threads_enter ();

  javaObj = obj;
  gref = gtkpeer_get_global_ref (env, comp);
  
  connect_signals_for_widget (widget);

  gtkdragsourcecontextpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                         "gnu/java/awt/dnd/peer/gtk/GtkDragSourceContextPeer");
  
  dragEnterID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), 
                                            gtkdragsourcecontextpeer,
                                               "dragEnter", "(II)V");
  dragExitID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), 
                                            gtkdragsourcecontextpeer,
                                               "dragExit", "(III)V");
  dragDropEndID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), 
                                             gtkdragsourcecontextpeer,
                                               "dragDropEnd", "(IZII)V");
  dragMouseMovedID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                             gtkdragsourcecontextpeer,
                                               "dragMouseMoved", "(II)V");
  dragOverID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                             gtkdragsourcecontextpeer,
                                               "dragOver", "(II)V");
  dragActionChangedID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                             gtkdragsourcecontextpeer,
                                               "dragActionChanged", "(II)V");


  gtkdroptargetcontextpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                         "gnu/java/awt/dnd/peer/gtk/GtkDropTargetContextPeer");
                         
  acceptDragID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), 
                                            gtkdroptargetcontextpeer,
                                               "acceptDrag", "(I)V");
  rejectDragID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), 
                                            gtkdroptargetcontextpeer,
                                               "rejectDrag", "()V");
  acceptDropID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), 
                                             gtkdroptargetcontextpeer,
                                               "acceptDrop", "(I)V");
  rejectDropID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                             gtkdroptargetcontextpeer,
                                               "rejectDrop", "()V");
  dropCompleteID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                             gtkdroptargetcontextpeer,
                                               "dropComplete", "(Z)V");
  
  gdk_threads_leave ();
}

static void
connect_signals_for_widget (GtkWidget *w __attribute__((unused)))
{
  /* FIXME: Not implemented. */
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer_setTarget
  (JNIEnv *env, jobject obj, jobject target)
{
  void *ptr;
  
  gdk_threads_enter ();
  
  javaObj = obj;
  ptr = gtkpeer_get_widget (env, target);
  tgt = get_widget (GTK_WIDGET (ptr));
  connect_signals_for_widget (tgt);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_dnd_peer_gtk_GtkDragSourceContextPeer_nativeStartDrag
  (JNIEnv *env, jobject obj, jobject img, jint x, jint y, jint act,
   jstring target)
{
  const gchar *data;
  GtkTargetEntry tar[1];
  GdkEvent *event;
  GdkPixbuf *image = NULL;
  GdkDragContext *context = NULL;
  GdkDragAction action = GDK_ACTION_DEFAULT;
  
  gdk_threads_enter ();
  
  javaObj = obj;

  data = (*env)->GetStringUTFChars (env, target, NULL);
  tar[0].target = (gchar *) data;  
  event = gdk_event_new (GDK_ALL_EVENTS_MASK);
  
  switch (act)
    {
    case ACTION_COPY:
      action = GDK_ACTION_COPY;
      break;
    case ACTION_MOVE:
      action = GDK_ACTION_MOVE;
      break;
    case ACTION_COPY_OR_MOVE:
      action = GDK_ACTION_COPY | GDK_ACTION_MOVE;
      break;
    case ACTION_LINK:
      action = GDK_ACTION_LINK;
      break;
    default:
      action = GDK_ACTION_DEFAULT;
    }

  gtk_drag_dest_set (widget, GTK_DEST_DEFAULT_ALL, tar,
                                             sizeof (tar) / sizeof (GtkTargetEntry),
                                             action);
  context = gtk_drag_begin (widget, 
             gtk_target_list_new (tar, sizeof (tar) / sizeof (GtkTargetEntry)), 
             action, GDK_BUTTON1_MASK | GDK_BUTTON2_MASK, event);

  if (img != NULL)
  {
    image = cp_gtk_image_get_pixbuf (env, img);
    gtk_drag_set_icon_pixbuf (context, image, x, y);
  }
  
  if (tgt != NULL)
    gtk_drag_dest_set (tgt, GTK_DEST_DEFAULT_ALL, tar,
                                        sizeof (tar) / sizeof (GtkTargetEntry),
                                         action);

  gdk_event_free (event);
  (*env)->ReleaseStringUTFChars (env, target, data);  
  
  gdk_threads_leave ();
}

static GtkWidget *
get_widget (GtkWidget *widget)
{
  GtkWidget *w;
  
  if (GTK_IS_EVENT_BOX (widget) || GTK_IS_CONTAINER (widget))
    w = gtk_bin_get_child (GTK_BIN(widget));
  else
    w = widget;

  return w;
}
