/* gtkmainthread.c -- Native implementation of GtkMainThread
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
#include "gnu_java_awt_peer_gtk_GtkMainThread.h"
#include "gthread-jni.h"

#ifdef JVM_SUN
  struct state_table *native_state_table;
  struct state_table *native_global_ref_table;
#endif

jmethodID setBoundsCallbackID;

jmethodID postActionEventID;
jmethodID postMenuActionEventID;
jmethodID postMouseEventID;
jmethodID postConfigureEventID;
jmethodID postExposeEventID;
jmethodID postKeyEventID;
jmethodID postFocusEventID;
jmethodID postAdjustmentEventID;
jmethodID postItemEventID;
jmethodID choicePostItemEventID;
jmethodID postListItemEventID;
jmethodID postTextEventID;
jmethodID postWindowEventID;

JNIEnv *gdk_env;

#ifdef PORTABLE_NATIVE_SYNC
JavaVM *gdk_vm;
#endif

GtkWindowGroup *global_gtk_window_group;

/*
 * Call gtk_init.  It is very important that this happen before any other
 * gtk calls.
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkMainThread_gtkInit (JNIEnv *env, jclass clazz)
{
  int argc = 1;
  char **argv;
  char *homedir, *rcpath = NULL;
/*    jclass gtkgenericpeer; */
  jclass gtkcomponentpeer, gtkchoicepeer, gtkwindowpeer, gtkscrollbarpeer, gtklistpeer,
    gtkmenuitempeer, gtktextcomponentpeer, window;

  NSA_INIT (env, clazz);

  /* GTK requires a program's argc and argv variables, and requires that they
     be valid.  */

  argv = (char **) malloc (sizeof (char *) * 2);
  argv[0] = "";
  argv[1] = NULL;

  /* until we have JDK 1.2 JNI, assume we have a VM with threads that 
     match what GLIB was compiled for */
#ifdef PORTABLE_NATIVE_SYNC
  (*env)->GetJavaVM( env, &gdk_vm );
  g_thread_init ( &g_thread_jni_functions );
  printf("called gthread init\n");
#else
  g_thread_init ( NULL );
#endif

  /* From GDK 2.0 onwards we have to explicitly call gdk_threads_init */
  gdk_threads_init();

  gtk_init (&argc, &argv);

  gdk_rgb_init ();
  gtk_widget_set_default_colormap (gdk_rgb_get_cmap ());
  gtk_widget_set_default_visual (gdk_rgb_get_visual ());

  /* Make sure queued calls don't get sent to GTK/GDK while 
     we're shutting down. */
  atexit (gdk_threads_enter);

  gdk_env = env;
  gdk_event_handler_set ((GdkEventFunc)awt_event_handler, NULL, NULL);

  if ((homedir = getenv ("HOME")))
    {
      rcpath = (char *) malloc (strlen (homedir) + strlen (RC_FILE) + 2);
      sprintf (rcpath, "%s/%s", homedir, RC_FILE);
    }
  
  gtk_rc_parse ((rcpath) ? rcpath : RC_FILE);

  if (rcpath)
    free (rcpath);

  free (argv);

  /* setup cached IDs for posting GTK events to Java */
/*    gtkgenericpeer = (*env)->FindClass (env,  */
/*  				      "gnu/java/awt/peer/gtk/GtkGenericPeer"); */

  window = (*env)->FindClass (env, "java/awt/Window");

  gtkcomponentpeer = (*env)->FindClass (env,
				     "gnu/java/awt/peer/gtk/GtkComponentPeer");
  gtkchoicepeer = (*env)->FindClass (env,
				     "gnu/java/awt/peer/gtk/GtkChoicePeer");
  gtkwindowpeer = (*env)->FindClass (env,
				     "gnu/java/awt/peer/gtk/GtkWindowPeer");
  gtkscrollbarpeer = (*env)->FindClass (env, 
				     "gnu/java/awt/peer/gtk/GtkScrollbarPeer");
  gtklistpeer = (*env)->FindClass (env, "gnu/java/awt/peer/gtk/GtkListPeer");
  gtkmenuitempeer = (*env)->FindClass (env,
                                     "gnu/java/awt/peer/gtk/GtkMenuItemPeer");
  gtktextcomponentpeer = (*env)->FindClass (env,
                                     "gnu/java/awt/peer/gtk/GtkTextComponentPeer");
/*    gdkColor = (*env)->FindClass (env, */
/*  				"gnu/java/awt/peer/gtk/GdkColor"); */
/*    gdkColorID = (*env)->GetMethodID (env, gdkColor, "<init>", "(III)V"); */
/*    postActionEventID = (*env)->GetMethodID (env, gtkgenericpeer,  */
/*  					   "postActionEvent",  */
/*  					   "(Ljava/lang/String;I)V"); */

  setBoundsCallbackID = (*env)->GetMethodID (env, window,
					     "setBoundsCallback",
					     "(IIII)V");

  postMenuActionEventID = (*env)->GetMethodID (env, gtkmenuitempeer,
					       "postMenuActionEvent",
					       "()V");
  postMouseEventID = (*env)->GetMethodID (env, gtkcomponentpeer, 
					  "postMouseEvent", "(IJIIIIZ)V");
  postConfigureEventID = (*env)->GetMethodID (env, gtkwindowpeer, 
					      "postConfigureEvent", "(IIII)V");
  postWindowEventID = (*env)->GetMethodID (env, gtkwindowpeer,
					   "postWindowEvent",
					   "(ILjava/awt/Window;I)V");
  postExposeEventID = (*env)->GetMethodID (env, gtkcomponentpeer, 
					  "postExposeEvent", "(IIII)V");
  postKeyEventID = (*env)->GetMethodID (env, gtkcomponentpeer,
					"postKeyEvent", "(IJIICI)V");
  postFocusEventID = (*env)->GetMethodID (env, gtkcomponentpeer,
					  "postFocusEvent", "(IZ)V");
  postAdjustmentEventID = (*env)->GetMethodID (env, gtkscrollbarpeer,
					       "postAdjustmentEvent", 
					       "(II)V");
  postItemEventID = (*env)->GetMethodID (env, gtkcomponentpeer,
					 "postItemEvent", 
					 "(Ljava/lang/Object;I)V");
  choicePostItemEventID = (*env)->GetMethodID (env, gtkchoicepeer,
					 "choicePostItemEvent", 
					 "(Ljava/lang/String;I)V");
  postListItemEventID = (*env)->GetMethodID (env, gtklistpeer,
					     "postItemEvent",
					     "(II)V");
  postTextEventID = (*env)->GetMethodID (env, gtktextcomponentpeer,
					     "postTextEvent",
					     "()V");
  global_gtk_window_group = gtk_window_group_new ();
}

/*
 * Run gtk_main and block.
 */ 
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkMainThread_gtkMain
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  gdk_threads_enter ();
  gtk_main ();
  gdk_threads_leave ();
}
