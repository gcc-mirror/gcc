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

GtkWindowGroup *global_gtk_window_group;

static void init_glib_threads(JNIEnv *, jint);

double dpi_conversion_factor;

static void init_dpi_conversion_factor ();
static void dpi_changed_cb (GtkSettings  *settings,
                            GParamSpec   *pspec);

/*
 * Call gtk_init.  It is very important that this happen before any other
 * gtk calls.
 *
 * The portableNativeSync argument may have the values:
 *   1 if the Java property gnu.classpath.awt.gtk.portable.native.sync
 *     is set to "true".  
 *   0 if it is set to "false"
 *  -1 if unset.
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkMainThread_gtkInit (JNIEnv *env, jclass clazz,
                                                  jint portableNativeSync)
{
  int argc = 1;
  char **argv;
  char *homedir, *rcpath = NULL;
/*    jclass gtkgenericpeer; */
  jclass gtkcomponentpeer, gtkchoicepeer, gtkwindowpeer, gtkscrollbarpeer, gtklistpeer,
    gtkmenuitempeer, gtktextcomponentpeer, window;

  NSA_INIT (env, clazz);
  gdk_env = env;

  /* GTK requires a program's argc and argv variables, and requires that they
     be valid.   Set it up. */
  argv = (char **) g_malloc (sizeof (char *) * 2);
  argv[0] = (char *) g_malloc(1);
#if 1
  strcpy(argv[0], "");
#else  /* The following is a more efficient alternative, but less intuitively
	* expresses what we are trying to do.   This code is only run once, so
	* I'm going for intuitive. */
  argv[0][0] = '\0';
#endif
  argv[1] = NULL;

  init_glib_threads(env, portableNativeSync);

  /* From GDK 2.0 onwards we have to explicitly call gdk_threads_init */
  gdk_threads_init();

  gtk_init (&argc, &argv);

  gdk_rgb_init ();
  gtk_widget_set_default_colormap (gdk_rgb_get_cmap ());
  gtk_widget_set_default_visual (gdk_rgb_get_visual ());

  /* Make sure queued calls don't get sent to GTK/GDK while 
     we're shutting down. */
  atexit (gdk_threads_enter);

  gdk_event_handler_set ((GdkEventFunc)awt_event_handler, NULL, NULL);

  if ((homedir = getenv ("HOME")))
    {
      rcpath = (char *) g_malloc (strlen (homedir) + strlen (RC_FILE) + 2);
      sprintf (rcpath, "%s/%s", homedir, RC_FILE);
    }
  
  gtk_rc_parse ((rcpath) ? rcpath : RC_FILE);

  g_free (rcpath);
  g_free (argv[0]);
  g_free (argv);

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

  init_dpi_conversion_factor ();
}


/** Initialize GLIB's threads properly, based on the value of the
    gnu.classpath.awt.gtk.portable.native.sync Java system property.  If
    that's unset, use the PORTABLE_NATIVE_SYNC config.h macro.  (TODO: 
    In some release following 0.10, that config.h macro will go away.)
    */ 
static void 
init_glib_threads(JNIEnv *env, jint portableNativeSync)
{
  if (portableNativeSync < 0)
    {
#ifdef PORTABLE_NATIVE_SYNC /* Default value, if not set by the Java system
                               property */ 
      portableNativeSync = 1;
#else
      portableNativeSync = 0;
#endif
    }
  
  (*env)->GetJavaVM( env, &the_vm );
  if (portableNativeSync)
    g_thread_init ( &portable_native_sync_jni_functions );
  else
    g_thread_init ( NULL );

  /* Debugging progress message; uncomment if needed: */
  /*   printf("called gthread init\n"); */
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

/* This is a big hack, needed until this pango bug is resolved:
   http://bugzilla.gnome.org/show_bug.cgi?id=119081.
   See: http://mail.gnome.org/archives/gtk-i18n-list/2003-August/msg00001.html
   for details. */
static void
init_dpi_conversion_factor ()
{
  GtkSettings *settings = gtk_settings_get_default ();
  GObjectClass *klass;

  klass = G_OBJECT_CLASS (GTK_SETTINGS_GET_CLASS (settings));
  if (g_object_class_find_property (klass, "gtk-xft-dpi"))
    {
      int int_dpi;
      g_object_get (settings, "gtk-xft-dpi", &int_dpi, NULL);
      /* If int_dpi == -1 gtk-xft-dpi returns the default value. So we
	 have to do approximate calculation here.  */
      if (int_dpi < 0)
	dpi_conversion_factor = PANGO_SCALE * 72.0 / 96.;
      else
	dpi_conversion_factor = PANGO_SCALE * 72.0 / (int_dpi / PANGO_SCALE);

      g_signal_connect (settings, "notify::gtk-xft-dpi",
			G_CALLBACK (dpi_changed_cb), NULL);
    }
  else
    /* Approximate. */
    dpi_conversion_factor = PANGO_SCALE * 72.0 / 96.;
}

static void
dpi_changed_cb (GtkSettings  *settings,
		GParamSpec *pspec __attribute__((unused)))
{
  int int_dpi;
  g_object_get (settings, "gtk-xft-dpi", &int_dpi, NULL);
  if (int_dpi < 0)
    dpi_conversion_factor = PANGO_SCALE * 72.0 / 96.;
  else
    dpi_conversion_factor = PANGO_SCALE * 72.0 / (int_dpi / PANGO_SCALE);
}
