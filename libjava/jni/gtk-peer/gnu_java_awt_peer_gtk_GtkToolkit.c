/* gtktoolkit.c -- Native portion of GtkToolkit
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
#include "gnu_java_awt_peer_gtk_GtkToolkit.h"
#include "gthread-jni.h"

#include <sys/time.h>

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
jmethodID postInsetsChangedEventID;
jmethodID windowGetWidthID;
jmethodID windowGetHeightID;

jmethodID beginNativeRepaintID;
jmethodID endNativeRepaintID;

jmethodID initComponentGraphicsID;
jmethodID initComponentGraphics2DID;
jmethodID setCursorID;

JavaVM *java_vm;

union env_union
{
  void *void_env;
  JNIEnv *jni_env;
};

JNIEnv *
gdk_env()
{
  union env_union tmp;
  g_assert((*java_vm)->GetEnv(java_vm, &tmp.void_env, JNI_VERSION_1_2) == JNI_OK);
  return tmp.jni_env;
}


GtkWindowGroup *global_gtk_window_group;

static void init_glib_threads(JNIEnv *, jint);

double dpi_conversion_factor;

static void init_dpi_conversion_factor (void);
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
Java_gnu_java_awt_peer_gtk_GtkToolkit_gtkInit (JNIEnv *env, 
					       jclass clazz __attribute__((unused)),
					       jint portableNativeSync)
{
  int argc = 1;
  char **argv;
  char *homedir, *rcpath = NULL;

  jclass gtkgenericpeer, gtkcomponentpeer, gtkchoicepeer, gtkwindowpeer, gtkscrollbarpeer, gtklistpeer,
    gtkmenuitempeer, gtktextcomponentpeer, window, gdkgraphics, gdkgraphics2d;

  gtkgenericpeer = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GtkGenericPeer");

  NSA_INIT (env, gtkgenericpeer);

  g_assert((*env)->GetJavaVM(env, &java_vm) == 0);

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
  gdkgraphics = (*env)->FindClass (env,
                                   "gnu/java/awt/peer/gtk/GdkGraphics");
  gdkgraphics2d = (*env)->FindClass (env,
                                     "gnu/java/awt/peer/gtk/GdkGraphics2D");
  setBoundsCallbackID = (*env)->GetMethodID (env, window,
					     "setBoundsCallback",
					     "(IIII)V");

  postMenuActionEventID = (*env)->GetMethodID (env, gtkmenuitempeer,
					       "postMenuActionEvent",
					       "()V");
  postMouseEventID = (*env)->GetMethodID (env, gtkcomponentpeer, 
                                          "postMouseEvent", "(IJIIIIZ)V");
  setCursorID = (*env)->GetMethodID (env, gtkcomponentpeer,
                                     "setCursor", "()V");
  beginNativeRepaintID = (*env)->GetMethodID (env, gtkcomponentpeer, 
                                              "beginNativeRepaint", "()V");

  endNativeRepaintID = (*env)->GetMethodID (env, gtkcomponentpeer, 
                                            "endNativeRepaint", "()V");

  postConfigureEventID = (*env)->GetMethodID (env, gtkwindowpeer, 
					      "postConfigureEvent", "(IIII)V");
  postWindowEventID = (*env)->GetMethodID (env, gtkwindowpeer,
					   "postWindowEvent",
					   "(ILjava/awt/Window;I)V");
  postInsetsChangedEventID = (*env)->GetMethodID (env, gtkwindowpeer,
						  "postInsetsChangedEvent",
						  "(IIII)V");
  windowGetWidthID = (*env)->GetMethodID (env, gtkwindowpeer,
					  "getWidth", "()I");
  windowGetHeightID = (*env)->GetMethodID (env, gtkwindowpeer,
					  "getHeight", "()I");

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
  initComponentGraphicsID = (*env)->GetMethodID (env, gdkgraphics,
                                                 "initComponentGraphics",
                                                 "()V");
  initComponentGraphics2DID = (*env)->GetMethodID (env, gdkgraphics2d,
                                                   "initComponentGraphics2D",
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
  if (!g_thread_supported ())
    {
      if (portableNativeSync)
        g_thread_init ( &portable_native_sync_jni_functions );
      else
        g_thread_init ( NULL );
    }
  else
    {
      /* Warn if portable native sync is desired but the threading
         system is already initialized.  In that case we can't
         override the threading implementation with our portable
         native sync functions. */
      if (portableNativeSync)
        g_printerr ("peer warning: portable native sync disabled.\n");
    }

  /* Debugging progress message; uncomment if needed: */
  /*   printf("called gthread init\n"); */
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

static int
within_human_latency_tolerance(struct timeval *init)
{
  struct timeval curr;
  unsigned long milliseconds_elapsed;

  gettimeofday(&curr, NULL);
  
  milliseconds_elapsed = (((curr.tv_sec * 1000) + (curr.tv_usec / 1000))
			  - ((init->tv_sec * 1000) + (init->tv_usec / 1000)));
  
  return milliseconds_elapsed < 100;
}


JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_iterateNativeQueue
(JNIEnv *env, 
 jobject self __attribute__((unused)),
 jobject lockedQueue,
 jboolean block)
{
  /* We're holding an EventQueue lock, and we're about to acquire the GDK
   * lock before dropping the EventQueue lock. This can deadlock if someone
   * holds the GDK lock and wants to acquire the EventQueue lock; however
   * all callbacks from GTK happen with the GDK lock released, so this
   * would only happen in an odd case such as some JNI helper code
   * acquiring the GDK lock and calling back into
   * EventQueue.getNextEvent().
   */

  struct timeval init;
  gettimeofday(&init, NULL);

  gdk_threads_enter ();
  (*env)->MonitorExit (env, lockedQueue);

  if (block)
    {
      
      /* If we're blocking-when-empty, we want a do .. while loop. */
      do 
	gtk_main_iteration ();
      while (within_human_latency_tolerance (&init) 
	     && gtk_events_pending ());
    }
  else
    {
      /* If we're not blocking-when-empty, we want a while loop. */
      while (within_human_latency_tolerance (&init) 
	     && gtk_events_pending ())
	gtk_main_iteration ();      
    }
  
  (*env)->MonitorEnter (env, lockedQueue);
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_wakeNativeQueue
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  g_main_context_wakeup (NULL);
}

JNIEXPORT jboolean JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_nativeQueueEmpty
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  jboolean empty = FALSE;
  gdk_threads_enter ();
  empty = ! gtk_events_pending();
  gdk_threads_leave ();
  return empty;
}


static jint gdk_color_to_java_color (GdkColor color);


JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_beep
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  gdk_threads_enter ();
  gdk_beep ();
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_sync
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  gdk_threads_enter ();
  gdk_flush ();
  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_getScreenSizeDimensions
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
   jintArray jdims)
{
  jint *dims = (*env)->GetIntArrayElements (env, jdims, 0);  

  gdk_threads_enter ();

  dims[0] = gdk_screen_width ();
  dims[1] = gdk_screen_height ();

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements(env, jdims, dims, 0);
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_getScreenResolution
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  jint res;

  gdk_threads_enter ();

  res = gdk_screen_width () / (gdk_screen_width_mm () / 25.4);

  gdk_threads_leave ();
  return res;
}

#define CONVERT(type, state) \
  gdk_color_to_java_color (style->type[GTK_STATE_ ## state])

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkToolkit_loadSystemColors
  (JNIEnv *env, jobject obj __attribute__((unused)),
   jintArray jcolors)
{
  jint *colors;
  GtkStyle *style;

  colors = (*env)->GetIntArrayElements (env, jcolors, 0);

  gdk_threads_enter ();

  style = gtk_widget_get_default_style ();

  colors[AWT_DESKTOP]                 = CONVERT (bg, SELECTED);
  colors[AWT_ACTIVE_CAPTION]          = CONVERT (bg, SELECTED);
  colors[AWT_ACTIVE_CAPTION_TEXT]     = CONVERT (text, SELECTED);
  colors[AWT_ACTIVE_CAPTION_BORDER]   = CONVERT (fg, NORMAL);
  colors[AWT_INACTIVE_CAPTION]        = CONVERT (base, INSENSITIVE);
  colors[AWT_INACTIVE_CAPTION_TEXT]   = CONVERT (fg, INSENSITIVE);
  colors[AWT_INACTIVE_CAPTION_BORDER] = CONVERT (fg, INSENSITIVE);
  colors[AWT_WINDOW]                  = CONVERT (bg, NORMAL);
  colors[AWT_WINDOW_BORDER]           = CONVERT (fg, NORMAL);
  colors[AWT_WINDOW_TEXT]             = CONVERT (fg, NORMAL);
  colors[AWT_MENU]                    = CONVERT (bg, NORMAL);
  colors[AWT_MENU_TEXT]               = CONVERT (fg, NORMAL);
  colors[AWT_TEXT]                    = CONVERT (bg, NORMAL);
  colors[AWT_TEXT_TEXT]               = CONVERT (fg, NORMAL);
  colors[AWT_TEXT_HIGHLIGHT]          = CONVERT (bg, SELECTED);
  colors[AWT_TEXT_HIGHLIGHT_TEXT]     = CONVERT (fg, SELECTED);
  colors[AWT_TEXT_INACTIVE_TEXT]      = CONVERT (bg, INSENSITIVE);
  colors[AWT_CONTROL]                 = CONVERT (bg, NORMAL);
  colors[AWT_CONTROL_TEXT]            = CONVERT (fg, NORMAL);
  colors[AWT_CONTROL_HIGHLIGHT]       = CONVERT (base, ACTIVE);
  colors[AWT_CONTROL_LT_HIGHLIGHT]    = CONVERT (bg, PRELIGHT);
  colors[AWT_CONTROL_SHADOW]          = CONVERT (bg, ACTIVE);
  colors[AWT_CONTROL_DK_SHADOW]       = CONVERT (fg, INSENSITIVE);
  colors[AWT_SCROLLBAR]               = CONVERT (base, INSENSITIVE);
  colors[AWT_INFO]                    = CONVERT (bg, NORMAL);
  colors[AWT_INFO_TEXT]               = CONVERT (fg, NORMAL);

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements(env, jcolors, colors, 0);
}

#undef CONVERT

static jint
gdk_color_to_java_color (GdkColor gdk_color)
{
  guchar red;
  guchar green;
  guchar blue;
  float factor;

  factor = 255.0 / 65535.0;

  red   = (float) gdk_color.red   * factor;
  green = (float) gdk_color.green * factor;
  blue  = (float) gdk_color.blue  * factor;

  return (jint) (0xff000000 | (red << 16) | (green << 8) | blue);
}
