
/* gtktoolkit.c -- Native portion of GtkToolkit
   Copyright (C) 1998, 1999, 2005, 2007, 2010  Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkToolkit.h"
#include "jcl.h"

#define RC_FILE ".classpath-gtkrc"

/* From java.awt.SystemColor */
#define AWT_DESKTOP                  0
#define AWT_ACTIVE_CAPTION           1
#define AWT_ACTIVE_CAPTION_TEXT      2
#define AWT_ACTIVE_CAPTION_BORDER    3
#define AWT_INACTIVE_CAPTION         4
#define AWT_INACTIVE_CAPTION_TEXT    5
#define AWT_INACTIVE_CAPTION_BORDER  6
#define AWT_WINDOW                   7
#define AWT_WINDOW_BORDER            8
#define AWT_WINDOW_TEXT              9
#define AWT_MENU                    10
#define AWT_MENU_TEXT               11
#define AWT_TEXT                    12
#define AWT_TEXT_TEXT               13
#define AWT_TEXT_HIGHLIGHT          14
#define AWT_TEXT_HIGHLIGHT_TEXT     15
#define AWT_TEXT_INACTIVE_TEXT      16
#define AWT_CONTROL                 17
#define AWT_CONTROL_TEXT            18
#define AWT_CONTROL_HIGHLIGHT       19
#define AWT_CONTROL_LT_HIGHLIGHT    20
#define AWT_CONTROL_SHADOW          21
#define AWT_CONTROL_DK_SHADOW       22
#define AWT_SCROLLBAR               23
#define AWT_INFO                    24
#define AWT_INFO_TEXT               25
#define AWT_NUM_COLORS              26

#define VK_SHIFT 16
#define VK_CONTROL 17
#define VK_ALT 18
#define VK_CAPS_LOCK 20
#define VK_META 157

static jclass gtkgenericpeer;
static jclass gtktoolkit;
static JavaVM *java_vm;
static jmethodID printCurrentThreadID;
static jmethodID setRunningID;

/**
 * The global AWT lock object.
 */
static jobject global_lock;

union env_union
{
  void *void_env;
  JNIEnv *jni_env;
};

JNIEnv *
cp_gtk_gdk_env()
{
  union env_union tmp;
  g_assert((*java_vm)->GetEnv(java_vm, &tmp.void_env, JNI_VERSION_1_2) == JNI_OK);
  return tmp.jni_env;
}


GtkWindowGroup *cp_gtk_global_window_group;
double cp_gtk_dpi_conversion_factor;

static void jni_lock_cb();
static void jni_unlock_cb();
static void init_glib_threads(JNIEnv*, jint, jobject);
static gboolean post_set_running_flag (gpointer);
static gboolean set_running_flag (gpointer);
static gboolean clear_running_flag (gpointer);
static void init_dpi_conversion_factor (void);
static void dpi_changed_cb (GtkSettings  *settings,
                            GParamSpec   *pspec);

#if GTK_MINOR_VERSION > 4
static GLogFunc old_glog_func;
static void glog_func (const gchar *log_domain,
		       GLogLevelFlags log_level,
		       const gchar *message,
		       gpointer user_data);
#endif

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkToolkit_initIDs
(JNIEnv *env, jclass cls __attribute__((unused)))
{
  gtkpeer_init_pointer_IDs(env);
}

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
					       jint portableNativeSync,
                                               jobject lock)
{
  int argc = 1;
  char **argv;
  char *homedir, *rcpath = NULL;

  gtkgenericpeer = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GtkGenericPeer");

  gtkgenericpeer = (*env)->NewGlobalRef(env, gtkgenericpeer);

  printCurrentThreadID = (*env)->GetStaticMethodID (env, gtkgenericpeer,
                                                    "printCurrentThread", "()V");
 
  g_assert((*env)->GetJavaVM(env, &java_vm) == 0);

  /* GTK requires a program's argc and argv variables, and requires that they
     be valid.  Set it up. */
  argv = (char **) g_malloc (sizeof (char *) * 2);
  argv[0] = (char *) g_malloc(1);
  argv[0][0] = '\0';
  argv[1] = NULL;
  
  init_glib_threads(env, portableNativeSync, lock);

  /* From GDK 2.0 onwards we have to explicitly call gdk_threads_init */
  gdk_threads_init();

  gtk_init (&argc, &argv);

#if SYNCHRONIZE_GDK
  XSynchronize (GDK_DISPLAY_XDISPLAY (gdk_display_get_default ()), True);
#endif

  gtk_widget_set_default_colormap (gdk_rgb_get_colormap ());

  if ((homedir = getenv ("HOME")))
    {
      rcpath = (char *) g_malloc (strlen (homedir) + strlen (RC_FILE) + 2);
      sprintf (rcpath, "%s/%s", homedir, RC_FILE);
    }
  
  gtk_rc_parse ((rcpath) ? rcpath : RC_FILE);

  g_free (rcpath);
  g_free (argv[0]);
  g_free (argv);

  /* On errors or warning print a whole stacktrace. */
#if GTK_MINOR_VERSION > 4
  old_glog_func = g_log_set_default_handler (&glog_func, NULL);
#endif

  cp_gtk_button_init_jni (env);
  cp_gtk_checkbox_init_jni ();
  cp_gtk_choice_init_jni ();
  cp_gtk_component_init_jni ();
  cp_gtk_filedialog_init_jni ();
  cp_gtk_list_init_jni ();
  cp_gtk_menuitem_init_jni ();
  cp_gtk_scrollbar_init_jni ();
  cp_gtk_textcomponent_init_jni ();
  cp_gtk_window_init_jni ();

  cp_gtk_global_window_group = gtk_window_group_new ();

  init_dpi_conversion_factor ();

  gtktoolkit = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GtkMainThread");
  gtktoolkit = (*env)->NewGlobalRef(env, gtktoolkit); /* bug fix #40889 */
  setRunningID = (*env)->GetStaticMethodID (env, gtktoolkit,
                                            "setRunning", "(Z)V");
}

/**
 * A callback function that implements gdk_threads_enter(). This is
 * implemented to wrap the JNI MonitorEnter() function.
 */
static void jni_lock_cb()
{
  JNIEnv * env = cp_gtk_gdk_env();
  if ((*env)->MonitorEnter(env, global_lock) != JNI_OK)
    {
      printf("failure while entering GTK monitor\n");
    }
}

/**
 * A callback function that implements gdk_threads_leave(). This is
 * implemented to wrap the JNI MonitorExit() function.
 */
static void jni_unlock_cb()
{

  JNIEnv * env = cp_gtk_gdk_env();
  if ((*env)->MonitorExit(env, global_lock))
    {
      printf("failure while exiting GTK monitor\n");
    }
}

/** Initialize GLIB's threads properly, based on the value of the
    gnu.classpath.awt.gtk.portable.native.sync Java system property.  If
    that's unset, use the PORTABLE_NATIVE_SYNC config.h macro.  (TODO: 
    In some release following 0.10, that config.h macro will go away.)
    */ 
static void 
init_glib_threads(JNIEnv *env, jint portableNativeSync, jobject lock)
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
  
  if (!g_thread_supported ())
    {
      if (portableNativeSync)
        {
          global_lock = (*env)->NewGlobalRef(env, lock);
          gdk_threads_set_lock_functions(&jni_lock_cb, &jni_unlock_cb);
        }
      g_thread_init(NULL);
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

void
cp_gtk_print_current_thread (void)
{
  (*cp_gtk_gdk_env())->CallStaticVoidMethod (cp_gtk_gdk_env(), gtkgenericpeer, printCurrentThreadID);
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
	cp_gtk_dpi_conversion_factor = PANGO_SCALE * 72.0 / 96.;
      else
	cp_gtk_dpi_conversion_factor =
          PANGO_SCALE * 72.0 / (int_dpi / PANGO_SCALE);

      g_signal_connect (settings, "notify::gtk-xft-dpi",
			G_CALLBACK (dpi_changed_cb), NULL);
    }
  else
    /* Approximate. */
    cp_gtk_dpi_conversion_factor = PANGO_SCALE * 72.0 / 96.;
}

static void
dpi_changed_cb (GtkSettings  *settings,
		GParamSpec *pspec __attribute__((unused)))
{
  int int_dpi;
  g_object_get (settings, "gtk-xft-dpi", &int_dpi, NULL);
  if (int_dpi < 0)
    cp_gtk_dpi_conversion_factor = PANGO_SCALE * 72.0 / 96.;
  else
    cp_gtk_dpi_conversion_factor =
      PANGO_SCALE * 72.0 / (int_dpi / PANGO_SCALE);
}

#if GTK_MINOR_VERSION > 4
static void
glog_func (const gchar *log_domain,
	   GLogLevelFlags log_level,
	   const gchar *message,
	   gpointer user_data)
{
  old_glog_func (log_domain, log_level, message, user_data);
  if (log_level & (G_LOG_LEVEL_ERROR
		   | G_LOG_LEVEL_CRITICAL
		   | G_LOG_LEVEL_WARNING))
    {
      JNIEnv *env = cp_gtk_gdk_env ();
      jthrowable *exc = (*env)->ExceptionOccurred(env);
      gchar *detail = g_strconcat (log_domain, ": ", message, NULL);
      JCL_ThrowException (env, "java/lang/InternalError", detail);
      g_free (detail);
      (*env)->ExceptionDescribe (env);
      if (exc != NULL)
	(*env)->Throw (env, exc);
      else
	(*env)->ExceptionClear (env);
    }
}
#endif

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkToolkit_gtkMain
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  gdk_threads_enter();

  gtk_init_add (post_set_running_flag, NULL);
  gtk_quit_add (gtk_main_level (), clear_running_flag, NULL);

  gtk_main ();

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkToolkit_gtkQuit
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  gdk_threads_enter ();

  gtk_main_quit ();

  gdk_threads_leave ();
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

/**
 * Report the number of mouse buttons
 * Returns the number of buttons of the first mouse found, or -1 if no mouse
 * seems to be connected.
 */
JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_getMouseNumberOfButtons
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  jint res = -1;
  GList *devices;
  GdkDevice *d;

  gdk_threads_enter ();

  /* FIXME: Why doesn't this return the correct number? */
  devices = gdk_devices_list();

  while( res == -1 && devices != NULL )
    {
      d = GDK_DEVICE( devices->data );
      if( d->source == GDK_SOURCE_MOUSE )
	res = d->num_keys;
      devices = devices->next;
    }

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

  /* FIXME: this was deadlocking so assume it is thread-safe for now;
     we need to replace this call with a .properties file anyway. */
#if 0
  gdk_threads_enter ();
#endif

  colors = (*env)->GetIntArrayElements (env, jcolors, 0);

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

  (*env)->ReleaseIntArrayElements(env, jcolors, colors, 0);

#if 0
  gdk_threads_leave ();
#endif
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

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkToolkit_getLockState
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
   jint key)
{
  gint coord;
  GdkModifierType state, mask;
  GdkWindow *root_window;

  gdk_threads_enter ();

  root_window = gdk_get_default_root_window ();
  gdk_window_get_pointer (root_window, &coord, &coord, &state);

  switch (key)
    {
    case VK_SHIFT:
      mask = GDK_SHIFT_MASK;
      break;
    case VK_CONTROL:
      mask = GDK_CONTROL_MASK;
      break;
    case VK_ALT:
      /* This is dubious, since MOD1 could have been mapped to something
         other than ALT. */
      mask = GDK_MOD1_MASK;
      break;
#if GTK_CHECK_VERSION(2, 10, 0)
    case VK_META:
      mask = GDK_META_MASK;
      break;
#endif
    case VK_CAPS_LOCK:
      mask = GDK_LOCK_MASK;
      break;
    default:
      mask = 0;
    }

  gdk_threads_leave ();

  if (mask == 0)
    return -1;

  return state & mask ? 1 : 0;
}

static gboolean
post_set_running_flag (gpointer data __attribute__((unused)))
{
  g_idle_add (set_running_flag, NULL);
  return FALSE;
}

static gboolean
set_running_flag (gpointer data __attribute__((unused)))
{
  (*cp_gtk_gdk_env ())->CallStaticVoidMethod (cp_gtk_gdk_env (),
                                              gtktoolkit,
                                              setRunningID, TRUE);
  return FALSE;
}

static gboolean
clear_running_flag (gpointer data __attribute__((unused)))
{
  (*cp_gtk_gdk_env ())->CallStaticVoidMethod (cp_gtk_gdk_env (),
                                              gtktoolkit,
                                              setRunningID, FALSE);
  return FALSE;
}

