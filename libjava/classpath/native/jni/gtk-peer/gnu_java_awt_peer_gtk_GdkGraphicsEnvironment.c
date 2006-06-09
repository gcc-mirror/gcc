/* gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.c
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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

#include <glib.h>
#include <gdk/gdk.h>

#include "gdkfont.h"
#include "gdkdisplay.h"
#include "gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h"

struct state_table *cp_gtk_native_display_state_table;

jclass gdkGraphicsEnvironment_class;

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_initStaticState
(JNIEnv *env, jclass klazz __attribute__((unused)))
{
    gdkGraphicsEnvironment_class = (*env)->NewGlobalRef
    (env, klazz);

	NSA_DISPLAY_INIT(env, gdkGraphicsEnvironment_class);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_nativeInitState
(JNIEnv *env, jobject obj)
{
	GdkDisplay *defaultDisplay;
	
    gdk_threads_enter();
    
    /* Retrieve the default display. */
    defaultDisplay = gdk_display_get_default();
    
    gdk_threads_leave();
    
    /* Store display pointer in GdkGraphicsEnvironment instance. */
    NSA_SET_DISPLAY_PTR(env, obj, (void *) defaultDisplay);
}

static gint
cmp_families (const void *a, const void *b)
{
  const char *a_name = pango_font_family_get_name (*(PangoFontFamily **)a);
  const char *b_name = pango_font_family_get_name (*(PangoFontFamily **)b);

  return g_utf8_collate (a_name, b_name);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_nativeGetFontFamilies
(JNIEnv *env, jobject  self __attribute__((unused)), jobjectArray family_name)
{
  PangoContext *context = NULL;
  PangoFontFamily **families = NULL;
  int n_families = 0;
  int idx = 0;

  gdk_threads_enter ();

  context = gdk_pango_context_get();
  g_assert (context != NULL);

  pango_context_list_families (context, &families, &n_families);

  qsort (families, n_families, sizeof (PangoFontFamily *), cmp_families);

  for (idx = 0;  idx < n_families;  idx++)
    {
      const char *name_tmp =  pango_font_family_get_name (families[idx]);
      jstring name = (*env)->NewStringUTF (env, name_tmp);
      (*env)->SetObjectArrayElement (env, family_name, idx, name);
      (*env)->DeleteLocalRef(env, name);
    }
  g_free (families);

  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_nativeGetNumFontFamilies
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  PangoContext *context = NULL;
  PangoFontFamily **families = NULL;
  gint n_families = 0;
  gint num = 0;

  gdk_threads_enter ();

  context = gdk_pango_context_get();
  g_assert (context != NULL);

  pango_context_list_families (context, &families, &n_families);

  num = n_families;
  g_free (families);

  gdk_threads_leave ();
  
  return num;
}

JNIEXPORT jobjectArray JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_nativeGetScreenDevices
(JNIEnv *env, jobject obj)
{
	jmethodID gdkScreenGraphicsDevice_ctor, gdkScreenGraphicsDevice_init;
	jclass gdkScreenGraphicsDevice_class;
	int numScreens = 0, i = 0;
	GdkDisplay *display;
	jobjectArray array;
	jobject instance;
	
	gdkScreenGraphicsDevice_class = (*env)->FindClass 
    (env, "gnu/java/awt/peer/gtk/GdkScreenGraphicsDevice");
    
	gdkScreenGraphicsDevice_ctor = (*env)->GetMethodID 
    (env, gdkScreenGraphicsDevice_class, "<init>",
     "(Lgnu/java/awt/peer/gtk/GdkGraphicsEnvironment;)V");

	gdkScreenGraphicsDevice_init = (*env)->GetMethodID 
    (env, gdkScreenGraphicsDevice_class, "init", "()V");

	display = (GdkDisplay *) NSA_GET_DISPLAY_PTR(env, obj);
	
	gdk_threads_enter();
	
	numScreens = gdk_display_get_n_screens(display);
	
	
	/* Create a suitably sized array. */
	array = (*env)->NewObjectArray(env,
                                   numScreens,
                                   gdkScreenGraphicsDevice_class,
                                   NULL);
	
	/* Create GdkScreenGraphicsDevice instances, store the native pointer to
	 * the GScreen object with them, run a 2nd initialization phase and
	 * put the new instance into the result array.
	 */
	for ( ; i < numScreens ; i++)
	{
		instance = (*env)->NewObject (env, 
                                      gdkScreenGraphicsDevice_class,
                                      gdkScreenGraphicsDevice_ctor,
                                      obj);
									  
        NSA_SET_SCREEN_PTR(env,
                           instance,
                           gdk_display_get_screen(display, i));
						   
        gdk_threads_leave();
        (*env)->CallVoidMethod(env,
                               instance,
                               gdkScreenGraphicsDevice_init);
		gdk_threads_enter();
		
        (*env)->SetObjectArrayElement(env, array, i, instance);
    }
	
    gdk_threads_leave();
	
    return array;
}

JNIEXPORT jobject JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_nativeGetDefaultScreenDevice
(JNIEnv *env, jobject obj)
{
    jclass gdkScreenGraphicsDevice_class;
    jmethodID gdkScreenGraphicsDevice_ctor, gdkScreenGraphicsDevice_init;
    jobject defaultDevice;
    GdkScreen *defaultScreen;
    
    gdkScreenGraphicsDevice_class = (*env)->FindClass 
    (env, "gnu/java/awt/peer/gtk/GdkScreenGraphicsDevice");
    
    gdkScreenGraphicsDevice_ctor = (*env)->GetMethodID 
    (env, gdkScreenGraphicsDevice_class, "<init>",
     "(Lgnu/java/awt/peer/gtk/GdkGraphicsEnvironment;)V");
    
    gdkScreenGraphicsDevice_init = (*env)->GetMethodID 
    (env, gdkScreenGraphicsDevice_class, "init", "()V");
    
    /* Create the GdkScreenGraphicsDevice instance. */
    defaultDevice = (*env)->NewObject (env, 
									   gdkScreenGraphicsDevice_class,
									   gdkScreenGraphicsDevice_ctor,
									   obj);
									   
    gdk_threads_enter();
	
    defaultScreen = gdk_screen_get_default();
	
    gdk_threads_leave();
									   
	/* Class initialization will have set up the native_state storage
	 * mechanism for GdkScreenGraphicsDevice.
	 */
    NSA_SET_SCREEN_PTR(env, defaultDevice, defaultScreen);

    (*env)->CallVoidMethod(env,
                           defaultDevice,
                           gdkScreenGraphicsDevice_init);

    return defaultDevice;	
}
