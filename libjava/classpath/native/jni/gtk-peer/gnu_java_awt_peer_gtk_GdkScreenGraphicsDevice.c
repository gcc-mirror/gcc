/* gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice.c
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

#include <jcl.h>

#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#ifdef HAVE_XRANDR
#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>
#endif

#include "gdkdisplay.h"

#include "gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice.h"

struct state_table *cp_gtk_native_screen_state_table;

jclass gdkScreenGraphicsDevice_class;

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_initIDs
(JNIEnv *env, jclass klazz __attribute__((unused)))
{
  gtkpeer_init_screen_IDs(env);
}

JNIEXPORT jobject JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_nativeGetFixedDisplayMode
(JNIEnv *env, jobject obj, jobject gdkGraphicsEnv __attribute__((unused)))
{
	jclass displayMode_class;
	jmethodID displayMode_ctor;
	GdkScreen *screen;
	jobject fixedDisplayMode = NULL;
#ifdef HAVE_XRANDR
	int temp1, temp2;
	GdkDisplay *display;

    display = (GdkDisplay *) gtkpeer_get_display(env, gdkGraphicsEnv);

	gdk_threads_enter();

	if (!XRRQueryExtension(GDK_DISPLAY_XDISPLAY(display), &temp1, &temp2))
	  {
        displayMode_class = (*env)->FindClass(env, "java/awt/DisplayMode");
        displayMode_ctor = (*env)->GetMethodID(env,
                                               displayMode_class,
                                               "<init>",
                                               "(IIII)V");

        screen = (GdkScreen *) gtkpeer_get_screen(env, obj);
	  	
	  	fixedDisplayMode = (*env)->NewObject(env,
	  	                                     displayMode_class,
	  	                                     displayMode_ctor,
	  	                                     gdk_screen_get_width(screen),
	  	                                     gdk_screen_get_height(screen),
	  	                                     -1,
	  	                                     0);
	  }

	gdk_threads_leave();
	  
#else

    displayMode_class = (*env)->FindClass(env, "java/awt/DisplayMode");
    displayMode_ctor = (*env)->GetMethodID(env,
                                           displayMode_class,
                                           "<init>",
                                           "(IIII)V");

    screen = (GdkScreen *) gtkpeer_get_screen(env, obj);
	  	
    fixedDisplayMode = (*env)->NewObject(env,
	                                     displayMode_class,
	                                     displayMode_ctor,
	                                     gdk_screen_get_width(screen),
	                                     gdk_screen_get_height(screen),
	                                     -1,
	                                     0);

#endif	  
	return fixedDisplayMode;
}

JNIEXPORT jstring JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_nativeGetIDString
(JNIEnv *env, jobject obj)
{
	GdkScreen *screen;
	gchar* displayName;
	jstring string;

    screen = (GdkScreen *) gtkpeer_get_screen(env, obj);
	
	gdk_threads_enter();
	
	displayName = gdk_screen_make_display_name(screen);
	
	gdk_threads_leave();
	
    string = (*env)->NewStringUTF(env, displayName);
    
    g_free(displayName);
    
    return string;
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_nativeGetDisplayModeRate
(JNIEnv *env, jobject obj __attribute__((unused)), jobject gdkGraphicsEnv __attribute__((unused)))
{
#ifdef HAVE_XRANDR

	GdkDisplay *display;
	XRRScreenConfiguration *config;
	int rate;
	
	display = (GdkDisplay *) gtkpeer_get_display(env, gdkGraphicsEnv);
	
	gdk_threads_enter();
	
	config = XRRGetScreenInfo (GDK_DISPLAY_XDISPLAY(display), GDK_ROOT_WINDOW());

	rate = (int) XRRConfigCurrentRate (config);

    XRRFreeScreenConfigInfo (config);

	gdk_threads_leave();
	
	return rate;
#else
    JCL_ThrowException(env,
                       "java/lang/InternalError",
                       "Method should not have been invoked.");
    
    return -1;
#endif    
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_nativeGetDisplayModeIndex
(JNIEnv *env, jobject obj __attribute__((unused)), jobject gdkGraphicsEnv __attribute__((unused)))
{
#ifdef HAVE_XRANDR	

	GdkDisplay *display;
	XRRScreenConfiguration *config;
	SizeID index;
	Rotation rotation;
	
	display = (GdkDisplay *) gtkpeer_get_display(env, gdkGraphicsEnv);
	
	gdk_threads_enter();
	
	config = XRRGetScreenInfo (GDK_DISPLAY_XDISPLAY(display), GDK_ROOT_WINDOW());

	index = XRRConfigCurrentConfiguration (config, &rotation);

    XRRFreeScreenConfigInfo (config);

	gdk_threads_leave();
	
	return (int) index;

#else

    JCL_ThrowException(env,
                       "java/lang/InternalError",
                       "Method should not have been invoked.");
   
    return -1;
   
#endif	
}

JNIEXPORT jobjectArray JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_nativeGetDisplayModes
(JNIEnv *env, jobject obj __attribute__((unused)), jobject gdkGraphicsEnv __attribute__((unused)))
{
#ifdef HAVE_XRANDR
	GdkDisplay *display;
	XRRScreenConfiguration *config;
	XRRScreenSize *screenSizes;
	int nsizes = 0, nrates = 0, i = 0;
	jclass x11DisplayMode_class;
	jmethodID x11DisplayMode_ctor;
	jobjectArray array;
	jobject instance;
	short *rates;
	jshortArray shortArray;
	
	display = (GdkDisplay *) gtkpeer_get_display(env, gdkGraphicsEnv);
	
	gdk_threads_enter();
	
	config = XRRGetScreenInfo (GDK_DISPLAY_XDISPLAY(display), GDK_ROOT_WINDOW());
	
	screenSizes = XRRConfigSizes(config, &nsizes);
	
	x11DisplayMode_class = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GdkScreenGraphicsDevice$X11DisplayMode");
    
	x11DisplayMode_ctor = (*env)->GetMethodID(env, x11DisplayMode_class, "<init>", "(II[S)V");

	array = (*env)->NewObjectArray(env, nsizes, x11DisplayMode_class, NULL);

	for (; i < nsizes ; i++)
	  {
	  	/* Retrieves refresh rate information. */
	  	rates = XRRConfigRates(config, i, &nrates);
	  	
	  	/* Create a Java short array and put them in. */
	  	shortArray = (*env)->NewShortArray(env, nrates);
		(*env)->SetShortArrayRegion(env, shortArray, 0, nrates, (jshort *) rates);
	  	
	  	/* Create a GdkScreenGraphicsDevice.X11DisplayMode instance. */
        instance = (*env)->NewObject(env,
									 x11DisplayMode_class,
									 x11DisplayMode_ctor,
									 screenSizes[i].width,
									 screenSizes[i].height,
									 shortArray);
									 
		/* Put it into the result array. */
        (*env)->SetObjectArrayElement(env, array, i, instance);
	  }
	  
	/* Free everything acquired by xlib. */
	XRRFreeScreenConfigInfo (config);

	gdk_threads_leave();
	
	return array;
#else
    JCL_ThrowException(env,
                       "java/lang/InternalError",
                       "Method should not have been invoked.");
    
    return NULL;
   
#endif	
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_nativeSetDisplayMode
(JNIEnv *env, jobject obj __attribute__((unused)), jobject gdkGraphicsEnv __attribute__((unused)), jint index __attribute__((unused)), jshort rate __attribute__((unused)))
{
#ifdef HAVE_XRANDR
	GdkDisplay *display;
	XRRScreenConfiguration *config;
	Rotation rotation;
	
	display = (GdkDisplay *) gtkpeer_get_display(env, gdkGraphicsEnv);
	
	gdk_threads_enter();
	
	config = XRRGetScreenInfo (GDK_DISPLAY_XDISPLAY(display), GDK_ROOT_WINDOW());

	/* The rotation is not exposed to the Java API. So we retrieve its current
	 * value and set it to the same when switching resolution.
	 */
	XRRConfigCurrentConfiguration (config, &rotation);
	
	XRRSetScreenConfigAndRate (GDK_DISPLAY_XDISPLAY(display),
                        		   config,
                               GDK_ROOT_WINDOW(),
                               index,
                               rotation,
                               rate,
                               CurrentTime);
	
	XRRFreeScreenConfigInfo(config);
	
	gdk_threads_leave();

#else
    JCL_ThrowException(env,
                       "java/lang/InternalError",
                       "Method should not have been invoked.");
#endif	
}

JNIEXPORT jobject JNICALL
Java_gnu_java_awt_peer_gtk_GdkScreenGraphicsDevice_nativeGetBounds
(JNIEnv *env, jobject obj)
{
	jclass rectangle_class;
	jmethodID rectangle_ctor;
	GdkScreen *screen;
	GdkWindow *window;
	int x, y, w, h;
	jobject instance;
    
	rectangle_class = (*env)->FindClass(env, "java/awt/Rectangle");
    
    rectangle_ctor = (*env)->GetMethodID 
    (env, rectangle_class, "<init>", "(IIII)V");

    screen = (GdkScreen *) gtkpeer_get_screen(env, obj);

	gdk_threads_enter();
	
	window = gdk_screen_get_root_window(screen);
	
	gdk_window_get_geometry(window, &x, &y, &w, &h, NULL);

	gdk_threads_leave();
	
    instance = (*env)->NewObject(env,
								 rectangle_class,
								 rectangle_ctor,
								 x, y, w, h);
	
	return instance;
}
