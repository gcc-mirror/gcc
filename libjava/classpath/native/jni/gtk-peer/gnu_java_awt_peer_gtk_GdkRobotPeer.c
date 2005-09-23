/* gdkrobotpeer.c
   Copyright (C) 2004 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GdkRobotPeer.h"
#include <gdk/gdkx.h>
#include <X11/extensions/XTest.h>

static int
awt_button_mask_to_num (int buttons)
{
  switch (buttons)
    {
    case AWT_BUTTON1_MASK:
      return 1;
    case AWT_BUTTON2_MASK:
      return 2;
    case AWT_BUTTON3_MASK:
      return 3;
    }

  return 0;
}

JNIEXPORT jboolean JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_initXTest
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  GdkDisplay *display;
  Display *xdisplay;
  int event_basep;
  int error_basep;
  int majorp;
  int minorp;
  jboolean result;

  gdk_threads_enter ();

  display = gdk_display_get_default ();
  xdisplay = GDK_DISPLAY_XDISPLAY (display);

  result = XTestQueryExtension (xdisplay,
				&event_basep,
				&error_basep,
				&majorp,
				&minorp);

  gdk_threads_leave ();

  return result;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_mouseMove
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)), jint x, jint y)
{
  GdkDisplay *display;
  Display *xdisplay;
  int result;

  gdk_threads_enter ();

  display = gdk_display_get_default ();
  xdisplay = GDK_DISPLAY_XDISPLAY (display);

  result = XTestFakeMotionEvent (xdisplay,
				 -1,
				 x, y, CurrentTime);

  XFlush (xdisplay);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_mousePress
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)), jint buttons)
{
  GdkDisplay *display;
  Display *xdisplay;
  int result;

  gdk_threads_enter ();

  display = gdk_display_get_default ();
  xdisplay = GDK_DISPLAY_XDISPLAY (display);

  result = XTestFakeButtonEvent (xdisplay,
				 awt_button_mask_to_num (buttons),
				 True, CurrentTime);

  XFlush (xdisplay);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_mouseRelease
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)), jint buttons)
{
  GdkDisplay *display;
  Display *xdisplay;
  int result;

  gdk_threads_enter ();

  display = gdk_display_get_default ();
  xdisplay = GDK_DISPLAY_XDISPLAY (display);

  result = XTestFakeButtonEvent (xdisplay,
				 awt_button_mask_to_num (buttons),
				 False, CurrentTime);

  XFlush (xdisplay);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_mouseWheel
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)), jint wheelAmt)
{
  GdkDisplay *display;
  Display *xdisplay;
  int i = 0;

  gdk_threads_enter ();

  display = gdk_display_get_default ();
  xdisplay = GDK_DISPLAY_XDISPLAY (display);

  if (wheelAmt < 0)
    for (i = 0; i < -wheelAmt; i++)
      {
	XTestFakeButtonEvent (xdisplay,
			      4,
			      True, CurrentTime);
	XTestFakeButtonEvent (xdisplay,
			      4,
			      False, CurrentTime);
      }
  else
    for (i = 0; i < wheelAmt; i++)
      {
	XTestFakeButtonEvent (xdisplay,
			      5,
			      True, CurrentTime);
	XTestFakeButtonEvent (xdisplay,
			      5,
			      False, CurrentTime);
      }

  XFlush (xdisplay);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_keyPress
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)), jint keycode)
{
  GdkDisplay *display;
  Display *xdisplay;
  GdkKeymapKey *keymap_keys = NULL;
  gint n_keys = 0;
  guint lookup_keyval = 0;
  int result;

  gdk_threads_enter ();

  display = gdk_display_get_default ();
  xdisplay = GDK_DISPLAY_XDISPLAY (display);

  lookup_keyval = cp_gtk_awt_keycode_to_keysym (keycode,
                                                       AWT_KEY_LOCATION_LEFT);

  if (!gdk_keymap_get_entries_for_keyval (gdk_keymap_get_default (),
                                          lookup_keyval,
                                          &keymap_keys,
                                          &n_keys))
    {
      /* No matching keymap entry was found. */
      g_printerr ("No matching keymap entries were found\n");
      gdk_threads_leave ();
      return;
    }

  /* If n_keys > 1 then there are multiple hardware keycodes that
     translate to lookup_keyval.  We arbitrarily choose the first
     hardware keycode from the list returned by
     gdk_keymap_get_entries_for_keyval. */
  result = XTestFakeKeyEvent (xdisplay,
			      keymap_keys[0].keycode,
			      True, CurrentTime);

  g_free (keymap_keys);

  XFlush (xdisplay);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_keyRelease
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)), jint keycode)
{
  GdkDisplay *display;
  Display *xdisplay;
  GdkKeymapKey *keymap_keys = NULL;
  gint n_keys = 0;
  guint lookup_keyval = 0;
  int result;

  gdk_threads_enter ();

  display = gdk_display_get_default ();
  xdisplay = GDK_DISPLAY_XDISPLAY (display);

  lookup_keyval = cp_gtk_awt_keycode_to_keysym (keycode,
                                                       AWT_KEY_LOCATION_LEFT);

  if (!gdk_keymap_get_entries_for_keyval (gdk_keymap_get_default (),
                                          lookup_keyval,
                                          &keymap_keys,
                                          &n_keys))
    {
      /* No matching keymap entry was found. */
      g_printerr ("No matching keymap entries were found\n");
      gdk_threads_leave ();
      return;
    }

  /* If n_keys > 1 then there are multiple hardware keycodes that
     translate to lookup_keyval.  We arbitrarily choose the first
     hardware keycode from the list returned by
     gdk_keymap_get_entries_for_keyval. */
  result = XTestFakeKeyEvent (xdisplay,
			      keymap_keys[0].keycode,
			      False, CurrentTime);

  g_free (keymap_keys);

  XFlush (xdisplay);

  gdk_threads_leave ();
}

JNIEXPORT jintArray JNICALL
Java_gnu_java_awt_peer_gtk_GdkRobotPeer_nativeGetRGBPixels
  (JNIEnv *env, jobject obj __attribute__((unused)), jint x, jint y,
   jint width, jint height)
{
  jint stride_bytes, stride_pixels, n_channels, n_pixels;
  jintArray jpixels;  
  jint *java_pixels;
  guchar *gdk_pixels;
  GdkPixbuf *pixbuf_no_alpha = NULL;
  GdkPixbuf *pixbuf = NULL;

#ifndef WORDS_BIGENDIAN
  int i;
#endif

  gdk_threads_enter ();

  pixbuf_no_alpha = gdk_pixbuf_get_from_drawable (NULL,
						  gdk_get_default_root_window (),
						  NULL, x, y, 0, 0,
						  width, height);

  pixbuf = gdk_pixbuf_add_alpha(pixbuf_no_alpha, FALSE, 0, 0, 0);
  g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
  
  stride_bytes = gdk_pixbuf_get_rowstride (pixbuf);
  n_channels = gdk_pixbuf_get_n_channels (pixbuf);
  stride_pixels =  stride_bytes / n_channels;
  n_pixels = height * stride_pixels;
  gdk_pixels = gdk_pixbuf_get_pixels (pixbuf);

  jpixels = (*env)->NewIntArray (env, n_pixels);

  java_pixels = (*env)->GetIntArrayElements (env, jpixels, NULL);

  memcpy (java_pixels,
	  gdk_pixels,
	  (height * stride_bytes));

#ifndef WORDS_BIGENDIAN
  /* convert pixels from 0xBBGGRRAA to 0xAARRGGBB */
  for (i = 0; i < n_pixels; ++i)
    {
      java_pixels[i] = SWAPU32 ((unsigned)java_pixels[i]);
    }
#endif

  g_object_unref (pixbuf);

  (*env)->ReleaseIntArrayElements (env, jpixels, java_pixels, 0);

  gdk_threads_leave ();

  return jpixels;
}
