/* DemoJAWT.c -- native portion of AWT Native Interface demo
   Copyright (C) 2005  Free Software Foundation, Inc.

This file is part of GNU Classpath examples.

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
02110-1301 USA. */

#include "DemoJAWT.h"
#include "jawt_md.h"
#include <string.h>

JNIEXPORT void JNICALL
Java_gnu_classpath_examples_jawt_DemoJAWT_paintIt (JNIEnv* env,
						   jobject canvas,
						   jobject graphics,
						   jboolean on)
{
  JAWT awt;
  JAWT_DrawingSurface* surface;
  JAWT_DrawingSurfaceInfo* surface_info;
  JAWT_X11DrawingSurfaceInfo* surface_info_x11;
  jint lock;
  GC gc;
  int c;
  char* test_string = "JAWT";
  XColor orange;
  XColor yellow;
  XColor blue;
  Display* display;
  Drawable drawable;
  Status status;

  awt.version = JAWT_VERSION_1_3;
  if (JAWT_GetAWT (env, &awt) == JNI_FALSE)
    {
      printf ("couldn't find AWT\n");
      return;
    }

  surface = awt.GetDrawingSurface (env, canvas);
  if (surface == NULL)
    {
      printf ("drawing surface is NULL\n");
      return;
    }

  lock = surface->Lock (surface);
  if ((lock & JAWT_LOCK_ERROR) != 0)
    {
      printf ("couldn't lock drawing surface\n");
      awt.FreeDrawingSurface (surface);
      return;
    }

  surface_info = surface->GetDrawingSurfaceInfo (surface);
  if (surface_info == NULL)
    {
      printf ("couldn't get surface information\n");
      surface->Unlock (surface);
      awt.FreeDrawingSurface (surface);
      return;
    }

  surface_info_x11 = (JAWT_X11DrawingSurfaceInfo*) surface_info->platformInfo;

  display = surface_info_x11->display;
  drawable = surface_info_x11->drawable;

  gc = XCreateGC (display, drawable, 0, 0);
  XSetBackground (display, gc, 0);

  orange.red = 254 * 65535 / 255;
  orange.green = 90 * 65535 / 255;
  orange.blue = 16 * 65535 / 255;

  /* assume color lookups succeed */
  status = XAllocColor (display, DefaultColormap (display,
						  DefaultScreen (display)),
			&orange);

  if (!status)
    {
      printf ("color allocation failed\n");
      goto cleanup;
    }

  yellow.red = 255 * 65535 / 255;
  yellow.green = 255 * 65535 / 255;
  yellow.blue = 0 * 65535 / 255;

  XAllocColor (display, DefaultColormap (display,
					 DefaultScreen (display)),
	       &yellow);

  if (!status)
    {
      printf ("color allocation failed\n");
      goto cleanup;
    }

  blue.red = 16 * 65535 / 255;
  blue.green = 30 * 65535 / 255;
  blue.blue = 137 * 65535 / 255;

  XAllocColor (display, DefaultColormap (display,
					 DefaultScreen (display)),
		&blue);

  if (!status)
    {
      printf ("color allocation failed\n");
      goto cleanup;
    }

  for (c = 5; c >= 0; c--)
    {
      if (c % 2 == on)
	XSetForeground (display, gc, yellow.pixel);
      else
	XSetForeground (display, gc, orange.pixel);

      XFillArc (display, drawable, gc, 140 - c * 15, 140 - c * 15, c * 30, c * 30, 0, 360 * 64);
    }

  XSetForeground (display, gc, blue.pixel);
  XDrawString (display, drawable,
	       gc, 129, 145, test_string, strlen (test_string));

 cleanup:
  XFreeGC (display, gc);

  surface->FreeDrawingSurfaceInfo (surface_info);

  surface->Unlock (surface);

  awt.FreeDrawingSurface (surface);
}
