// -*- c++ -*-
// gtkutils.h - Common defines and inline functions for the gtk AWT peers.

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GTKCOMMON_H__
#define __GTKCOMMON_H__

#include <gtk/gtk.h>
#include <gdk/gdkx.h>

#include <java/awt/Color.h>

class _Jv_GdkThreadLock
{
public:
  _Jv_GdkThreadLock ()
  {
    GDK_THREADS_ENTER ();
  }

  ~_Jv_GdkThreadLock ()
  {
    GDK_THREADS_LEAVE ();
  }
};

// Convert AWT Color to gdk color value.
static inline void 
_Jv_ConvertAwtColor(java::awt::Color* awtcolor, GdkColor* gdkcolor)
{
  jint rgb = awtcolor->getRGB();
  gushort r = (rgb >> 16) & 0xFF;
  gushort g = (rgb >> 8) & 0xFF;
  gushort b = rgb & 0xFF;
  
  gdkcolor->red = (r << 8) + r;
  gdkcolor->green = (g << 8) + g;
  gdkcolor->blue = (b << 8) + b;
  
  // FIXME: Deal with colormap? gdk_color_alloc()?
}				    

// Convert gdk color value to AWT Color.
static inline java::awt::Color* 
_Jv_ConvertGtkColor (GdkColor* gdkcolor)
{
  jint r = gdkcolor->red >> 8;
  jint g = gdkcolor->green >> 8;
  jint b = gdkcolor->blue >> 8;

  java::awt::Color *c = new java::awt::Color(r,g,b);
  
  return c;
}				    

static inline void  
_Jv_GdkScaleColor (GdkColor* oldc, GdkColor* newc, gfloat scale)
{
  // FIXME: Need to deal with overflows or find a better way
  *newc = *oldc;
  newc->red += (gushort) (newc->red * scale);
  newc->green += (gushort) (newc->green * scale);
  newc->blue += (gushort) (newc->blue * scale);
}

// Normally the X queue gets flushed automatically when gtk's event loop goes 
// idle. However, some calls do not cause any activitity on the event loop,
// so we need to occasionally flush pending requests manually because we arn't 
// running from the gtk_main thread. Note that gdk_flush calls XSync(), which 
// is more than what is needed here.
static inline void
_Jv_FlushRequests ()
{
  // FIXME: What about platforms that arn't X?
  XFlush (GDK_DISPLAY ());
}

#endif /* __GTKUTILS_H__ */
