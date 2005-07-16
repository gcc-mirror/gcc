#ifndef __GTKCAIROPEER_H__
#define __GTKCAIROPEER_H__

/* gtkcairopeer.h -- Some global variables and #defines
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
#include <cairo.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

/* 
   A graphics2d struct is both simpler and uglier than a graphics
   struct. 

   Most of the graphics2d drawing state is held in the referenced cairo_t
   and corresponding cairo_surface_t, so we can ignore it.

   In addition to the cairo_t, we need to hold an extra reference to the
   underlying GdkDrawable so its refcount matches the lifecycle of the java
   Graphics object which is peering with us; also a reference to a byte
   buffer and cairo_surface_t which contain the pattern you're drawing from
   (if it exists).

   Finally, it is possible that we are using a non-RENDER capable X server,
   therefore we will be drawing to an cairo_surface_t which is actually a
   pixbuf. When this is the case, the pointer to a GdkPixbuf will be
   non-NULL and any drawing operation needs to be bracketed by pixbuf
   load/save operations. If the GdkPixbuf pointer is NULL, we will treat
   the cairo_surface_t as RENDER-capable.
 */

struct graphics2d
{
  cairo_t *cr;
  cairo_surface_t *surface;
  GdkDrawable *drawable;
  GdkWindow *win;
  GdkPixbuf *drawbuf;
  char *pattern_pixels;
  cairo_surface_t *pattern_surface;
  cairo_pattern_t *pattern;
  gboolean debug;
  enum 
    { 
      MODE_DRAWABLE_WITH_RENDER,
      MODE_DRAWABLE_NO_RENDER,
      MODE_JAVA_ARRAY
    } 
  mode;

  /* Support for MODE_JAVA_ARRAY */
  jintArray jarray;
  jint width, height;
  jint *javabuf;
  jint *javabuf_copy;
  jboolean isCopy;
};

#endif /* __GTKCAIROPEER_H */
