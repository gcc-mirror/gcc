/* cairographics2d.h -- 
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

#ifndef CAIROGRAPHICS2D_H
#define CAIROGRAPHICS2D_H


#include <cairo.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <config.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include <jni.h>

/*
 * These public final constants are part of the java2d public API, so we
 * write them explicitly here to save fetching them from the constant pool
 * all the time. 
 */
enum java_awt_alpha_composite_rule
  {
    java_awt_alpha_composite_CLEAR = 1,
    java_awt_alpha_composite_SRC = 2,
    java_awt_alpha_composite_SRC_OVER = 3,
    java_awt_alpha_composite_DST_OVER = 4,
    java_awt_alpha_composite_SRC_IN = 5,
    java_awt_alpha_composite_DST_IN = 6,
    java_awt_alpha_composite_SRC_OUT = 7,
    java_awt_alpha_composite_DST_OUT = 8,
    java_awt_alpha_composite_DST = 9,
    java_awt_alpha_composite_SRC_ATOP = 10,
    java_awt_alpha_composite_DST_ATOP = 11,
    java_awt_alpha_composite_XOR = 12
  };

enum java_awt_basic_stroke_join_rule
  {
    java_awt_basic_stroke_JOIN_MITER = 0,
    java_awt_basic_stroke_JOIN_ROUND = 1,
    java_awt_basic_stroke_JOIN_BEVEL = 2
  };

enum java_awt_basic_stroke_cap_rule
  {
    java_awt_basic_stroke_CAP_BUTT = 0,
    java_awt_basic_stroke_CAP_ROUND = 1,
    java_awt_basic_stroke_CAP_SQUARE = 2
  };

enum java_awt_geom_path_iterator_winding_rule
  {
    java_awt_geom_path_iterator_WIND_EVEN_ODD = 0,
    java_awt_geom_path_iterator_WIND_NON_ZERO = 1
  };

enum java_awt_rendering_hints_filter
  {
    java_awt_rendering_hints_VALUE_INTERPOLATION_NEAREST_NEIGHBOR = 0,    
    java_awt_rendering_hints_VALUE_INTERPOLATION_BILINEAR = 1,
    java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_SPEED = 2,
    java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_QUALITY = 3,
    java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_DEFAULT = 4,
    java_awt_rendering_hints_VALUE_INTERPOLATION_BICUBIC = 5
 
  };

/**
 * A structure which basically contains the cairo_t pointer. 
 * The rest is for gradient and texture fills.
 */
struct cairographics2d
{
  cairo_t *cr;
  cairo_surface_t *pattern_surface;
  cairo_pattern_t *pattern; 
  char *pattern_pixels;
};

#endif
