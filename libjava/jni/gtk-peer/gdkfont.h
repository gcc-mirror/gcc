#ifndef __GDKFONT_H__
#define __GDKFONT_H__

/* gdkfont.h -- Some global stuff related to fonts and glyphs 
   Copyright (C) 2003 Free Software Foundation, Inc.
   
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

#include <pango/pango.h>
#include <pango/pango-context.h>
#include <pango/pango-fontmap.h>
#include <pango/pangoft2.h>

extern struct state_table *native_font_state_table;
extern struct state_table *native_glyphvector_state_table;
extern struct state_table *native_text_layout_state_table;

#define NSA_FONT_INIT(env, clazz) \
  native_font_state_table = init_state_table (env, clazz)

#define NSA_GET_FONT_PTR(env, obj) \
  get_state (env, obj, native_font_state_table)

#define NSA_SET_FONT_PTR(env, obj, ptr) \
  set_state (env, obj, native_font_state_table, (void *)ptr)

#define NSA_DEL_FONT_PTR(env, obj) \
  remove_state_slot (env, obj, native_font_state_table)


#define NSA_GV_INIT(env, clazz) \
  native_glyphvector_state_table = init_state_table (env, clazz)

#define NSA_GET_GV_PTR(env, obj) \
  get_state (env, obj, native_glyphvector_state_table)

#define NSA_SET_GV_PTR(env, obj, ptr) \
  set_state (env, obj, native_glyphvector_state_table, (void *)ptr)

#define NSA_DEL_GV_PTR(env, obj) \
  remove_state_slot (env, obj, native_glyphvector_state_table)


#define NSA_TEXT_LAYOUT_INIT(env, clazz) \
  native_text_layout_state_table = init_state_table (env, clazz)

#define NSA_GET_TEXT_LAYOUT_PTR(env, obj) \
  get_state (env, obj, native_text_layout_state_table)

#define NSA_SET_TEXT_LAYOUT_PTR(env, obj, ptr) \
  set_state (env, obj, native_text_layout_state_table, (void *)ptr)

#define NSA_DEL_TEXT_LAYOUT_PTR(env, obj) \
  remove_state_slot (env, obj, native_text_layout_state_table)

#define FONT_METRICS_ASCENT      0
#define FONT_METRICS_MAX_ASCENT  1
#define FONT_METRICS_DESCENT     2
#define FONT_METRICS_MAX_DESCENT 3
#define FONT_METRICS_MAX_ADVANCE 4
#define NUM_FONT_METRICS 5

#define TEXT_METRICS_X_BEARING 0
#define TEXT_METRICS_Y_BEARING 1
#define TEXT_METRICS_WIDTH     2
#define TEXT_METRICS_HEIGHT    3
#define TEXT_METRICS_X_ADVANCE 4
#define TEXT_METRICS_Y_ADVANCE 5
#define NUM_TEXT_METRICS 6

#define NUM_GLYPH_METRICS 10

#define GLYPH_LOG_X(i)      (NUM_GLYPH_METRICS * (i)    )
#define GLYPH_LOG_Y(i)      (NUM_GLYPH_METRICS * (i) + 1)
#define GLYPH_LOG_WIDTH(i)  (NUM_GLYPH_METRICS * (i) + 2)
#define GLYPH_LOG_HEIGHT(i) (NUM_GLYPH_METRICS * (i) + 3)

#define GLYPH_INK_X(i)      (NUM_GLYPH_METRICS * (i) + 4)
#define GLYPH_INK_Y(i)      (NUM_GLYPH_METRICS * (i) + 5)
#define GLYPH_INK_WIDTH(i)  (NUM_GLYPH_METRICS * (i) + 6)
#define GLYPH_INK_HEIGHT(i) (NUM_GLYPH_METRICS * (i) + 7)

#define GLYPH_POS_X(i)      (NUM_GLYPH_METRICS * (i) + 8)
#define GLYPH_POS_Y(i)      (NUM_GLYPH_METRICS * (i) + 9)

struct peerfont
{
  PangoFont *font;
  PangoFontDescription *desc;
  PangoContext *ctx;
  PangoLayout *layout;
  /* 
   * The GdkGraphics2D (using cairo) may store a pointer to a
   * cairo_font_t here; since we want to work equally well with
   * the GdkGraphics class (using GDK) we do not explicitly mention
   * cairo types here; it is up to the higher level driver routine
   * in GdkClasspathFontPeer.java to decide which backend functions
   * to invoke. 
   */
  void *graphics_resource;
};

struct textlayout
{
  PangoLayout *pango_layout;
};

#endif /* __GDKFONT_H__ */
