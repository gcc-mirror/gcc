/* gdkglyphvector.c
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

#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_GdkGlyphVector.h"

struct state_table *native_glyphvector_state_table;


JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_initStaticState 
  (JNIEnv *env, jclass clazz)
{
  NSA_GV_INIT (env, clazz);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_initState 
  (JNIEnv *env, jobject self, jobject font, jobject ctx)
{
  struct glyphvec *vec = NULL;
  struct peerfont *pfont = NULL;

  gdk_threads_enter ();
  g_assert (font != NULL);
  pfont = (struct peerfont *)NSA_GET_FONT_PTR (env, font);
  g_assert (pfont != NULL);
  g_assert (pfont->ctx != NULL);
  g_assert (pfont->desc != NULL);

  g_assert (self != NULL);
  vec = (struct glyphvec *) g_malloc0 (sizeof (struct glyphvec));
  g_assert (vec != NULL);

  vec->desc = pango_font_description_copy (pfont->desc);
  g_assert (vec->desc != NULL);
    
  vec->ctx = pfont->ctx;
  g_object_ref (vec->ctx);

  NSA_SET_GV_PTR (env, self, vec);
  gdk_threads_leave ();
}

static void free_glyphitems (GList *list)
{
  GList *i = NULL;
  PangoGlyphItem *gi = NULL;

  for (i = g_list_first (list); i != NULL; i = g_list_next (i))
    {
      g_assert (i->data != NULL);
      gi = (PangoGlyphItem *)i->data;

      if (gi->glyphs != NULL)
	pango_glyph_string_free (gi->glyphs);

      if (gi->item != NULL)
	g_free (gi->item);
    }      
  g_list_free (list);
}

static void seek_glyphstring_idx (GList *list, int idx, 
				  int *nidx, 
				  PangoGlyphString **gs,
				  PangoFont **fnt)
{
  GList *i = NULL;
  PangoGlyphItem *gi = NULL;

  g_assert (list != NULL);
  g_assert (gs != NULL);
  g_assert (nidx != NULL);

  int begin = 0;
  for (i = g_list_first (list); i != NULL; i = g_list_next (i))
    {
      g_assert (i->data != NULL);
      gi = (PangoGlyphItem *)i->data;

      g_assert (gi->glyphs != NULL);
      
      if (begin <= idx && idx < begin + gi->glyphs->num_glyphs)
	{	  
	  *gs = gi->glyphs;
	  *nidx = idx - begin;
	  if (fnt && gi->item)
	    *fnt = gi->item->analysis.font;
	  return;
	}
      else
	{
	  begin += gi->glyphs->num_glyphs;
	}
    }
  *gs = NULL;
  *nidx = -1;
}

static void seek_glyph_idx (GList *list, int idx, 
			    PangoGlyphInfo **g,
			    PangoFont **fnt)
{
  PangoGlyphString *gs = NULL;
  int nidx = -1;

  g_assert (list != NULL);
  g_assert (g != NULL);

  seek_glyphstring_idx (list, idx, &nidx, &gs, fnt);

  g_assert (gs != NULL);
  g_assert (nidx != -1);
  g_assert (nidx < gs->num_glyphs);
  g_assert (gs->glyphs != NULL);

  *g = gs->glyphs + nidx;
}

static void union_rects (PangoRectangle *r1, 
			 const PangoRectangle *r2)
{
  PangoRectangle r;

  g_assert (r1 != NULL);
  g_assert (r2 != NULL);

  /* 
     x is the left edge of the rect,
     y is the top edge of the rect
  */

#ifndef min
#define min(x,y) ((x) < (y) ? (x) : (y))
#endif

#ifndef max
#define max(x,y) ((x) < (y) ? (y) : (x))
#endif

  r.x = min(r1->x, r2->x);

  r.y = min(r1->y, r2->y);

  r.width = max(r1->x + r1->width,
		r2->x + r2->width) - r.x;

  r.height = max(r1->y + r1->height,
		 r2->y + r2->height) - r.y;

  *r1 = r;  
}

static jdoubleArray rect_to_array (JNIEnv *env, const PangoRectangle *r)
{
  /* We often return rectangles as arrays : { x, y, w, h } */
  jdoubleArray ret;
  double *rp = NULL;
  g_assert (r != NULL);
  ret = (*env)->NewDoubleArray (env, 4);
  rp = (*env)->GetDoubleArrayElements (env, ret, NULL);
  g_assert (rp != NULL);
  rp[0] = r->x / (double)PANGO_SCALE;
  /* freetype and pango's view of space is upside down from java2d's */
  rp[1] = (r->y / (double)PANGO_SCALE) * -1;
  rp[2] = r->width / (double)PANGO_SCALE;
  rp[3] = r->height / (double)PANGO_SCALE;
  (*env)->ReleaseDoubleArrayElements (env, ret, rp, 0);
  return ret;
}


JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_dispose
  (JNIEnv *env, jobject self)
{
  struct glyphvec *vec = NULL;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_DEL_GV_PTR (env, self);
  g_assert (vec != NULL);

  if (vec->glyphitems != NULL)
    {
      free_glyphitems (vec->glyphitems);
      vec->glyphitems = NULL;
    }
      
  if (vec->desc != NULL)
    pango_font_description_free (vec->desc);

  if (vec->ctx != NULL)
    g_object_unref (vec->ctx);

  g_free (vec);
  gdk_threads_leave ();
}


JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_setChars 
  (JNIEnv *env, jobject self, jstring chars)
{
  struct glyphvec *vec = NULL;
  gchar *str = NULL;
  GList *items = NULL, *item = NULL;
  PangoGlyphItem *gi;
  PangoAttrList *attrs = NULL; 
  gint len = 0;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);
  g_assert (vec->desc != NULL);
  g_assert (vec->ctx != NULL);
  
  len = (*gdk_env)->GetStringUTFLength (env, chars);
  str = (gchar *)(*env)->GetStringUTFChars (env, chars, NULL);
  g_assert (str != NULL);

  /* step 1: mark the text as having our FontFescription as an 
     attribute, then "itemize" the text */

  attrs = pango_attr_list_new ();
  g_assert (attrs != NULL);
  
  PangoAttribute *da = pango_attr_font_desc_new(vec->desc);
  g_assert (da != NULL);
  da->start_index = 0;
  da->end_index = len;
  
  pango_attr_list_insert (attrs, da);
  items = pango_itemize (vec->ctx, str, 0, len, attrs, NULL);
  g_assert (items != NULL);
  
  /*
    step 2: for each item:
    - shape the item into a glyphstring
    - store the (item, glyphstring) pair in the vec->glyphitems list
  */
  
  if (vec->glyphitems != NULL)
    {
      free_glyphitems (vec->glyphitems);
      vec->glyphitems = NULL;
    }

  for (item = g_list_first (items); item != NULL; item = g_list_next (item))
    {
      g_assert (item->data != NULL);

      gi = NULL;
      gi = g_malloc0 (sizeof(PangoGlyphItem));
      g_assert (gi != NULL);

      gi->item = (PangoItem *)item->data;
      gi->glyphs = pango_glyph_string_new ();
      g_assert (gi->glyphs != NULL);

      pango_shape (str + gi->item->offset, 
		   gi->item->length, 
		   &(gi->item->analysis), 
		   gi->glyphs);

      vec->glyphitems = g_list_append (vec->glyphitems, gi);
    }

  /* 
     ownership of each item has been transferred to glyphitems, 
     but the list should be freed.
  */

  g_list_free (items);
  pango_attr_list_unref (attrs);

  (*env)->ReleaseStringUTFChars (env, chars, str);
  gdk_threads_leave ();
}


JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_setGlyphCodes 
  (JNIEnv *env, jobject self, jintArray codes)
{
  struct glyphvec *vec = NULL;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);

  /*
    FIXME: setting glyph codes doesn't seem particularly plausible at the
    moment. 
   */

  gdk_threads_leave ();

}


JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_glyphCode 
  (JNIEnv *env, jobject self, jint idx)
{
  struct glyphvec *vec = NULL;
  PangoGlyphInfo *gi = NULL;
  jint ret = 0;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);
  g_assert (vec->glyphitems != NULL);

  seek_glyph_idx (vec->glyphitems, idx, &gi, NULL);
  g_assert (gi != NULL);
  ret = gi->glyph;
  gdk_threads_leave ();

  return (jint)(ret);  
}


JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_numGlyphs 
  (JNIEnv *env, jobject self)
{
  GList *i = NULL;
  PangoGlyphItem *gi = NULL;
  struct glyphvec *vec = NULL;
  jint count = 0;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);

  for (i = g_list_first (vec->glyphitems); i != NULL; i = g_list_next (i))
    {
      g_assert (i->data != NULL);
      gi = (PangoGlyphItem *)i->data;
      g_assert (gi->glyphs != NULL);
      count += gi->glyphs->num_glyphs;
    }      
  gdk_threads_leave ();

  return count;
}


JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_glyphCharIndex 
  (JNIEnv *env, jobject self, jint idx)
{
  /* 
     FIXME: this is not correct, rather it assumes a (broken) 1:1
     glyph:char model. it can be implemented in terms of bytes (also
     broken) using pango's current interface, or perhaps in terms of
     characters if some better byte->character conversion operator is
     found. for the time being we leave it broken.
  */
  return idx;
}


JNIEXPORT jdoubleArray JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_allLogicalExtents 
  (JNIEnv *env, jobject self)
{
  struct glyphvec *vec = NULL;
  GList *i;
  PangoGlyphItem *gi = NULL;
  PangoRectangle rect = {0,0,0,0};
  PangoRectangle tmp, dummy;  
  jdoubleArray ret;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);
  g_assert (vec->glyphitems != NULL);

  for (i = g_list_first (vec->glyphitems); i != NULL; i = g_list_next (i))
    {
      g_assert (i->data != NULL);
      gi = (PangoGlyphItem *)i->data;
      g_assert (gi->glyphs != NULL);
      
      pango_glyph_string_extents (gi->glyphs,
				  gi->item->analysis.font,
				  &dummy,
				  &tmp);
      union_rects (&rect, &tmp);
    }      

  ret = rect_to_array (env, &rect);
  gdk_threads_leave ();
  return ret;
}


JNIEXPORT jdoubleArray JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_allInkExtents 
  (JNIEnv *env, jobject self)
{
  struct glyphvec *vec = NULL;
  GList *i;
  PangoGlyphItem *gi = NULL;
  PangoRectangle rect = {0,0,0,0};
  PangoRectangle tmp, dummy;  
  jdoubleArray ret;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);
  g_assert (vec->glyphitems != NULL);

  for (i = g_list_first (vec->glyphitems); i != NULL; i = g_list_next (i))
    {
      g_assert (i->data != NULL);
      gi = (PangoGlyphItem *)i->data;
      g_assert (gi->glyphs != NULL);
      
      pango_glyph_string_extents (gi->glyphs,
				  gi->item->analysis.font,
				  &tmp,
				  &dummy);
      union_rects (&rect, &tmp);
    }      

  ret = rect_to_array (env, &rect);
  gdk_threads_leave ();
  return ret;
}

JNIEXPORT jdoubleArray JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_glyphLogicalExtents 
  (JNIEnv *env, jobject self, jint idx)
{
  struct glyphvec *vec = NULL;
  PangoRectangle rect = {0,0,0,0};
  PangoRectangle dummy;  
  PangoGlyphInfo *gi = NULL;
  PangoFont *font = NULL;
  jdoubleArray ret;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);
  g_assert (vec->glyphitems != NULL);

  seek_glyph_idx (vec->glyphitems, idx, &gi, &font);
  g_assert (gi != NULL);
  g_assert (font != NULL);

  pango_font_get_glyph_extents (font, gi->glyph, &dummy, &rect);

  ret = rect_to_array (env, &rect);
  gdk_threads_leave ();
  return ret;
}


JNIEXPORT jdoubleArray JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_glyphInkExtents 
  (JNIEnv *env, jobject self, jint idx)
{
  struct glyphvec *vec = NULL;
  PangoRectangle rect = {0,0,0,0};
  PangoRectangle dummy;  
  PangoGlyphInfo *gi = NULL;
  PangoFont *font = NULL;
  jdoubleArray ret;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);
  g_assert (vec->glyphitems != NULL);

  seek_glyph_idx (vec->glyphitems, idx, &gi, &font);
  g_assert (gi != NULL);
  g_assert (font != NULL);

  pango_font_get_glyph_extents (font, gi->glyph, &rect, &dummy);

  ret = rect_to_array (env, &rect);
  gdk_threads_leave ();
  return ret;
}

JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_glyphIsHorizontal 
  (JNIEnv *env, jobject self, jint idx)
{
  struct glyphvec *vec = NULL;
  PangoDirection dir;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  g_assert (vec != NULL);
  g_assert (vec->desc != NULL);
  g_assert (vec->ctx != NULL);

  /* 
     FIXME: this is an approximation; it's not clear to me whether
     glyphs themselves are horizontal or vertical so much as the
     writing system or writing context. pango thinks it's a context
     issue, so we use that for now.
   */

  dir = pango_context_get_base_dir (vec->ctx);

  gdk_threads_leave ();

  return 
    ((dir == PANGO_DIRECTION_LTR) ||
     (dir == PANGO_DIRECTION_RTL));    
}


JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_gtk_GdkGlyphVector_isEqual 
  (JNIEnv *env, jobject self, jobject other)
{
  struct glyphvec *vec1 = NULL, *vec2 = NULL;
  jboolean eq = 0;

  gdk_threads_enter ();
  g_assert (self != NULL);
  vec1 = (struct glyphvec *)NSA_GET_GV_PTR (env, self);
  vec2 = (struct glyphvec *)NSA_GET_GV_PTR (env, other);
  g_assert (vec1 != NULL);
  g_assert (vec2 != NULL);
  
  /* FIXME: is there some more advantageous definition of equality for
     glyph vectors? */
  eq = (vec1 == vec2);
  
  gdk_threads_leave ();
  return eq;
}


