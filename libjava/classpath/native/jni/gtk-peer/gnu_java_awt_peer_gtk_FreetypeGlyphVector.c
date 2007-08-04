/* gnu_java_awt_FreetypeGlyphVector.c
   Copyright (C) 2006  Free Software Foundation, Inc.

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

#define PANGO_ENABLE_ENGINE
#include <jni.h>
#include <gtk/gtk.h>
#include <string.h>
#include <pango/pango.h>
#include <pango/pangoft2.h>
#include <pango/pangofc-font.h>
#include <freetype/ftglyph.h>
#include <freetype/ftoutln.h>
#include "jcl.h"
#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_FreetypeGlyphVector.h"
#include "cairographics2d.h"

typedef struct gp
{
  JNIEnv *env;
  jobject obj;
  double px;
  double py;
  double sx;
  double sy;
} generalpath ;

static PangoFcFont *
getFont(JNIEnv *env, jobject obj)
{
  jfieldID fid;
  jobject data;
  jclass cls;
  struct peerfont *pfont;

  cls = (*env)->GetObjectClass (env, obj);
  fid = (*env)->GetFieldID (env, cls, "peer", 
				 "Lgnu/java/awt/peer/gtk/GdkFontPeer;");
  g_assert (fid != 0);

  data = (*env)->GetObjectField (env, obj, fid);
  g_assert (data != NULL);

  pfont = (struct peerfont *) gtkpeer_get_font(env, data);
  g_assert (pfont != NULL);
  g_assert (pfont->font != NULL);

  return (PangoFcFont *)pfont->font;
}

static PangoFontset *
getFontSet(JNIEnv *env, jobject obj)
{
  jfieldID fid;
  jobject data;
  jclass cls;
  struct peerfont *pfont;

  cls = (*env)->GetObjectClass (env, obj);
  fid = (*env)->GetFieldID (env, cls, "peer", 
				 "Lgnu/java/awt/peer/gtk/GdkFontPeer;");
  g_assert (fid != 0);

  data = (*env)->GetObjectField (env, obj, fid);
  g_assert (data != NULL);

  pfont = (struct peerfont *) gtkpeer_get_font (env, data);
  g_assert (pfont != NULL);
  g_assert (pfont->font != NULL);

  return (PangoFontset *)pfont->set;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_FreetypeGlyphVector_getGlyphs
  (JNIEnv *env, jobject obj, jintArray codepoints, jintArray glyphs,
   jlongArray fonts)
{
  PangoFcFont *default_font, *current_font;
  PangoFontset *pfs;
  jint *cpvals;
  jint length;
  int i;

  /* Set up default font and fontset */
  default_font = getFont(env, obj);
  current_font = default_font;
  pfs = getFontSet(env, obj);

  /* Retrieve string information */
  length = (*env)->GetArrayLength (env, codepoints);
  cpvals = (*env)->GetIntArrayElements (env, codepoints, NULL);
  
  jint *glyphArray = (*env)->GetIntArrayElements (env, glyphs, NULL);
  jlong *fontArray = (*env)->GetLongArrayElements (env, fonts, NULL);

  /* A design goal of Pango is to be threadsafe, but it's admitted that it is
   * not actually threadsafe at the moment.  Using gdk locking here to be safe,
   * but I don't know if if actually helps at all... */ 
  gdk_threads_enter();

  for( i = 0; i < length; i++ )
  {
  	/* Ensure the current font has the requested character; if it doesn't,
  	 * try the default font before pulling a new font out of the fontset.
  	 * Once chosen, a font will be used until a character not in the font is
  	 * encountered. */ 
  	if (!pango_fc_font_has_char(current_font, cpvals[i]))
  	  {
  	    if (pango_fc_font_has_char(default_font, cpvals[i]))
  	      {
  	        current_font = default_font;
            g_object_ref(current_font);
  	      }
  	    else
  	      {
  	        current_font = (PangoFcFont*)pango_fontset_get_font(pfs, cpvals[i]);
  	      }
  	  }
  	else
      {
        g_object_ref(current_font);
      }
  	
  	/* Get glyph, and store both glyph and pointer to font */
    glyphArray[i] = (int)pango_fc_font_get_glyph(current_font,
                                                 (gunichar)cpvals[i]);
    fontArray[i] = PTR_TO_JLONG(current_font);
  }
  
  gdk_threads_leave();

  (*env)->ReleaseIntArrayElements (env, glyphs, glyphArray, 0);
  (*env)->ReleaseIntArrayElements (env, codepoints, cpvals, 0);
  (*env)->ReleaseLongArrayElements (env, fonts, fontArray, 0);
}

JNIEXPORT jobject JNICALL 
Java_gnu_java_awt_peer_gtk_FreetypeGlyphVector_getKerning
(JNIEnv *env, jobject obj __attribute__((unused)), jint rightGlyph, jint leftGlyph, jlong fnt)
{
  FT_Face ft_face;
  FT_Vector kern;
  jclass cls;
  jmethodID method;
  jvalue values[2];
  PangoFcFont *font;

  font = JLONG_TO_PTR(PangoFcFont, fnt);
  ft_face = pango_fc_font_lock_face( font );
  g_assert (ft_face != NULL);
  FT_Get_Kerning( ft_face, rightGlyph, leftGlyph, FT_KERNING_DEFAULT, &kern );

  pango_fc_font_unlock_face( font );

  values[0].d = (jdouble)kern.x/64.0;
  values[1].d = (jdouble)kern.y/64.0;

  cls = (*env)->FindClass (env, "java/awt/geom/Point2D$Double");
  method = (*env)->GetMethodID (env, cls, "<init>", "(DD)V");
  return (*env)->NewObjectA(env, cls, method, values);
}

JNIEXPORT jdoubleArray JNICALL 
Java_gnu_java_awt_peer_gtk_FreetypeGlyphVector_getMetricsNative
(JNIEnv *env, jobject obj __attribute__((unused)), jint glyphIndex, jlong fnt)
{
  FT_Face ft_face;
  jdouble *values;
  jdoubleArray retArray = NULL;
  PangoFcFont *font;

  font = JLONG_TO_PTR(PangoFcFont, fnt);
  ft_face = pango_fc_font_lock_face( font );

  g_assert (ft_face != NULL);

  FT_Set_Transform( ft_face, NULL, NULL );

  if( FT_Load_Glyph( ft_face, glyphIndex, FT_LOAD_NO_BITMAP ) != 0 )
    {
      pango_fc_font_unlock_face( font );
      printf("Couldn't load glyph %i\n", glyphIndex);
      return NULL;
    }

  retArray = (*env)->NewDoubleArray (env, 8);
  values = (*env)->GetDoubleArrayElements (env, retArray, NULL);

  values[0] = 0;
  values[1] = (jdouble)ft_face->glyph->advance.x/64.0;
  values[2] = (jdouble)ft_face->glyph->advance.y/64.0;
  values[3] = (jdouble)ft_face->glyph->metrics.horiBearingX/64.0;
  values[4] = -(jdouble)ft_face->glyph->metrics.horiBearingY/64.0;
  values[5] = (jdouble)ft_face->glyph->metrics.width/64.0;
  values[6] = (jdouble)ft_face->glyph->metrics.height/64.0;
  values[7] = 0;

  (*env)->ReleaseDoubleArrayElements (env, retArray, values, 0);
  pango_fc_font_unlock_face( font );

  return retArray;
}

/* GetOutline code follows ****************************/
/********* Freetype callback functions *****************************/

static int _moveTo( const FT_Vector* to,
		    void *p)
{
  JNIEnv *env;
  jobject obj;
  jclass cls;
  jmethodID method;
  jvalue values[2];
  generalpath *path = (generalpath *) p;

  env = path->env;
  obj = path->obj;

  values[0].f = (jfloat)(to->x * path->sx + path->px);
  values[1].f = (jfloat)(to->y * path->sy + path->py);

  cls = (*env)->FindClass (env, "java/awt/geom/GeneralPath");
  method = (*env)->GetMethodID (env, cls, "moveTo", "(FF)V");
  (*env)->CallVoidMethodA(env, obj, method, values );

  return 0;
}

static int _lineTo( const FT_Vector*  to,
		    void *p)
{
  JNIEnv *env;
  jobject obj;
  jclass cls;
  jmethodID method;
  jvalue values[2];
  generalpath *path = (generalpath *) p;

  env = path->env;
  obj = path->obj; 
  values[0].f = (jfloat)(to->x * path->sx + path->px);
  values[1].f = (jfloat)(to->y * path->sy + path->py);

  cls = (*env)->FindClass (env, "java/awt/geom/GeneralPath");
  method = (*env)->GetMethodID (env, cls, "lineTo", "(FF)V");
  (*env)->CallVoidMethodA(env, obj, method, values );

  return 0;
}

static int _quadTo( const FT_Vector*  cp,
		    const FT_Vector*  to,
		    void *p)
{
  JNIEnv *env;
  jobject obj;
  jclass cls;
  jmethodID method;
  jvalue values[4];
  generalpath *path = (generalpath *) p;

  env = path->env;
  obj = path->obj;
  values[0].f = (jfloat)(cp->x * path->sx + path->px);
  values[1].f = (jfloat)(cp->y * path->sy + path->py);
  values[2].f = (jfloat)(to->x * path->sx + path->px);
  values[3].f = (jfloat)(to->y * path->sy + path->py);

  cls = (*env)->FindClass (env, "java/awt/geom/GeneralPath");
  method = (*env)->GetMethodID (env, cls, "quadTo", "(FFFF)V");
  (*env)->CallVoidMethodA(env, obj, method, values );

  return 0;
}

static int _curveTo( const FT_Vector*  cp1,
		     const FT_Vector*  cp2,
		     const FT_Vector*  to,
		     void *p)
{
  JNIEnv *env;
  jobject obj;
  jclass cls;
  jmethodID method;
  jvalue values[6];
  generalpath *path = (generalpath *) p;

  env = path->env;
  obj = path->obj;
  values[0].f = (jfloat)(cp1->x * path->sx + path->px);
  values[1].f = (jfloat)(cp1->y * path->sy + path->py);
  values[2].f = (jfloat)(cp2->x * path->sx + path->px);
  values[3].f = (jfloat)(cp2->y * path->sy + path->py);
  values[4].f = (jfloat)(to->x * path->sx + path->px);
  values[5].f = (jfloat)(to->y * path->sy + path->py);

  cls = (*env)->FindClass (env, "java/awt/geom/GeneralPath");
  method = (*env)->GetMethodID (env, cls, "curveTo", "(FFFFFF)V");
  (*env)->CallVoidMethodA(env, obj, method, values );

  return 0;
}


JNIEXPORT jobject JNICALL 
Java_gnu_java_awt_peer_gtk_FreetypeGlyphVector_getGlyphOutlineNative
 (JNIEnv *env, jobject obj __attribute__((unused)), jint glyphIndex, jlong fnt)
{
  generalpath *path;
  jobject gp;
  FT_Outline_Funcs ftCallbacks =
    {
      (FT_Outline_MoveToFunc) _moveTo,
      (FT_Outline_LineToFunc) _lineTo,
      (FT_Outline_ConicToFunc) _quadTo,
      (FT_Outline_CubicToFunc) _curveTo,
      0,
      0
    };
  PangoFcFont *font;
  FT_Face ft_face;
  FT_Glyph glyph;

  font = JLONG_TO_PTR(PangoFcFont, fnt);
  ft_face = pango_fc_font_lock_face( font );

  g_assert (ft_face != NULL);

  path = g_malloc0 (sizeof (generalpath));
  g_assert(path != NULL);
  path->env = env;

  path->px = path->py = 0.0;
  path->sx = 1.0/64.0;
  path->sy = -1.0/64.0;

  {  /* create a GeneralPath instance */
    jclass cls;
    jmethodID method;
    
    cls = (*env)->FindClass (env, "java/awt/geom/GeneralPath");
    method = (*env)->GetMethodID (env, cls, "<init>", "()V");
    gp = path->obj = (*env)->NewObject (env, cls, method);
  }
	      
  if(FT_Load_Glyph(ft_face,
		   (FT_UInt)(glyphIndex),
		   FT_LOAD_DEFAULT | FT_LOAD_NO_BITMAP) != 0)
    {
      pango_fc_font_unlock_face( font );
      g_free(path); 
      return NULL;
    }

  FT_Get_Glyph( ft_face->glyph, &glyph );
  FT_Outline_Decompose (&(((FT_OutlineGlyph)glyph)->outline),
			&ftCallbacks, path);
  FT_Done_Glyph( glyph );
  
  pango_fc_font_unlock_face( font );

  g_free(path); 

  return gp; 
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_FreetypeGlyphVector_dispose
 (JNIEnv *env, jobject obj __attribute__((unused)), jlongArray fontset)
{
  PangoFcFont *font;
  jlong *fontArray; 
  int i, length;

  length = (*env)->GetArrayLength (env, fontset);
  fontArray = (*env)->GetLongArrayElements (env, fontset, NULL);
  
  gdk_threads_enter();
  
  for( i = 0; i < length; i++ )
  {
    font = JLONG_TO_PTR(PangoFcFont, fontArray[i]);
    g_object_unref(font);
  }
  
  gdk_threads_leave();

  (*env)->ReleaseLongArrayElements (env, fontset, fontArray, 0);
}

JNIEXPORT jlong JNICALL 
Java_gnu_java_awt_peer_gtk_FreetypeGlyphVector_getNativeFontPointer
 (JNIEnv *env, jobject obj, jint n)
{
  int i;
  PangoFcFont *font = getFont(env, obj);
  
  for (i = 0; i < n; i++)
    g_object_ref(font);
  
  return PTR_TO_JLONG(font);
}
