/* gnu_java_awt_peer_gtk_GdkClasspathFontPeerMetrics.c
   Copyright (C) 1999, 2003 Free Software Foundation, Inc.

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

#include <math.h>

#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_GdkClasspathFontPeerMetrics.h"

#define ASCENT      0
#define MAX_ASCENT  1
#define DESCENT     2
#define MAX_DESCENT 3
#define MAX_ADVANCE 4
#define NUM_METRICS 5

JNIEXPORT jintArray JNICALL Java_gnu_java_awt_peer_gtk_GdkClasspathFontPeerMetrics_initState
  (JNIEnv *env, jobject self, jobject font)
{
  jintArray array;
  jint *metrics;
  struct peerfont *pf = NULL;
  FT_Matrix mat;

  pf = NSA_GET_FONT_PTR(env, font);
  g_assert (pf != NULL);

  array = (*env)->NewIntArray (env, NUM_METRICS);
  metrics = (*env)->GetIntArrayElements (env, array, NULL);

  gdk_threads_enter ();

#define DOUBLE_TO_26_6(d) ((FT_F26Dot6)((d) * 64.0))
#define DOUBLE_FROM_26_6(t) ((double)(t) / 64.0)
#define DOUBLE_TO_16_16(d) ((FT_Fixed)((d) * 65536.0))
#define DOUBLE_FROM_16_16(t) ((double)(t) / 65536.0)

  double pointsize = pango_font_description_get_size (pf->desc);
  pointsize /= (double) PANGO_SCALE;

  mat.xx = DOUBLE_TO_16_16(1);
  mat.xy = DOUBLE_TO_16_16(0);
  mat.yx = DOUBLE_TO_16_16(0);
  mat.yy = DOUBLE_TO_16_16(1);  
  
  FT_Face face = pango_ft2_font_get_face (pf->font);  
  FT_Set_Transform(face, &mat, NULL);
  FT_Set_Char_Size( face, 
		    DOUBLE_TO_26_6 (pointsize),
		    DOUBLE_TO_26_6 (pointsize),
		    0, 0);

  metrics[ASCENT]      = ceil (DOUBLE_FROM_26_6(face->size->metrics.ascender));
  metrics[MAX_ASCENT]  = metrics[ASCENT];
  metrics[DESCENT]     = floor (DOUBLE_FROM_26_6(face->size->metrics.descender));
  if (metrics[DESCENT] < 0)
    metrics[DESCENT] = - metrics[DESCENT];
  metrics[MAX_DESCENT] = metrics[DESCENT];
  metrics[MAX_ADVANCE] = ceil (DOUBLE_FROM_26_6(face->size->metrics.max_advance));

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements (env, array, metrics, 0);

  return array;
}

