/* gnu_java_awt_GdkTextLayout.c
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


#include <jni.h>
#include <gtk/gtk.h>
#include "native_state.h"
#include "gdkfont.h"
#include "gnu_java_awt_peer_gtk_GdkTextLayout.h"

struct state_table *cp_gtk_native_text_layout_state_table;

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkTextLayout_initStaticState 
  (JNIEnv *env, jclass clazz)
{
  NSA_TEXT_LAYOUT_INIT (env, clazz);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkTextLayout_initState
  (JNIEnv *env, jobject self)
{
  struct textlayout *tl;

  gdk_threads_enter ();

  g_assert(self != NULL);
  tl = g_malloc0 (sizeof (struct textlayout));
  g_assert(tl != NULL);
  tl->pango_layout = pango_layout_new(gdk_pango_context_get());
  g_assert(tl->pango_layout != NULL);
  NSA_SET_TEXT_LAYOUT_PTR (env, self, tl);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkTextLayout_setText
  (JNIEnv *env, jobject self, jstring text)
{
  struct textlayout *tl;
  gchar *str = NULL;
  gint len = 0;

  gdk_threads_enter ();

  g_assert(self != NULL);
  g_assert(text != NULL);

  tl = (struct textlayout *)NSA_GET_TEXT_LAYOUT_PTR (env, self);
  g_assert(tl != NULL);
  g_assert(tl->pango_layout != NULL);
  
  len = (*env)->GetStringUTFLength (env, text);
  str = (gchar *)(*env)->GetStringUTFChars (env, text, NULL);
  g_assert (str != NULL);

  pango_layout_set_text (tl->pango_layout, text, len);

  (*env)->ReleaseStringUTFChars (env, text, str);

  gdk_threads_leave ();  
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkTextLayout_indexToPos
  (JNIEnv *env, jobject self, jint idx, jdoubleArray javaPos)
{
  struct textlayout *tl;
  PangoRectangle pangoPos;
  jdouble *nativePos;

  gdk_threads_enter ();

  g_assert(self != NULL);
  g_assert(javaPos != NULL);

  tl = (struct textlayout *)NSA_GET_TEXT_LAYOUT_PTR (env, self);
  g_assert(tl != NULL);
  g_assert(tl->pango_layout != NULL);
  
  g_assert((*env)->GetArrayLength (env, javaPos) == 4);

  nativePos = (*env)->GetDoubleArrayElements (env, javaPos, NULL);    

  pango_layout_index_to_pos (tl->pango_layout, idx, &pangoPos);

  nativePos[0] = (jdouble) pangoPos.x;
  nativePos[1] = (jdouble) pangoPos.y;
  nativePos[2] = (jdouble) pangoPos.width;
  nativePos[3] = (jdouble) pangoPos.height;

  (*env)->ReleaseDoubleArrayElements (env, javaPos, nativePos, 0);

  gdk_threads_leave ();  
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkTextLayout_getExtents
  (JNIEnv *env, jobject self, jdoubleArray javaInkExtents, jdoubleArray javaLogExtents)
{
  struct textlayout *tl;
  PangoRectangle pangoInkExtents, pangoLogExtents;
  jdouble *nativeInkExtents, *nativeLogExtents;

  gdk_threads_enter ();

  g_assert(self != NULL);
  g_assert(javaInkExtents != NULL);
  g_assert(javaLogExtents != NULL);

  tl = (struct textlayout *)NSA_GET_TEXT_LAYOUT_PTR (env, self);
  g_assert(tl != NULL);
  g_assert(tl->pango_layout != NULL);
  
  g_assert((*env)->GetArrayLength (env, javaInkExtents) == 4);
  g_assert((*env)->GetArrayLength (env, javaLogExtents) == 4);

  nativeInkExtents = (*env)->GetDoubleArrayElements (env, javaInkExtents, NULL);    
  nativeLogExtents = (*env)->GetDoubleArrayElements (env, javaLogExtents, NULL);  

  pango_layout_get_extents (tl->pango_layout, 
			    &pangoInkExtents, &pangoLogExtents);

  nativeInkExtents[0] = (jdouble) pangoInkExtents.x;
  nativeInkExtents[1] = (jdouble) pangoInkExtents.y;
  nativeInkExtents[2] = (jdouble) pangoInkExtents.width;
  nativeInkExtents[3] = (jdouble) pangoInkExtents.height;

  nativeLogExtents[0] = (jdouble) pangoLogExtents.x;
  nativeLogExtents[1] = (jdouble) pangoLogExtents.y;
  nativeLogExtents[2] = (jdouble) pangoLogExtents.width;
  nativeLogExtents[3] = (jdouble) pangoLogExtents.height;

  (*env)->ReleaseDoubleArrayElements (env, javaInkExtents, nativeInkExtents, 0);
  (*env)->ReleaseDoubleArrayElements (env, javaLogExtents, nativeLogExtents, 0);

  gdk_threads_leave ();    
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkTextLayout_dispose
  (JNIEnv *env, jobject self)
{
  struct textlayout *tl;

  gdk_threads_enter ();

  g_assert(self != NULL);
  tl = (struct textlayout *) NSA_DEL_TEXT_LAYOUT_PTR (env, self);
  g_assert(tl != NULL);
  if (tl->pango_layout != NULL)
    g_object_unref (tl->pango_layout);
  g_free(tl);

  gdk_threads_leave ();
}
