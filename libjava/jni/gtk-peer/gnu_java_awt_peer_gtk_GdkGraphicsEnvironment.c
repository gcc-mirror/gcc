/* gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.c
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
#include "gnu_java_awt_peer_gtk_GdkGraphicsEnvironment.h"


static gint
cmp_families (const void *a, const void *b)
{
  const char *a_name = pango_font_family_get_name (*(PangoFontFamily **)a);
  const char *b_name = pango_font_family_get_name (*(PangoFontFamily **)b);

  return g_utf8_collate (a_name, b_name);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_nativeGetFontFamilies
(JNIEnv *env, jobject  self __attribute__((unused)), jobjectArray family_name)
{
  PangoContext *context;
  PangoFontFamily **families;
  int n_families, idx;

  gdk_threads_enter ();

  context = gdk_pango_context_get();
  g_assert (context != NULL);

  pango_context_list_families (context, &families, &n_families);

  qsort (families, n_families, sizeof (PangoFontFamily *), cmp_families);

  for (idx = 0;  idx < n_families;  idx++)
    {
      const char *name_tmp =  pango_font_family_get_name (families[idx]);
      jstring name = (*env)->NewStringUTF (env, name_tmp);
      (*env)->SetObjectArrayElement (env, family_name, idx, name);
    }
  g_free (families);
  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL
Java_gnu_java_awt_peer_gtk_GdkGraphicsEnvironment_nativeGetNumFontFamilies
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)))
{
  PangoContext *context;
  PangoFontFamily **families;
  gint n_families;
  jint num;

  gdk_threads_enter ();

  context = gdk_pango_context_get();
  g_assert (context != NULL);

  pango_context_list_families (context, &families, &n_families);

  num = n_families;
  g_free (families);
  gdk_threads_leave ();
  
  return num;
}
