/* gdkpixbufdecoder.c
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


#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixbuf-loader.h>

#include "gtkpeer.h"
#include "gnu_java_awt_peer_gtk_GdkPixbufDecoder.h"

struct state_table *native_pixbufdecoder_state_table;

#define NSA_PB_INIT(env, clazz) \
  native_pixbufdecoder_state_table = init_state_table (env, clazz)

#define NSA_GET_PB_PTR(env, obj) \
  get_state (env, obj, native_pixbufdecoder_state_table)

#define NSA_SET_PB_PTR(env, obj, ptr) \
  set_state (env, obj, native_pixbufdecoder_state_table, (void *)ptr)

#define NSA_DEL_PB_PTR(env, obj) \
  remove_state_slot (env, obj, native_pixbufdecoder_state_table)


jmethodID areaPreparedID;
jmethodID areaUpdatedID;

static void
area_prepared (GdkPixbufLoader *loader, 
	       jobject *decoder)
{
  jint width, height;

  GdkPixbuf *pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
  if (pixbuf == NULL)
    return;

  width = gdk_pixbuf_get_width (pixbuf); 
  height = gdk_pixbuf_get_height (pixbuf);

  gdk_threads_leave ();

  g_assert (decoder != NULL);

  (*gdk_env)->CallVoidMethod (gdk_env,
			      *decoder,
			      areaPreparedID,
			      width, height);

  gdk_threads_enter ();
}

static void
area_updated (GdkPixbufLoader *loader, 
	      gint x, gint y, 
	      gint width, gint height,
	      jobject *decoder)
{
  jint stride_bytes, stride_pixels, n_channels, n_pixels;
  int i, px;
  jintArray jpixels;  
  jint *java_pixels;
  guchar *gdk_pixels;

  GdkPixbuf *pixbuf_no_alpha = NULL;
  GdkPixbuf *pixbuf = NULL;
  
  pixbuf_no_alpha = gdk_pixbuf_loader_get_pixbuf (loader);
  if (pixbuf_no_alpha == NULL)
    return;

  pixbuf = gdk_pixbuf_add_alpha(pixbuf_no_alpha, FALSE, 0, 0, 0);
  g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
  
  stride_bytes = gdk_pixbuf_get_rowstride (pixbuf);
  n_channels = gdk_pixbuf_get_n_channels (pixbuf);
  stride_pixels =  stride_bytes / n_channels;
  n_pixels = height * stride_pixels;
  gdk_pixels = gdk_pixbuf_get_pixels (pixbuf);

  jpixels = (*gdk_env)->NewIntArray (gdk_env, n_pixels);
  java_pixels = (*gdk_env)->GetIntArrayElements (gdk_env, jpixels, NULL);

  memcpy (java_pixels, 
	  gdk_pixels + (y * stride_bytes), 
	  (height * stride_bytes));

  for (i = 0; i < n_pixels; ++i)
    {
      px = java_pixels[i];

      /* move alpha around (GdkPixbufLoader results are AGBR not GBRA, in
	 the lsb sense) */
      /* px = ((px >> 24) & 0xff) | ((px << 8) & 0xffffff00); */

      /* it appears to require a full byte swap, now, not just a shift to
	 the A channel. why did this change? don't know. */
      px = ((px >>  8) & 0x00ff00ff) | ((px <<  8) & 0xff00ff00); 
      px = ((px >> 16) & 0x0000ffff) | ((px << 16) & 0xffff0000); 

      java_pixels[i] = px;
    }

  g_object_unref (pixbuf);

  gdk_threads_leave ();

  (*gdk_env)->ReleaseIntArrayElements (gdk_env, jpixels, java_pixels, 0);
  (*gdk_env)->CallVoidMethod (gdk_env, 
			      *decoder, 
			      areaUpdatedID,
			      (jint) x, (jint) y,
			      (jint) width, (jint) height,
			      jpixels,
			      stride_pixels);
  gdk_threads_enter ();
}

static void
closed (GdkPixbufLoader *loader __attribute__((unused)), jobject *decoder)
{
  gdk_threads_leave ();
  (*gdk_env)->DeleteGlobalRef (gdk_env, *decoder); 
  free (decoder);
  gdk_threads_enter ();
}



JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_initState
  (JNIEnv *env, jobject obj)
{
  GdkPixbufLoader *loader = NULL;
  jobject *decoder = NULL;

  decoder = (jobject *) malloc (sizeof (jobject));
  g_assert (decoder != NULL);
  *decoder = (*env)->NewGlobalRef (env, obj);

  gdk_threads_enter ();
  loader = gdk_pixbuf_loader_new ();
  g_assert (loader != NULL);  
  g_signal_connect (loader, "area-prepared", G_CALLBACK (area_prepared), decoder);  
  g_signal_connect (loader, "area-updated", G_CALLBACK (area_updated), decoder);
  g_signal_connect (loader, "closed", G_CALLBACK (closed), decoder);
  gdk_threads_leave ();

  NSA_SET_PB_PTR (env, obj, loader);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_initStaticState 
  (JNIEnv *env, jclass clazz)
{
  areaPreparedID = (*env)->GetMethodID (env, clazz, 
				        "areaPrepared", 
					"(II)V");

  areaUpdatedID = (*env)->GetMethodID (env, clazz,
				       "areaUpdated",
				       "(IIII[II)V");
  NSA_PB_INIT (env, clazz);
}


JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_finish
  (JNIEnv *env, jobject obj)
{
  GdkPixbufLoader *loader = NULL;

  loader = (GdkPixbufLoader *)NSA_DEL_PB_PTR (env, obj);
  if (loader == NULL)
    return;

  gdk_threads_enter ();
  gdk_pixbuf_loader_close (loader, NULL);
  g_object_unref (loader);
  gdk_threads_leave (); 
}


JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_pumpBytes
  (JNIEnv *env, jobject obj, jbyteArray jarr, jint len)
{
  GdkPixbufLoader *loader = NULL;
  jbyte *bytes = NULL;

  if (len < 1)
    return;

  bytes = (*gdk_env)->GetByteArrayElements (gdk_env, jarr, NULL);
  g_assert (bytes != NULL);
  loader = (GdkPixbufLoader *)NSA_GET_PB_PTR (env, obj);
  g_assert (loader != NULL);

  gdk_threads_enter ();
  gdk_pixbuf_loader_write (loader, bytes, len, NULL);
  gdk_threads_leave ();

  (*gdk_env)->ReleaseByteArrayElements (gdk_env, jarr, bytes, 0);
}
