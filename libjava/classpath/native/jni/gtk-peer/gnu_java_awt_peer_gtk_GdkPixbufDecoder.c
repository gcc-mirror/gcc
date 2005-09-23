/* gdkpixbufdecoder.c
   Copyright (C) 1999, 2003, 2004, 2005 Free Software Foundation, Inc.

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

#include <gtkpeer.h>
#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixbuf-loader.h>

#include <jni.h>
#include <jcl.h>
#include "native_state.h"
#include "gnu_java_awt_peer_gtk_GdkPixbufDecoder.h"

#include <string.h>
#include <stdlib.h>

static struct state_table *native_pixbufdecoder_state_table;

#define NSA_PB_INIT(env, clazz) \
  native_pixbufdecoder_state_table = cp_gtk_init_state_table (env, clazz)

#define NSA_GET_PB_PTR(env, obj) \
  cp_gtk_get_state (env, obj, native_pixbufdecoder_state_table)

#define NSA_SET_PB_PTR(env, obj, ptr) \
  cp_gtk_set_state (env, obj, native_pixbufdecoder_state_table, (void *)ptr)

#define NSA_DEL_PB_PTR(env, obj) \
  cp_gtk_remove_state_slot (env, obj, native_pixbufdecoder_state_table)

/* Union used for type punning. */
union env_union
{
  void **void_env;
  JNIEnv **jni_env;
};

static JavaVM *vm;

static jmethodID areaPreparedID;
static jmethodID areaUpdatedID;
static jmethodID dataOutputWriteID;
static jmethodID registerFormatID;

static void
area_prepared_cb (GdkPixbufLoader *loader, 
	       jobject *decoder)
{
  JNIEnv *env = NULL;
  union env_union e;
  jint width = 0;
  jint height = 0;
  GdkPixbuf *pixbuf = NULL;

  pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
  g_assert (pixbuf != NULL);

  width = gdk_pixbuf_get_width (pixbuf); 
  height = gdk_pixbuf_get_height (pixbuf);

  g_assert (decoder != NULL);

  e.jni_env = &env;
  (*vm)->GetEnv (vm, e.void_env, JNI_VERSION_1_1);

  (*env)->CallVoidMethod (env,
			  *decoder,
			  areaPreparedID,
			  width, height);
}

static void
area_updated_cb (GdkPixbufLoader *loader, 
	      gint x, gint y, 
	      gint width, gint height,
	      jobject *decoder)
{
  JNIEnv *env;
  union env_union e;
  jint stride_bytes, stride_pixels, n_channels, n_pixels;
  jintArray jpixels;  
  jint *java_pixels;
  guchar *gdk_pixels;

  GdkPixbuf *pixbuf_no_alpha = NULL;
  GdkPixbuf *pixbuf = NULL;

#ifndef WORDS_BIGENDIAN
  int i;
#endif

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

  e.jni_env = &env;
  (*vm)->GetEnv (vm, e.void_env, JNI_VERSION_1_1);

  jpixels = (*env)->NewIntArray (env, n_pixels);

  java_pixels = (*env)->GetIntArrayElements (env, jpixels, NULL);

  memcpy (java_pixels, 
	  gdk_pixels + (y * stride_bytes), 
	  (height * stride_bytes));

#ifndef WORDS_BIGENDIAN
  /* convert pixels from 0xBBGGRRAA to 0xAARRGGBB */
  for (i = 0; i < n_pixels; ++i)
    {
      java_pixels[i] = SWAPU32 ((unsigned)java_pixels[i]);
    }
#endif

  g_object_unref (pixbuf);

  (*env)->ReleaseIntArrayElements (env, jpixels, java_pixels, 0);

  (*env)->CallVoidMethod (env, 
			  *decoder, 
			  areaUpdatedID,
			  (jint) x, (jint) y,
			  (jint) width, (jint) height,
			  jpixels,
			  stride_pixels);

  (*env)->DeleteLocalRef(env, jpixels);
}

static void
closed_cb (GdkPixbufLoader *loader __attribute__((unused)), jobject *decoder)
{
  JNIEnv *env;
  union env_union e;
  e.jni_env = &env;
  (*vm)->GetEnv (vm, e.void_env, JNI_VERSION_1_1);

  (*env)->DeleteGlobalRef (env, *decoder); 
  g_free (decoder);
}



JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_initState
  (JNIEnv *env, jobject obj)
{
  GdkPixbufLoader *loader = NULL;
  jobject *decoder = NULL;

  gdk_threads_enter ();

  decoder = (jobject *) g_malloc (sizeof (jobject));
  g_assert (decoder != NULL);
  *decoder = (*env)->NewGlobalRef (env, obj);

  loader = gdk_pixbuf_loader_new ();
  g_assert (loader != NULL);  
  g_signal_connect (loader, "area-prepared", G_CALLBACK (area_prepared_cb), decoder);  
  g_signal_connect (loader, "area-updated", G_CALLBACK (area_updated_cb), decoder);
  g_signal_connect (loader, "closed", G_CALLBACK (closed_cb), decoder);

  NSA_SET_PB_PTR (env, obj, loader);

  gdk_threads_leave ();
}

static void
query_formats (JNIEnv *env, jclass clazz)
{
  jobject jformat;
  GSList *formats, *f;
  GdkPixbufFormat *format;
  char **ch, *name;

  jclass formatClass;
  jmethodID addExtensionID;
  jmethodID addMimeTypeID;
  jobject string;

  formatClass = (*env)->FindClass
    (env, "gnu/java/awt/peer/gtk/GdkPixbufDecoder$ImageFormatSpec");

  g_assert(formatClass != NULL);

  addExtensionID = (*env)->GetMethodID (env, formatClass, 
				        "addExtension", 
					"(Ljava/lang/String;)V");

  addMimeTypeID = (*env)->GetMethodID (env, formatClass, 
				       "addMimeType", 
				       "(Ljava/lang/String;)V");
  
  formats = gdk_pixbuf_get_formats ();

  for (f = formats; f; f = f->next)
    {
      format = (GdkPixbufFormat *) f->data;
      name = gdk_pixbuf_format_get_name(format);

      string = (*env)->NewStringUTF(env, name);
      g_assert(string != NULL);

      jformat = (*env)->CallStaticObjectMethod 
	(env, clazz, registerFormatID, string,
	 (jboolean) gdk_pixbuf_format_is_writable(format));
      (*env)->DeleteLocalRef(env, string);

      g_assert(jformat != NULL);
      
      ch = gdk_pixbuf_format_get_extensions(format);
      while (*ch)
	{
	  string = (*env)->NewStringUTF(env, *ch);
	  g_assert(string != NULL);
	  (*env)->CallVoidMethod (env, jformat, addExtensionID, string);
	  (*env)->DeleteLocalRef(env, string);
	  ++ch;
	}
      
      ch = gdk_pixbuf_format_get_mime_types(format);
      while (*ch)
	{
	  string = (*env)->NewStringUTF(env, *ch);
	  g_assert(string != NULL);
	  (*env)->CallVoidMethod (env, jformat, addMimeTypeID, string);
	  (*env)->DeleteLocalRef(env, string);
	  ++ch;
	}
    }
  
  g_slist_free(formats);  
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_initStaticState 
  (JNIEnv *env, jclass clazz)
{
  jclass dataOutputClass;

  (*env)->GetJavaVM(env, &vm);

  areaPreparedID = (*env)->GetMethodID (env, clazz, 
				        "areaPrepared", 
					"(II)V");

  areaUpdatedID = (*env)->GetMethodID (env, clazz,
				       "areaUpdated",
				       "(IIII[II)V");

  registerFormatID = (*env)->GetStaticMethodID 
    (env, clazz, 
     "registerFormat", 
     "(Ljava/lang/String;Z)"
     "Lgnu/java/awt/peer/gtk/GdkPixbufDecoder$ImageFormatSpec;");

  
  dataOutputClass = (*env)->FindClass(env, "java/io/DataOutput");
  dataOutputWriteID = (*env)->GetMethodID (env, dataOutputClass,
					     "write", "([B)V");

  query_formats (env, clazz);
  
  NSA_PB_INIT (env, clazz);
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_finish
(JNIEnv *env, jobject obj, jboolean needs_close)
{
  GdkPixbufLoader *loader = NULL;

  gdk_threads_enter ();

  loader = (GdkPixbufLoader *)NSA_DEL_PB_PTR (env, obj);
  if (loader == NULL)
    return;

  if (needs_close)
    gdk_pixbuf_loader_close (loader, NULL);
  g_object_unref (loader);

  gdk_threads_leave (); 
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_pumpDone
(JNIEnv *env, jobject obj)
{
  GError *err = NULL;
  GdkPixbufLoader *loader = NULL;

  gdk_threads_enter ();

  loader = (GdkPixbufLoader *)NSA_GET_PB_PTR (env, obj);
  g_assert (loader != NULL);

  gdk_pixbuf_loader_close (loader, &err);

  if (err != NULL)
    {
      JCL_ThrowException (env, "java/io/IOException", err->message);
      g_error_free (err);
    }

  gdk_threads_leave ();
}

struct stream_save_request
{
  JNIEnv *env;
  jobject *stream;
};

static gboolean
save_to_stream(const gchar *buf,
	       gsize count,
	       GError **error __attribute__((unused)),
	       gpointer data)
{
  struct stream_save_request *ssr = (struct stream_save_request *)data;

  jbyteArray jbuf;
  jbyte *cbuf;

  gdk_threads_leave ();

  jbuf = (*(ssr->env))->NewByteArray ((ssr->env), count);
  cbuf = (*(ssr->env))->GetByteArrayElements ((ssr->env), jbuf, NULL);
  memcpy (cbuf, buf, count);
  (*(ssr->env))->ReleaseByteArrayElements ((ssr->env), jbuf, cbuf, 0);
  (*(ssr->env))->CallVoidMethod ((ssr->env), *(ssr->stream), 
				 dataOutputWriteID, jbuf);  

  gdk_threads_enter ();

  return TRUE;
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_streamImage
(JNIEnv *env, jclass clazz __attribute__((unused)), 
 jintArray jarr, jstring jenctype, jint width, jint height, 
 jboolean hasAlpha, jobject stream)
{
  GdkPixbuf* pixbuf;  
  jint *ints;
  guchar a, r, g, b, *pix, *p;
  GError *err = NULL;
  const char *enctype;
  int i;
  struct stream_save_request ssr;

  gdk_threads_enter ();

  ssr.stream = &stream;
  ssr.env = env;

  ints = (*env)->GetIntArrayElements (env, jarr, NULL);
  pix = g_malloc(width * height * (hasAlpha ? 4 : 3));

  enctype = (*env)->GetStringUTFChars (env, jenctype, NULL);
  g_assert(enctype != NULL);

  g_assert (pix != NULL);
  g_assert (ints != NULL);

  p = pix;
  for (i = 0; i < width*height; ++i)
    {
      /* 
       * Java encodes pixels as integers in a predictable arithmetic order:
       * 0xAARRGGBB. Since these are jints, JNI has already byte-swapped
       * them for us if necessary, so they're in "our" endianness, whatever
       * that is. It uses 4 bytes per pixel whether or not there's an alpha
       * channel.
       */

      a = 0xff & (ints[i] >> 24);
      r = 0xff & (ints[i] >> 16);
      g = 0xff & (ints[i] >> 8);
      b = 0xff & ints[i];

      /* 
       * GDK-pixbuf has a very different storage model:
       *
       *  - A different alpha order (alpha after colors).
       *  - A different packing model (no alpha -> 3-bytes-per-pixel).
       *  - A different "RGB" order (host memory order, not endian-neutral).
       */

      *p++ = r;
      *p++ = g;
      *p++ = b;
      if (hasAlpha)
	*p++ = a;
    }

  pixbuf =  gdk_pixbuf_new_from_data (pix,
				      GDK_COLORSPACE_RGB,
				      (gboolean) hasAlpha,
				      8, width, height, 
				      width * (hasAlpha ? 4 : 3), /* rowstride */
				      NULL, NULL);
  g_assert (pixbuf != NULL);

  g_assert(gdk_pixbuf_save_to_callback (pixbuf,
					&save_to_stream,
					&ssr,
					enctype,
					&err, NULL));

  g_object_unref (pixbuf);

  g_free(pix);

  (*env)->ReleaseStringUTFChars (env, jenctype, enctype);  
  (*env)->ReleaseIntArrayElements (env, jarr, ints, 0);

  gdk_threads_leave ();
}


JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GdkPixbufDecoder_pumpBytes
  (JNIEnv *env, jobject obj, jbyteArray jarr, jint len)
{
  GdkPixbufLoader *loader = NULL;
  jbyte *bytes = NULL;
  GError *err = NULL;

  gdk_threads_enter ();

  g_assert (len >= 1);
  g_assert (jarr != NULL);

  bytes = (*env)->GetByteArrayElements (env, jarr, NULL);
  g_assert (bytes != NULL);
  loader = (GdkPixbufLoader *)NSA_GET_PB_PTR (env, obj);
  g_assert (loader != NULL);

  gdk_pixbuf_loader_write (loader, (const guchar *) bytes, len, &err);

  (*env)->ReleaseByteArrayElements (env, jarr, bytes, 0);

  if (err != NULL)
    {
      JCL_ThrowException (env, "java/io/IOException", err->message);
      g_error_free (err);
    }

  gdk_threads_leave ();
}
