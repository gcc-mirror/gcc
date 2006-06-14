/* gnu_java_awt_peer_gtk_CairoSurface.c
   Copyright (C)  2006 Free Software Foundation, Inc.

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

#include "jcl.h"
#include "gtkpeer.h"
#include <cairo-xlib.h>
#include <gdk/gdkx.h>

#include "gnu_java_awt_peer_gtk_CairoSurface.h"
#include "cairographics2d.h"

/**
 * Field names in CairoSurface.java
 */
#define SURFACE "surfacePointer"
#define BUFFER "bufferPointer"

/* prototypes */
static void setNativeObject( JNIEnv *env, jobject obj, void *ptr, const char *pointer );

/**
 * Creates a cairo surface, ARGB32, native ordering, premultiplied alpha.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_create (JNIEnv *env, jobject obj, jint width, jint height, jint stride)
{
  cairo_surface_t* surface;
  void *data = g_malloc(stride * height * 4);
  memset(data, 0, stride * height * 4);
  setNativeObject(env, obj, data, BUFFER);

  surface = cairo_image_surface_create_for_data
    (data, CAIRO_FORMAT_ARGB32, width, height, stride * 4);

  setNativeObject(env, obj, surface, SURFACE);
}

/**
 * Destroy the surface
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_destroy
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong surfacePointer, jlong bufferPointer)
{
  void *buffer;
  cairo_surface_t* surface = JLONG_TO_PTR(void, surfacePointer);
  if( surface != NULL )
    cairo_surface_destroy(surface);

  buffer = JLONG_TO_PTR(void, bufferPointer);
  if( buffer != NULL )
    g_free(buffer);
}

/**
 * Gets a pixel
 */
JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_nativeGetElem
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong bufferPointer, jint i)
{
  jint *pixeldata = JLONG_TO_PTR(void, bufferPointer);

  if( pixeldata == NULL )
    return 0;

  return pixeldata[i];
}

/**
 * Sets a pixel
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_nativeSetElem 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong bufferPointer, jint i, jint val)
{
  jint *pixeldata = JLONG_TO_PTR(void, bufferPointer);

  if( pixeldata == NULL )
    return;

  pixeldata[i] = val;
}

/**
 * Gets all pixels in an array
 */
JNIEXPORT jintArray JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_nativeGetPixels
(JNIEnv *env __attribute((unused)), jobject obj __attribute((unused)),
 jlong bufferPointer, int size)
{
  jint *pixeldata, *jpixdata;
  jintArray jpixels;

  pixeldata = JLONG_TO_PTR(void, bufferPointer);
  g_assert(pixeldata != NULL);

  jpixels = (*env)->NewIntArray (env, size);
  jpixdata = (*env)->GetIntArrayElements (env, jpixels, NULL);
  memcpy (jpixdata, pixeldata, size * sizeof( jint ));

  (*env)->ReleaseIntArrayElements (env, jpixels, jpixdata, 0);
  return jpixels;
}

/**
 * Sets all pixels by an array.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_nativeSetPixels
(JNIEnv *env, jobject obj, jlong bufferPointer, jintArray jpixels)
{
  jint *pixeldata, *jpixdata;
  int size;
  int width, height;
  jclass cls;
  jfieldID field;

  if( jpixels == NULL )
    return;

  cls = (*env)->GetObjectClass (env, obj);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  width = (*env)->GetIntField (env, obj, field);

  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  height = (*env)->GetIntField (env, obj, field);

  pixeldata = JLONG_TO_PTR(void, bufferPointer);
  g_assert(pixeldata != NULL);
  
  jpixdata = (*env)->GetIntArrayElements (env, jpixels, NULL);
  size = (*env)->GetArrayLength( env, jpixels );
  if( size > width * height ) size = width * height; /* stop overflows. */
  
  memcpy (pixeldata, jpixdata, size * sizeof( jint ));

  (*env)->ReleaseIntArrayElements (env, jpixels, jpixdata, 0);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoSurface_nativeDrawSurface 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong surfacePointer, jlong context, jdoubleArray java_matrix, double alpha)
{
  struct cairographics2d *gr = JLONG_TO_PTR(struct cairographics2d, context);
  cairo_t *cr = gr->cr;
  jdouble *native_matrix = NULL;
  cairo_surface_t* surface = JLONG_TO_PTR(void, surfacePointer);
  g_assert(surface != NULL);
  g_assert(cr != NULL);

  native_matrix = (*env)->GetDoubleArrayElements (env, java_matrix, NULL);
  g_assert (native_matrix != NULL);
  g_assert ((*env)->GetArrayLength (env, java_matrix) == 6);

 {
   cairo_matrix_t mat;
   cairo_pattern_t *p;
   cairo_matrix_init_identity (&mat);
   cairo_matrix_init (&mat, 
                      native_matrix[0], native_matrix[1],
                      native_matrix[2], native_matrix[3],
                      native_matrix[4], native_matrix[5]);

   p = cairo_pattern_create_for_surface (surface);
   cairo_pattern_set_matrix (p, &mat);

   cairo_set_source(cr, p);
   if (alpha == 1.0)
     cairo_paint(cr);
   else
     cairo_paint_with_alpha(cr, alpha);

   cairo_pattern_destroy(p);
 }
  
 (*env)->ReleaseDoubleArrayElements (env, java_matrix, native_matrix, 0);
}

JNIEXPORT jlong JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_getFlippedBuffer 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong bufferPointer, jint size)
{
  jint *dst;
  jint *src = JLONG_TO_PTR(void, bufferPointer);
  int i;
  int t;

  g_assert( src != NULL );
  dst = g_malloc( size * sizeof( jint ) );

  for(i = 0; i < size; i++ )
    {
      t = (src[i] & 0x0000FF) << 16;
      dst[i] = (src[i] & 0x00FF0000) >> 16;
      dst[i] |= (src[i] & 0xFF00FF00);
      dst[i] |= t;
    }

  return PTR_TO_JLONG(dst);
}

/**
 * Create and return a cairo context for drawing to the surface.
 */
JNIEXPORT jlong JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_nativeNewCairoContext
(JNIEnv *env __attribute((unused)), jobject obj __attribute((unused)),
 jlong surfacePointer)
{
  cairo_surface_t* surface = JLONG_TO_PTR(cairo_surface_t, surfacePointer);
  cairo_t *ptr;
  g_assert(surface != NULL);
  ptr = cairo_create(surface);
  g_assert(ptr != NULL);

  return PTR_TO_JLONG(ptr);
}

/**
 * copyArea.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_copyAreaNative2
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong bufferPointer,
 jint x, jint y, jint w, jint h, jint dx, jint dy, jint stride)
{
  int row;
  int srcOffset, dstOffset;
  jint *temp;
  jint *pixeldata = JLONG_TO_PTR(jint, bufferPointer);
  g_assert( pixeldata != NULL );

  temp = g_malloc( h * w * 4 );
  g_assert( temp != NULL );

  srcOffset = x + (y * stride);
  dstOffset = (x + dx) + ((y + dy) * stride);

  for( row = 0; row < h; row++ )
    memcpy( temp + (w * row), pixeldata + srcOffset + (stride * row), w * 4 );

  for( row = 0; row < h; row++ )
    memcpy( pixeldata + dstOffset + (stride * row), temp + (w * row), w * 4 );

  g_free( temp );
}

/*
 * Sets the native object field.
 */
static void 
setNativeObject( JNIEnv *env, jobject obj, void *ptr, const char *pointer )
{   
  jclass cls;
  jlong value;
  jfieldID nofid;
  cls = (*env)->GetObjectClass( env, obj );
  value = PTR_TO_JLONG(ptr); 
  nofid = (*env)->GetFieldID( env, cls, pointer, "J" );
  (*env)->SetLongField( env, obj, nofid, value );
  (*env)->DeleteLocalRef( env, cls );
}
