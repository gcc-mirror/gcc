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

#include "gnu_java_awt_peer_gtk_CairoSurface.h"
#include "cairographics2d.h"

/**
 * Field names in CairoSurface.java
 */
#define SURFACE "surfacePointer"
#define SHARED "sharedBuffer"

/* prototypes */
static void setNativeObject( JNIEnv *env, jobject obj, void *ptr, const char *pointer );

/**
 * Creates a cairo surface, ARGB32, native ordering, premultiplied alpha.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_create
(JNIEnv *env, jobject obj, jint width, jint height, jint stride,
 jintArray buf )
{
  cairo_surface_t* surface;
  jboolean isCopy;

  /* Retrieve java-created data array */
  void *data = (*env)->GetIntArrayElements (env, buf, &isCopy);
  
  /* Set sharedBuffer variable */
  jclass cls = (*env)->GetObjectClass (env, obj);
  jfieldID field = (*env)->GetFieldID (env, cls, SHARED, "Z");
  g_assert (field != 0);
  
  if (isCopy == JNI_TRUE)
    {
      (*env)->SetBooleanField (env, obj, field, JNI_FALSE);
      void* temp = g_malloc(stride * height * 4);
      memcpy(temp, data, stride * height * 4);
      (*env)->ReleaseIntArrayElements (env, buf, data, 0);
      data = temp;
    }
  else
    (*env)->SetBooleanField (env, obj, field, JNI_TRUE);

  /* Create the cairo surface and set the java pointer */  	
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
 jlong surfacePointer, jintArray buf)
{
  cairo_surface_t* surface = JLONG_TO_PTR(void, surfacePointer);
  void *data = cairo_image_surface_get_data(surface);
  if( surface != NULL )
  {
  	/* Release or free the data buffer as appropriate */
    jclass cls = (*env)->GetObjectClass (env, obj);
    jfieldID field = (*env)->GetFieldID (env, cls, SHARED, "Z");
    g_assert (field != 0);
    jboolean sharedBuffer = (*env)->GetBooleanField (env, obj, field);

    if (sharedBuffer == JNI_TRUE)
  	  (*env)->ReleaseIntArrayElements (env, buf, data, 0);
  	else
  	  g_free(data);
  	  
  	/* Destroy the cairo surface itself */
    cairo_surface_destroy(surface);
  }
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_CairoSurface_nativeDrawSurface 
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong surfacePointer, jlong context, jdoubleArray java_matrix, double alpha,
 jint interpolation)
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
   switch ((enum java_awt_rendering_hints_filter) interpolation)
     {
     case java_awt_rendering_hints_VALUE_INTERPOLATION_NEAREST_NEIGHBOR:
       cairo_pattern_set_filter (p, CAIRO_FILTER_NEAREST);
       break;
     case java_awt_rendering_hints_VALUE_INTERPOLATION_BILINEAR:
       cairo_pattern_set_filter (p, CAIRO_FILTER_BILINEAR);
       break; 
     case java_awt_rendering_hints_VALUE_INTERPOLATION_BICUBIC:
       cairo_pattern_set_filter (p, CAIRO_FILTER_GAUSSIAN);
       break; 
     case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_SPEED:
       cairo_pattern_set_filter (p, CAIRO_FILTER_FAST);
       break;
     case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_DEFAULT:
       cairo_pattern_set_filter (p, CAIRO_FILTER_NEAREST);
       break;
     case java_awt_rendering_hints_VALUE_ALPHA_INTERPOLATION_QUALITY:
       cairo_pattern_set_filter (p, CAIRO_FILTER_BEST);
       break;
     }

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
 jlong surfacePointer)
{
  cairo_surface_t* surface;
  jint *src;
  jint *dst;
  int i, t, width, height;
  jclass cls;
  jfieldID field;

  /* Retrieve pointer to cairo data buffer */  
  surface = JLONG_TO_PTR(void, surfacePointer);
  src = (jint*)cairo_image_surface_get_data(surface);
  
  /* Retrieve dimensions of surface, from java fields */
  cls = (*env)->GetObjectClass (env, obj);
  field = (*env)->GetFieldID (env, cls, "width", "I");
  g_assert (field != 0);
  width = (*env)->GetIntField (env, obj, field);

  field = (*env)->GetFieldID (env, cls, "height", "I");
  g_assert (field != 0);
  height = (*env)->GetIntField (env, obj, field);

  /* Create destination array */
  g_assert( src != NULL );
  dst = g_malloc( width * height * sizeof( jint ) );

  /* Copy data into destination array, reversing sample order of each pixel */
  for(i = 0; i < (height * width); i++ )
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
 jlong surfacePointer,
 jint x, jint y, jint w, jint h, jint dx, jint dy, jint stride)
{
  int row;
  int srcOffset, dstOffset;
  jint *temp;
  
  /* Retrieve pointer to cairo data buffer */  
  cairo_surface_t* surface = JLONG_TO_PTR(void, surfacePointer);
  jint *pixeldata = (jint*)cairo_image_surface_get_data(surface);
  g_assert( pixeldata != NULL );

  /* Create temporary buffer and calculate offsets */
  temp = g_malloc( h * w * 4 );
  g_assert( temp != NULL );

  srcOffset = x + (y * stride);
  dstOffset = (x + dx) + ((y + dy) * stride);

  /* Copy desired region into temporary buffer */
  for( row = 0; row < h; row++ )
    memcpy( temp + (w * row), pixeldata + srcOffset + (stride * row), w * 4 );

  /* Copy out of buffer and to destination */
  for( row = 0; row < h; row++ )
    memcpy( pixeldata + dstOffset + (stride * row), temp + (w * row), w * 4 );

  g_free( temp );
}

/*
 * Synchronizes the java and native data buffers, copying any changes made in
 * the java array into the native array.
 * This method should only be called if (sharedBuffer == false). 
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_syncJavaToNative
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong surfacePointer, jintArray buffer)
{
  /* Get size of java array */
  int size = (*env)->GetArrayLength(env, buffer);
  
  /* Get native data buffer */
  cairo_surface_t* surface = JLONG_TO_PTR(void, surfacePointer);
  g_assert(surface != NULL);
  void* nativeBuffer = cairo_image_surface_get_data(surface);
  
  /* Sync buffers */
  (*env)->GetIntArrayRegion(env, buffer, 0, size, nativeBuffer);
}

/*
 * Synchronizes the java and native data buffers, copying any changes made in
 * the native array into the java array.
 * This method should only be called if (sharedBuffer == false). 
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_CairoSurface_syncNativeToJava
(JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
 jlong surfacePointer, jintArray buffer)
{
  /* Get size of java array */
  int size = (*env)->GetArrayLength(env, buffer);
  
  /* Get native data buffer */
  cairo_surface_t* surface = JLONG_TO_PTR(void, surfacePointer);
  g_assert(surface != NULL);
  void* nativeBuffer = cairo_image_surface_get_data(surface);
  
  /* Sync buffers */
  (*env)->SetIntArrayRegion(env, buffer, 0, size, nativeBuffer);
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
