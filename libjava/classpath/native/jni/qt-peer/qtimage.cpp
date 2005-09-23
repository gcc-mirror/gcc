/* qtimage.cpp --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

#include <assert.h>
#include <QImage>
#include <QColor>
#include <QMatrix>
#include <QPainter>
#include <gnu_java_awt_peer_qt_QtImage.h>
#include "qtimage.h"
#include "qtstrings.h"
#include "qtgraphics.h"
#include "nativewrapper.h"

/* The constant fields in java.awt.Image */   
#define SCALE_DEFAULT      1
#define SCALE_FAST         2
#define SCALE_SMOOTH       4
#define SCALE_REPLICATE    8 
#define SCALE_AREA_AVERAGING  16

QImage *getQtImage( JNIEnv *env, jobject obj )
{
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "nativeObject", "J" );
  return (QImage *)env->GetLongField( obj, field );
}

static void setNativePtr( JNIEnv *env, jobject obj, void *value )
{
  jlong longValue = (jlong) value; 
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "nativeObject", "J" );
  env->SetLongField( obj, field, longValue );
}

/*
 * Creates a QImage.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_createImage
(JNIEnv *env, jobject obj)
{
  int width, height;
  jclass cls;
  jfieldID field;

  cls = env->GetObjectClass( obj );
  field = env->GetFieldID (cls, "width", "I");
  assert (field != 0);
  width = env->GetIntField(obj, field);

  field = env->GetFieldID(cls, "height", "I");
  assert (field != 0);
  height = env->GetIntField(obj, field);
  
  QImage *image = new QImage ( width, height, 
			       QImage::Format_ARGB32_Premultiplied );
  setNativePtr(env, obj, image);
}

/*
 * Frees the image data.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_freeImage
(JNIEnv *env, jobject obj)
{
   QImage *image = getQtImage(env, obj);
   setNativePtr(env, obj, NULL);
   if ( image )
     delete image;
}

/*
 * Clears the image to zero.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_clear
(JNIEnv *env, jobject obj)
{
  QImage *image = getQtImage(env, obj);
  assert( image );
  image->fill(0);
}

/*
 * Returns the pixel data in an int array.
 */
JNIEXPORT jintArray JNICALL Java_gnu_java_awt_peer_qt_QtImage_getPixels
(JNIEnv *env, jobject obj)
{
  QImage *image = getQtImage(env, obj);
  jintArray result_array;
  jint *result_array_ptr, *dst;
  int x, y;
  jint pixel;
  QRgb current;

  assert( image );

  result_array = env->NewIntArray (image->width() * image->height());
  dst = result_array_ptr = 
    env->GetIntArrayElements(result_array, NULL);

  // A bit inefficient.
  for ( y = 0; y < image->height(); y++)
      for ( x = 0; x < image->width(); x++)
	{
	  current = image->pixel(x, y);
	  pixel = 0;
	  pixel = (qAlpha(current) & 0xFF) << 24 | 
	    (qRed(current) & 0xFF) << 16 |
	    (qGreen(current) & 0xFF) << 8 |
	    (qBlue(current) & 0xFF);
	  *dst = pixel;
	  dst++;
	}

  env->ReleaseIntArrayElements (result_array, result_array_ptr, 0);
  return result_array;
}

/*
 * Sets the pixel data.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_setPixels
(JNIEnv *env, jobject obj, jintArray pixels)
{
  QImage *image = getQtImage(env, obj);
  assert( image );

  int width, height;
  jint *src_array, *src;

  width =  image->width();
  height = image->height();

  src = src_array = 
    env->GetIntArrayElements(pixels, NULL);

  for(int i = 0 ; i < height; i++)
    {
      uchar *scanline = image->scanLine( i );
      memcpy((void *)scanline, (void *)src, width * 4);
      src += width;
    }

  env->ReleaseIntArrayElements(pixels, src_array, 0);
}


/*
 * Loads an image from a file, 
 * returns true on success, false on failure.
 */
JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_qt_QtImage_loadImage
(JNIEnv *env, jobject obj, jstring fn)
{
  QString *filename = getQString(env, fn);

  QImage *image = new QImage();
  bool retVal = image->load( *filename );
  delete filename;

  if(image->isNull() && !retVal)
    {
      setNativePtr(env, obj, NULL);
      return JNI_FALSE;
    }

  setNativePtr(env, obj, image);
  
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "width", "I" );
  env->SetIntField( obj, field, image->width() );
  field = env->GetFieldID( cls, "height", "I" );
  env->SetIntField( obj, field, image->height() );
  
  return JNI_TRUE;
}

/*
 * Creates the image from an array of java bytes.
 */
JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_qt_QtImage_loadImageFromData
(JNIEnv *env, jobject obj, jbyteArray data)
{
  jbyte *src_array, *src;
  bool retVal;

  src = env->GetByteArrayElements(data, NULL);
  int len = env->GetArrayLength( data );

  QImage *image = new QImage();
  retVal = image->loadFromData( (uchar *) src, len);
  env->ReleaseByteArrayElements(data, src, 0);

  if(image->isNull() || retVal == false)
    {
      setNativePtr(env, obj, NULL);
      return JNI_FALSE;
    }

  setNativePtr(env, obj, image);
  
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "width", "I" );
  env->SetIntField( obj, field, image->width() );
  field = env->GetFieldID( cls, "height", "I" );
  env->SetIntField( obj, field, image->height() );
  
  return JNI_TRUE;
}


/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_createScaledImage
(JNIEnv *env, jobject obj, jobject src, jint hints)
{
  int w,h;
  jclass cls;
  jfieldID field;

  cls = env->GetObjectClass( obj );
  field = env->GetFieldID(cls, "width", "I");
  assert (field != 0);
  w = env->GetIntField(obj, field);

  field = env->GetFieldID(cls, "height", "I");
  assert (field != 0);
  h = env->GetIntField(obj, field);

  QImage *image = getQtImage(env, src);
  assert( image );
  QImage imageScaled;

  if (hints == SCALE_SMOOTH || hints == SCALE_AREA_AVERAGING)
    imageScaled = image->scaled(w, h, 
				Qt::IgnoreAspectRatio, 
				Qt::SmoothTransformation);
  else
    imageScaled = image->scaled(w, h, 
				Qt::IgnoreAspectRatio, 
				Qt::FastTransformation);
  QImage *scaledPtr = new QImage( imageScaled );
  
  // create new QtImage object
  setNativePtr( env, obj, scaledPtr );
}

/*
 * Simple draw without scaling.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_drawPixels
(JNIEnv *env, jobject obj, jobject graphics, jint bg_red, jint bg_green, 
 jint bg_blue, jint x, jint y, jboolean composite)
{
  QImage *image = getQtImage(env, obj);
  assert( image );
  QPainter *painter = getPainter( env, graphics );
  assert( painter );
  if(composite == JNI_TRUE)
    painter->fillRect ( x, y, image->width(), image->height(), 
 			QColor(bg_red, bg_green, bg_blue ) );
  painter->drawImage ( QPoint(x, y), *image );
}

/*
 * Draw the image with scaling.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_drawPixelsScaled
(JNIEnv *env, jobject obj, jobject graphics, 
 jint bg_red, jint bg_green, jint bg_blue, 
 jint x, jint y, jint w, jint h, jboolean composite)
{
  QImage *image = getQtImage(env, obj);
  assert( image );
  QPainter *painter = getPainter( env, graphics );
  assert( painter );

  if(composite == JNI_TRUE)
    painter->fillRect ( x, y, w, h, QColor(bg_red, bg_green, bg_blue ) );
  
  QRectF *srcRect = new QRectF((qreal)0, (qreal)0,
			       (qreal)image->width(), (qreal)image->height());
  QRectF *dstRect = new QRectF((qreal)x, (qreal)y,
			       (qreal)w, (qreal)h);
  
  if(composite == JNI_TRUE)
    painter->fillRect( *dstRect, QColor(bg_red, bg_green, bg_blue ) );

  painter->drawImage( *dstRect, *image, *srcRect);

  delete srcRect;
  delete dstRect;
}

/*
 * Draws a transformed image.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_drawPixelsTransformed
(JNIEnv *env, jobject obj, jobject graphics, jobject transform)
{
  QImage *originalImage = getQtImage(env, obj);
  assert( originalImage );
  QPainter *painter = getPainter( env, graphics );
  assert( painter );
  QMatrix *matrix = (QMatrix *)getNativeObject(env, transform);
  assert( matrix );

  // FIXME : Add rendering hint support here.
  QPoint p = matrix->map( QPoint(0,0) );
  QImage image = originalImage->transformed ( *matrix, Qt::FastTransformation );
  painter->drawImage(p, image);
}

/**
 * Draws the pixbuf at x, y, scaled to width and height and 
 * optionally composited and/or flipped with a given background color.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_qt_QtImage_drawPixelsScaledFlipped 
(JNIEnv *env, jobject obj, jobject graphics,
 jint bg_red, jint bg_green, jint bg_blue, 
 jboolean flipx, jboolean flipy,
 jint srcx, jint srcy, jint srcwidth, jint srcheight, 
 jint dstx, jint dsty, jint dstwidth, jint dstheight, 
 jboolean composite)
{
  QImage *originalImage = getQtImage(env, obj);
  assert( originalImage );
  QPainter *painter = getPainter( env, graphics );
  assert( painter );

  QRectF *srcRect = new QRectF((qreal)srcx, (qreal)srcy,
			       (qreal)srcwidth, (qreal)srcheight);
  QRectF *dstRect = new QRectF((qreal)dstx, (qreal)dsty,
			       (qreal)dstwidth, (qreal)dstheight);
  
  QImage image;
  if( flipx == JNI_TRUE || flipy == JNI_TRUE)
    image = originalImage->mirrored ( (flipx == JNI_TRUE), 
				      (flipy == JNI_TRUE) );
  else
    image = *originalImage;

  if(composite == JNI_TRUE)
    painter->fillRect( *dstRect, QColor(bg_red, bg_green, bg_blue ) );

  painter->drawImage( *dstRect, image, *srcRect);

  delete srcRect;
  delete dstRect;
}

/**
 * Copies an area of the image (used by Graphics)
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtImage_copyArea
(JNIEnv *env, jobject obj , jint x, jint y, jint w, jint h, jint dx, jint dy)
{
  QImage *image = getQtImage(env, obj);
  assert( image );
  QImage area = image->copy(x, y, w, h);
  QPainter *p = new QPainter( image );
  p->drawImage( x + dx, y + dy, area );
  delete p;
}
