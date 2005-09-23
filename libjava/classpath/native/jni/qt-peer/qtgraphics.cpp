/* qtgraphics.cpp --
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
#include <jni.h>
#include <QPainter>
#include <QBrush>
#include <QLinearGradient>
#include <QPen>
#include <QPaintDevice>
#include <QPainterPath>
#include <QImage>
#include <QColor>
#include <gnu_java_awt_peer_qt_QtGraphics.h>
#include "nativewrapper.h"
#include "qtimage.h"
#include "qtstrings.h"
#include "qtcomponent.h"
#include "qtgraphics.h"
#include "qtfont.h"

// Constants from java.awt.AlphaComposite
#define CLEAR     1
#define SRC       2
#define DST       9
#define SRC_OVER  3
#define DST_OVER  4
#define SRC_IN    5
#define DST_IN    6
#define SRC_OUT   7
#define DST_OUT   8
#define SRC_ATOP  10
#define DST_ATOP  11
#define XOR       12

GraphicsPainter *getPainter( JNIEnv *env, jobject obj )
{
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "nativeObject", "J" );
  return (GraphicsPainter *)env->GetLongField( obj, field );
}

static void setNativePtr( JNIEnv *env, jobject obj, void *value )
{
  jlong longValue = (jlong) value;
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "nativeObject", "J" );
  env->SetLongField( obj, field, longValue );
}

static jobject getToolkit( JNIEnv *env, jobject obj )
{
  jclass cls = env->FindClass( "gnu/java/awt/peer/qt/QtGraphics" );

  jfieldID field = env->GetFieldID( cls, "toolkit",
				    "Lgnu/java/awt/peer/qt/QtToolkit;" );
  return env->GetObjectField( obj, field );
}

///////////////////////// JNI methods ////////////////////////////////

/**
 * Clones the parent QPainter object.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_cloneNativeContext
(JNIEnv *env, jobject obj, jobject parent)
{
  GraphicsPainter *painter = (GraphicsPainter *)getPainter( env, parent );
  assert( painter );
  QPainter *newPainter = new GraphicsPainter( painter->device() );
  assert( newPainter );
  setNativePtr(env, obj, newPainter);
}

/*
 * Start of JNI methods 
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_initImage
(JNIEnv *env, jobject obj, jobject image)
{
  QImage *im = getQtImage( env, image );
  assert( im );
  QPainter *painter = new GraphicsPainter( im );
  assert( painter );
  setNativePtr(env, obj, painter);
  painter->setRenderHint(QPainter::TextAntialiasing);
  painter->setRenderHint(QPainter::Antialiasing);
  painter->setRenderHint(QPainter::SmoothPixmapTransform);
}

/*
 * Start of JNI methods 
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_initVolatileImage
(JNIEnv *env, jobject obj, jobject image)
{
  QPixmap *im = getQtVolatileImage( env, image );
  assert( im );
  QPainter *painter = new GraphicsPainter( im );
  assert( painter );
  setNativePtr(env, obj, painter);
  painter->setRenderHint(QPainter::TextAntialiasing);
  painter->setRenderHint(QPainter::Antialiasing);
  painter->setRenderHint(QPainter::SmoothPixmapTransform);
}

/**
 * Deletes the QPainter
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_delete
(JNIEnv *env, jobject obj)
{
  GraphicsPainter *painter = (GraphicsPainter *)getPainter( env, obj );
  setNativePtr( env, obj, NULL );
  if( painter )
    {
      if( painter->isActive() )
 	painter->end();
      delete painter;
    }
}

///////////////////////////////////////////////////////////
/*
 * Sets the clip to a path.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setClipNative
(JNIEnv *env, jobject obj, jobject path)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  QPainterPath *pp = (QPainterPath *)getNativeObject( env, path );
  assert( pp );
  painter->setClipPath( *pp );
}

/*
 * Sets the clip to a rectangle.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setClipRectNative
(JNIEnv *env, jobject obj, jint x, jint y, jint w, jint h)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  painter->setClipRect( x, y, w, h );
}

/*
 * Intersects a shape with the current clip.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_intersectClipNative
(JNIEnv *env, jobject obj, jobject path)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  QPainterPath *pp = (QPainterPath *)getNativeObject( env, path );
  assert( pp );
  painter->setClipPath( *pp, Qt::IntersectClip );
}

/*
 * Intersect a rectangle with the current clip.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_intersectClipRectNative
(JNIEnv *env, jobject obj, jint x, jint y, jint w, jint h)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  painter->setClipRect( x, y, w, h, Qt::IntersectClip );
}

/*
 * Returns a QPainterPath object with the clip path of this painter.
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_getClipNative
(JNIEnv *env, jobject obj)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  jclass cls = env->FindClass("gnu/java/awt/peer/qt/QPainterPath");
  jmethodID method = env->GetMethodID(cls, "<init>", "()V");

  jobject ppo = env->NewObject(cls, method);
  QPainterPath qpp = painter->clipPath();
  setNativeObject(env, ppo, &qpp);

  env->DeleteLocalRef( cls );
  return ppo;
}

/*
 * Returns a Rectangle with the bounds of this painters clip path.
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_getClipBounds
(JNIEnv *env, jobject obj)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  qreal x, y, w, h;
  painter->clipPath().boundingRect().getRect(&x, &y, &w, &h);    

  jclass cls = env->FindClass("java/awt/Rectangle");
  assert( cls != NULL);
  jmethodID mid = env->GetMethodID(cls, "<init>", "(IIII)V");
  assert( mid != NULL);
  jvalue values[4];

  values[0].i = (jint) x;
  values[1].i = (jint) y;
  values[2].i = (jint) w;
  values[3].i = (jint) h;

  return env->NewObjectA(cls, mid, values);
}

///////////////////////// Color stuff ////////////////////////
/**
 *
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setColor
(JNIEnv *env, jobject obj, jint r, jint g, jint b, jint alpha)
{
  GraphicsPainter *painter = (GraphicsPainter *)getPainter( env, obj );
  assert( painter );
  painter->currentPen->setColor( QColor(r, g, b, alpha) );
  painter->setPen( *painter->currentPen );
  painter->currentBrush = new QBrush( QColor(r, g, b, alpha) );
  painter->setBrush( *painter->currentBrush );
  painter->currentColor = new QColor(r, g, b, alpha);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setAlphaNative
  (JNIEnv *env, jobject obj, jdouble alpha)
{
  GraphicsPainter *painter = (GraphicsPainter *)getPainter( env, obj );
  assert( painter );

  QColor c = painter->currentPen->color();
  c.setAlphaF( (qreal)alpha );
  painter->currentPen->setColor(c);

  c = painter->currentBrush->color();
  c.setAlphaF( (qreal)alpha );
  painter->currentBrush->setColor( c );
}

/*
 * Class:     gnu_java_awt_peer_qt_QtGraphics
 * Method:    drawNative
 * Signature: (Lgnu/java/awt/peer/qt/QPainterPath;)V
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_drawNative
(JNIEnv *env, jobject obj, jobject path)
{
  GraphicsPainter *painter = (GraphicsPainter *)getPainter( env, obj );
  assert( painter );
  QPainterPath *pp = (QPainterPath *)getNativeObject( env, path );
  assert( pp );
  painter->setPen( *painter->currentPen );
  painter->setBrush( Qt::NoBrush );
  painter->drawPath( *pp );
}

/*
 * Class:     gnu_java_awt_peer_qt_QtGraphics
 * Method:    fillNative
 * Signature: (Lgnu/java/awt/peer/qt/QPainterPath;)V
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_fillNative
(JNIEnv *env, jobject obj, jobject path)
{
  GraphicsPainter *painter = (GraphicsPainter *)getPainter( env, obj );
  assert( painter );
  QPainterPath *pp = (QPainterPath *)getNativeObject( env, path );
  assert( pp );

  painter->setPen(Qt::NoPen);
  painter->setBrush( *painter->currentBrush );
  painter->drawPath( *pp );
}

/**
 * Draws a string.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_drawStringNative
(JNIEnv *env, jobject obj, jstring str, jdouble x, jdouble y)
{
  GraphicsPainter *painter = getPainter( env, obj );
  assert( painter );
  QString *qStr = getQString(env, str);
  painter->setBrush( Qt::NoBrush );
  painter->setPen( *painter->currentPen );
  painter->drawText(QPointF( (qreal)x, (qreal)y ), *qStr);
  delete qStr;
}

/*
 * Sets the native stroke
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setNativeStroke
(JNIEnv *env, jobject obj, jobject stroke)
{
  GraphicsPainter *painter = (GraphicsPainter *)getPainter( env, obj );
  assert( painter );
  QPen *pen = (QPen *)getNativeObject(env, stroke);
  assert( pen );
  painter->currentPen = new QPen( *pen );
  painter->setPen( *painter->currentPen );
}

/*
 * Sets the transform
 */ 
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setQtTransform
(JNIEnv *env, jobject obj, jobject matrix)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  QMatrix *m = (QMatrix *)getNativeObject( env, matrix );
  assert( m );
  painter->setMatrix( *m );
}

/**
 * Set the font
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setFontNative
(JNIEnv *env, jobject obj, jobject fontpeer)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  QFont *font = (QFont *) getFont( env, fontpeer );
  assert( font );
  painter->setFont( *font );
}

/*
 * Sets Porter-Duff compositing.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setNativeComposite
(JNIEnv *env, jobject obj, jint compositeMode)
{
  QPainter *painter = getPainter( env, obj );
  assert( painter );
  QPainter::CompositionMode mode;

  switch( compositeMode )
    {
    case CLEAR:
      mode = QPainter::CompositionMode_Clear;
      break;
    case SRC:
      mode = QPainter::CompositionMode_Source;
      break;
    case DST:
      mode = QPainter::CompositionMode_Destination;
      break;
    case SRC_OVER:
      mode = QPainter::CompositionMode_SourceOver;
      break;
    case DST_OVER:
      mode = QPainter::CompositionMode_DestinationOver;
      break;
    case SRC_IN:
      mode = QPainter::CompositionMode_SourceIn;
      break;
    case DST_IN:
      mode = QPainter::CompositionMode_DestinationIn;
      break;
    case SRC_OUT:
      mode = QPainter::CompositionMode_SourceOut;
      break;
    case DST_OUT:
      mode = QPainter::CompositionMode_DestinationOut;
      break;
    case SRC_ATOP:
      mode = QPainter::CompositionMode_SourceAtop;
      break;
    case DST_ATOP:
      mode = QPainter::CompositionMode_DestinationAtop;
      break;
    case XOR:
      mode = QPainter::CompositionMode_Xor;
      break;
    }
  painter->setCompositionMode( mode );
}

/**
 * Sets the current brush to a linear gradient.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_setLinearGradient
(JNIEnv *env, jobject obj, jint r1, jint g1, jint b1, jint r2, jint g2, 
jint b2, jdouble x1, jdouble y1, jdouble x2, jdouble y2, jboolean cyclic)
{
  GraphicsPainter *painter = getPainter( env, obj );
  assert( painter );
  QLinearGradient *lg = new QLinearGradient(QPointF( (qreal)x1, (qreal)y1 ),
					    QPointF( (qreal)x2, (qreal)y2 ) );
  lg->setColorAt( (qreal)0.0, QColor(r1, g1, b1) );
  lg->setColorAt( (qreal)1.0, QColor(r2, g2, b2) );
  if( cyclic == JNI_TRUE )
    lg->setSpread( QGradient::ReflectSpread );
  else
    lg->setSpread( QGradient::PadSpread );
  painter->currentBrush = new QBrush( *lg );
  delete lg;
}

/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_fill3DRect
(JNIEnv *env, jobject obj, jint x, jint y, jint w, jint h, jboolean raised)
{
  GraphicsPainter *painter = getPainter( env, obj );
  assert( painter );
  // FIXME: Adjust colors
  painter->fillRect ( x, y, w, h, QBrush( *painter->currentColor) );
  QPen *p = new QPen( *painter->currentColor );
  p->setWidth( 1 );
  painter->setPen( *p );
  painter->drawLine( x + w, y, x + w, y + h);
  painter->drawLine( x, y + h, x + w, y + h);
}

/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtGraphics_draw3DRect
(JNIEnv *env, jobject obj, jint x, jint y, jint w, jint h, jboolean raised)
{
  GraphicsPainter *painter = getPainter( env, obj );
  assert( painter );
  // FIXME: Adjust colors
  QPen *p = new QPen( *painter->currentColor );
  p->setWidth( 1 );
  painter->setPen( *p );
  painter->drawLine( x, y, x + w, y );
  painter->drawLine( x, y, x, y + h);
  painter->drawLine( x + w, y, x + w, y + h);
  painter->drawLine( x, y + h, x + w, y + h);
}
