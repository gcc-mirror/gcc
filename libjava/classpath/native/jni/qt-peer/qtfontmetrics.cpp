/* qtfontmetrics.cpp --
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
#include <QChar>
#include <QFont>
#include <QFontMetrics>
#include <QString>
#include <QPainter>
#include <QStringList>
#include <QFontDatabase>
#include <gnu_java_awt_peer_qt_QtFontMetrics.h>
#include "qtfont.h"
#include "qtstrings.h"
#include "qtgraphics.h"

QFontMetrics *getFontMetrics( JNIEnv *env, jobject obj )
{
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "nativeObject", "J" );
  return (QFontMetrics *)env->GetLongField( obj, field );
}

static void setNativePtr( JNIEnv *env, jobject obj, void *value )
{
  jlong longValue = (jlong) value;
  jclass cls = env->GetObjectClass( obj );
  jfieldID field = env->GetFieldID( cls, "nativeObject", "J" );
  env->SetLongField( obj, field, longValue );
}

static jobject makeRectangle(JNIEnv *env, QRect *rect)
{
  if( rect == NULL )
    return NULL;
  if( rect->isNull() || !rect->isValid() )
    return NULL;
  jclass cls = env->FindClass("java/awt/Rectangle");
  jmethodID mid = env->GetMethodID(cls, "<init>", "(IIII)V");
  jvalue values[4];

  int x,y,w,h;
  rect->getRect(&x, &y, &w, &h);    
  values[0].i = (jint) x;
  values[1].i = (jint) y;
  values[2].i = (jint) w;
  values[3].i = (jint) h;

  return env->NewObjectA(cls, mid, values);
}

/*
 * Create font metrics from a font.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_init
(JNIEnv *env, jobject obj, jobject fontPeer)
{
  QFont *f = getFont(env, fontPeer);
  assert( f );
  QFontMetrics *fm = new QFontMetrics( *f );
  assert( fm );
  setNativePtr( env, obj, fm );
}

/*
 * Create font metrics from a font.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_initGraphics
(JNIEnv *env, jobject obj, jobject fontPeer, jobject graphics)
{
  QFont *f = getFont(env, fontPeer);
  assert( f );
  QPainter *painter = getPainter( env, graphics );
  assert( painter );
  QFontMetrics *fm = new QFontMetrics( *f , painter->device());
  assert( fm );
  setNativePtr( env, obj, fm );
}

/*
 * Dispose
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_dispose
(JNIEnv *env, jobject obj)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  if ( fm )
    delete fm;
  setNativePtr( env, obj, NULL );
}

/*
 * Returns JNI_TRUE if a character is displayable.
 */
JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_canDisplay
(JNIEnv *env, jobject obj, jint c)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  bool result = fm->inFont( QChar( (unsigned int) c ) );
  return (result ? JNI_TRUE : JNI_FALSE);
}

/*
 * Returns the ascent.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_getAscent
(JNIEnv *env, jobject obj)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  return fm->ascent();
}

/*
 * Returns the descent
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_getDescent
(JNIEnv *env, jobject obj)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  return fm->descent();
}

/*
 * Returns the height.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_getHeight
(JNIEnv *env, jobject obj)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  return fm->height();
}

/*
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_getLeading
(JNIEnv *env, jobject obj)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  return fm->leading();
}

/*
 * getStringBounds
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_getStringBounds
(JNIEnv *env, jobject obj, jstring str)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  QString *qStr = getQString(env, str);
  QRect r = fm->boundingRect( *qStr );
  delete qStr;

  return makeRectangle( env, &r );
}

/*
 * Returns the width of the widest character.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_getMaxAdvance
(JNIEnv *env, jobject obj)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  return fm->maxWidth();
}

/*
 * Returns the width of a given character.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_charWidth
(JNIEnv *env, jobject obj, jchar c)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  return fm->width( QChar( (unsigned short)c ) );
}

/*
 * Returns the width of a string.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtFontMetrics_stringWidth
(JNIEnv *env, jobject obj, jstring str)
{
  QFontMetrics *fm = getFontMetrics( env, obj );
  assert( fm );
  QString *qStr = getQString(env, str);
  int width = fm->width( *qStr );
  delete qStr;
  return width;
}



