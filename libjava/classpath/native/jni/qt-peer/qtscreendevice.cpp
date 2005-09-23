/* qtscreendevice.cpp --
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
#include <QApplication>
#include <QDesktopWidget>
#include <gnu_java_awt_peer_qt_QtScreenDevice.h>
#include "nativewrapper.h"

extern QApplication *qApplication;

/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScreenDevice_init
(JNIEnv *env, jobject obj, jint id)
{
  QWidget *widget = qApplication->desktop()->screen( id );
  assert( widget );
  setNativeObject(env, obj, widget);
}

/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScreenDevice_dispose
(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *)getNativeObject(env, obj);
  setNativeObject(env, obj, NULL);
  if( widget )
    delete widget;
}

/*
 * Returns the bounds
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtScreenDevice_getBounds
(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *)getNativeObject(env, obj);
  assert( widget );

  jclass cls = env->FindClass("java/awt/Rectangle");
  jmethodID mid = env->GetMethodID(cls, "<init>", "(IIII)V");
  jvalue values[4];

  int x,y,w,h;
  widget->geometry().getRect( &x, &y, &w, &h );
  
  values[0].i = (jint) x;
  values[1].i = (jint) y;
  values[2].i = (jint) w;
  values[3].i = (jint) h;

  return env->NewObjectA(cls, mid, values);
}

/*
 * Returns the X DPI
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtScreenDevice_getDpiX
(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *)getNativeObject(env, obj);
  assert( widget );
  return widget->logicalDpiX();
}

/*
 * Returns the Y DPI
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtScreenDevice_getDpiY
(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *)getNativeObject(env, obj);
  assert( widget );
  return widget->logicalDpiY();
}

/*
 * Returns the bitplane depth
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtScreenDevice_depth
(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *)getNativeObject(env, obj);
  assert( widget );
  return widget->depth();
}

