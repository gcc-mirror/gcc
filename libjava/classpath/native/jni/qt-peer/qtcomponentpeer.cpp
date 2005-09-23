/* qtcomponentpeer.cpp --
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
#include <QShowEvent>
#include <QHideEvent>
#include <QColor>
#include <QCursor>
#include <QWidget>
#include <gnu_java_awt_peer_qt_QtComponentPeer.h>
#include "qtcomponent.h"
#include "componentevent.h"
#include "qtfont.h"

extern QApplication *qApplication;

// Java Cursor types.
#define DEFAULT_CURSOR 	        0
#define CROSSHAIR_CURSOR 	1
#define TEXT_CURSOR 	        2
#define WAIT_CURSOR 	        3
#define SW_RESIZE_CURSOR 	4
#define SE_RESIZE_CURSOR 	5
#define NW_RESIZE_CURSOR 	6
#define NE_RESIZE_CURSOR 	7
#define N_RESIZE_CURSOR 	8
#define S_RESIZE_CURSOR 	9
#define W_RESIZE_CURSOR 	10
#define E_RESIZE_CURSOR 	11
#define HAND_CURSOR 	        12
#define MOVE_CURSOR 	        13

/**
 * Call back the init() method from the main thread.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_callInit
(JNIEnv *env, jobject obj)
{
  mainThread->postEventToMain( new AWTInitEvent( env, obj ) );
}

/*
 * Generic disposal.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_disposeNative
(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  setNativeObject(env, obj, NULL);
  mainThread->postEventToMain( new AWTDestroyEvent( widget ) );
}

/**
 * Returns the on-screen location of the component.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_getLocationOnScreenNative
(JNIEnv *env, jobject obj, jobject point)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );
  mainThread->postEventToMain( new AWTGetOriginEvent( widget, env, point) );
}

/*
 * Get the preferred/minimum size of the widget
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_getSizeNative
(JNIEnv *env, jobject obj, jobject size, jboolean preferred)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );

  mainThread->postEventToMain
    (new GetSizeEvent( widget, env, size, (preferred == JNI_TRUE)));
}

/*
 */
JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_isObscured
(JNIEnv *env, jobject obj)
{
  jboolean retVal;

  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );

  retVal = (widget->isVisible() == TRUE) ? JNI_TRUE : JNI_FALSE;

  return retVal;
}

/*
 * Returns whether the widget is focusable or not.
 */
JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_isFocusable
(JNIEnv *env, jobject obj)
{
  jboolean retVal;

  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );

  retVal = (widget->focusPolicy() != Qt::NoFocus) ? JNI_TRUE : JNI_FALSE;

  return retVal;
}

/**
 * Requests the focus
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_requestFocus
  (JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );
  mainThread->postEventToMain( new AWTReqFocusEvent( widget ) );
}

/*
 * Sets the size and position. Important.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_setBoundsNative
(JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );
  mainThread->postEventToMain
    (new AWTResizeEvent( widget, x, y, width, height ) );
}

/*
 * Sets the mouse cursor
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_setCursor
  (JNIEnv *env, jobject obj, jint cursortype)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );

  Qt::CursorShape shape;
  switch(cursortype)
    {
    case CROSSHAIR_CURSOR:
      shape = Qt::CrossCursor;
      break;

    case W_RESIZE_CURSOR:
    case E_RESIZE_CURSOR:
      shape = Qt::SizeHorCursor;
      break;
    case N_RESIZE_CURSOR:
    case S_RESIZE_CURSOR:
      shape = Qt::SizeVerCursor;
      break;
    case HAND_CURSOR:
      shape = Qt::PointingHandCursor;
      break;
    case MOVE_CURSOR:
      shape = Qt::SizeAllCursor;
      break;

    case NE_RESIZE_CURSOR:
    case SW_RESIZE_CURSOR:
      shape = Qt::SizeBDiagCursor;
      break;
    case NW_RESIZE_CURSOR:
    case SE_RESIZE_CURSOR:
      shape = Qt::SizeFDiagCursor;
      break;
    case TEXT_CURSOR:
      shape = Qt::IBeamCursor;
      break;
    case WAIT_CURSOR:
      shape = Qt::WaitCursor;
      break;
      
    case DEFAULT_CURSOR:
    default:
      shape = Qt::ArrowCursor;
      break;
    }

  mainThread->postEventToMain( new AWTCursorEvent( widget, shape ) );
}

/*
 * Enable, disable.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_setEnabled
(JNIEnv *env, jobject obj, jboolean state)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert(widget != NULL);

  mainThread->postEventToMain( new AWTEnableEvent( widget, (state == JNI_TRUE) ) );
}

/**
 * Set the font
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_setFontNative
(JNIEnv *env, jobject obj, jobject fontpeer)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );
  QFont *font = (QFont *) getFont( env, fontpeer );
  assert( font );

  mainThread->postEventToMain( new AWTFontEvent(widget, font) );
}

/*
 * Sets the back- or foreground color.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_setGround
(JNIEnv *env, jobject obj, jint r, jint g, jint b, jboolean isForeground)
{
  QColor *color = new QColor(r, g, b);

  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert(widget);
  mainThread->postEventToMain( new AWTBackgroundEvent(widget, 
						      (isForeground == JNI_TRUE),
						      color) );
}

/*
 * Sets the visibility.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_setVisible
(JNIEnv *env, jobject obj, jboolean state)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert(widget != NULL);
  mainThread->postEventToMain( new AWTShowEvent( widget, (state == JNI_TRUE) ) );
}

/*
 * Returns whether the widget handles wheel scrolling. 
 */
JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_handlesWheelScrolling
(JNIEnv *env, jobject obj)
{
  jboolean handles = JNI_FALSE;

  QWidget *cb = (QWidget *) getNativeObject( env, obj );
  if( cb )
    if( cb->focusPolicy() & Qt::WheelFocus )
      handles = JNI_TRUE;

  return handles;
}

/**
 * calls qwidget::update on the compnent.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_QtUpdateArea
(JNIEnv *env, jobject obj, jint x, jint y, jint w, jint h)
{
  QWidget *cb = (QWidget *) getNativeObject( env, obj );
  if( cb )
    mainThread->postEventToMain( new AWTUpdateEvent
				 (cb, false, x, y, w, h ) );
}

/*
 * calls qwidget::update on the compnent.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_QtUpdate
(JNIEnv *env, jobject obj)
{
  QWidget *cb = (QWidget *) getNativeObject( env, obj );
  if( cb )
    mainThread->postEventToMain( new AWTUpdateEvent
				 ( cb, true, 0, 0, 0, 0 ) );
}

/*
 * Returns the native background color.
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_getNativeBackground
(JNIEnv *env, jobject obj)
{
  QWidget *cb = (QWidget *) getNativeObject( env, obj );
  assert(cb);
  QColor c = cb->palette().background().color().toRgb();

  jclass cls = env->FindClass("java/awt/Color");
  jmethodID mid = env->GetMethodID(cls, "<init>", "(III)V");
  jvalue values[3];
  
  values[0].i = (jint) c.red();
  values[1].i = (jint) c.green();
  values[2].i = (jint) c.blue();

  return env->NewObjectA(cls, mid, values);
}

/*
 * Returns which screen the component is on.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_whichScreen
(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );
  return (jint) qApplication->desktop()->screenNumber( widget );
}

/*
 * Reparents the widget.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_reparentNative
(JNIEnv *env, jobject obj, jobject newparent)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );
  QWidget *parentWidget = (QWidget *) getNativeObject( env, newparent );
  assert( parentWidget );
  mainThread->postEventToMain( new AWTReparent(widget, parentWidget ) );
}

/*
 * Get the preferred size of the widget
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtComponentPeer_getBounds

(JNIEnv *env, jobject obj)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );

  int x, y, w, h;
  widget->geometry().getRect(&x, &y, &w, &h);    

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
