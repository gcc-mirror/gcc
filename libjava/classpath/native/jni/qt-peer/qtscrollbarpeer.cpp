/* qtscrollbarpeer.cpp --
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
#include <QScrollBar>
#include <gnu_java_awt_peer_qt_QtScrollbarPeer.h>
#include "keybindings.h"
#include "slotcallbacks.h"
#include "qtcomponent.h"

// Constant fields from java.awt.Scrollbar
#define HORIZONTAL 0
#define VERTICAL   1

class MyScrollBar : public QScrollBar
{
public:
  MyScrollBar(JNIEnv *env, jobject obj, QWidget *parent) : QScrollBar( parent )
  {
    setup(env, obj);
  }

  ~MyScrollBar()
  {
    destroy();
  }

#define I_KNOW_WHAT_IM_DOING
#define PARENT QScrollBar
#include "eventmethods.h"
};

/*
 * Construct a QScrollbar object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollbarPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *)getParentWidget( env, obj );
  assert( parentWidget );
  //  QScrollBar *scrollbar = new QScrollBar( parentWidget );
  MyScrollBar *scrollbar = new MyScrollBar( env, obj, parentWidget );
  assert( scrollbar );
      
  setNativeObject( env, obj, scrollbar );
  connectScrollBar(scrollbar, env, obj);
}

/*
 * Set the line increment.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollbarPeer_setLineIncrement
(JNIEnv *env, jobject obj, jint inc)
{
  QScrollBar *bar = (QScrollBar *) getNativeObject( env, obj );
  assert( bar );

  bar->setSingleStep(inc);
}

/**
 * Sets the orientation of the scrollbar
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollbarPeer_setOrientation
(JNIEnv *env, jobject obj, jint orientation)
{
  QScrollBar *bar = (QScrollBar *) getNativeObject( env, obj );
  assert( bar );

  switch(orientation)
    {
    case HORIZONTAL:
      bar->setOrientation ( Qt::Horizontal );
      break;
      
    default:
    case VERTICAL:
      bar->setOrientation ( Qt::Vertical );
      break;
    }
}

/**
 * Sets the page increment (equivalent to slider size)
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollbarPeer_setPageIncrement
(JNIEnv *env, jobject obj, jint inc)
{
  QScrollBar *bar = (QScrollBar *) getNativeObject( env, obj );
  assert( bar );

  bar->setPageStep( inc );
}

/*
 * Setvalues.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollbarPeer_setValues
(JNIEnv *env, jobject obj, jint value, jint visible, jint min, jint max)
{
  QScrollBar *bar = (QScrollBar *) getNativeObject( env, obj );
  assert( bar );

  bar->setValue(value);
  bar->setPageStep( visible ); // page step and slider size are the same in Qt
  bar->setRange( min , max );
}

