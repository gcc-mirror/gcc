/* qtwindowpeer.cpp --
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
#include <QWidget>
#include <qstyle.h>
#include <gnu_java_awt_peer_qt_QtWindowPeer.h>
#include "qtcomponent.h"
#include "keybindings.h"
#include "qtstrings.h"
#include "containers.h"
#include "mainthreadinterface.h"

/*
 * Our QMainWindow subclass
 */
class MyWindow : public QWidget
{
public:
  MyWindow(JNIEnv *env, jobject obj) : QWidget(0, (Qt::Window | Qt::FramelessWindowHint))
  {
    setup(env, obj);
  }

  ~MyWindow()
  {
    destroy();
  }

#define I_KNOW_WHAT_IM_DOING
#define PARENT QWidget
#include "eventmethods.h"
};


class RaiseLower : public AWTEvent {
  
 private:
  QWidget *widget;
  bool raise;

 public:
  RaiseLower(QWidget *w, bool r) : AWTEvent()
  {
    widget = w;
    raise = r;
  }

  void runEvent()
  {
    if (raise)
      widget->raise();
    else
      widget->lower();
  }
};

class FrameTitleEvent : public AWTEvent {
  
 private:
  QWidget *widget;
  QString *string;
  
 public:
  FrameTitleEvent(QWidget *w, QString *s) : AWTEvent()
  {
    widget = w;
    string = s;
  }

  void runEvent()
  {
    widget->setWindowTitle( *string );
    delete string;
  }
};

/*
 * Constructs a top-level QWidget native object.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtWindowPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *window = new MyWindow(env, obj);
  assert( window );
  //  Qt::WStyle_StaysOnTop
  setNativeObject( env, obj, window );
}

/*
 * Lower the window.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtWindowPeer_toBack
(JNIEnv *env, jobject obj)
{
  QWidget *window = (QWidget *) getNativeObject( env, obj );
  assert( window );
  mainThread->postEventToMain( new RaiseLower( window, false ) );
}

/*
 * Raise the window.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtWindowPeer_toFront
(JNIEnv *env, jobject obj)
{
  QWidget *window = (QWidget *) getNativeObject( env, obj );
  assert( window );
  mainThread->postEventToMain( new RaiseLower( window, true ) );
}

/*
 * Title.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtWindowPeer_setTitle
(JNIEnv *env, jobject obj, jstring string)
{
  QWidget *frame = (QWidget *) getNativeObject( env, obj );
  assert( frame );
  QString *qStr = getQString(env, string);
  mainThread->postEventToMain( new FrameTitleEvent( frame, qStr ) );
}

