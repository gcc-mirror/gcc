/* qtdialogpeer.cpp --
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
#include <qdialog.h>
#include <gnu_java_awt_peer_qt_QtDialogPeer.h>
#include "qtcomponent.h"
#include "containers.h"
#include "qtstrings.h"
#include "keybindings.h"
#include "mainthreadinterface.h"

class MyDialog : public QDialog
{
public:
  MyDialog(JNIEnv *env, jobject obj, QWidget *parent) : QDialog(parent)
  {
    setup(env, obj);
  }

  ~MyDialog()
  {
    destroy();
  }

#define I_KNOW_WHAT_IM_DOING
#define PARENT QDialog
#include "eventmethods.h"
};

class DialogSettingsEvent : public AWTEvent {
  
 private:
  QDialog *widget;
  bool modal;
  bool value;
  
 public:
  DialogSettingsEvent(QDialog *w, bool m, bool v) : AWTEvent()
  {
    widget = w;
    modal = m; 
    value = v;
  }

  void runEvent()
  {
    if( modal )
      widget->setModal( value );
    else
      widget->setSizeGripEnabled( value );
  }
};

class DialogResizeEvent : public AWTEvent {
  
 private:
  QWidget *widget;
  bool fixed;
  int x, y, w, h;
  
 public:
  DialogResizeEvent(QWidget *wid, int x0, int y0, int w0, int h0, bool f)
  {
    widget = wid;
    fixed = f;
    x = x0; y = y0;
    w = w0; h = h0;
    if(w == 0 && h == 0) w = h = 10;
  }

  void runEvent()
  {
    if( fixed )
      widget->setFixedSize( w, h );
    widget->setGeometry( x, y, w, h );
  }
};

/*
 * Constructs a QDialog native object.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtDialogPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *) getParentWidget( env, obj );
  //  QDialog *dialog = new QDialog(parentWidget);
  MyDialog *dialog = new MyDialog(env, obj, parentWidget);
  assert( dialog );
  setNativeObject( env, obj, dialog );
}


/*
 * Sets the modality of the dialog.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtDialogPeer_setModal
(JNIEnv *env, jobject obj, jboolean flag)
{
  QDialog *dialog = (QDialog *) getNativeObject( env, obj );
  assert( dialog );
  mainThread->postEventToMain( new DialogSettingsEvent(dialog, true, (flag == JNI_TRUE)));
}


/*
 * Set resizeable.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtDialogPeer_setResizable
(JNIEnv *env, jobject obj, jboolean flag)
{
  QDialog *dialog = (QDialog *) getNativeObject( env, obj );
  assert( dialog );
  mainThread->postEventToMain( new DialogSettingsEvent(dialog, false, (flag == JNI_TRUE)));
}

/*
 * Overloaded to allow for size locking.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtDialogPeer_setBoundsNative
(JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height, jboolean fixed)
{
  QWidget *widget = (QWidget *) getNativeObject( env, obj );
  assert( widget );

  QRect g = widget->geometry();
  if(g.x() != x || g.y() != y || 
     g.width() != width || g.height() != height)
    mainThread->postEventToMain( new DialogResizeEvent( widget, x, y, width, height, (fixed == JNI_TRUE) ) );
}
