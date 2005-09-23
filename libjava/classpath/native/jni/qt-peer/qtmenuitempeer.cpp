/* qtmenuitempeer.cpp --
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
#include <QMenu>
#include <QAction>
#include <QThread>
#include <gnu_java_awt_peer_qt_QtMenuItemPeer.h>
#include "nativewrapper.h"
#include "qtstrings.h"
#include "mainthreadinterface.h"

class MenuItemDestroyEvent : public AWTEvent {
  
 private:
  QAction *widget;

 public:
  MenuItemDestroyEvent(QAction *w)
  {
    widget = w;
  }
  
  void runEvent()
  {
    delete widget;
  }
};

class MenuItemLabelEvent : public AWTEvent {
  
 private:
  QAction *widget;
  QString *string;
  
 public:
  MenuItemLabelEvent(QAction *w, QString *s) : AWTEvent()
  {
    widget = w;
    string = s;
  }

  void runEvent()
  {
    widget->setText( *string );
    delete string;
  }
};

class MenuItemStatusEvent : public AWTEvent {
  
 private:
  QAction *widget;
  bool enabled;
  bool value;
  
 public:
  MenuItemStatusEvent(QAction *w, bool e, bool v) : AWTEvent()
  {
    widget = w;
    enabled = e; 
    value = v;
  }

  void runEvent()
  {
    if( enabled )
      widget->setEnabled( value );
    else
      widget->setChecked( value );
  }
};

/*
 * Creates a QAction object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuItemPeer_create
(JNIEnv *env, jobject obj, jstring label, jboolean isSeperator, jboolean isCheckable)
{
  QAction *action;
  if(label == NULL || isSeperator == JNI_TRUE)
    {
      action = new QAction(NULL);
      action->setSeparator(true);
      assert( action );
    } 
  else
    {
      QString *qStr = getQString(env, label);
      action = new QAction(*qStr, NULL);
      delete qStr;
      assert( action );
      action->setCheckable( (isCheckable == JNI_TRUE) );
    }
  
  setNativeObject( env, obj, action );
}

/**
 * Disposal.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuItemPeer_dispose
(JNIEnv *env, jobject obj)
{
  QAction *action = (QAction *)getNativeObject( env, obj );
  assert( action );
  mainThread->postEventToMain( new MenuItemDestroyEvent( action ) );
}

/*
 * Enables/disables the item
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuItemPeer_setEnabled
(JNIEnv *env, jobject obj, jboolean enabled)
{
  QAction *action = (QAction *)getNativeObject( env, obj );
  assert( action );
  mainThread->postEventToMain(new MenuItemStatusEvent(action, true,
						      (enabled == JNI_TRUE)));
}

/*
 * Sets the item label.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuItemPeer_setLabel
(JNIEnv *env, jobject obj, jstring label)
{
  QAction *action = (QAction *)getNativeObject( env, obj );
  assert( action );

  QString *qStr = getQString(env, label);
  mainThread->postEventToMain( new MenuItemLabelEvent( action, qStr ) );
}

/*
 * Sets the checkbox state.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuItemPeer_setState
(JNIEnv *env, jobject obj, jboolean state)
{
  QAction *action = (QAction *)getNativeObject( env, obj );
  assert( action );
  mainThread->postEventToMain(new MenuItemStatusEvent(action, false,
						      (state == JNI_TRUE)));
}



