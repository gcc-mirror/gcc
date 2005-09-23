/* qtchoicepeer.cpp --
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
#include <QComboBox>
#include <gnu_java_awt_peer_qt_QtChoicePeer.h>
#include "qtcomponent.h"
#include "qtstrings.h"
#include "mainthreadinterface.h"
#include "slotcallbacks.h"

class InsertEvent : public AWTEvent {
  
 private:
  QComboBox *widget;
  QString *string;
  int index;
  
 public:
  InsertEvent(QComboBox *w, QString *s, int i) : AWTEvent()
  {
    widget = w;
    string = s;
    index = i;
  }

  void runEvent()
  {
    widget->insertItem( index, *string );
    delete string;
  }
};

class RemoveSelectEvent : public AWTEvent {
  
 private:
  QComboBox *widget;
  int index;
  bool remove;

 public:
  RemoveSelectEvent(QComboBox *w, int i, bool r) : AWTEvent()
  {
    widget = w;
    index = i;
    remove = r;
  }

  void runEvent()
  {
    if (remove)
      widget->removeItem( index );
    else
      widget->setCurrentIndex( index );
  }
};

/*
 * Constructs tha QComboBox object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtChoicePeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *)getParentWidget(env, obj);
      
  QComboBox *box = new QComboBox( parentWidget );
  assert( box );

  setNativeObject( env, obj, box );

  connectChoice(box, env, obj); // connect the fireChoice method.
}

/*
 * Inserts a choice box item at index.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtChoicePeer_add
(JNIEnv *env, jobject obj, jstring itemLabel, jint index)
{
  QComboBox *box = (QComboBox *) getNativeObject( env, obj );
  assert( box );

  QString *qStr = getQString( env, itemLabel );
  mainThread->postEventToMain( new InsertEvent( box, qStr, index ) );
}

/*
 * Removes a choice box item at index.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtChoicePeer_remove
(JNIEnv *env, jobject obj, jint index)
{
  QComboBox *box = (QComboBox *) getNativeObject( env, obj );
  assert( box );
  mainThread->postEventToMain( new RemoveSelectEvent( box, index, true ) );
}

/**
 * Selects a choice box item.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtChoicePeer_select
(JNIEnv *env, jobject obj, jint index)
{
  QComboBox *box = (QComboBox *) getNativeObject( env, obj );
  assert( box );
  mainThread->postEventToMain( new RemoveSelectEvent( box, index, false ) );
}

