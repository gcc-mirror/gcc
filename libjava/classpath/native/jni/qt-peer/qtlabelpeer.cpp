/* qtlabelpeer.cpp --
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
#include <QLabel>
#include <QString>
#include <gnu_java_awt_peer_qt_QtLabelPeer.h>
#include "qtcomponent.h"
#include "qtstrings.h"
#include "mainthreadinterface.h"
#include "keybindings.h"

// java.awt.Label justification fields
#define LEFT   0
#define CENTER 1
#define RIGHT  2

class MyLabel : public QLabel
{
public:
  MyLabel(JNIEnv *env, jobject obj, QWidget *parent) : QLabel( parent )
  {
    setup(env, obj);
  }
  
  ~MyLabel()
  {
    destroy();
  }

#define I_KNOW_WHAT_IM_DOING
#define PARENT QLabel
#include "eventmethods.h"
};

class LabelTitle : public AWTEvent {
  
 private:
  QLabel *widget;
  QString *string;
  Qt::Alignment alignment;

 public:
  LabelTitle(QLabel *w, QString *s, Qt::Alignment a) : AWTEvent()
  {
    widget = w;
    string = s;
    alignment = a;
  }

  void runEvent()
  {
    if( string != NULL)
      {
	widget->setText( *string );
	delete string;
      }
    else
      {
	widget->setAlignment( alignment );
      }
  }
};

/*
 * Init a QLabel
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtLabelPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *) getParentWidget( env, obj );  
  QLabel *label = new MyLabel( env, obj, parentWidget );
  assert( label );
  setNativeObject( env, obj, label );
}

/*
 * Sets the text
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtLabelPeer_setText
(JNIEnv *env, jobject obj, jstring str)
{
  QLabel *label = (QLabel *) getNativeObject( env, obj );
  assert( label );

  QString *qStr = getQString(env, str);
  mainThread->postEventToMain( new LabelTitle( label, qStr, 0 ) );
}

/*
 * Sets the alignment
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtLabelPeer_setAlignment
(JNIEnv *env, jobject obj, jint align)
{
  Qt::Alignment alignment = Qt::AlignVCenter;
  
  QLabel *label = (QLabel *) getNativeObject( env, obj );
  assert( label );
  
  switch(align)
    {
    case LEFT:
      alignment |= Qt::AlignLeft;
      break;

    case RIGHT:
      alignment |= Qt::AlignRight;
      break;

    default:
    case CENTER:
      alignment |= Qt::AlignHCenter;
      break;
    }
  mainThread->postEventToMain( new LabelTitle( label, NULL, alignment ) );
}
