/* qtscrollpanepeer.cpp --
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
#include <QScrollArea>
#include <QScrollBar>
#include <gnu_java_awt_peer_qt_QtScrollPanePeer.h>
#include "qtcomponent.h"
#include "containers.h"
#include "mainthreadinterface.h"
#include "componentevent.h"
#include "keybindings.h"

// Constants in java.awt.ScrollPane
#define	SCROLLBARS_AS_NEEDED 	0
#define SCROLLBARS_ALWAYS 	1
#define	SCROLLBARS_NEVER 	2


class MyScrollArea : public QScrollArea
{
public:
  MyScrollArea(JNIEnv *env, jobject obj, QWidget *parent) : QScrollArea( parent )
  {
    setup(env, obj);
  }

  ~MyScrollArea()
  {
    destroy();
  }

#define I_KNOW_WHAT_IM_DOING
#define PARENT QScrollArea
#include "eventmethods.h"
};


class ScrollPanePolicy : public AWTEvent {
  
 private:
  QScrollArea *widget;
  Qt::ScrollBarPolicy policy;

 public:
  ScrollPanePolicy(QScrollArea *w, Qt::ScrollBarPolicy p) : AWTEvent()
  {
    widget = w;
    policy = p;
  }

  void runEvent()
  {
    widget->setHorizontalScrollBarPolicy(policy);
    widget->setVerticalScrollBarPolicy(policy);
  }
};

/**
 * Returns the child widget, given the owner Component.
 */
QWidget *scrollPaneChildWidget( JNIEnv *env, jobject component )
{
  jclass scrollpaneCls = env->FindClass( "java/awt/ScrollPane" );
  jmethodID getPeerMID = env->GetMethodID( scrollpaneCls,
					   "getPeer",
					   "()Ljava/awt/peer/ComponentPeer;");
  assert(getPeerMID != 0);
  jobject scrollpanepeerobj = env->CallObjectMethod( component, getPeerMID, NULL );
  QScrollArea *view = (QScrollArea *) getNativeObject( env, scrollpanepeerobj );
  assert(view != 0);
  return view->viewport();
}

/*
 * Creates a QScrollArea object.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollPanePeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *) getParentWidget( env, obj );  
  assert( parentWidget );
  //  QScrollArea *pane = new MyScrollArea( env, obj, parentWidget );
  QScrollArea *pane = new QScrollArea( parentWidget );
  assert( pane );
  setNativeObject( env, obj, pane );
}

/*
 * Resize the child.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollPanePeer_childResized
(JNIEnv *env, jobject obj, jint w, jint h)
{  
  QScrollArea *view = (QScrollArea *) getNativeObject( env, obj );
  assert( view );

  QWidget *child = view->viewport();
  assert( child );
  //  child->setGeometry( 0, 0, w, h );
//   child->update();
  mainThread->postEventToMain( new AWTResizeEvent(child, 0, 0, w, h) );
}

/*
 * Returns the horizontal scrollbar height.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtScrollPanePeer_getHScrollbarHeight
(JNIEnv *env, jobject obj)
{
  QScrollArea *pane = (QScrollArea *) getNativeObject( env, obj );
  assert( pane );
  QScrollBar *hbar = pane->horizontalScrollBar();
  if(hbar == NULL)
    return 0;
  if(!hbar->isVisible())
    return 0;
  int height = hbar->height();

  return height;
}

/*
 * Returns the vertical scrollbar width.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtScrollPanePeer_getVScrollbarWidth
(JNIEnv *env, jobject obj)
{
  QScrollArea *pane = (QScrollArea *) getNativeObject( env, obj );
  assert( pane );
  QScrollBar *vbar = pane->verticalScrollBar();
  if(vbar == NULL)
    return 0;
  if(!vbar->isVisible())
    return 0;
  int width = vbar->width();

  return width;
}

/*
 * Sets the current upper-left corner to x, y.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollPanePeer_setScrollPosition
(JNIEnv *env, jobject obj, jint x, jint y)
{
  QScrollArea *pane = (QScrollArea *) getNativeObject( env, obj );
  assert( pane );
  // pane->scrollContentsBy( x, y );
}

/*
 * Sets the scrollbar policy
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtScrollPanePeer_setPolicy
(JNIEnv *env, jobject obj, jint policy)
{
  QScrollArea *pane = (QScrollArea *) getNativeObject( env, obj );
  assert( pane );

  Qt::ScrollBarPolicy qtpolicy;
  switch( policy )
    {
    case SCROLLBARS_ALWAYS:
      qtpolicy = Qt::ScrollBarAlwaysOn;
      break;

    case SCROLLBARS_NEVER:
      qtpolicy = Qt::ScrollBarAlwaysOff;
      break;

    default:
    case SCROLLBARS_AS_NEEDED:
      qtpolicy = Qt::ScrollBarAsNeeded;
      break;
   }

  mainThread->postEventToMain( new ScrollPanePolicy( pane, qtpolicy ) );
}

