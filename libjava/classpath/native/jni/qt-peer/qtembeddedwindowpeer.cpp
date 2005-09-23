/* qtembeddedwindowpeer.cpp --
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
#include <QX11EmbedWidget>
#include <gnu_java_awt_peer_qt_QtEmbeddedWindowPeer.h>
#include "qtcomponent.h"
#include "keybindings.h"
#include "mainthreadinterface.h"

/**
 * Event wrapper for embedding.
 */
class EmbedEvent : public AWTEvent {
  
 private:
  QX11EmbedWidget *widget;
  WId id;
  
 public:
  EmbedEvent(QX11EmbedWidget *w, WId i) : AWTEvent()
  {
    widget = w;
    id = i;
  }

  void runEvent()
  {
    widget->embedInto( id );
  }
};


class MyEmbeddedPanel : public QX11EmbedWidget
{
public:
  MyEmbeddedPanel(JNIEnv *env, jobject obj, QWidget *parent) : QX11EmbedWidget( parent )
  {
    setup(env, obj);
  }

  ~MyEmbeddedPanel()
  {
    destroy();
  }

#define I_KNOW_WHAT_IM_DOING
#define PARENT QX11EmbedWidget
#include "eventmethods.h"
#undef I_KNOW_WHAT_IM_DOING
#undef PARENT
};

/**
 * Init
 */ 
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtEmbeddedWindowPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *) getParentWidget( env, obj );  
  QWidget *panel = new MyEmbeddedPanel( env, obj, parentWidget );
  assert( panel );
  
  setNativeObject( env, obj, panel );
}

/**
 * Embed the thing.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtEmbeddedWindowPeer_embed
(JNIEnv *env, jobject obj, jlong wid)
{
  MyEmbeddedPanel *panel = (MyEmbeddedPanel *)getNativeObject( env, obj );
  assert( panel );
  
  mainThread->postEventToMain( new EmbedEvent( panel, (WId)wid ) );
}

