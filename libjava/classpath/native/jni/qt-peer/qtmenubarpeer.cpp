/* qtmenubarpeer.cpp --
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
#include <QMenuBar>
#include <QToolBar>
#include <QMenu>
#include <QList>
#include <QThread>
#include <gnu_java_awt_peer_qt_QtMenuBarPeer.h>
#include "nativewrapper.h"
#include "qtstrings.h"
#include "mainthreadinterface.h"

/*
 * Event wrapper to add a menu to a menu bar
 */
class MenuBarAdd : public AWTEvent {
  
 private:
  QMenuBar *widget;
  QMenu *menu;
  bool isHelp;
  
 public:
  MenuBarAdd(QMenuBar *w, QMenu *m, bool help) : AWTEvent()
  {
    widget = w;
    menu = m;
    isHelp = help;
  }

  void runEvent()
  {
    if ( isHelp )
      widget->addSeparator();
    QAction *ptr = widget->addMenu( menu );
  }
};

/**
 * Event wrapper to remove a menu from a menu bar.
 */
class MenuBarRemove : public AWTEvent {
  
 private:
  QMenuBar *widget;
  QMenu *menu;
  
 public:
  MenuBarRemove(QMenuBar *w, QMenu *m) : AWTEvent()
  {
    widget = w;
    menu = m;
  }

  void runEvent()
  {
    // FIXME
  }
};


/*
 * Constructs a QMenuBar object.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuBarPeer_init
  (JNIEnv *env, jobject obj)
{
  QMenuBar *menubar = new QMenuBar();
  assert( menubar );
  setNativeObject( env, obj, menubar );
}

/*
 * Adds a menu item.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuBarPeer_addMenu
(JNIEnv *env, jobject obj, jobject menuPeer)
{
  QMenuBar *menubar = (QMenuBar *)getNativeObject(env, obj);
  assert( menubar );
  QMenu *menu = (QMenu *)getNativeObject(env, menuPeer);
  assert( menu );
  mainThread->postEventToMain( new MenuBarAdd( menubar, menu, false ) );
}

/*
 * Add a help menu.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuBarPeer_addHelpMenu
(JNIEnv *env, jobject obj, jobject menuPeer)
{
  QMenuBar *menubar = (QMenuBar *)getNativeObject(env, obj);
  assert( menubar );
  QMenu *menu = (QMenu *)getNativeObject(env, menuPeer);
  assert( menu );

  mainThread->postEventToMain( new MenuBarAdd( menubar, menu, true ) );
}

/*
 * Delete a menu.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuBarPeer_delMenu
(JNIEnv *env, jobject obj, jobject menuPeer)
{
  QMenuBar *menubar = (QMenuBar *)getNativeObject(env, obj);
  assert( menubar );
  QMenu *menu = (QMenu *)getNativeObject(env, menuPeer);
  assert( menu );

  // FIXME

}

