/* qtmenupeer.cpp --
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
#include <gnu_java_awt_peer_qt_QtMenuPeer.h>
#include "nativewrapper.h"
#include "qtstrings.h"
#include "mainthreadinterface.h"
#include "slotcallbacks.h"
#include "componentevent.h"

typedef enum ActionType {
  ActionMenu,
  ActionItem,
  ActionSeparator
} ActionType;

// Sets the title, but also tear-off.
class MenuTitleEvent : public AWTEvent {
  
 private:
  QMenu *widget;
  QString *string;
  bool tearOff;
  
 public:
  MenuTitleEvent(QMenu *w, QString *s, bool tear) : AWTEvent()
  {
    widget = w;
    string = s;
    tearOff = tear;
  }

  void runEvent()
  {
    if (tearOff)
      widget->setTearOffEnabled( true );
    else
      {
	widget->setTitle( *string );
	delete string;
      }
  }
};

class MenuAction : public AWTEvent {
  
 private:
  QMenu *menu;
  QAction *action;
  ActionType actionType; // type of action to add
  JavaVM *vm;
  jobject menuPeer;
  jobject itemPeer;

public:
  MenuAction(JNIEnv *env, jobject mp, jobject ip, QMenu *m, QAction *a, 
	     ActionType actionType) : AWTEvent()
  {
    menu = m;
    action = a;
    this->actionType = actionType;
    env->GetJavaVM( &vm );
    menuPeer = env->NewGlobalRef( mp );
    if( ip != NULL )
      itemPeer = env->NewGlobalRef( ip );
    else
      itemPeer = NULL;
  }
  
  void runEvent()
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    if (actionType == ActionMenu)
      menu->addMenu ((QMenu *) action);
    else
      menu->addAction (action);

    jclass menuCls = env->GetObjectClass( menuPeer );
    jmethodID mid = env->GetMethodID(menuCls, "add", "(J)V");
    env->DeleteLocalRef(menuCls);
    env->CallVoidMethod( menuPeer, mid, (jlong)action );

    env->DeleteGlobalRef( menuPeer );
    if( itemPeer != NULL )
      {
	setNativeObject( env, itemPeer, action );
	connectAction(action, env, itemPeer);
	env->DeleteGlobalRef( itemPeer );
      }
  }
};

class MenuRemoveAction : public AWTEvent {
  
 private:
  QMenu *menu;
  QAction *action;

public:
  MenuRemoveAction(QMenu *m, QAction *a) : AWTEvent()
  {
    menu = m;
    action = a;
  }

  void runEvent()
  {
    if (action)
      menu->removeAction(action);
  }
};

/*
 * Constructs a QMenu item
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_init
(JNIEnv *env, jobject obj)
{
  QMenu *menu = new QMenu();
  assert( menu );

  setNativeObject( env, obj, menu );
}

/**
 * Allows tear-off: Only called once, if ever.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_allowTearOff
(JNIEnv *env, jobject obj)
{
  QMenu *menu = (QMenu *)getNativeObject( env, obj );
  assert( menu );
  mainThread->postEventToMain( new MenuTitleEvent( menu, NULL, true ) );
}

/*
 * Inserts a seperator.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_insertSeperator
(JNIEnv *env, jobject obj)
{
  QMenu *menu = (QMenu *)getNativeObject( env, obj );
  assert( menu );
  mainThread->postEventToMain( new MenuAction( env, obj, NULL,
					       menu, NULL, ActionSeparator ) );
}

/*
 * Inserts an item.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_insertItem
(JNIEnv *env, jobject obj, jobject item)
{
  QMenu *menu = (QMenu *)getNativeObject( env, obj );
  assert( menu );

  QAction *action = (QAction *)getNativeObject( env, item );
  assert( action );

  mainThread->postEventToMain( new MenuAction( env, obj, item, menu, action, ActionItem ));
}

/*
 * Inserts a sub-menu
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_insertMenu
(JNIEnv *env, jobject obj, jobject menu)
{
  assert( menu );
  QMenu *thisMenu = (QMenu *)getNativeObject( env, obj );
  assert( thisMenu );
  QMenu *insMenu = (QMenu *)getNativeObject(env, menu);
  assert( insMenu );

  mainThread->postEventToMain( new MenuAction( env, obj, menu, thisMenu, (QAction *)insMenu, ActionMenu ) );
}

/*
 * Removes an item at index.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_delItem
(JNIEnv *env, jobject obj, jlong ptr)
{
  QMenu *menu = (QMenu *)getNativeObject( env, obj );
  assert( menu );
  QAction *action = (QAction *)ptr;

  mainThread->postEventToMain( new MenuRemoveAction( menu, action ) );
}

/*
 * Enables/Disables the menu.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_setEnabled
(JNIEnv *env, jobject obj, jboolean enabled)
{
  QMenu *menu = (QMenu *)getNativeObject( env, obj );
  assert( menu );

  mainThread->postEventToMain(new AWTEnableEvent(menu, (enabled == JNI_TRUE)));
}

/*
 * Sets the menu title.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtMenuPeer_setLabel
(JNIEnv *env, jobject obj, jstring label)
{
  if(label == NULL)
    return;

  QMenu *menu = (QMenu *)getNativeObject( env, obj );
  assert( menu );
  QString *qStr = getQString(env, label);
  mainThread->postEventToMain( new MenuTitleEvent( menu, qStr, false ) );
}

