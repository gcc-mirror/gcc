/* slotcallbacks.cpp --
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

#include <QObject>
#include <QAbstractButton>
#include <QAbstractSlider>
#include <QAction>
#include <QComboBox>
#include <QListWidget>
#include <QLineEdit>
#include <QPushButton>
#include <QTextEdit>
#include <gnu_java_awt_peer_qt_QtButtonPeer.h>
#include "qtcomponent.h"
#include "qtstrings.h"
#include "keybindings.h"
#include "buttonevent.h"
#include "slotcallbacks.h"

// AdjustmentEvent constants
#define UNIT_INCREMENT   1
#define UNIT_DECREMENT   2
#define BLOCK_DECREMENT  3
#define BLOCK_INCREMENT  4
#define TRACK  5


class SlotCallback : public QObject {
  Q_OBJECT;

private:
  JavaVM* vm;
  jobject target;
  jclass componentCls;
  jmethodID fireEventID;

public:
  QScrollBar *sb; // used only by the scrollbar method.
  QListWidget *lw; // used only by the listitemclicked method

  SlotCallback(JNIEnv *env, jobject t)
  {  
    env->GetJavaVM(&vm);
    target = t;
    target = env->NewGlobalRef(t);
  }
  
  ~SlotCallback()
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    env->DeleteGlobalRef(target);
  }

  public slots:

  void buttonClicked()
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    componentCls = env->GetObjectClass( target );
    fireEventID = env->GetMethodID( componentCls,
				    "fireClick", 
				    "(I)V" );
    int modifiers = getAEKeyModifiers( QApplication::keyboardModifiers() );
    env->CallVoidMethod( target, fireEventID, modifiers );
    env->DeleteLocalRef( componentCls );
  }

  void buttonToggled(bool checked)
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    componentCls = env->GetObjectClass( target );
    fireEventID = env->GetMethodID( componentCls,
				    "fireToggle", 
				    "(Z)V" );
    if(checked)
      env->CallVoidMethod( target, fireEventID, JNI_TRUE );
    else
      env->CallVoidMethod( target, fireEventID, JNI_FALSE );
    env->DeleteLocalRef( componentCls );
  }

  // Used for List and Choice
  void choiceActivated( int index )
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    componentCls = env->GetObjectClass( target );
    fireEventID = env->GetMethodID( componentCls,
				    "fireChoice", 
				    "(I)V" );
    env->CallVoidMethod( target, fireEventID, (jint)index );
    env->DeleteLocalRef( componentCls );
  }

  void textChanged()
  {
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    componentCls = env->GetObjectClass( target );
    fireEventID = env->GetMethodID( componentCls,
				    "textChanged", 
				    "()V" );
    env->CallVoidMethod( target, fireEventID );
    env->DeleteLocalRef( componentCls );
  }

  void scrollBarAction( int action )
  {
    JNIEnv *env;
    int type;
    int index;
    switch(action)
      {
      case QAbstractSlider::SliderNoAction:
	return;
      case QAbstractSlider::SliderSingleStepAdd:
	type = UNIT_INCREMENT;
	break;
      case QAbstractSlider::SliderSingleStepSub:
	type = UNIT_DECREMENT;
	break;
      case QAbstractSlider::SliderPageStepAdd:
	type = BLOCK_INCREMENT;
	break;
      case QAbstractSlider::SliderPageStepSub:
	type = BLOCK_DECREMENT;
	break;
      case QAbstractSlider::SliderToMinimum:
	type = TRACK;
	break;
      case QAbstractSlider::SliderToMaximum:
	type = TRACK;
	break;
      case QAbstractSlider::SliderMove:
	type = TRACK;
	break;
      }
    index = sb->value();
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    componentCls = env->GetObjectClass( target );
    fireEventID = env->GetMethodID( componentCls,
				    "fireMoved", 
				    "(II)V" );
    env->CallVoidMethod( target, fireEventID, (jint)type, (jint)index );
    env->DeleteLocalRef( componentCls );
  }

  void listItemClicked( QListWidgetItem * item )
  {
    int index = lw->row( item );
    JNIEnv *env;
    vm->GetEnv((void **)&env, JNI_VERSION_1_1);
    componentCls = env->GetObjectClass( target );
    fireEventID = env->GetMethodID( componentCls,
				    "itemDoubleClicked", 
				    "(II)V" );
    int modifiers = getAEKeyModifiers( QApplication::keyboardModifiers() );
    env->CallVoidMethod( target, fireEventID, index, modifiers );
    env->DeleteLocalRef( componentCls );
  }
};

#include "slotcallbacks.moc.h"

void connectButton(QPushButton *button, JNIEnv *env, jobject buttonobj)
{
  SlotCallback *scb = new SlotCallback(env, buttonobj);
  QObject::connect( button, SIGNAL( clicked() ), scb, SLOT( buttonClicked() ) );
}

void connectChoice(QComboBox *choice, JNIEnv *env, jobject choiceobj)
{
  SlotCallback *scb = new SlotCallback(env, choiceobj);
  QObject::connect( choice, SIGNAL( activated(int) ), scb, SLOT( choiceActivated(int) ) );
}

void connectList(QListWidget *list, JNIEnv *env, jobject listobj)
{
  SlotCallback *scb = new SlotCallback(env, listobj);
  scb->lw = list;
  QObject::connect( list, SIGNAL( currentRowChanged(int) ), 
		    scb, SLOT( choiceActivated(int) ) );
  QObject::connect( list, SIGNAL( itemDoubleClicked( QListWidgetItem * )), 
		    scb, SLOT( listItemClicked( QListWidgetItem * )));
}

void connectAction(QAction *action, JNIEnv *env, jobject obj)
{
  SlotCallback *scb = new SlotCallback(env, obj);
  QObject::connect( action, SIGNAL( triggered() ), scb, SLOT( buttonClicked() ) );
}

void connectToggle(QAbstractButton *action, JNIEnv *env, jobject obj)
{
  SlotCallback *scb = new SlotCallback(env, obj);
  QObject::connect( action, SIGNAL( toggled(bool) ), scb, SLOT( buttonToggled(bool) ) );
}

void connectScrollBar(QScrollBar *scroll, JNIEnv *env, jobject obj)
{
  SlotCallback *scb = new SlotCallback(env, obj);
  scb->sb = scroll;
  QObject::connect( scroll, SIGNAL( actionTriggered(int) ), scb, SLOT( scrollBarAction(int) ) );
}

void connectTextEdit(QTextEdit *edit, JNIEnv *env, jobject obj)
{
  SlotCallback *scb = new SlotCallback(env, obj);
  QObject::connect( edit, SIGNAL( textChanged() ), 
		    scb, SLOT( textChanged() ) );
}

void connectLineEdit(QLineEdit *edit, JNIEnv *env, jobject obj)
{
  SlotCallback *scb = new SlotCallback(env, obj);
  QObject::connect( edit, SIGNAL(textChanged( QString ) ), 
		    scb, SLOT( textChanged() ) );
}

