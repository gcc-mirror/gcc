/* qtcheckboxpeer.cpp --
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
#include <QAbstractButton>
#include <QCheckBox>
#include <QRadioButton>
#include <gnu_java_awt_peer_qt_QtCheckboxPeer.h>
#include "qtstrings.h"
#include "qtcomponent.h"
#include "keybindings.h"
#include "buttonevent.h"
#include "slotcallbacks.h"

class CheckboxCheckEvent : public AWTEvent {
  
 private:
  QAbstractButton *widget;
  bool checked;

 public:
  CheckboxCheckEvent(QAbstractButton *w, bool c)
  {
    widget = w;
    checked = c;
  }
  
  void runEvent()
  {    
    if (checked != widget->isChecked())
      widget->setChecked( checked );
  }
};

class MyCheckBox : public QCheckBox
{
public:
  MyCheckBox(JNIEnv *env, jobject obj, QWidget *parent) : QCheckBox( parent )
  {
    setup(env, obj);
  }

  ~MyCheckBox()
  {
    destroy();
  }

#define I_KNOW_WHAT_IM_DOING
#define PARENT QCheckBox
#include "eventmethods.h"
};

/**
 * Determines whether the darn native object should be a radio button or not
 */
static bool isRadioButton( JNIEnv *env, jobject obj )
{
  jclass cls = env->FindClass( "gnu/java/awt/peer/qt/QtCheckboxPeer" );
  jfieldID field = env->GetFieldID( cls, "group", "Ljava/awt/CheckboxGroup;" );
  if (env->GetObjectField( obj, field ) != NULL)
    return true;
  return false;
}

/**
 * Construct the native object.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtCheckboxPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *)getParentWidget(env, obj);

  QAbstractButton *cb = (QAbstractButton *) getNativeObject( env, obj );
  if (cb)
    delete cb;

  bool radioButton;
  {
    jclass cls = env->GetObjectClass( obj );
    jfieldID field = env->GetFieldID( cls, "owner", "Ljava/awt/Component;" );
    assert(field != NULL);
    jobject owner = env->GetObjectField( obj, field );
    assert(owner != NULL);
    cls = env->GetObjectClass( owner );
    jmethodID method = env->GetMethodID( cls,
					 "getCheckboxGroup", 
					 "()Ljava/awt/CheckboxGroup;" );
    assert(method != NULL);
    jobject group = env->CallObjectMethod( owner, method, 0);
    radioButton = (group != NULL);
  }

  if(radioButton)
    cb = new QRadioButton( parentWidget );
  else
    cb = new QCheckBox( parentWidget );
  //    cb = new MyCheckBox( env, obj, parentWidget );
  assert( cb );

  connectToggle(cb, env, obj); // connect the native event.

  setNativeObject( env, obj, cb );
}

/*
 * Sets the checkbox label.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtCheckboxPeer_setLabel
(JNIEnv *env, jobject obj, jstring label)
{
  /* Both QCheckbox and QRadioButton inherit QAbstractButton */
  QAbstractButton *cb = (QAbstractButton *) getNativeObject( env, obj );
  assert( cb );

  QString *qStr = getQString(env, label); 
  mainThread->postEventToMain( new AWTLabelEvent( cb, qStr ) );
  // AWTLabelEvent takes care of disposal of qStr
}

/*
 * Sets the checkbox state.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtCheckboxPeer_setState
(JNIEnv *env, jobject obj, jboolean state)
{
  QAbstractButton *cb = (QAbstractButton *) getNativeObject( env, obj );
  assert( cb );
  mainThread->postEventToMain( new CheckboxCheckEvent( cb, (state == JNI_TRUE) ) );
}

