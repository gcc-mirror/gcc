/* qtcomponent.cpp --
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

#include <jni.h>
#include <assert.h>
#include <qmainwindow.h>
#include <qwidget.h>
#include "qtcomponent.h"
#include "containers.h"

#define COMPONENT_CLASS "gnu/java/awt/peer/qt/QtComponentPeer"

/*
 * Returns the parent widget for a QtComponentPeer
 */
void *getParentWidget( JNIEnv *env, jobject qtcomponentpeer )
{
  jclass componentCls = env->GetObjectClass( qtcomponentpeer );
  jfieldID ownerField = env->GetFieldID( componentCls,
					 "owner", "Ljava/awt/Component;" );
  assert( ownerField );
  jobject owner = env->GetObjectField( qtcomponentpeer, ownerField );
  if (owner == NULL)
    return NULL;

  jclass ownerCls = env->GetObjectClass( owner );
  jmethodID getParentMID = env->GetMethodID( ownerCls,
					     "getParent", 
					     "()Ljava/awt/Container;" );
  assert(getParentMID);

  jobject parent = env->CallObjectMethod( owner, getParentMID, 0);
  assert(parent);

  // Get the parents peer
  jclass parentCls = env->GetObjectClass( parent );
  {
    jclass frameCls = env->FindClass( "java/awt/Frame" );
    if(env->IsInstanceOf( parent, frameCls ) == JNI_TRUE)
      return frameChildWidget( env, parent );
  }
  {
    jclass scrollpaneCls = env->FindClass( "java/awt/ScrollPane" );
    if(env->IsInstanceOf( parent, scrollpaneCls ) == JNI_TRUE)
      return scrollPaneChildWidget( env, parent );
  }

  jmethodID getPeerMID = env->GetMethodID( parentCls,
					   "getPeer",
					   "()Ljava/awt/peer/ComponentPeer;" );
  assert(getPeerMID);
  return getNativeObject(env, env->CallObjectMethod( parent, getPeerMID, 0));
}

/*
 * Creates a java.awt.Dimension object from a QSize.
 */
jobject makeDimension(JNIEnv *env, QSize *size)
{
  if( size == NULL )
    return NULL;
  if( size->isNull() || !size->isValid() )
    return NULL;
  jclass cls = env->FindClass("java/awt/Dimension");
  jmethodID mid = env->GetMethodID(cls, "<init>", "(II)V");
  jvalue values[2];
  
  values[0].i = (jint) size->width();
  values[1].i = (jint) size->height();

  return env->NewObjectA(cls, mid, values);
}

/*
 * Creates a java.awt.Point object from a QPoint.
 */
jobject makePoint(JNIEnv *env, QPoint &p)
{
  jclass cls = env->FindClass("java/awt/Point");
  jmethodID mid = env->GetMethodID(cls, "<init>", "(II)V");
  jvalue values[2];
  
  values[0].i = (jint) p.x();
  values[1].i = (jint) p.y();

  return env->NewObjectA(cls, mid, values);
}

