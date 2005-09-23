/* qtlistpeer.cpp --
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
#include <QListWidget>
#include <gnu_java_awt_peer_qt_QtListPeer.h>
#include "qtcomponent.h"
#include "qtstrings.h"
#include "mainthreadinterface.h"
#include "slotcallbacks.h"

class ListInsert : public AWTEvent {
  
 private:
  QListWidget *widget;
  QString *string;
  int index;
  
 public:
  ListInsert(QListWidget *w, QString *s, int i) : AWTEvent()
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

class SelectEvent : public AWTEvent {
  
 private:
  QListWidget *widget;
  int index;
  bool selected;

 public:
  SelectEvent(QListWidget *w, int i, bool s) : AWTEvent()
  {
    widget = w;
    index = i;
    selected = s;
  }

  void runEvent()
  {
    widget->setItemSelected ( widget->item(index), selected );
  }
};

class ListDelete : public AWTEvent {
  
 private:
  QListWidget *widget;
  int startIndex, endIndex;

 public:
  ListDelete(QListWidget *w, int starti, int endi) : AWTEvent()
  {
    widget = w;
    startIndex = starti;
    endIndex = endi;
  }

  void runEvent()
  {
    for (int i = endIndex; i >= startIndex; i--)
      delete widget->takeItem(i);
  }
};

/*
 * Construct a QListWidget object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtListPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *)getParentWidget(env, obj);
  assert( parentWidget );
  QListWidget *list = new QListWidget( parentWidget );
  assert( list );
      
  setNativeObject( env, obj, list );
  connectList(list, env, obj);
}

/*
 * Adds an element.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtListPeer_add
(JNIEnv *env, jobject obj, jstring str, jint index)
{
  QListWidget *list = (QListWidget *) getNativeObject( env, obj );
  assert( list );
  QString *qStr = getQString(env, str);
  mainThread->postEventToMain( new ListInsert(list, qStr, index) );
}

/*
 * Delete items
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtListPeer_delItems
(JNIEnv *env, jobject obj, jint startindex, jint endindex)
{
  QListWidget *list = (QListWidget *) getNativeObject( env, obj );
  assert( list );
  mainThread->postEventToMain( new ListDelete(list, startindex, endindex) );
}

/*
 * (De)select an element.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtListPeer_select
(JNIEnv *env, jobject obj, jint index, jboolean sel)
{
  QListWidget *list = (QListWidget *) getNativeObject( env, obj );
  assert( list );

  mainThread->postEventToMain( new SelectEvent(list, index, 
  					       (sel == JNI_TRUE)) );
}

/**
 * Returns the indices of the selected items.
 */
JNIEXPORT jintArray JNICALL Java_gnu_java_awt_peer_qt_QtListPeer_getSelectedIndexes
  (JNIEnv *env, jobject obj)
{
  jintArray retArray;
  jint *arr;

  QListWidget *list = (QListWidget *) getNativeObject( env, obj );
  assert( list );

  QList<QListWidgetItem *> items = list->selectedItems();
  retArray = env->NewIntArray( items.count() );
  arr = env->GetIntArrayElements( retArray, NULL );

  for(int i = 0; i < items.count(); i++)
    arr[i] = list->row(items.at(i)); 

  env->ReleaseIntArrayElements( retArray, arr, 0 );
  return retArray;
}

/*
 * Sets the current item and makes it visible.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtListPeer_makeVisible
  (JNIEnv *env, jobject obj, jint index)
{

  QListWidget *list = (QListWidget *) getNativeObject( env, obj );
  assert( list );

  list->scrollToItem( list->item(index) );
}

/*
 * Set multiple selection mode.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtListPeer_setMultipleMode
(JNIEnv *env, jobject obj, jboolean allow)
{
  QListWidget *list = (QListWidget *) getNativeObject( env, obj );
  assert( list );

  // FIXME: Multiple selection is buggy in Qt4. Workaround needed.
  list->setSelectionMode( ((allow == JNI_TRUE) ? QAbstractItemView::MultiSelection : QAbstractItemView::SingleSelection) );
}

