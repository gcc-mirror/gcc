/* qttextareapeer.cpp --
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
#include <time.h>
#include <QTextEdit>
#include <QTextCursor>
#include <gnu_java_awt_peer_qt_QtTextAreaPeer.h>
#include "mainthreadinterface.h"
#include "componentevent.h"
#include "slotcallbacks.h"
#include "qtcomponent.h"
#include "qtstrings.h"

class TASetText : public AWTEvent {
 private:
  QTextEdit *area;
  QString *text;

 public:
  TASetText(QTextEdit *w, QString *t) : AWTEvent()
  {
    area = w;
    text = t;
  }

  void runEvent()
  {
    area->setPlainText( *text );
    delete text;
  }
};

/*
 * Construct a QTextEdit object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *)getParentWidget( env, obj );
  assert( parentWidget );
  QTextEdit *editor = new QTextEdit( parentWidget );
  editor->setGeometry( 0, 0, 400, 400 );
  assert( editor );

  //  setLineWrapColumnOrWidth ( int w );
  setNativeObject( env, obj, editor );

  // Connect TextChanged events.
  connectTextEdit(editor, env, obj);
}

/*
 * Returns the cursor position.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_getCaretPosition
(JNIEnv *env, jobject obj)
{
  int index;
  
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );

  index = editor->textCursor().position();;

  return (jint)index;
}

/*
 * Returns the char index at a given screen point
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_getIndexAtPoint
(JNIEnv *env, jobject obj, jint x, jint y)
{
  QPoint *p = new QPoint(x,y);
  
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );
  QTextCursor curs = editor->cursorForPosition( *p );
  delete p;

  return curs.position();
}

/*
 * Returns the start (start = true) or end (start = false) of the selection.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_getSelection
(JNIEnv *env, jobject obj, jboolean isStart)
{
  int start, end;
  
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );
  start = editor->textCursor().selectionStart();
  end =  editor->textCursor().selectionEnd();

  return ((isStart == JNI_TRUE) ? start : end);
}

/*
 * Returns the text.
 */
JNIEXPORT jstring JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_getText
(JNIEnv *env, jobject obj)
{
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );
  QString text = editor->toPlainText();

  return getJavaString(env, &text);
}

/*
 * Sets the editor text.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_setText
(JNIEnv *env, jobject obj, jstring str)
{
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );

  QString *qStr = getQString(env, str);
  mainThread->postEventToMain( new TASetText( editor, qStr ) );
}

/*
 * Sets the selection.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_select
(JNIEnv *env, jobject obj, jint startpos, jint endpos)
{
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );

  QTextCursor curs(editor->document());
  curs.setPosition(startpos);
  curs.setPosition(endpos, QTextCursor::KeepAnchor);
  editor->setTextCursor( curs );
}

/*
 * Allow or disallow editing.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_setEditable
(JNIEnv *env, jobject obj, jboolean editable)
{
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );
  editor->setReadOnly( (editable != JNI_TRUE) );
}

/*
 * Sets the cursor position
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextAreaPeer_setCaretPosition
(JNIEnv *env, jobject obj, jint index)
{
  QTextEdit *editor = (QTextEdit *) getNativeObject( env, obj );
  assert( editor );

  editor->textCursor().setPosition( index );
}
