/* qttextfieldpeer.cpp --
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
#include <QLineEdit>
#include <QWidget>
#include <gnu_java_awt_peer_qt_QtTextFieldPeer.h>
#include "qtcomponent.h"
#include "qtstrings.h"
#include "slotcallbacks.h"
#include "mainthreadinterface.h"

class TFEchoChar : public AWTEvent {
 private:
  QLineEdit *line;
  jchar c;

 public:
  TFEchoChar(QLineEdit *w, jchar ch) : AWTEvent()
  {
    line = w;
    c = ch;
  }

  void runEvent()
  {
    line->setEchoMode( (c) ? QLineEdit::Password : QLineEdit::Normal );
  }
};

class TFEditable : public AWTEvent {
 private:
  QLineEdit *line;
  bool editable;

 public:
  TFEditable(QLineEdit *w, bool e) : AWTEvent()
  {
    line = w;
    editable = e;
  }

  void runEvent()
  {
    line->setReadOnly( editable );
  }
};

class TFSetText : public AWTEvent {
 private:
  QLineEdit *line;
  QString *text;

 public:
  TFSetText(QLineEdit *w, QString *t) : AWTEvent()
  {
    line = w;
    text = t;
  }

  void runEvent()
  {
    line->setText( *text );
    delete text;
  }
};

class TFSetCursorPos : public AWTEvent {
 private:
  QLineEdit *line;
  int pos;

 public:
  TFSetCursorPos(QLineEdit *w, int p) : AWTEvent()
  {
    line = w;
    pos = p;
  }

  void runEvent()
  {
    line->setCursorPosition(pos);
  }
};

class TFSelect : public AWTEvent {
 private:
  QLineEdit *line;
  int start,end;

 public:
  TFSelect(QLineEdit *w, int s, int e) : AWTEvent()
  {
    line = w;
    start = s;
    end = e;
  }

  void runEvent()
  {
    line->setSelection(start, end - start);
  }
};


/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *)getParentWidget(env, obj);
  assert( parentWidget );
  QLineEdit *line = new QLineEdit( parentWidget );
  assert( line );
  
  setNativeObject( env, obj, line );
  connectLineEdit(line, env, obj);
}


/*
 * Sets the echo char.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_setEchoChar
(JNIEnv *env, jobject obj, jchar echo)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );
  mainThread->postEventToMain( new TFEchoChar( line, echo ) );
}

/*
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_getMinimumSizeNative
(JNIEnv *env, jobject obj, jint columns)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );

  // FIXME does this work?
  int old = line->maxLength();
  line->setMaxLength(columns);
  QSize size = line->minimumSizeHint();
  line->setMaxLength(old);

  return makeDimension(env, &size);
}

/*
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_getPreferredSizeNative
(JNIEnv *env, jobject obj, jint columns)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );

  int old = line->maxLength();
  line->setMaxLength(columns);
  QSize size = line->sizeHint();
  line->setMaxLength(old);

  return makeDimension(env, &size);
}

/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_setEditable
(JNIEnv *env, jobject obj, jboolean edit)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );

  mainThread->postEventToMain( new TFEditable( line, (edit != JNI_TRUE) ) );
}

/*
 * Gets the text.
 */
JNIEXPORT jstring JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_getText
(JNIEnv *env, jobject obj)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );
  QString text = line->text();

  return getJavaString(env, &text);
}

/*
 * Sets the text
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_setText
(JNIEnv *env, jobject obj, jstring text)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );

  QString *qStr = getQString(env, text);
  mainThread->postEventToMain( new TFSetText( line, qStr ) );
}

/*
 * Returns the start (start = true) or end (start = false) of the selection.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_getSelection
(JNIEnv *env, jobject obj, jboolean start)
{
  int index, length;
  
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );
  index = line->selectionStart();

  if(start == JNI_TRUE)
    return index;

  length = (line->selectedText()).length();

  return index + length;
}

/*
 * Sets the selection.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_select
(JNIEnv *env, jobject obj, jint start, jint end)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );

  mainThread->postEventToMain( new TFSelect( line, start, end ) );
}

/*
 * Sets the cursor position.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_setCaretPosition
(JNIEnv *env, jobject obj, jint pos)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );
  mainThread->postEventToMain( new TFSetCursorPos( line, (int)pos ) );
}

/*
 * Returns the caret position.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtTextFieldPeer_getCaretPosition
(JNIEnv *env, jobject obj)
{
  QLineEdit *line = (QLineEdit *) getNativeObject( env, obj );
  assert( line );

  return line->cursorPosition();
}

