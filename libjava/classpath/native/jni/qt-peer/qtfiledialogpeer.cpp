/* qtfiledialogpeer.cpp --
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
#include <QFileDialog>
#include <QDialog>
#include <gnu_java_awt_peer_qt_QtFileDialogPeer.h>
#include "qtcomponent.h"
#include "qtstrings.h"
#include "mainthreadinterface.h"

// Constants from FileDialog
#define LOAD 0
#define SAVE 1

class FileDialogMode : public AWTEvent {
  
 private:
  QFileDialog *widget;
  bool open;
  
 public:
  FileDialogMode(QFileDialog *w, bool o) : AWTEvent()
  {
    widget = w;
    open = o;
  }

  void runEvent()
  {
    if( open )
      {
	widget->setAcceptMode( QFileDialog::AcceptOpen );
	widget->setFileMode( QFileDialog::ExistingFile );
      }
    else
      {
	widget->setAcceptMode( QFileDialog::AcceptSave );
	widget->setFileMode( QFileDialog::AnyFile );	
      }
  }
};


/*
 * Constructs a QDialog native object.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtFileDialogPeer_init
(JNIEnv *env, jobject obj)
{
  QWidget *parentWidget = (QWidget *) getParentWidget( env, obj );
  QFileDialog *dialog = new QFileDialog(parentWidget);
  assert( dialog );
  setNativeObject( env, obj, dialog );
}

/**
 * Sets the mode (LOAD or SAVE)
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtFileDialogPeer_setMode
(JNIEnv *env, jobject obj, jint mode)
{
  QFileDialog *filedialog = (QFileDialog *) getNativeObject( env, obj );
  assert( filedialog );

  mainThread->postEventToMain( new FileDialogMode( filedialog, (mode != SAVE) ) );
}
