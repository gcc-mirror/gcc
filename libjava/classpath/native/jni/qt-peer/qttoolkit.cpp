/* qttoolkit.cpp --
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

#include <QApplication>
#include <QDesktopWidget>
#include <QString>
#include <QStringList>
#include <QFontDatabase>
#include <gnu_java_awt_peer_qt_QtToolkit.h>
#include "qtcomponent.h"
#include "mainthreadinterface.h"
#include "qtstrings.h"

extern QApplication *qApplication;

/**
 * Calls syncX();
 */
class AWTSyncEvent : public AWTEvent {
  
 private:
  QApplication *application;
  
 public:
  AWTSyncEvent(QApplication *app) : AWTEvent()
  {
    application = app;
  }

  void runEvent()
  {
    application->syncX();
  }
};

/*
 * Causes your machine to beep. Wow.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtToolkit_beep
(JNIEnv *env, jobject obj)
{
  qApplication->beep();
}

/**
 * Returns the # of the default screen.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtToolkit_defaultScreen
(JNIEnv *env, jobject obj)
{
  return (jint) qApplication->desktop()->primaryScreen();
}

/**
 * Returns the # of screens.
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtToolkit_numScreens
(JNIEnv *env, jobject obj)
{ 
  return (jint) qApplication->desktop()->numScreens();
}

/*
 * Returns the screen size.
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QtToolkit_getScreenSize
(JNIEnv *env, jobject obj)
{
  QDesktopWidget *d = QApplication::desktop();
  QSize s = d->size();
  return makeDimension( env, &s );
}

/*
 * Returns the available fonts
 */
JNIEXPORT jobjectArray JNICALL Java_gnu_java_awt_peer_qt_QtToolkit_nativeFontFamilies
(JNIEnv *env, jobject obj)
{
  jobjectArray result_array;
  jobject *result_array_ptr;
  QFontDatabase db;
  QStringList fonts = db.families();

  result_array = env->NewObjectArray(fonts.size(), 
				     env->FindClass("java/lang/String"),
				     env->NewStringUTF(""));
  for (int i = 0; i < fonts.size(); i++)
    {
      QString qstr = fonts.at(i);
      jstring jstr = getJavaString(env, &qstr);
      env->SetObjectArrayElement( result_array, i, jstr );
    }
  return result_array;
}


/*
 * Returns the screen resolution 
 */
JNIEXPORT jint JNICALL Java_gnu_java_awt_peer_qt_QtToolkit_getScreenResolution
(JNIEnv *env, jobject obj)
{
  QDesktopWidget *d = qApplication->desktop();

  // Java assumes square pixels. Qt, more intelligently, does not.
  // What to do? Well.. Average them?
  // FIXME: Weird values?
  int dpi = (d->logicalDpiX() + d->logicalDpiY()) >> 1;

  return (jint)dpi;
}

/*
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtToolkit_sync
(JNIEnv *env, jobject obj)
{
  // SyncX needs to be called from the main thread.
  mainThread->postEventToMain( new AWTSyncEvent( qApplication ) );
}

