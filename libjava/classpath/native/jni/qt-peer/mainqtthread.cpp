/* mainqtthread.cpp --
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
#include <jni.h>
#include <QApplication>
#include <QThread>
#include <gnu_java_awt_peer_qt_MainQtThread.h>
#include "mainthreadinterface.h"

MainThreadInterface *mainThread;
QApplication *qApplication;

#if defined(Q_WS_X11)
extern void qt_x11_set_global_double_buffer( bool );
#endif

/**
 * Starts up a QApplication
 */
JNIEXPORT jlong JNICALL Java_gnu_java_awt_peer_qt_MainQtThread_init
(JNIEnv *env, jobject obj, jstring theme, jboolean doublebuffer)
{
  int *argc;
  char **argv;

  if(theme != NULL)
    {
      argc = (int*)malloc(sizeof(int));
      *argc = 3;
      argv = (char **)malloc( 3 * sizeof(char *) );
      argv[0] = (char *)malloc(10 * sizeof(char));
      argv[1] = (char *)malloc(10 * sizeof(char));
      argv[2] = (char *)malloc(100 * sizeof(char));
      strncpy(argv[0], "\0", 2);
      strncpy(argv[1], "-style\0", 8);
      strncpy(argv[2], (char *)env->GetStringUTFChars( theme, NULL ), 100);
    } 
   else
    {
      argc = (int*)malloc(sizeof(int));
      *argc = 1;
      argv = (char **)malloc( 3 * sizeof(char *) );
      argv[0] = (char *)malloc(10 * sizeof(char));
      strncpy(argv[0], " \0", 3);
    }
  QApplication *qtApp = new QApplication( *argc, argv );
  assert( qtApp );

  qApplication = qtApp;

  if( theme != NULL)
    env->ReleaseStringUTFChars( theme, argv[1] );

  mainThread = new MainThreadInterface( qtApp );

  jclass cls = env->GetObjectClass(obj);
  jfieldID nofid = env->GetFieldID( cls, "mainThreadInterface", "J" );
  env->SetLongField( obj, nofid, (jlong)mainThread );

#if defined(Q_WS_X11)
  // turn off double-buffering.
  qt_x11_set_global_double_buffer( (doublebuffer == JNI_TRUE) );
#endif

  return (jlong)qtApp;
}

/*
 * Calls QApplication::exec()
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_MainQtThread_exec
(JNIEnv *env, jobject obj, jlong ptr)
{
  QApplication *app = (QApplication *)ptr;
  if(app)
    app->exec();
}
