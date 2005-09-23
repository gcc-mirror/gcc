/* qtaudioclip.cpp --
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
#include <QString>
#include <QSound>
#include <gnu_java_awt_peer_qt_QtAudioClip.h>
#include "qtstrings.h"
#include "nativewrapper.h"

/*
 * Loads an audio clip. Returns JNI_TRUE if the load succeded, 
 * JNI_FALSE otherwise.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtAudioClip_loadClip
(JNIEnv *env, jobject obj, jstring filename)
{ 
  QString *qStr = getQString(env, filename);

  QSound *sound = new QSound( *qStr );

  delete qStr;

  setNativeObject( env, obj, sound );
}

/*
 * Plays the audio clip, plays looped if loop equals JNI_TRUE.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtAudioClip_play
(JNIEnv *env, jobject obj, jboolean loop)
{
  QSound *sound = (QSound *)getNativeObject(env, obj);
  if( sound != NULL )
    {
      sound->setLoops( (loop == JNI_TRUE) ? -1 : 1 );
      sound->play();
    }
}

/*
 * Stops the audio playback.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtAudioClip_stop
(JNIEnv *env, jobject obj)
{
  QSound *sound = (QSound *)getNativeObject(env, obj);
  if( sound != NULL )
    sound->stop();
}

/**
 * Disposes the audio clip
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QtAudioClip_dispose
(JNIEnv *env, jobject obj)
{
  QSound *sound = (QSound *)getNativeObject(env, obj);
  if( sound != NULL )
    {
      setNativeObject( env, obj, NULL );
      if( !sound->isFinished() )
	sound->stop();
      delete sound;
   }
}

/**
 * Returns whether sound is available.
 */
JNIEXPORT jboolean JNICALL Java_gnu_java_awt_peer_qt_QtAudioClip_isAvailable
(JNIEnv *env, jobject obj)
{
  if( QSound::isAvailable() )
    return JNI_TRUE;
  else
    return JNI_FALSE;
}

