/* qmatrix.cpp --
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
#include <QMatrix>
#include <gnu_java_awt_peer_qt_QMatrix.h>
#include "nativewrapper.h"

/*
 * Creates a QMatrix
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QMatrix_init
(JNIEnv *env, jobject obj, jdouble m00, jdouble m10, jdouble m01, jdouble m11, 
 jdouble m02, jdouble m12 )
{
  QMatrix *matrix = new QMatrix( (qreal) m00, (qreal) m10, 
				 (qreal) m01, (qreal) m11, 
				 (qreal) m02, (qreal) m12 );
  assert( matrix );
  setNativeObject(env, obj, matrix);
}

/*
 * Returns the matrix a java array of doubles, 
 * in m00, m10, m01, m11, m02, m12 (java notation) format.
 * Note that qt has different notations for the array elements.
 */
JNIEXPORT jdoubleArray JNICALL Java_gnu_java_awt_peer_qt_QMatrix_getMatrix
(JNIEnv *env, jobject obj)
{
  QMatrix *matrix = (QMatrix *)getNativeObject(env, obj);
  assert( matrix );

  jdoubleArray result_array;
  jdouble *dst;

  result_array = env->NewDoubleArray( 6 );
  dst = env->GetDoubleArrayElements(result_array, NULL);
  
  dst[0] = (jdouble)matrix->m11(); // qt m11 = java m00
  dst[1] = (jdouble)matrix->m12(); // qt m12 = java m10
  dst[2] = (jdouble)matrix->m21(); // qt m21 = java m01
  dst[3] = (jdouble)matrix->m22(); // qt m22 = java m11
  dst[4] = (jdouble)matrix->dx();  // qt dx  = java m02
  dst[5] = (jdouble)matrix->dy();  // qt dy  = java m12

  env->ReleaseDoubleArrayElements (result_array, dst, 0);
  return result_array;
}

/*
 * Dispose of the thing.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QMatrix_dispose
(JNIEnv *env, jobject obj)
{
  QMatrix *matrix = (QMatrix *)getNativeObject(env, obj);
  if( matrix )
    delete matrix;
  setNativeObject(env, obj, NULL);
}

