/* qpen.cpp --
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
#include <QColor>
#include <QPen>
#include <gnu_java_awt_peer_qt_QPen.h>
#include "nativewrapper.h"

/*
 * java.awt.geom.BasicStroke constants.
 */
#define JOIN_MITER 0
#define JOIN_ROUND 1
#define JOIN_BEVEL 2
#define CAP_BUTT   0
#define CAP_ROUND  1
#define CAP_SQUARE 2

/**
 * Create a QPen object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPen_init
(JNIEnv *env, jobject obj , jdouble width, jint cap, jint join, jdouble miterlimit)
{
  Qt::PenCapStyle qtCap;
  Qt::PenJoinStyle qtJoin;

  switch(cap)
    {
    case CAP_BUTT:
      qtCap = Qt::FlatCap;
      break;
    case CAP_ROUND:
      qtCap = Qt::RoundCap;
      break;
    case CAP_SQUARE:
      qtCap = Qt::SquareCap;
      break;
    }

  switch(join)
    {
    case JOIN_MITER:
      qtJoin = Qt::MiterJoin;
      break;
    case JOIN_ROUND:
      qtJoin = Qt::RoundJoin;
      break;
    case JOIN_BEVEL:
      qtJoin = Qt::BevelJoin;
      break;
    }

  QPen *pen = new QPen();
  assert( pen );
  pen->setWidthF( (qreal)width );
  pen->setCapStyle( qtCap );
  pen->setJoinStyle( qtJoin );

  setNativeObject(env, obj, pen);
}

/**
 * Dispose of the QPen object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPen_dispose
(JNIEnv *env, jobject obj)
{
  QPen *pen = (QPen *)getNativeObject(env, obj);
  if ( pen )
    delete pen;
}

