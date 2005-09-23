/* qpainterpath.cpp --
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
#include <QPainterPath>
#include <gnu_java_awt_peer_qt_QPainterPath.h>
#include "nativewrapper.h"

// java.awt.geom.PathIterator constants.
#define WIND_EVEN_ODD 0
#define WIND_NON_ZERO 1


/*
 * Creates an empty QPainterPath.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_init
(JNIEnv *env, jobject obj, jint windingRule)
{
  QPainterPath *path = new QPainterPath();
  assert( path );
  path->setFillRule( (windingRule == WIND_EVEN_ODD) ? 
		     Qt::OddEvenFill : Qt::WindingFill );
  setNativeObject(env, obj, path);
}

/*
 * MoveTo
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_moveTo
(JNIEnv *env, jobject obj, jdouble x, jdouble y)
{
  QPainterPath *path = (QPainterPath *)getNativeObject(env, obj);
  assert( path );
  path->moveTo( (qreal)x, (qreal)y );
}

/*
 * Closes the subpath.
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_close
(JNIEnv *env, jobject obj)
{
  QPainterPath *path = (QPainterPath *)getNativeObject(env, obj);
  assert( path );
  path->closeSubpath();
}

/*
 * LineTo
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_lineTo
(JNIEnv *env, jobject obj, jdouble x, jdouble y)
{
  QPainterPath *path = (QPainterPath *)getNativeObject(env, obj);
  assert( path );
  path->lineTo( (qreal)x, (qreal)y );
}

/*
 * QuadraticTo
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_quadTo
(JNIEnv *env, jobject obj, jdouble x, jdouble y, jdouble x2, jdouble y2)
{
  QPainterPath *path = (QPainterPath *)getNativeObject(env, obj);
  assert( path );
  path->quadTo( (qreal)x, (qreal)y, (qreal)x2, (qreal)y2 );
}

/*
 * CubicTo
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_cubicTo
(JNIEnv *env, jobject obj, jdouble x, jdouble y, jdouble x2, jdouble y2, 
 jdouble x3, jdouble y3)
{
  QPainterPath *path = (QPainterPath *)getNativeObject(env, obj);
  assert( path );
  path->cubicTo( (qreal)x, (qreal)y, 
		 (qreal)x2, (qreal)y2,
		 (qreal)x3, (qreal)y3 );
}

/*
 * Delete the native object
 */
JNIEXPORT void JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_dispose
  (JNIEnv *env, jobject obj)
{
  QPainterPath *path = (QPainterPath *)getNativeObject(env, obj);
  if ( path )
    delete path;
}

/********* GeneralPath functions *****************************/

// FIXME : Cache method ids.

static void gp_moveTo( JNIEnv *env,
		       jobject gp,
		       jclass cls,
		       double x1,
		       double y1 )
{
  jmethodID method;
  jvalue values[2];

  values[0].f = (jfloat) x1;
  values[1].f = (jfloat) y1;

  method = env->GetMethodID(cls, "moveTo", "(FF)V");
  env->CallVoidMethodA( gp, method, values );
}

static void gp_lineTo( JNIEnv *env,
		       jobject gp,
		       jclass cls,
		       double x1,
		       double y1 )
{
  jmethodID method;
  jvalue values[2];

  values[0].f = (jfloat) x1;
  values[1].f = (jfloat) y1;

  method = env->GetMethodID(cls, "lineTo", "(FF)V");
  env->CallVoidMethodA( gp, method, values );
}

static void gp_curveTo( JNIEnv *env,
			jobject gp,
			jclass cls,
			double x1,
			double y1,
			double x2,
			double y2,
			double x3,
			double y3 )
{
  jmethodID method;
  jvalue values[6];

  values[0].f = (jfloat) x1;
  values[1].f = (jfloat) y1;
  values[2].f = (jfloat) x2;
  values[3].f = (jfloat) y2;
  values[4].f = (jfloat) x3;
  values[5].f = (jfloat) y3;

  method = env->GetMethodID(cls, "curveTo", "(FFFFFF)V");
  env->CallVoidMethodA( gp, method, values );
}

/**
 * Returns the QPainterPath obj as a java.awt.geom.GeneralPath.
 */
JNIEXPORT jobject JNICALL Java_gnu_java_awt_peer_qt_QPainterPath_getPath
(JNIEnv *env, jobject obj)
{
  jclass cls;
  jmethodID method;
  jobject gp;
  QPainterPath::Element currElement;
  int windingRule;

  QPainterPath *path = new QPainterPath();
  assert( path );

  windingRule = (path->fillRule() == Qt::OddEvenFill) ? 
    WIND_EVEN_ODD : WIND_NON_ZERO;

  cls = env->FindClass("java/awt/geom/GeneralPath");
  method = env->GetMethodID(cls, "<init>", "(I)V");
  gp = env->NewObject(cls, method, windingRule);
  
  for( int i = 0; i < path->elementCount(); i++)
    {
      currElement = path->elementAt( i );
      switch(currElement.type)
	{
	case QPainterPath::MoveToElement:
	  gp_moveTo(env, gp, cls, currElement.x, currElement.y);
	  break;
	case QPainterPath::LineToElement:
	  gp_lineTo(env, gp, cls, currElement.x, currElement.y);
	  break;
	case QPainterPath::CurveToElement:
	  if( i + 2 >= path->elementCount() )
	    break;
	  if(path->elementAt(i + 1).type != QPainterPath::CurveToDataElement ||
	     path->elementAt(i + 2).type != QPainterPath::CurveToDataElement)
	    break;
	  gp_curveTo(env, gp, cls, currElement.x, currElement.y,
		     path->elementAt(i + 1).x, path->elementAt(i + 1).y, 
		     path->elementAt(i + 2).x, path->elementAt(i + 2).y );
	  i += 2;
	  break;
	}
    }
  env->DeleteLocalRef( cls );
  return gp;
}

