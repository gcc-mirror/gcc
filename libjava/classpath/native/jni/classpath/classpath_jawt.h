/* classpath_awt.h -- libjawt's interface to the peer library
   Copyright (C) 2005 Free Software Foundation, Inc.

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
   Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.

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

/*
 * libjawt.so is linked directly to the peer library with -l.  This
 * header declares all the functions that libjawt.so needs -- X-based
 * peer libraries wanting to support libjawt.so must implement these
 * functions.
 */

#ifndef __classpath_jawt_h__
#define __classpath_jawt_h__

#include <jni.h>
#include <X11/Xlib.h>

#define CLASSPATH_JAWT_VERSION 0x10004

jint     classpath_jawt_get_awt_version ();
Display* classpath_jawt_get_default_display (JNIEnv* env, jobject canvas);
Drawable classpath_jawt_get_drawable (JNIEnv* env, jobject canvas);
VisualID classpath_jawt_get_visualID (JNIEnv* env, jobject canvas);
jint     classpath_jawt_object_lock (jobject lock);
void     classpath_jawt_object_unlock (jobject lock);
jint     classpath_jawt_lock ();
void     classpath_jawt_unlock ();
jobject  classpath_jawt_create_lock ();
void     classpath_jawt_destroy_lock (jobject lock);

#endif /* __classpath_jawt_h__ */
