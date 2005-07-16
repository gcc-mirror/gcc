/* JNILINK 1.1: JNI version.
   Copyright (C) 1998 Free Software Foundation, Inc.

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


#ifndef __JNILINK_H__
#define __JNILINK_H__

#include <jni.h>

typedef void *linkedClass;

#define LINK_LinkClass(env,c,name)              ((c)==NULL ? LINK_ReallyLinkClass((env),&(c),(name)) : (c))
#define LINK_LinkKnownClass(env,c,newClass)     ((c)==NULL ? LINK_ReallyLinkKnownClass((env),&(c),(newClass)) : (c))
#define LINK_LinkMethod(env,m,c,name,sig)       ((m)==NULL ? LINK_RelinkMethod((env),&(m),(c),(name),(sig)) : (m))
#define LINK_LinkStaticMethod(env,m,c,name,sig) ((m)==NULL ? LINK_RelinkStaticMethod((env),&(m),(c),(name),(sig)) : (m))
#define LINK_LinkField(env,f,c,name,sig)        ((m)==NULL ? LINK_RelinkField((env),&(f),(c),(name),(sig)) : (f))
#define LINK_LinkStaticField(env,f,c,name,sig)  ((m)==NULL ? LINK_RelinkStaticField((env),&(f),(c),(name),(sig)) : (f))

#define LINK_LinkConstructor(env,m,c,sig)       ((m)==NULL ? LINK_RelinkMethod((env),&(m),(c),"<init>",(sig)) : (m))

JNIEXPORT jclass JNICALL
LINK_ReallyLinkClass (JNIEnv * env, linkedClass * c, const char *name);
JNIEXPORT jclass JNICALL
LINK_ReallyLinkKnownClass (JNIEnv * env, linkedClass * c, jclass newClass);
JNIEXPORT jclass JNICALL
LINK_RelinkClass (JNIEnv * env, linkedClass * c, const char *name);
JNIEXPORT jclass JNICALL
LINK_RelinkKnownClass (JNIEnv * env, linkedClass * c, jclass newClass);
JNIEXPORT jmethodID JNICALL
LINK_RelinkMethod (JNIEnv * env, jmethodID * m, linkedClass c,
		   const char *name, const char *sig);
JNIEXPORT jmethodID JNICALL
LINK_RelinkStaticMethod (JNIEnv * env, jmethodID * m, linkedClass c,
			 const char *name, const char *sig);
JNIEXPORT jfieldID JNICALL
LINK_RelinkField (JNIEnv * env, jfieldID * f, linkedClass c,
		  const char *name, const char *sig);
JNIEXPORT jfieldID JNICALL
LINK_RelinkStaticField (JNIEnv * env, jfieldID * f, linkedClass c,
			const char *name, const char *sig);

/* These are for when the class referencing the symbols is unloaded; it
destroys any object references
 * the linker might have kept around.
 */
JNIEXPORT void JNICALL LINK_UnlinkClass (JNIEnv * env, linkedClass * c);

#endif
