/* JNILINK 1.1: JNI version.
   Copyright (C) 1998, 2004 Free Software Foundation, Inc.

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


#include "jnilink.h"
#include <string.h>
#include <jcl.h>

#include <stdlib.h>

#define GETCLASS(c) *(jclass*)(c)

JNIEXPORT jclass JNICALL
LINK_RelinkClass     (JNIEnv * env, linkedClass * c, char * name) {
	jclass found;
	LINK_UnlinkClass(env,*c);

	found = (*env)->FindClass(env,name);
	if(found == NULL)
		return NULL;

	*c = JCL_malloc(env,sizeof(jclass));
	if(*c == NULL)
		return NULL;

	GETCLASS(*c) = (*env)->NewGlobalRef(env,found);
	return GETCLASS(*c);
}

JNIEXPORT jclass JNICALL
LINK_RelinkKnownClass(JNIEnv * env, linkedClass * c, jclass newClass) {
	LINK_UnlinkClass(env,*c);

	*c = JCL_malloc(env,sizeof(jclass));
	if(*c == NULL)
		return NULL;

	GETCLASS(*c) = (*env)->NewGlobalRef(env,newClass);
	return newClass;
}

JNIEXPORT jmethodID JNICALL
LINK_RelinkMethod      (JNIEnv * env, jmethodID * m, linkedClass c,
                        char * name, char * sig) {
	*m = (*env)->GetMethodID(env,GETCLASS(c),name,sig);
	return *m;
}

JNIEXPORT jmethodID JNICALL
LINK_RelinkStaticMethod(JNIEnv * env, jmethodID * m, linkedClass c,
                        char * name, char * sig) {
	*m = (*env)->GetStaticMethodID(env,GETCLASS(c),name,sig);
	return *m;
}

JNIEXPORT jfieldID JNICALL
LINK_RelinkField       (JNIEnv * env, jfieldID * f, linkedClass c,
                        char * name, char * sig) {
	*f = (*env)->GetFieldID(env,GETCLASS(c),name,sig);
	return *f;
}

JNIEXPORT jfieldID JNICALL
LINK_RelinkStaticField (JNIEnv * env, jfieldID * f, linkedClass c,
                        char * name, char * sig) {
	*f = (*env)->GetStaticFieldID(env,GETCLASS(c),name,sig);
	return *f;
}


/* These are for when the class referencing the symbols is unloaded; it
destroys any object references
 * the linker might have kept around.
 */
JNIEXPORT void JNICALL LINK_UnlinkClass       (JNIEnv * env, linkedClass * c) {
	if(*c != NULL) {
		if(GETCLASS(*c) != NULL)
			(*env)->DeleteGlobalRef(env,GETCLASS(*c));
		JCL_free(env,*c);
		*c = NULL;
	}
}

