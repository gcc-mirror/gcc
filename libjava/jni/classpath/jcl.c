/* jcl.c
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

#include <stdio.h>
#include <jcl.h>
#include <stdlib.h>

#ifndef __GNUC__
#define __attribute__(x) /* nothing */
#endif

/*
 * This way the memory is pre-allocated, so that we do not have to worry
 * if we are out of memory.
 */
static char errstr[4098];

JNIEXPORT void JNICALL JCL_ThrowException(JNIEnv * env, char * className, char * errMsg) {
	jclass excClass;
	if((*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
	}
	excClass = (*env)->FindClass(env, className);
	if(excClass == NULL) {
		jclass errExcClass;
		errExcClass = (*env)->FindClass(env, "java/lang/ClassNotFoundException");
		if(errExcClass == NULL) {
			errExcClass = (*env)->FindClass(env, "java/lang/InternalError");
			if(errExcClass == NULL) {
				sprintf(errstr,"JCL: Utterly failed to throw exeption %s with message %s.",className,errMsg);
				fprintf(stderr, errstr);
				return;
			}
		}
		sprintf(errstr,"JCL: Failed to throw exception %s with message %s: could not find exception class.", className, errMsg);
		(*env)->ThrowNew(env, errExcClass, errstr);
	}
	(*env)->ThrowNew(env, excClass, errMsg);
}

JNIEXPORT void * JNICALL JCL_malloc(JNIEnv * env, size_t size) {
	void * mem = malloc(size);
	if(mem == NULL) {
		JCL_ThrowException(env, "java/lang/OutOfMemoryError", "malloc() failed.");
		return NULL;
	}
	return mem;
}

JNIEXPORT void * JNICALL JCL_realloc(JNIEnv *env, void *ptr, size_t size)
{
  ptr = realloc(ptr, size);
  if (ptr == 0)
    {
      JCL_ThrowException(env, "java/lang/OutOfMemoryError",
                             "malloc() failed.");
      return NULL;
    }
  return(ptr);
}

JNIEXPORT void JNICALL JCL_free(JNIEnv * env __attribute__((unused)),
				void * p)
{
	if(p != NULL) {
		free(p);
	}
}

JNIEXPORT char * JNICALL JCL_jstring_to_cstring(JNIEnv * env, jstring s) {
	char* cstr;
	if(s == NULL) {
		JCL_ThrowException(env, "java/lang/NullPointerException","Null string");
		return NULL;
	}
	cstr = (char*)(*env)->GetStringUTFChars(env, s, NULL);
	if(cstr == NULL) {
		JCL_ThrowException(env, "java/lang/InternalError", "GetStringUTFChars() failed.");
		return NULL;
	}
	return cstr;
}

JNIEXPORT void JNICALL JCL_free_cstring(JNIEnv * env, jstring s, char * cstr) {
	(*env)->ReleaseStringUTFChars(env, s, cstr);
}

JNIEXPORT jint JNICALL JCL_MonitorEnter(JNIEnv * env, jobject o) {
	jint retval = (*env)->MonitorEnter(env,o);
	if(retval != 0) {
		JCL_ThrowException(env, "java/lang/InternalError", "MonitorEnter() failed.");
	}
	return retval;
}

JNIEXPORT jint JNICALL JCL_MonitorExit(JNIEnv * env, jobject o) {
	jint retval = (*env)->MonitorExit(env,o);
	if(retval != 0) {
		JCL_ThrowException(env, "java/lang/InternalError", "MonitorExit() failed.");
	}
	return retval;
}

JNIEXPORT jclass JNICALL JCL_FindClass(JNIEnv * env, char * className) {
	jclass retval = (*env)->FindClass(env,className);
	if(retval == NULL) {
		JCL_ThrowException(env, "java/lang/ClassNotFoundException", className);
	}
	return retval;
}
