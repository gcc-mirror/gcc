/* jcl.h
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

#ifndef __JCL_H__
#define __JCL_H__

#include <stddef.h>
#include <jni.h>
#include <config.h>

JNIEXPORT jclass JNICALL JCL_FindClass(JNIEnv * env, char * className);
JNIEXPORT void JNICALL JCL_ThrowException(JNIEnv * env, char * className, char * errMsg);
JNIEXPORT void * JNICALL JCL_malloc(JNIEnv *env, size_t size);
JNIEXPORT void * JNICALL JCL_realloc(JNIEnv *env, void *ptr, size_t size);
JNIEXPORT void JNICALL JCL_free(JNIEnv *env, void * p);
JNIEXPORT char * JNICALL JCL_jstring_to_cstring(JNIEnv *env, jstring s);
JNIEXPORT void JNICALL JCL_free_cstring(JNIEnv *env, jstring s, char * cstr);
JNIEXPORT jint JNICALL JCL_MonitorEnter(JNIEnv *env, jobject o);
JNIEXPORT jint JNICALL JCL_MonitorExit(JNIEnv *env, jobject o);

#define JCL_RETHROW_EXCEPTION(env) if((*(env))->ExceptionOccurred((env)) != NULL) return NULL;

/* Simple debug macro */
#ifdef DEBUG
#define DBG(x) fprintf(stderr, (x));
#else
#define DBG(x)
#endif

#endif
