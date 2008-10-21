/* jni.h
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008  Free Software Foundation

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


/* Note: this file must be compilable by the C compiler (for now,
   assuming GNU C is ok).  This means you must never use `//'
   comments, and all C++-specific code must be conditional on
   __cplusplus.  */

#ifndef _CLASSPATH_JNI_H
#define _CLASSPATH_JNI_H

/* We include <stdio.h> for compatibility with Sun's <jni.h>.  */
#include <stdio.h>

#include <stdarg.h>

#include "jni_md.h"

/* The VM might define jobject and friends.  */
#ifndef _CLASSPATH_VM_JNI_TYPES_DEFINED

# ifdef __cplusplus

/* Define dummy classes and then define the JNI types as pointers.  */
struct __jobject {};
struct __jclass : __jobject {};
struct __jstring : __jobject {};
struct __jthrowable : __jobject {};
struct __jweak : __jobject {};
struct __jarray : __jobject {};
struct __jobjectArray : __jarray {};
struct __jbyteArray : __jarray {};
struct __jshortArray : __jarray {};
struct __jintArray : __jarray {};
struct __jlongArray : __jarray {};
struct __jbooleanArray : __jarray {};
struct __jcharArray : __jarray {};
struct __jfloatArray : __jarray {};
struct __jdoubleArray : __jarray {};

typedef __jobject *jobject;
typedef __jclass *jclass;
typedef __jstring *jstring;
typedef __jthrowable *jthrowable;
typedef __jweak *jweak;
typedef __jarray *jarray;
typedef __jobjectArray *jobjectArray;
typedef __jbyteArray *jbyteArray;
typedef __jshortArray *jshortArray;
typedef __jintArray *jintArray;
typedef __jlongArray *jlongArray;
typedef __jbooleanArray *jbooleanArray;
typedef __jcharArray *jcharArray;
typedef __jfloatArray *jfloatArray;
typedef __jdoubleArray *jdoubleArray;

#define JNI_TRUE true
#define JNI_FALSE false

typedef struct _Jv_JNIEnv JNIEnv;
typedef struct _Jv_JavaVM JavaVM;

# else /* __cplusplus */

/* For C, simply define the class types as generic pointers.  */
typedef void *jobject;
typedef jobject jclass;
typedef jobject jstring;
typedef jobject jthrowable;
typedef jobject jweak;
typedef jobject jarray;
typedef jobject jobjectArray;
typedef jobject jbyteArray;
typedef jobject jshortArray;
typedef jobject jintArray;
typedef jobject jlongArray;
typedef jobject jbooleanArray;
typedef jobject jcharArray;
typedef jobject jfloatArray;
typedef jobject jdoubleArray;

#define JNI_TRUE  1
#define JNI_FALSE 0

typedef const struct JNINativeInterface_ *JNIEnv;
typedef const struct JNIInvokeInterface_ *JavaVM;

# endif /* __cplusplus */

#endif /* _CLASSPATH_VM_JNI_TYPES_DEFINED */

/* 
 * Before jni.h is #included within a typical JVM, the source code should 
 * #define _JNI_VM_INTERNAL_TYPES_DEFINED and provide the real declarations
 * for 'jobject', 'jfieldID', 'jmethodID' and other implementation types.
 * If _JNI_VM_INTERNAL_TYPES_DEFINED is not defined, the following 
 * declares the old versions of the types.
 */
#ifndef _CLASSPATH_VM_INTERNAL_TYPES_DEFINED
struct _jfieldID;
struct _jmethodID;
typedef struct _jfieldID *jfieldID;
typedef struct _jmethodID *jmethodID;
#endif 

/* Version numbers.  */
#define JNI_VERSION_1_1 0x00010001
#define JNI_VERSION_1_2 0x00010002
#define JNI_VERSION_1_4 0x00010004
#define JNI_VERSION_1_6 0x00010006

/* Used when releasing array elements.  */
#define JNI_COMMIT 1
#define JNI_ABORT  2

/* Error codes */
#define JNI_OK            0
#define JNI_ERR          (-1)
#define JNI_EDETACHED    (-2)
#define JNI_EVERSION     (-3)

enum _jobjectRefType
{
  JNIInvalidRefType    = 0,
  JNILocalRefType      = 1,
  JNIGlobalRefType     = 2,
  JNIWeakGlobalRefType = 3 
};

typedef enum _jobjectRefType jobjectRefType;

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

/* These functions might be defined in libraries which we load; the
   JNI implementation calls them at the appropriate times.  */
extern JNIEXPORT jint JNICALL JNI_OnLoad (JavaVM *, void *);
extern JNIEXPORT void JNICALL JNI_OnUnload (JavaVM *, void *);

/* This can be defined as JNIIMPORT or JNIEXPORT by the md file,
   depending on whether this is the implementation or a user.  */
#ifndef _CLASSPATH_JNIIMPEXP
#define _CLASSPATH_JNIIMPEXP JNIIMPORT
#endif

/* These functions are called by user code to start using the
   invocation API.  */
extern _CLASSPATH_JNIIMPEXP jint JNICALL
JNI_GetDefaultJavaVMInitArgs (void *);

extern _CLASSPATH_JNIIMPEXP jint JNICALL
JNI_CreateJavaVM (JavaVM **, void **, void *);

extern _CLASSPATH_JNIIMPEXP jint JNICALL
JNI_GetCreatedJavaVMs (JavaVM **, jsize, jsize *);

#ifdef __cplusplus
}
#endif /* __cplusplus */

typedef union jvalue
{
  jboolean z;
  jbyte    b;
  jchar    c;
  jshort   s;
  jint     i;
  jlong    j;
  jfloat   f;
  jdouble  d;
  jobject  l;
} jvalue;

/* This structure is used when registering native methods.  */
typedef struct
{
  char *name;
  char *signature;
  void *fnPtr;			/* Sigh.  */
} JNINativeMethod;

struct JNINativeInterface_
{
  void *reserved0;
  void *reserved1;
  void *reserved2;
  void *reserved3;

  jint     (JNICALL *GetVersion)                   (JNIEnv *);
  jclass   (JNICALL *DefineClass)                  (JNIEnv *, const char *,
						    jobject, const jbyte *,
						    jsize);
  jclass   (JNICALL *FindClass)                    (JNIEnv *, const char *);

  jmethodID (JNICALL *FromReflectedMethod)	   (JNIEnv *, jobject);
  jfieldID  (JNICALL *FromReflectedField)	   (JNIEnv *, jobject);
  jobject   (JNICALL *ToReflectedMethod)	   (JNIEnv *, jclass,
						    jmethodID, jboolean);

  jclass   (JNICALL *GetSuperclass)                (JNIEnv *, jclass);
  jboolean (JNICALL *IsAssignableFrom)             (JNIEnv *, jclass, jclass);

  jobject  (JNICALL *ToReflectedField)		   (JNIEnv *, jclass, jfieldID,
                                                    jboolean);

  jint     (JNICALL *Throw)                        (JNIEnv *, jthrowable);
  jint     (JNICALL *ThrowNew)                     (JNIEnv *, jclass, 
                                                    const char *);
  jthrowable (JNICALL *ExceptionOccurred)          (JNIEnv *);
  void     (JNICALL *ExceptionDescribe)            (JNIEnv *);
  void     (JNICALL *ExceptionClear)               (JNIEnv *);
  void     (JNICALL *FatalError)                   (JNIEnv *, const char *);

  jint     (JNICALL *PushLocalFrame)		   (JNIEnv *, jint);
  jobject  (JNICALL *PopLocalFrame)		   (JNIEnv *, jobject);

  jobject  (JNICALL *NewGlobalRef)                 (JNIEnv *, jobject);
  void     (JNICALL *DeleteGlobalRef)              (JNIEnv *, jobject);
  void     (JNICALL *DeleteLocalRef)               (JNIEnv *, jobject);
  jboolean (JNICALL *IsSameObject)                 (JNIEnv *, jobject, 
                                                    jobject);

  jobject  (JNICALL *NewLocalRef)		   (JNIEnv *, jobject);
  jint     (JNICALL *EnsureLocalCapacity)	   (JNIEnv *, jint);

  jobject  (JNICALL *AllocObject)                  (JNIEnv *, jclass);
  jobject (JNICALL *NewObject)			   (JNIEnv *, jclass, 
                                                    jmethodID, ...);
  jobject (JNICALL *NewObjectV)			   (JNIEnv *, jclass, 
                                                    jmethodID, va_list);
  jobject (JNICALL *NewObjectA)			   (JNIEnv *, jclass, 
                                                    jmethodID, const jvalue *);

  jclass   (JNICALL *GetObjectClass)               (JNIEnv *, jobject);
  jboolean (JNICALL *IsInstanceOf)                 (JNIEnv *, jobject, jclass);
  jmethodID (JNICALL *GetMethodID)                 (JNIEnv *, jclass, 
                                                    const char *, const char *);

  jobject (JNICALL *CallObjectMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jobject (JNICALL *CallObjectMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jobject (JNICALL *CallObjectMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jboolean (JNICALL *CallBooleanMethod)	   (JNIEnv *, jobject, jmethodID,
                                            ...);
  jboolean (JNICALL *CallBooleanMethodV)   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jboolean (JNICALL *CallBooleanMethodA)   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jbyte (JNICALL *CallByteMethod)   (JNIEnv *, jobject, jmethodID, ...);
  jbyte (JNICALL *CallByteMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jbyte (JNICALL *CallByteMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jchar (JNICALL *CallCharMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jchar (JNICALL *CallCharMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jchar (JNICALL *CallCharMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jshort (JNICALL *CallShortMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jshort (JNICALL *CallShortMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jshort (JNICALL *CallShortMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jint 	(JNICALL *CallIntMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jint 	(JNICALL *CallIntMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jint 	(JNICALL *CallIntMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jlong (JNICALL *CallLongMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jlong (JNICALL *CallLongMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jlong (JNICALL *CallLongMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jfloat (JNICALL *CallFloatMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jfloat (JNICALL *CallFloatMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jfloat (JNICALL *CallFloatMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  jdouble (JNICALL *CallDoubleMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  jdouble (JNICALL *CallDoubleMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  jdouble (JNICALL *CallDoubleMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);
  void  (JNICALL *CallVoidMethod)	   (JNIEnv *, jobject, jmethodID, ...);
  void  (JNICALL *CallVoidMethodV)	   (JNIEnv *, jobject, jmethodID,
                                            va_list);
  void  (JNICALL *CallVoidMethodA)	   (JNIEnv *, jobject, jmethodID,
                                            const jvalue *);

  jobject   (JNICALL *CallNonvirtualObjectMethod)  (JNIEnv *, jobject, jclass,
                                                    jmethodID, ...);
  jobject   (JNICALL *CallNonvirtualObjectMethodV) (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jobject   (JNICALL *CallNonvirtualObjectMethodA) (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  jboolean  (JNICALL *CallNonvirtualBooleanMethod) (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jboolean  (JNICALL *CallNonvirtualBooleanMethodV) (JNIEnv *, jobject, jclass,
					             jmethodID, va_list);
  jboolean  (JNICALL *CallNonvirtualBooleanMethodA) (JNIEnv *, jobject, jclass,
					             jmethodID, const jvalue *);
  jbyte     (JNICALL *CallNonvirtualByteMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jbyte     (JNICALL *CallNonvirtualByteMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jbyte     (JNICALL *CallNonvirtualByteMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  jchar     (JNICALL *CallNonvirtualCharMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jchar     (JNICALL *CallNonvirtualCharMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jchar     (JNICALL *CallNonvirtualCharMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  jshort    (JNICALL *CallNonvirtualShortMethod)   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jshort    (JNICALL *CallNonvirtualShortMethodV)  (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jshort    (JNICALL *CallNonvirtualShortMethodA)  (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  jint 	    (JNICALL *CallNonvirtualIntMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jint 	    (JNICALL *CallNonvirtualIntMethodV)	   (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jint 	    (JNICALL *CallNonvirtualIntMethodA)	   (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  jlong     (JNICALL *CallNonvirtualLongMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jlong     (JNICALL *CallNonvirtualLongMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jlong     (JNICALL *CallNonvirtualLongMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  jfloat    (JNICALL *CallNonvirtualFloatMethod)   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jfloat    (JNICALL *CallNonvirtualFloatMethodV)  (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jfloat    (JNICALL *CallNonvirtualFloatMethodA)  (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  jdouble   (JNICALL *CallNonvirtualDoubleMethod)  (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  jdouble   (JNICALL *CallNonvirtualDoubleMethodV) (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  jdouble   (JNICALL *CallNonvirtualDoubleMethodA) (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);
  void      (JNICALL *CallNonvirtualVoidMethod)	   (JNIEnv *, jobject, jclass,
					            jmethodID, ...);
  void      (JNICALL *CallNonvirtualVoidMethodV)   (JNIEnv *, jobject, jclass,
					            jmethodID, va_list);
  void      (JNICALL *CallNonvirtualVoidMethodA)   (JNIEnv *, jobject, jclass,
					            jmethodID, const jvalue *);

  jfieldID  (JNICALL *GetFieldID)          (JNIEnv *, jclass, const char *,
					    const char *);

  jobject  (JNICALL *GetObjectField)       (JNIEnv *, jobject, jfieldID);
  jboolean (JNICALL *GetBooleanField)      (JNIEnv *, jobject, jfieldID);
  jbyte    (JNICALL *GetByteField)         (JNIEnv *, jobject, jfieldID);
  jchar    (JNICALL *GetCharField)         (JNIEnv *, jobject, jfieldID);
  jshort   (JNICALL *GetShortField)        (JNIEnv *, jobject, jfieldID);
  jint     (JNICALL *GetIntField)          (JNIEnv *, jobject, jfieldID);
  jlong    (JNICALL *GetLongField)         (JNIEnv *, jobject, jfieldID);
  jfloat   (JNICALL *GetFloatField)        (JNIEnv *, jobject, jfieldID);
  jdouble  (JNICALL *GetDoubleField)       (JNIEnv *, jobject, jfieldID);

  void	(JNICALL *SetObjectField)	   (JNIEnv *, jobject,
					    jfieldID, jobject);
  void	(JNICALL *SetBooleanField)	   (JNIEnv *, jobject,
					    jfieldID, jboolean);
  void	(JNICALL *SetByteField)		   (JNIEnv *, jobject,
					    jfieldID, jbyte);
  void	(JNICALL *SetCharField)		   (JNIEnv *, jobject,
					    jfieldID, jchar);
  void	(JNICALL *SetShortField)	   (JNIEnv *, jobject,
					    jfieldID, jshort);
  void	(JNICALL *SetIntField)		   (JNIEnv *, jobject,
					    jfieldID, jint);
  void	(JNICALL *SetLongField)		   (JNIEnv *, jobject,
					    jfieldID, jlong);
  void	(JNICALL *SetFloatField)	   (JNIEnv *, jobject,
					    jfieldID, jfloat);
  void	(JNICALL *SetDoubleField)	   (JNIEnv *, jobject,
					    jfieldID, jdouble);

  jmethodID (JNICALL *GetStaticMethodID)   (JNIEnv *, jclass, const char *,
					    const char *);

  jobject  (JNICALL *CallStaticObjectMethod)  (JNIEnv *, jclass, jmethodID,
					       ...);
  jobject  (JNICALL *CallStaticObjectMethodV) (JNIEnv *, jclass, jmethodID,
					       va_list);
  jobject  (JNICALL *CallStaticObjectMethodA) (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  jboolean (JNICALL *CallStaticBooleanMethod) (JNIEnv *, jclass, jmethodID,
					       ...);
  jboolean (JNICALL *CallStaticBooleanMethodV) (JNIEnv *, jclass, jmethodID,
					        va_list);
  jboolean (JNICALL *CallStaticBooleanMethodA) (JNIEnv *, jclass, jmethodID,
					        const jvalue *);
  jbyte	   (JNICALL *CallStaticByteMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  jbyte    (JNICALL *CallStaticByteMethodV)   (JNIEnv *, jclass, jmethodID,
					       va_list);
  jbyte    (JNICALL *CallStaticByteMethodA)   (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  jchar    (JNICALL *CallStaticCharMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  jchar    (JNICALL *CallStaticCharMethodV)   (JNIEnv *, jclass, jmethodID,
					       va_list);
  jchar    (JNICALL *CallStaticCharMethodA)   (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  jshort   (JNICALL *CallStaticShortMethod)   (JNIEnv *, jclass, jmethodID,
					       ...);
  jshort   (JNICALL *CallStaticShortMethodV)  (JNIEnv *, jclass, jmethodID,
					       va_list);
  jshort   (JNICALL *CallStaticShortMethodA)  (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  jint 	   (JNICALL *CallStaticIntMethod)     (JNIEnv *, jclass, jmethodID,
					       ...);
  jint 	   (JNICALL *CallStaticIntMethodV)    (JNIEnv *, jclass, jmethodID,
					       va_list);
  jint 	   (JNICALL *CallStaticIntMethodA)    (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  jlong    (JNICALL *CallStaticLongMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  jlong    (JNICALL *CallStaticLongMethodV)   (JNIEnv *, jclass, jmethodID,
					       va_list);
  jlong    (JNICALL *CallStaticLongMethodA)   (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  jfloat   (JNICALL *CallStaticFloatMethod)   (JNIEnv *, jclass, jmethodID,
					       ...);
  jfloat   (JNICALL *CallStaticFloatMethodV)  (JNIEnv *, jclass, jmethodID,
					       va_list);
  jfloat   (JNICALL *CallStaticFloatMethodA)  (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  jdouble  (JNICALL *CallStaticDoubleMethod)  (JNIEnv *, jclass, jmethodID,
					       ...);
  jdouble  (JNICALL *CallStaticDoubleMethodV) (JNIEnv *, jclass, jmethodID,
					       va_list);
  jdouble  (JNICALL *CallStaticDoubleMethodA) (JNIEnv *, jclass, jmethodID,
					       const jvalue *);
  void     (JNICALL *CallStaticVoidMethod)    (JNIEnv *, jclass, jmethodID,
					       ...);
  void     (JNICALL *CallStaticVoidMethodV)   (JNIEnv *, jclass, jmethodID,
					       va_list);
  void     (JNICALL *CallStaticVoidMethodA)   (JNIEnv *, jclass, jmethodID,
					       const jvalue *);

  jfieldID (JNICALL *GetStaticFieldID)        (JNIEnv *, jclass, const char *,
					       const char *);

  jobject  (JNICALL *GetStaticObjectField)    (JNIEnv *, jclass, jfieldID);
  jboolean (JNICALL *GetStaticBooleanField)   (JNIEnv *, jclass, jfieldID);
  jbyte	   (JNICALL *GetStaticByteField)      (JNIEnv *, jclass, jfieldID);
  jchar	   (JNICALL *GetStaticCharField)      (JNIEnv *, jclass, jfieldID);
  jshort   (JNICALL *GetStaticShortField)     (JNIEnv *, jclass, jfieldID);
  jint	   (JNICALL *GetStaticIntField)	      (JNIEnv *, jclass, jfieldID);
  jlong	   (JNICALL *GetStaticLongField)      (JNIEnv *, jclass, jfieldID);
  jfloat   (JNICALL *GetStaticFloatField)     (JNIEnv *, jclass, jfieldID);
  jdouble  (JNICALL *GetStaticDoubleField)    (JNIEnv *, jclass, jfieldID);

  void 	(JNICALL *SetStaticObjectField)	   (JNIEnv *, jclass,
					    jfieldID, jobject);
  void 	(JNICALL *SetStaticBooleanField)   (JNIEnv *, jclass,
					    jfieldID, jboolean);
  void 	(JNICALL *SetStaticByteField)	   (JNIEnv *, jclass,
					    jfieldID, jbyte);
  void 	(JNICALL *SetStaticCharField)	   (JNIEnv *, jclass,
					    jfieldID, jchar);
  void 	(JNICALL *SetStaticShortField)	   (JNIEnv *, jclass,
					    jfieldID, jshort);
  void 	(JNICALL *SetStaticIntField)	   (JNIEnv *, jclass,
					    jfieldID, jint);
  void 	(JNICALL *SetStaticLongField)	   (JNIEnv *, jclass,
					    jfieldID, jlong);
  void 	(JNICALL *SetStaticFloatField)	   (JNIEnv *, jclass,
					    jfieldID, jfloat);
  void 	(JNICALL *SetStaticDoubleField)	   (JNIEnv *, jclass,
					    jfieldID, jdouble);

  jstring  (JNICALL *NewString)            (JNIEnv *, const jchar *, jsize);
  jsize    (JNICALL *GetStringLength)      (JNIEnv *, jstring);
  const jchar * (JNICALL *GetStringChars)  (JNIEnv *, jstring, jboolean *);
  void     (JNICALL *ReleaseStringChars)   (JNIEnv *, jstring, const jchar *);
  jstring  (JNICALL *NewStringUTF)         (JNIEnv *, const char *);
  jsize    (JNICALL *GetStringUTFLength)   (JNIEnv *, jstring);
  const char * (JNICALL *GetStringUTFChars) (JNIEnv *, jstring, jboolean *);
  void     (JNICALL *ReleaseStringUTFChars) (JNIEnv *, jstring, const char *);
  jsize    (JNICALL *GetArrayLength)       (JNIEnv *, jarray);
  jobjectArray (JNICALL *NewObjectArray)    (JNIEnv *, jsize, jclass, jobject);
  jobject  (JNICALL *GetObjectArrayElement) (JNIEnv *, jobjectArray, jsize);
  void     (JNICALL *SetObjectArrayElement) (JNIEnv *, jobjectArray, jsize,
					     jobject);

  jbooleanArray (JNICALL *NewBooleanArray)	   (JNIEnv *, jsize);
  jbyteArray    (JNICALL *NewByteArray)		   (JNIEnv *, jsize);
  jcharArray    (JNICALL *NewCharArray)		   (JNIEnv *, jsize);
  jshortArray   (JNICALL *NewShortArray)	   (JNIEnv *, jsize);
  jintArray     (JNICALL *NewIntArray)		   (JNIEnv *, jsize);
  jlongArray    (JNICALL *NewLongArray)		   (JNIEnv *, jsize);
  jfloatArray   (JNICALL *NewFloatArray)	   (JNIEnv *, jsize);
  jdoubleArray  (JNICALL *NewDoubleArray)	   (JNIEnv *, jsize);

  jboolean *	(JNICALL *GetBooleanArrayElements) (JNIEnv *, jbooleanArray,
					            jboolean *);
  jbyte *	(JNICALL *GetByteArrayElements)	   (JNIEnv *, jbyteArray,
					            jboolean *);
  jchar *	(JNICALL *GetCharArrayElements)	   (JNIEnv *, jcharArray,
					            jboolean *);
  jshort *	(JNICALL *GetShortArrayElements)   (JNIEnv *, jshortArray,
					            jboolean *);
  jint *	(JNICALL *GetIntArrayElements)	   (JNIEnv *, jintArray,
					            jboolean *);
  jlong *	(JNICALL *GetLongArrayElements)	   (JNIEnv *, jlongArray,
					            jboolean *);
  jfloat *	(JNICALL *GetFloatArrayElements)   (JNIEnv *, jfloatArray,
					            jboolean *);
  jdouble *	(JNICALL *GetDoubleArrayElements)  (JNIEnv *, jdoubleArray,
					            jboolean *);

  void		(JNICALL *ReleaseBooleanArrayElements) (JNIEnv *, jbooleanArray,
						        jboolean *, jint);
  void		(JNICALL *ReleaseByteArrayElements)    (JNIEnv *, jbyteArray,
					                jbyte *, jint);
  void		(JNICALL *ReleaseCharArrayElements)    (JNIEnv *, jcharArray,
						        jchar *, jint);
  void		(JNICALL *ReleaseShortArrayElements)   (JNIEnv *, jshortArray,
						        jshort *, jint);
  void		(JNICALL *ReleaseIntArrayElements)     (JNIEnv *, jintArray,
						        jint *, jint);
  void		(JNICALL *ReleaseLongArrayElements)    (JNIEnv *, jlongArray,
						        jlong *, jint);
  void		(JNICALL *ReleaseFloatArrayElements)   (JNIEnv *, jfloatArray,
						        jfloat *, jint);
  void		(JNICALL *ReleaseDoubleArrayElements)  (JNIEnv *, jdoubleArray,
						        jdouble *, jint);

  void 		(JNICALL *GetBooleanArrayRegion)   (JNIEnv *, jbooleanArray,
					            jsize, jsize, jboolean *);
  void 		(JNICALL *GetByteArrayRegion)	   (JNIEnv *, jbyteArray,
					            jsize, jsize, jbyte *);
  void 		(JNICALL *GetCharArrayRegion)	   (JNIEnv *, jcharArray,
					            jsize, jsize, jchar *);
  void 		(JNICALL *GetShortArrayRegion)	   (JNIEnv *, jshortArray,
					            jsize, jsize, jshort *);
  void 		(JNICALL *GetIntArrayRegion)	   (JNIEnv *, jintArray,
					            jsize, jsize, jint *);
  void 		(JNICALL *GetLongArrayRegion)	   (JNIEnv *, jlongArray,
					            jsize, jsize, jlong *);
  void 		(JNICALL *GetFloatArrayRegion)	   (JNIEnv *, jfloatArray,
					            jsize, jsize, jfloat *);
  void 		(JNICALL *GetDoubleArrayRegion)	   (JNIEnv *, jdoubleArray,
					            jsize, jsize, jdouble *);

  void 		(JNICALL *SetBooleanArrayRegion)   (JNIEnv *, jbooleanArray,
					            jsize, jsize,
                                                    const jboolean *);
  void 		(JNICALL *SetByteArrayRegion)	   (JNIEnv *, jbyteArray,
					            jsize, jsize,
                                                    const jbyte *);
  void 		(JNICALL *SetCharArrayRegion)	   (JNIEnv *, jcharArray,
					            jsize, jsize,
                                                    const jchar *);
  void 		(JNICALL *SetShortArrayRegion)	   (JNIEnv *, jshortArray,
					            jsize, jsize,
                                                    const jshort *);
  void 		(JNICALL *SetIntArrayRegion)	   (JNIEnv *, jintArray,
					            jsize, jsize,
                                                    const jint *);
  void 		(JNICALL *SetLongArrayRegion)	   (JNIEnv *, jlongArray,
					            jsize, jsize,
                                                    const jlong *);
  void 		(JNICALL *SetFloatArrayRegion)	   (JNIEnv *, jfloatArray,
					            jsize, jsize,
                                                    const jfloat *);
  void 		(JNICALL *SetDoubleArrayRegion)	   (JNIEnv *, jdoubleArray,
					            jsize, jsize,
                                                    const jdouble *);

  jint     (JNICALL *RegisterNatives)              (JNIEnv *, jclass,
					            const JNINativeMethod *, 
						    jint);
  jint     (JNICALL *UnregisterNatives)            (JNIEnv *, jclass);
  jint     (JNICALL *MonitorEnter)                 (JNIEnv *, jobject);
  jint     (JNICALL *MonitorExit)                  (JNIEnv *, jobject);
  jint     (JNICALL *GetJavaVM)                    (JNIEnv *, JavaVM **);

  /* ---- JNI 1.2 functions ---- */

  void	   (JNICALL *GetStringRegion)	           (JNIEnv *, jstring, jsize,
					            jsize, jchar *);
  void     (JNICALL *GetStringUTFRegion)	   (JNIEnv *, jstring, jsize,
					            jsize, char *);

  void * (JNICALL *GetPrimitiveArrayCritical)      (JNIEnv *, jarray, 
                                                    jboolean *);
  void   (JNICALL *ReleasePrimitiveArrayCritical)  (JNIEnv *, jarray, void *, 
                                                    jint);

  const jchar * (JNICALL *GetStringCritical)       (JNIEnv *, jstring, 
                                                    jboolean *);
  void          (JNICALL *ReleaseStringCritical)   (JNIEnv *, jstring, 
                                                    const jchar *);

  jweak  (JNICALL *NewWeakGlobalRef)               (JNIEnv *, jobject);
  void   (JNICALL *DeleteWeakGlobalRef)            (JNIEnv *, jweak);

  jboolean	(JNICALL *ExceptionCheck)	   (JNIEnv *);

  /* ---- JNI 1.4 functions ---- */

  jobject (JNICALL *NewDirectByteBuffer)           (JNIEnv *, void *, jlong);
  void *  (JNICALL *GetDirectBufferAddress)        (JNIEnv *, jobject);
  jlong   (JNICALL *GetDirectBufferCapacity)       (JNIEnv *, jobject);

  /* ---- JNI 1.6 functions ---- */

  jobjectRefType (JNICALL *GetObjectRefType)       (JNIEnv *, jobject);
};

#ifdef __cplusplus

class _Jv_JNIEnv
{
public:
  /* The method table.  */
  struct JNINativeInterface_ *functions;

#ifdef _CLASSPATH_JNIENV_CONTENTS
  _CLASSPATH_JNIENV_CONTENTS
#endif

  jint GetVersion ()
  { return functions->GetVersion (this); }

  jclass DefineClass (const char *name, jobject obj0, const jbyte * val1,
		      jsize val2)
  { return functions->DefineClass (this, name, obj0, val1, val2); }

  jclass FindClass (const char * val0)
  { return functions->FindClass (this, val0); }

  jmethodID FromReflectedMethod (jobject obj0)
  { return functions->FromReflectedMethod (this, obj0); }

  jfieldID FromReflectedField (jobject obj0)
  { return functions->FromReflectedField (this, obj0); }

  jobject ToReflectedMethod (jclass cl0, jmethodID meth1, jboolean val2)
  { return functions->ToReflectedMethod (this, cl0, meth1, val2); }

  jclass GetSuperclass (jclass cl0)
  { return functions->GetSuperclass (this, cl0); }

  jboolean IsAssignableFrom (jclass cl0, jclass cl1)
  { return functions->IsAssignableFrom (this, cl0, cl1); }

  jobject ToReflectedField (jclass cl0, jfieldID fld1, jboolean val2)
  { return functions->ToReflectedField (this, cl0, fld1, val2); }

  jint Throw (jthrowable val0)
  { return functions->Throw (this, val0); }

  jint ThrowNew (jclass cl0, const char * val1)
  { return functions->ThrowNew (this, cl0, val1); }

  jthrowable ExceptionOccurred ()
  { return functions->ExceptionOccurred (this); }

  void ExceptionDescribe ()
  { functions->ExceptionDescribe (this); }

  void ExceptionClear ()
  { functions->ExceptionClear (this); }

  void FatalError (const char * val0)
  { functions->FatalError (this, val0); }

  jint PushLocalFrame (jint val0)
  { return functions->PushLocalFrame (this, val0); }

  jobject PopLocalFrame (jobject obj0)
  { return functions->PopLocalFrame (this, obj0); }

  jobject NewGlobalRef (jobject obj0)
  { return functions->NewGlobalRef (this, obj0); }

  void DeleteGlobalRef (jobject obj0)
  { functions->DeleteGlobalRef (this, obj0); }

  void DeleteLocalRef (jobject obj0)
  { functions->DeleteLocalRef (this, obj0); }

  jboolean IsSameObject (jobject obj0, jobject obj1)
  { return functions->IsSameObject (this, obj0, obj1); }

  jobject NewLocalRef (jobject obj0)
  { return functions->NewLocalRef (this, obj0); }

  jint EnsureLocalCapacity (jint val0)
  { return functions->EnsureLocalCapacity (this, val0); }

  jobject AllocObject (jclass cl0)
  { return functions->AllocObject (this, cl0); }

  jobject NewObject (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jobject result = functions->NewObjectV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jobject NewObjectV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->NewObjectV (this, cl0, meth1, val2); }

  jobject NewObjectA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return functions->NewObjectA (this, cl0, meth1, val2); }

  jclass GetObjectClass (jobject obj0)
  { return functions->GetObjectClass (this, obj0); }

  jboolean IsInstanceOf (jobject obj0, jclass cl1)
  { return functions->IsInstanceOf (this, obj0, cl1); }

  jmethodID GetMethodID (jclass cl0, const char * val1, const char * val2)
  { return functions->GetMethodID (this, cl0, val1, val2); }

  jobject CallObjectMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jobject result = functions->CallObjectMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jobject CallObjectMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallObjectMethodV (this, obj0, meth1, val2); }

  jobject CallObjectMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return functions->CallObjectMethodA (this, obj0, meth1, val2); }

  jboolean CallBooleanMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jboolean result = functions->CallBooleanMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jboolean CallBooleanMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallBooleanMethodV (this, obj0, meth1, val2); }

  jboolean CallBooleanMethodA (jobject obj0, jmethodID meth1,
                               const jvalue * val2)
  { return functions->CallBooleanMethodA (this, obj0, meth1, val2); }

  jbyte CallByteMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jbyte result = functions->CallByteMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jbyte CallByteMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallByteMethodV (this, obj0, meth1, val2); }

  jbyte CallByteMethodA (jobject obj0, jmethodID meth1, const jvalue * val2)
  { return functions->CallByteMethodA (this, obj0, meth1, val2); }

  jchar CallCharMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jchar result = functions->CallCharMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jchar CallCharMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallCharMethodV (this, obj0, meth1, val2); }

  jchar CallCharMethodA (jobject obj0, jmethodID meth1, const jvalue * val2)
  { return functions->CallCharMethodA (this, obj0, meth1, val2); }

  jshort CallShortMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jshort result = functions->CallShortMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jshort CallShortMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallShortMethodV (this, obj0, meth1, val2); }

  jshort CallShortMethodA (jobject obj0, jmethodID meth1, const jvalue * val2)
  { return functions->CallShortMethodA (this, obj0, meth1, val2); }

  jint CallIntMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jint result = functions->CallIntMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jint CallIntMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallIntMethodV (this, obj0, meth1, val2); }

  jint CallIntMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return functions->CallIntMethodA (this, obj0, meth1, val2); }

  jlong CallLongMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jlong result = functions->CallLongMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jlong CallLongMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallLongMethodV (this, obj0, meth1, val2); }

  jlong CallLongMethodA (jobject obj0, jmethodID meth1, const jvalue * val2)
  { return functions->CallLongMethodA (this, obj0, meth1, val2); }

  jfloat CallFloatMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jfloat result = functions->CallFloatMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jfloat CallFloatMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallFloatMethodV (this, obj0, meth1, val2); }

  jfloat CallFloatMethodA (jobject obj0, jmethodID meth1, const jvalue * val2)
  { return functions->CallFloatMethodA (this, obj0, meth1, val2); }

  jdouble CallDoubleMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jdouble result = functions->CallDoubleMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jdouble CallDoubleMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { return functions->CallDoubleMethodV (this, obj0, meth1, val2); }

  jdouble CallDoubleMethodA (jobject obj0, jmethodID meth1, const jvalue * val2)
  { return functions->CallDoubleMethodA (this, obj0, meth1, val2); }

  void CallVoidMethod (jobject obj0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    functions->CallVoidMethodV (this, obj0, meth1, args);
    va_end (args);
  }

  void CallVoidMethodV (jobject obj0, jmethodID meth1, va_list val2)
  { functions->CallVoidMethodV (this, obj0, meth1, val2); }

  void CallVoidMethodA (jobject obj0, jmethodID meth1, const jvalue * val2)
  { functions->CallVoidMethodA (this, obj0, meth1, val2); }

  jobject CallNonvirtualObjectMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jobject result = functions->CallNonvirtualObjectMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jobject CallNonvirtualObjectMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualObjectMethodV (this, obj0, cl1, meth2, val3); }

  jobject CallNonvirtualObjectMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualObjectMethodA (this, obj0, cl1, meth2, val3); }

  jboolean CallNonvirtualBooleanMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jboolean result = functions->CallNonvirtualBooleanMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jboolean CallNonvirtualBooleanMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualBooleanMethodV (this, obj0, cl1, meth2, val3); }

  jboolean CallNonvirtualBooleanMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualBooleanMethodA (this, obj0, cl1, meth2, val3); }

  jbyte CallNonvirtualByteMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jbyte result = functions->CallNonvirtualByteMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jbyte CallNonvirtualByteMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualByteMethodV (this, obj0, cl1, meth2, val3); }

  jbyte CallNonvirtualByteMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualByteMethodA (this, obj0, cl1, meth2, val3); }

  jchar CallNonvirtualCharMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jchar result = functions->CallNonvirtualCharMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jchar CallNonvirtualCharMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualCharMethodV (this, obj0, cl1, meth2, val3); }

  jchar CallNonvirtualCharMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualCharMethodA (this, obj0, cl1, meth2, val3); }

  jshort CallNonvirtualShortMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jshort result = functions->CallNonvirtualShortMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jshort CallNonvirtualShortMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualShortMethodV (this, obj0, cl1, meth2, val3); }

  jshort CallNonvirtualShortMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualShortMethodA (this, obj0, cl1, meth2, val3); }

  jint CallNonvirtualIntMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jint result = functions->CallNonvirtualIntMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jint CallNonvirtualIntMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualIntMethodV (this, obj0, cl1, meth2, val3); }

  jint CallNonvirtualIntMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualIntMethodA (this, obj0, cl1, meth2, val3); }

  jlong CallNonvirtualLongMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jlong result = functions->CallNonvirtualLongMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jlong CallNonvirtualLongMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualLongMethodV (this, obj0, cl1, meth2, val3); }

  jlong CallNonvirtualLongMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualLongMethodA (this, obj0, cl1, meth2, val3); }

  jfloat CallNonvirtualFloatMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jfloat result = functions->CallNonvirtualFloatMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jfloat CallNonvirtualFloatMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualFloatMethodV (this, obj0, cl1, meth2, val3); }

  jfloat CallNonvirtualFloatMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualFloatMethodA (this, obj0, cl1, meth2, val3); }

  jdouble CallNonvirtualDoubleMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    jdouble result = functions->CallNonvirtualDoubleMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jdouble CallNonvirtualDoubleMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { return functions->CallNonvirtualDoubleMethodV (this, obj0, cl1, meth2, val3); }

  jdouble CallNonvirtualDoubleMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { return functions->CallNonvirtualDoubleMethodA (this, obj0, cl1, meth2, val3); }

  void CallNonvirtualVoidMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    va_list args;
    va_start (args, meth2);
    functions->CallNonvirtualVoidMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
  }

  void CallNonvirtualVoidMethodV (jobject obj0, jclass cl1, jmethodID meth2, va_list val3)
  { functions->CallNonvirtualVoidMethodV (this, obj0, cl1, meth2, val3); }

  void CallNonvirtualVoidMethodA (jobject obj0, jclass cl1, jmethodID meth2, const jvalue * val3)
  { functions->CallNonvirtualVoidMethodA (this, obj0, cl1, meth2, val3); }

  jfieldID GetFieldID (jclass cl0, const char * val1, const char * val2)
  { return functions->GetFieldID (this, cl0, val1, val2); }

  jobject GetObjectField (jobject obj0, jfieldID fld1)
  { return functions->GetObjectField (this, obj0, fld1); }

  jboolean GetBooleanField (jobject obj0, jfieldID fld1)
  { return functions->GetBooleanField (this, obj0, fld1); }

  jbyte GetByteField (jobject obj0, jfieldID fld1)
  { return functions->GetByteField (this, obj0, fld1); }

  jchar GetCharField (jobject obj0, jfieldID fld1)
  { return functions->GetCharField (this, obj0, fld1); }

  jshort GetShortField (jobject obj0, jfieldID fld1)
  { return functions->GetShortField (this, obj0, fld1); }

  jint GetIntField (jobject obj0, jfieldID fld1)
  { return functions->GetIntField (this, obj0, fld1); }

  jlong GetLongField (jobject obj0, jfieldID fld1)
  { return functions->GetLongField (this, obj0, fld1); }

  jfloat GetFloatField (jobject obj0, jfieldID fld1)
  { return functions->GetFloatField (this, obj0, fld1); }

  jdouble GetDoubleField (jobject obj0, jfieldID fld1)
  { return functions->GetDoubleField (this, obj0, fld1); }

  void SetObjectField (jobject obj0, jfieldID fld1, jobject obj2)
  { functions->SetObjectField (this, obj0, fld1, obj2); }

  void SetBooleanField (jobject obj0, jfieldID fld1, jboolean val2)
  { functions->SetBooleanField (this, obj0, fld1, val2); }

  void SetByteField (jobject obj0, jfieldID fld1, jbyte val2)
  { functions->SetByteField (this, obj0, fld1, val2); }

  void SetCharField (jobject obj0, jfieldID fld1, jchar val2)
  { functions->SetCharField (this, obj0, fld1, val2); }

  void SetShortField (jobject obj0, jfieldID fld1, jshort val2)
  { functions->SetShortField (this, obj0, fld1, val2); }

  void SetIntField (jobject obj0, jfieldID fld1, jint val2)
  { functions->SetIntField (this, obj0, fld1, val2); }

  void SetLongField (jobject obj0, jfieldID fld1, jlong val2)
  { functions->SetLongField (this, obj0, fld1, val2); }

  void SetFloatField (jobject obj0, jfieldID fld1, jfloat val2)
  { functions->SetFloatField (this, obj0, fld1, val2); }

  void SetDoubleField (jobject obj0, jfieldID fld1, jdouble val2)
  { functions->SetDoubleField (this, obj0, fld1, val2); }

  jmethodID GetStaticMethodID (jclass cl0, const char * val1, const char * val2)
  { return functions->GetStaticMethodID (this, cl0, val1, val2); }

  jobject CallStaticObjectMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jobject result = functions->CallStaticObjectMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jobject CallStaticObjectMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticObjectMethodV (this, cl0, meth1, val2); }

  jobject CallStaticObjectMethodA (jclass cl0, jmethodID meth1,
                                   const jvalue * val2)
  { return functions->CallStaticObjectMethodA (this, cl0, meth1, val2); }

  jboolean CallStaticBooleanMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jboolean result = functions->CallStaticBooleanMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jboolean CallStaticBooleanMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticBooleanMethodV (this, cl0, meth1, val2); }

  jboolean CallStaticBooleanMethodA (jclass cl0, jmethodID meth1,
                                     const jvalue * val2)
  { return functions->CallStaticBooleanMethodA (this, cl0, meth1, val2); }

  jbyte CallStaticByteMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jbyte result = functions->CallStaticByteMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jbyte CallStaticByteMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticByteMethodV (this, cl0, meth1, val2); }

  jbyte CallStaticByteMethodA (jclass cl0, jmethodID meth1, const jvalue * val2)
  { return functions->CallStaticByteMethodA (this, cl0, meth1, val2); }

  jchar CallStaticCharMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jchar result = functions->CallStaticCharMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jchar CallStaticCharMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticCharMethodV (this, cl0, meth1, val2); }

  jchar CallStaticCharMethodA (jclass cl0, jmethodID meth1, const jvalue * val2)
  { return functions->CallStaticCharMethodA (this, cl0, meth1, val2); }

  jshort CallStaticShortMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jshort result = functions->CallStaticShortMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jshort CallStaticShortMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticShortMethodV (this, cl0, meth1, val2); }

  jshort CallStaticShortMethodA (jclass cl0, jmethodID meth1,
                                 const jvalue * val2)
  { return functions->CallStaticShortMethodA (this, cl0, meth1, val2); }

  jint CallStaticIntMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jint result = functions->CallStaticIntMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jint CallStaticIntMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticIntMethodV (this, cl0, meth1, val2); }

  jint CallStaticIntMethodA (jclass cl0, jmethodID meth1, const jvalue * val2)
  { return functions->CallStaticIntMethodA (this, cl0, meth1, val2); }

  jlong CallStaticLongMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jlong result = functions->CallStaticLongMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jlong CallStaticLongMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticLongMethodV (this, cl0, meth1, val2); }

  jlong CallStaticLongMethodA (jclass cl0, jmethodID meth1, const jvalue * val2)
  { return functions->CallStaticLongMethodA (this, cl0, meth1, val2); }

  jfloat CallStaticFloatMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jfloat result = functions->CallStaticFloatMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jfloat CallStaticFloatMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticFloatMethodV (this, cl0, meth1, val2); }

  jfloat CallStaticFloatMethodA (jclass cl0, jmethodID meth1,
                                 const jvalue * val2)
  { return functions->CallStaticFloatMethodA (this, cl0, meth1, val2); }

  jdouble CallStaticDoubleMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    jdouble result = functions->CallStaticDoubleMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jdouble CallStaticDoubleMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { return functions->CallStaticDoubleMethodV (this, cl0, meth1, val2); }

  jdouble CallStaticDoubleMethodA (jclass cl0, jmethodID meth1,
                                   const jvalue * val2)
  { return functions->CallStaticDoubleMethodA (this, cl0, meth1, val2); }

  void CallStaticVoidMethod (jclass cl0, jmethodID meth1, ...)
  {
    va_list args;
    va_start (args, meth1);
    functions->CallStaticVoidMethodV (this, cl0, meth1, args);
    va_end (args);
  }

  void CallStaticVoidMethodV (jclass cl0, jmethodID meth1, va_list val2)
  { functions->CallStaticVoidMethodV (this, cl0, meth1, val2); }

  void CallStaticVoidMethodA (jclass cl0, jmethodID meth1, const jvalue * val2)
  { functions->CallStaticVoidMethodA (this, cl0, meth1, val2); }

  jfieldID GetStaticFieldID (jclass cl0, const char * val1, const char * val2)
  { return functions->GetStaticFieldID (this, cl0, val1, val2); }

  jobject GetStaticObjectField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticObjectField (this, cl0, fld1); }

  jboolean GetStaticBooleanField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticBooleanField (this, cl0, fld1); }

  jbyte GetStaticByteField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticByteField (this, cl0, fld1); }

  jchar GetStaticCharField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticCharField (this, cl0, fld1); }

  jshort GetStaticShortField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticShortField (this, cl0, fld1); }

  jint GetStaticIntField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticIntField (this, cl0, fld1); }

  jlong GetStaticLongField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticLongField (this, cl0, fld1); }

  jfloat GetStaticFloatField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticFloatField (this, cl0, fld1); }

  jdouble GetStaticDoubleField (jclass cl0, jfieldID fld1)
  { return functions->GetStaticDoubleField (this, cl0, fld1); }

  void SetStaticObjectField (jclass cl0, jfieldID fld1, jobject obj2)
  { functions->SetStaticObjectField (this, cl0, fld1, obj2); }

  void SetStaticBooleanField (jclass cl0, jfieldID fld1, jboolean val2)
  { functions->SetStaticBooleanField (this, cl0, fld1, val2); }

  void SetStaticByteField (jclass cl0, jfieldID fld1, jbyte val2)
  { functions->SetStaticByteField (this, cl0, fld1, val2); }

  void SetStaticCharField (jclass cl0, jfieldID fld1, jchar val2)
  { functions->SetStaticCharField (this, cl0, fld1, val2); }

  void SetStaticShortField (jclass cl0, jfieldID fld1, jshort val2)
  { functions->SetStaticShortField (this, cl0, fld1, val2); }

  void SetStaticIntField (jclass cl0, jfieldID fld1, jint val2)
  { functions->SetStaticIntField (this, cl0, fld1, val2); }

  void SetStaticLongField (jclass cl0, jfieldID fld1, jlong val2)
  { functions->SetStaticLongField (this, cl0, fld1, val2); }

  void SetStaticFloatField (jclass cl0, jfieldID fld1, jfloat val2)
  { functions->SetStaticFloatField (this, cl0, fld1, val2); }

  void SetStaticDoubleField (jclass cl0, jfieldID fld1, jdouble val2)
  { functions->SetStaticDoubleField (this, cl0, fld1, val2); }

  jstring NewString (const jchar * val0, jsize val1)
  { return functions->NewString (this, val0, val1); }

  jint GetStringLength (jstring val0)
  { return functions->GetStringLength (this, val0); }

  const jchar * GetStringChars (jstring val0, jboolean * val1)
  { return functions->GetStringChars (this, val0, val1); }

  void ReleaseStringChars (jstring val0, const jchar * val1)
  { functions->ReleaseStringChars (this, val0, val1); }

  jstring NewStringUTF (const char * val0)
  { return functions->NewStringUTF (this, val0); }

  jsize GetStringUTFLength (jstring val0)
  { return functions->GetStringUTFLength (this, val0); }

  const char * GetStringUTFChars (jstring val0, jboolean * val1)
  { return functions->GetStringUTFChars (this, val0, val1); }

  void ReleaseStringUTFChars (jstring val0, const char * val1)
  { functions->ReleaseStringUTFChars (this, val0, val1); }

  jsize GetArrayLength (jarray val0)
  { return functions->GetArrayLength (this, val0); }

  jobjectArray NewObjectArray (jsize val0, jclass cl1, jobject obj2)
  { return functions->NewObjectArray (this, val0, cl1, obj2); }

  jobject GetObjectArrayElement (jobjectArray val0, jsize val1)
  { return functions->GetObjectArrayElement (this, val0, val1); }

  void SetObjectArrayElement (jobjectArray val0, jsize val1, jobject obj2)
  { functions->SetObjectArrayElement (this, val0, val1, obj2); }

  jbooleanArray NewBooleanArray (jsize val0)
  { return functions->NewBooleanArray (this, val0); }

  jbyteArray NewByteArray (jsize val0)
  { return functions->NewByteArray (this, val0); }

  jcharArray NewCharArray (jsize val0)
  { return functions->NewCharArray (this, val0); }

  jshortArray NewShortArray (jsize val0)
  { return functions->NewShortArray (this, val0); }

  jintArray NewIntArray (jsize val0)
  { return functions->NewIntArray (this, val0); }

  jlongArray NewLongArray (jsize val0)
  { return functions->NewLongArray (this, val0); }

  jfloatArray NewFloatArray (jsize val0)
  { return functions->NewFloatArray (this, val0); }

  jdoubleArray NewDoubleArray (jsize val0)
  { return functions->NewDoubleArray (this, val0); }

  jboolean * GetBooleanArrayElements (jbooleanArray val0, jboolean * val1)
  { return functions->GetBooleanArrayElements (this, val0, val1); }

  jbyte * GetByteArrayElements (jbyteArray val0, jboolean * val1)
  { return functions->GetByteArrayElements (this, val0, val1); }

  jchar * GetCharArrayElements (jcharArray val0, jboolean * val1)
  { return functions->GetCharArrayElements (this, val0, val1); }

  jshort * GetShortArrayElements (jshortArray val0, jboolean * val1)
  { return functions->GetShortArrayElements (this, val0, val1); }

  jint * GetIntArrayElements (jintArray val0, jboolean * val1)
  { return functions->GetIntArrayElements (this, val0, val1); }

  jlong * GetLongArrayElements (jlongArray val0, jboolean * val1)
  { return functions->GetLongArrayElements (this, val0, val1); }

  jfloat * GetFloatArrayElements (jfloatArray val0, jboolean * val1)
  { return functions->GetFloatArrayElements (this, val0, val1); }

  jdouble * GetDoubleArrayElements (jdoubleArray val0, jboolean * val1)
  { return functions->GetDoubleArrayElements (this, val0, val1); }

  void ReleaseBooleanArrayElements (jbooleanArray val0, jboolean * val1, jint val2)
  { functions->ReleaseBooleanArrayElements (this, val0, val1, val2); }

  void ReleaseByteArrayElements (jbyteArray val0, jbyte * val1, jint val2)
  { functions->ReleaseByteArrayElements (this, val0, val1, val2); }

  void ReleaseCharArrayElements (jcharArray val0, jchar * val1, jint val2)
  { functions->ReleaseCharArrayElements (this, val0, val1, val2); }

  void ReleaseShortArrayElements (jshortArray val0, jshort * val1, jint val2)
  { functions->ReleaseShortArrayElements (this, val0, val1, val2); }

  void ReleaseIntArrayElements (jintArray val0, jint * val1, jint val2)
  { functions->ReleaseIntArrayElements (this, val0, val1, val2); }

  void ReleaseLongArrayElements (jlongArray val0, jlong * val1, jint val2)
  { functions->ReleaseLongArrayElements (this, val0, val1, val2); }

  void ReleaseFloatArrayElements (jfloatArray val0, jfloat * val1, jint val2)
  { functions->ReleaseFloatArrayElements (this, val0, val1, val2); }

  void ReleaseDoubleArrayElements (jdoubleArray val0, jdouble * val1, jint val2)
  { functions->ReleaseDoubleArrayElements (this, val0, val1, val2); }

  void GetBooleanArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { functions->GetBooleanArrayRegion (this, val0, val1, val2, val3); }

  void GetByteArrayRegion (jbyteArray val0, jsize val1, jsize val2, jbyte * val3)
  { functions->GetByteArrayRegion (this, val0, val1, val2, val3); }

  void GetCharArrayRegion (jcharArray val0, jsize val1, jsize val2, jchar * val3)
  { functions->GetCharArrayRegion (this, val0, val1, val2, val3); }

  void GetShortArrayRegion (jshortArray val0, jsize val1, jsize val2, jshort * val3)
  { functions->GetShortArrayRegion (this, val0, val1, val2, val3); }

  void GetIntArrayRegion (jintArray val0, jsize val1, jsize val2, jint * val3)
  { functions->GetIntArrayRegion (this, val0, val1, val2, val3); }

  void GetLongArrayRegion (jlongArray val0, jsize val1, jsize val2, jlong * val3)
  { functions->GetLongArrayRegion (this, val0, val1, val2, val3); }

  void GetFloatArrayRegion (jfloatArray val0, jsize val1, jsize val2, jfloat * val3)
  { functions->GetFloatArrayRegion (this, val0, val1, val2, val3); }

  void GetDoubleArrayRegion (jdoubleArray val0, jsize val1, jsize val2, jdouble * val3)
  { functions->GetDoubleArrayRegion (this, val0, val1, val2, val3); }

  void SetBooleanArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { functions->SetBooleanArrayRegion (this, val0, val1, val2, val3); }

  void SetByteArrayRegion (jbyteArray val0, jsize val1, jsize val2, jbyte * val3)
  { functions->SetByteArrayRegion (this, val0, val1, val2, val3); }

  void SetCharArrayRegion (jcharArray val0, jsize val1, jsize val2, jchar * val3)
  { functions->SetCharArrayRegion (this, val0, val1, val2, val3); }

  void SetShortArrayRegion (jshortArray val0, jsize val1, jsize val2, jshort * val3)
  { functions->SetShortArrayRegion (this, val0, val1, val2, val3); }

  void SetIntArrayRegion (jintArray val0, jsize val1, jsize val2, jint * val3)
  { functions->SetIntArrayRegion (this, val0, val1, val2, val3); }

  void SetLongArrayRegion (jlongArray val0, jsize val1, jsize val2, jlong * val3)
  { functions->SetLongArrayRegion (this, val0, val1, val2, val3); }

  void SetFloatArrayRegion (jfloatArray val0, jsize val1, jsize val2, jfloat * val3)
  { functions->SetFloatArrayRegion (this, val0, val1, val2, val3); }

  void SetDoubleArrayRegion (jdoubleArray val0, jsize val1, jsize val2, jdouble * val3)
  { functions->SetDoubleArrayRegion (this, val0, val1, val2, val3); }

  jint RegisterNatives (jclass cl0, const JNINativeMethod * val1, jint val2)
  { return functions->RegisterNatives (this, cl0, val1, val2); }

  jint UnregisterNatives (jclass cl0)
  { return functions->UnregisterNatives (this, cl0); }

  jint MonitorEnter (jobject obj0)
  { return functions->MonitorEnter (this, obj0); }

  jint MonitorExit (jobject obj0)
  { return functions->MonitorExit (this, obj0); }

  jint GetJavaVM (JavaVM ** val0)
  { return functions->GetJavaVM (this, val0); }

  /* ---- JNI 1.2 functions ---- */

  void GetStringRegion (jstring val0, jsize val1, jsize val2, jchar * val3)
  { functions->GetStringRegion (this, val0, val1, val2, val3); }

  void GetStringUTFRegion (jstring val0, jsize val1, jsize val2, char * val3)
  { functions->GetStringUTFRegion (this, val0, val1, val2, val3); }

  void * GetPrimitiveArrayCritical (jarray val0, jboolean * val1)
  { return functions->GetPrimitiveArrayCritical (this, val0, val1); }

  void ReleasePrimitiveArrayCritical (jarray val0, void * val1, jint val2)
  { functions->ReleasePrimitiveArrayCritical (this, val0, val1, val2); }

  const jchar * GetStringCritical (jstring val0, jboolean * val1)
  { return functions->GetStringCritical (this, val0, val1); }

  void ReleaseStringCritical (jstring val0, const jchar * val1)
  { functions->ReleaseStringCritical (this, val0, val1); }

  jweak NewWeakGlobalRef (jobject obj0)
  { return functions->NewWeakGlobalRef (this, obj0); }

  void DeleteWeakGlobalRef (jweak val0)
  { functions->DeleteWeakGlobalRef (this, val0); }

  jboolean ExceptionCheck ()
  { return functions->ExceptionCheck (this); }

  /* ---- JNI 1.4 functions ---- */

  jobject NewDirectByteBuffer (void *addr, jlong capacity)
  { return functions->NewDirectByteBuffer (this, addr, capacity); }

  void *GetDirectBufferAddress (jobject buf)
  { return functions->GetDirectBufferAddress (this, buf); }

  jlong GetDirectBufferCapacity (jobject buf)
  { return functions->GetDirectBufferCapacity (this, buf); }

  /* ---- JNI 1.6 functions ---- */

  jobjectRefType GetObjectRefType (jobject obj)
  { return functions->GetObjectRefType (this, obj); }
};

#endif /* __cplusplus */

/*
 * Invocation API.
 */

struct JNIInvokeInterface_
{
  void *reserved0;
  void *reserved1;
  void *reserved2;

  jint (JNICALL *DestroyJavaVM)         (JavaVM *);
  jint (JNICALL *AttachCurrentThread)   (JavaVM *, void **, void *);
  jint (JNICALL *DetachCurrentThread)   (JavaVM *);
  jint (JNICALL *GetEnv)                (JavaVM *, void **, jint);
  jint (JNICALL *AttachCurrentThreadAsDaemon) (JavaVM *, void **, void *);
};

#ifdef __cplusplus

class _Jv_JavaVM
{
public:
  const struct JNIInvokeInterface_ *functions;

  jint DestroyJavaVM ()
  { return functions->DestroyJavaVM (this); }

  jint AttachCurrentThread (void **penv, void *args)
  { return functions->AttachCurrentThread (this, penv, args); }

  jint DetachCurrentThread ()
  { return functions->DetachCurrentThread (this); }

  jint GetEnv (void **penv, jint version)
  { return functions->GetEnv (this, penv, version); }

  jint AttachCurrentThreadAsDaemon (void **penv, void *args)
  { return functions->AttachCurrentThreadAsDaemon (this, penv, args); }
};

#endif /* __cplusplus */

typedef struct JavaVMAttachArgs
{
  jint version;			/* Must be JNI_VERSION_1_2.  */
  char *name;			/* The name of the thread (or NULL).  */
  jobject group;		/* Global ref of a ThreadGroup object
				   (or NULL).  */
} JavaVMAttachArgs;

typedef struct JavaVMOption
{
  char *optionString;
  void *extraInfo;
} JavaVMOption;

typedef struct JavaVMInitArgs
{
  /* Must be JNI_VERSION_1_2.  */
  jint version;

  /* Number of options.  */
  jint nOptions;

  /* Options to the VM.  */
  JavaVMOption *options;

  /* Whether we should ignore unrecognized options.  */
  jboolean ignoreUnrecognized;
} JavaVMInitArgs;

typedef struct JDK1_1InitArgs
{
  /* VM version.  Should be JNI_VERSION_1_1.  Note that before JDK
     1.1.2, this field was named 'reserved0'.  (I don't know what the
     current 'reserved0' field was named then.)  */
  jint version;

  /* A null-terminated array of environment strings, each of the form
     "KEY=VALUE".  This is used to set system properties.  Note that
     before JDK 1.1.2, this field was named 'reserved1'.  */
  char **properties;

  jint checkSource;
  jint nativeStackSize;
  jint javaStackSize;
  jint minHeapSize;
  jint maxHeapSize;
  jint verifyMode;
  const char *classpath;

  jint (JNICALL *vfprintf) (FILE *file, const char *fmt, va_list args);
  void (JNICALL *exit) (jint value);
  void (JNICALL *abort) (void);

  jint enableClassGC;
  jint enableVerboseGC;
  jint disableAsyncGC;

  jint reserved0;
  jint reserved1;
  jint reserved2;
} JDK1_1InitArgs;

typedef struct JDK1_1AttachArgs
{
  /* Dummy field since C cannot have empty structs.  The name and type
     are chosen to comply with the spec.  */
  void *__padding;
} JDK1_1AttachArgs;


/* Keep c-font-lock-extra-types in alphabetical order. */
/*
Local Variables:
c-font-lock-extra-types: ("\\sw+_t" \
   "JNIEnv" "JNINativeMethod" "JavaVM" "JavaVMOption" "jarray" \
   "jboolean" "jbooleanArray" "jbyte" "jbyteArray" "jchar"  "jcharArray" \
   "jclass" "jdouble" "jdoubleArray" "jfieldID" "jfloat" "jfloatArray" \
   "jint" "jintArray" "jlong" "jlongArray" "jmethodID" "jobject" \
   "jstring" "jthrowable" "jvalue" "jweak")
End:
*/

#endif /* _CLASSPATH_JNI_H */
