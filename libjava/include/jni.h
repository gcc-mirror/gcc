/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Note: this file must be compilable by the C compiler (for now,
   assuming GNU C is ok).  This means you must never use `//'
   comments, and all C++-specific code must be conditional on
   __cplusplus.  */

#ifndef __GCJ_JNI_H__
#define __GCJ_JNI_H__

#include <stdarg.h>
#define _Jv_va_list va_list

#ifdef __cplusplus

/* This is wrong, because it pollutes the name-space too much! */
#include <gcj/javaprims.h>

typedef struct _Jv_JNIEnv JNIEnv;
typedef struct _Jv_JavaVM JavaVM;

#define JNI_TRUE true
#define JNI_FALSE false

#else /* __cplusplus */

typedef int    jbyte  __attribute__((__mode__(__QI__)));
typedef int    jshort __attribute__((__mode__(__HI__)));
typedef int    jint   __attribute__((__mode__(__SI__)));
typedef int    jlong  __attribute__((__mode__(__DI__)));
typedef bool   jboolean __attribute__((__mode__(__QI__)));
typedef unsigned short jchar __attribute__((__mode__(__HI__)));
typedef float  jfloat;
typedef double jdouble;
typedef jint jsize;

typedef void *jobject;
typedef jobject jclass;
typedef jobject jstring;
typedef jobject jarray;
typedef jobject jthrowable;
typedef jobject jobjectArray;
typedef jobject jbyteArray;
typedef jobject jshortArray;
typedef jobject jintArray;
typedef jobject jlongArray;
typedef jobject jbooleanArray;
typedef jobject jcharArray;
typedef jobject jfloatArray;
typedef jobject jdoubleArray;

/* Dummy defines.  */
typedef void *jfieldID;
typedef void *jmethodID;

typedef const struct JNINativeInterface *JNIEnv;
typedef const struct JNIInvokeInterface *JavaVM;

#define JNI_TRUE 1
#define JNI_TRUE 0

#endif /* __cplusplus */

/* FIXME: this is wrong.  */
typedef jobject jweak;

/* Version numbers.  */
#define JNI_VERSION_1_1 0x00010001
#define JNI_VERSION_1_2 0x00010002

/* Used when releasing array elements.  */
#define JNI_COMMIT 1
#define JNI_ABORT  2

/* Error codes */
#define JNI_OK            0
#define JNI_ERR          -1
#define JNI_EDETACHED    -2
#define JNI_EVERSION     -3

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

/* These functions might be defined in libraries which we load; the
   JNI implementation calls them at the appropriate times.  */
extern jint JNI_OnLoad (JavaVM *, void *);
extern void JNI_OnUnload (JavaVM *, void *);

/* These functions are called by user code to start using the
   invocation API.  */
extern jint JNI_GetDefaultJavaVMInitArgs (void *);
extern jint JNI_CreateJavaVM (JavaVM **, void **, void *);
extern jint JNI_GetCreatedJavaVMs(JavaVM **, jsize, jsize *);

#ifdef __cplusplus
};
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

typedef void * (*_Jv_func)(...);

/* This structure is used when registering native methods.  */
typedef struct
{
  char *name;
  char *signature;
  void *fnPtr;			/* Sigh.  */
} JNINativeMethod;

struct JNINativeInterface
{
  _Jv_func reserved0;
  _Jv_func reserved1;
  _Jv_func reserved2;
  _Jv_func reserved3;

  jint     (*GetVersion)                   (JNIEnv *);
  jclass   (*DefineClass)                  (JNIEnv *, jobject,
					    const jbyte *, jsize);
  jclass   (*FindClass)                    (JNIEnv *, const char *);

  jmethodID (*FromReflectedMethod)	   (JNIEnv *, jobject);
  jfieldID  (*FromReflectedField)	   (JNIEnv *, jobject);
  jobject   (*ToReflectedMethod)	   (JNIEnv *, jclass, jmethodID,
					    jboolean);

  jclass   (*GetSuperclass)                (JNIEnv *, jclass);
  jboolean (*IsAssignableFrom)             (JNIEnv *, jclass, jclass);

  jobject  (*ToReflectedField)		   (JNIEnv *, jclass, jfieldID,
					    jboolean);

  jint     (*Throw)                        (JNIEnv *, jthrowable);
  jint     (*ThrowNew)                     (JNIEnv *, jclass, const char *);
  jthrowable (*ExceptionOccurred)          (JNIEnv *);
  void     (*ExceptionDescribe)            (JNIEnv *);
  void     (*ExceptionClear)               (JNIEnv *);
  void     (*FatalError)                   (JNIEnv *, const char *);

  jint     (*PushLocalFrame)		   (JNIEnv *, jint);
  jobject  (*PopLocalFrame)		   (JNIEnv *, jobject);

  jobject  (*NewGlobalRef)                 (JNIEnv *, jobject);
  void     (*DeleteGlobalRef)              (JNIEnv *, jobject);
  void     (*DeleteLocalRef)               (JNIEnv *, jobject);;
  jboolean (*IsSameObject)                 (JNIEnv *, jobject, jobject);

  jobject  (*NewLocalRef)		   (JNIEnv *, jobject);
  jint     (*EnsureLocalCapacity)	   (JNIEnv *, jint);

  jobject  (*AllocObject)                  (JNIEnv *, jclass);
  jobject (*NewObject)			   (JNIEnv *, jclass, jmethodID, ...);
  jobject (*NewObjectV)			   (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jobject (*NewObjectA)			   (JNIEnv *, jclass, jmethodID,
					    jvalue *);

  jclass   (*GetObjectClass)               (JNIEnv *, jobject);
  jboolean (*IsInstanceOf)                 (JNIEnv *, jobject, jclass);
  jmethodID (*GetMethodID)                 (JNIEnv *, jclass, const char *,
					    const char *);

  jobject 	(*CallObjectMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jobject 	(*CallObjectMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jobject 	(*CallObjectMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jboolean 	(*CallBooleanMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jboolean 	(*CallBooleanMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jboolean 	(*CallBooleanMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jbyte 	(*CallByteMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jbyte 	(*CallByteMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jbyte 	(*CallByteMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jchar 	(*CallCharMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jchar 	(*CallCharMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jchar 	(*CallCharMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jshort 	(*CallShortMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jshort 	(*CallShortMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jshort 	(*CallShortMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jint 		(*CallIntMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jint 		(*CallIntMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jint 		(*CallIntMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jlong 	(*CallLongMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jlong 	(*CallLongMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jlong 	(*CallLongMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jfloat 	(*CallFloatMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jfloat 	(*CallFloatMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jfloat 	(*CallFloatMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  jdouble 	(*CallDoubleMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  jdouble 	(*CallDoubleMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  jdouble 	(*CallDoubleMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);
  void  	(*CallVoidMethod)	   (JNIEnv *, jobject, jmethodID,
					    ...);
  void  	(*CallVoidMethodV)	   (JNIEnv *, jobject, jmethodID,
					    _Jv_va_list);
  void  	(*CallVoidMethodA)	   (JNIEnv *, jobject, jmethodID,
					    jvalue *);

  jobject   (*CallNonvirtualObjectMethod)  (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jobject   (*CallNonvirtualObjectMethodV) (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jobject   (*CallNonvirtualObjectMethodA) (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  jboolean  (*CallNonvirtualBooleanMethod) (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jboolean  (*CallNonvirtualBooleanMethodV) (JNIEnv *, jobject, jclass,
					     jmethodID, _Jv_va_list);
  jboolean  (*CallNonvirtualBooleanMethodA) (JNIEnv *, jobject, jclass,
					     jmethodID, jvalue *);
  jbyte     (*CallNonvirtualByteMethod)	   (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jbyte     (*CallNonvirtualByteMethodV)   (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jbyte     (*CallNonvirtualByteMethodA)   (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  jchar     (*CallNonvirtualCharMethod)	   (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jchar     (*CallNonvirtualCharMethodV)   (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jchar     (*CallNonvirtualCharMethodA)   (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  jshort    (*CallNonvirtualShortMethod)   (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jshort    (*CallNonvirtualShortMethodV)  (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jshort    (*CallNonvirtualShortMethodA)  (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  jint 	    (*CallNonvirtualIntMethod)	   (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jint 	    (*CallNonvirtualIntMethodV)	   (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jint 	    (*CallNonvirtualIntMethodA)	   (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  jlong     (*CallNonvirtualLongMethod)	   (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jlong     (*CallNonvirtualLongMethodV)   (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jlong     (*CallNonvirtualLongMethodA)   (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  jfloat    (*CallNonvirtualFloatMethod)   (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jfloat    (*CallNonvirtualFloatMethodV)  (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jfloat    (*CallNonvirtualFloatMethodA)  (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  jdouble   (*CallNonvirtualDoubleMethod)  (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  jdouble   (*CallNonvirtualDoubleMethodV) (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  jdouble   (*CallNonvirtualDoubleMethodA) (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);
  void      (*CallNonvirtualVoidMethod)	   (JNIEnv *, jobject, jclass,
					    jmethodID, ...);
  void      (*CallNonvirtualVoidMethodV)   (JNIEnv *, jobject, jclass,
					    jmethodID, _Jv_va_list);
  void      (*CallNonvirtualVoidMethodA)   (JNIEnv *, jobject, jclass,
					    jmethodID, jvalue *);

  jfieldID      (*GetFieldID)              (JNIEnv *, jclass, const char *,
					    const char *);

  jobject  (*GetObjectField)               (JNIEnv *, jobject, jfieldID);
  jboolean (*GetBooleanField)              (JNIEnv *, jobject, jfieldID);
  jbyte    (*GetByteField)                 (JNIEnv *, jobject, jfieldID);
  jchar    (*GetCharField)                 (JNIEnv *, jobject, jfieldID);
  jshort   (*GetShortField)                (JNIEnv *, jobject, jfieldID);
  jint     (*GetIntField)                  (JNIEnv *, jobject, jfieldID);
  jlong    (*GetLongField)                 (JNIEnv *, jobject, jfieldID);
  jfloat   (*GetFloatField)                (JNIEnv *, jobject, jfieldID);
  jdouble  (*GetDoubleField)               (JNIEnv *, jobject, jfieldID);

  void		(*SetObjectField)	   (JNIEnv *, jobject,
					    jfieldID, jobject);
  void		(*SetBooleanField)	   (JNIEnv *, jobject,
					    jfieldID, jboolean);
  void		(*SetByteField)		   (JNIEnv *, jobject,
					    jfieldID, jbyte);
  void		(*SetCharField)		   (JNIEnv *, jobject,
					    jfieldID, jchar);
  void		(*SetShortField)	   (JNIEnv *, jobject,
					    jfieldID, jshort);
  void		(*SetIntField)		   (JNIEnv *, jobject,
					    jfieldID, jint);
  void		(*SetLongField)		   (JNIEnv *, jobject,
					    jfieldID, jlong);
  void		(*SetFloatField)	   (JNIEnv *, jobject,
					    jfieldID, jfloat);
  void		(*SetDoubleField)	   (JNIEnv *, jobject,
					    jfieldID, jdouble);

  jmethodID (*GetStaticMethodID)           (JNIEnv *, jclass, const char *,
					    const char *);

  jobject 	(*CallStaticObjectMethod)  (JNIEnv *, jclass, jmethodID,
					    ...);
  jobject 	(*CallStaticObjectMethodV) (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jobject 	(*CallStaticObjectMethodA) (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  jboolean 	(*CallStaticBooleanMethod) (JNIEnv *, jclass, jmethodID,
					    ...);
  jboolean 	(*CallStaticBooleanMethodV) (JNIEnv *, jclass, jmethodID,
					     _Jv_va_list);
  jboolean 	(*CallStaticBooleanMethodA) (JNIEnv *, jclass, jmethodID,
					     jvalue *);
  jbyte 	(*CallStaticByteMethod)    (JNIEnv *, jclass, jmethodID,
					    ...);
  jbyte 	(*CallStaticByteMethodV)   (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jbyte 	(*CallStaticByteMethodA)   (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  jchar 	(*CallStaticCharMethod)    (JNIEnv *, jclass, jmethodID,
					    ...);
  jchar 	(*CallStaticCharMethodV)   (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jchar 	(*CallStaticCharMethodA)   (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  jshort 	(*CallStaticShortMethod)   (JNIEnv *, jclass, jmethodID,
					    ...);
  jshort 	(*CallStaticShortMethodV)  (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jshort 	(*CallStaticShortMethodA)  (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  jint 		(*CallStaticIntMethod) 	   (JNIEnv *, jclass, jmethodID,
					    ...);
  jint 		(*CallStaticIntMethodV)    (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jint 		(*CallStaticIntMethodA)    (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  jlong 	(*CallStaticLongMethod)    (JNIEnv *, jclass, jmethodID,
					    ...);
  jlong 	(*CallStaticLongMethodV)   (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jlong 	(*CallStaticLongMethodA)   (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  jfloat 	(*CallStaticFloatMethod)   (JNIEnv *, jclass, jmethodID,
					    ...);
  jfloat 	(*CallStaticFloatMethodV)  (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jfloat 	(*CallStaticFloatMethodA)  (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  jdouble 	(*CallStaticDoubleMethod)  (JNIEnv *, jclass, jmethodID,
					    ...);
  jdouble 	(*CallStaticDoubleMethodV) (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  jdouble 	(*CallStaticDoubleMethodA) (JNIEnv *, jclass, jmethodID,
					    jvalue *);
  void  	(*CallStaticVoidMethod)    (JNIEnv *, jclass, jmethodID,
					    ...);
  void  	(*CallStaticVoidMethodV)   (JNIEnv *, jclass, jmethodID,
					    _Jv_va_list);
  void  	(*CallStaticVoidMethodA)   (JNIEnv *, jclass, jmethodID,
					    jvalue *);

  jfieldID      (*GetStaticFieldID)        (JNIEnv *, jclass, const char *,
					    const char *);

  jobject	(*GetStaticObjectField)	   (JNIEnv *, jclass, jfieldID);
  jboolean	(*GetStaticBooleanField)   (JNIEnv *, jclass, jfieldID);
  jbyte		(*GetStaticByteField)	   (JNIEnv *, jclass, jfieldID);
  jchar		(*GetStaticCharField)	   (JNIEnv *, jclass, jfieldID);
  jshort	(*GetStaticShortField)	   (JNIEnv *, jclass, jfieldID);
  jint		(*GetStaticIntField)	   (JNIEnv *, jclass, jfieldID);
  jlong		(*GetStaticLongField)	   (JNIEnv *, jclass, jfieldID);
  jfloat	(*GetStaticFloatField)	   (JNIEnv *, jclass, jfieldID);
  jdouble	(*GetStaticDoubleField)	   (JNIEnv *, jclass, jfieldID);

  void 		(*SetStaticObjectField)	   (JNIEnv *, jclass,
					    jfieldID, jobject);
  void 		(*SetStaticBooleanField)   (JNIEnv *, jclass,
					    jfieldID, jboolean);
  void 		(*SetStaticByteField)	   (JNIEnv *, jclass,
					    jfieldID, jbyte);
  void 		(*SetStaticCharField)	   (JNIEnv *, jclass,
					    jfieldID, jchar);
  void 		(*SetStaticShortField)	   (JNIEnv *, jclass,
					    jfieldID, jshort);
  void 		(*SetStaticIntField)	   (JNIEnv *, jclass,
					    jfieldID, jint);
  void 		(*SetStaticLongField)	   (JNIEnv *, jclass,
					    jfieldID, jlong);
  void 		(*SetStaticFloatField)	   (JNIEnv *, jclass,
					    jfieldID, jfloat);
  void 		(*SetStaticDoubleField)	   (JNIEnv *, jclass,
					    jfieldID, jdouble);

  jstring  (*NewString)                    (JNIEnv *, const jchar *, jsize);
  jint     (*GetStringLength)              (JNIEnv *, jstring);
  const jchar * (*GetStringChars)          (JNIEnv *, jstring, jboolean *);
  void     (*ReleaseStringChars)           (JNIEnv *, jstring, const jchar *);
  jstring  (*NewStringUTF)                 (JNIEnv *, const char *);
  jsize    (*GetStringUTFLength)           (JNIEnv *, jstring);
  const char * (*GetStringUTFChars)        (JNIEnv *, jstring, jboolean *);
  void     (*ReleaseStringUTFChars)        (JNIEnv *, jstring, const char *);
  jsize    (*GetArrayLength)               (JNIEnv *, jarray);
  jarray   (*NewObjectArray)               (JNIEnv *, jsize, jclass, jobject);
  jobject  (*GetObjectArrayElement)        (JNIEnv *, jobjectArray, jsize);
  void     (*SetObjectArrayElement)        (JNIEnv *, jobjectArray, jsize,
					    jobject);

  jbooleanArray (*NewBooleanArray)	   (JNIEnv *, jsize);
  jbyteArray    (*NewByteArray)		   (JNIEnv *, jsize);
  jcharArray    (*NewCharArray)		   (JNIEnv *, jsize);
  jshortArray   (*NewShortArray)	   (JNIEnv *, jsize);
  jintArray     (*NewIntArray)		   (JNIEnv *, jsize);
  jlongArray    (*NewLongArray)		   (JNIEnv *, jsize);
  jfloatArray   (*NewFloatArray)	   (JNIEnv *, jsize);
  jdoubleArray  (*NewDoubleArray)	   (JNIEnv *, jsize);

  jboolean *	(*GetBooleanArrayElements) (JNIEnv *, jbooleanArray,
					    jboolean *);
  jbyte *	(*GetByteArrayElements)	   (JNIEnv *, jbyteArray,
					    jboolean *);
  jchar *	(*GetCharArrayElements)	   (JNIEnv *, jcharArray,
					    jboolean *);
  jshort *	(*GetShortArrayElements)   (JNIEnv *, jshortArray,
					    jboolean *);
  jint *	(*GetIntArrayElements)	   (JNIEnv *, jintArray,
					    jboolean *);
  jlong *	(*GetLongArrayElements)	   (JNIEnv *, jlongArray,
					    jboolean *);
  jfloat *	(*GetFloatArrayElements)   (JNIEnv *, jfloatArray,
					    jboolean *);
  jdouble *	(*GetDoubleArrayElements)  (JNIEnv *, jdoubleArray,
					    jboolean *);

  void		(*ReleaseBooleanArrayElements) (JNIEnv *, jbooleanArray,
						jboolean *, jint);
  void		(*ReleaseByteArrayElements)    (JNIEnv *, jbyteArray,
					        jbyte *, jint);
  void		(*ReleaseCharArrayElements)    (JNIEnv *, jcharArray,
						jchar *, jint);
  void		(*ReleaseShortArrayElements)   (JNIEnv *, jshortArray,
						jshort *, jint);
  void		(*ReleaseIntArrayElements)     (JNIEnv *, jintArray,
						jint *, jint);
  void		(*ReleaseLongArrayElements)    (JNIEnv *, jlongArray,
						jlong *, jint);
  void		(*ReleaseFloatArrayElements)   (JNIEnv *, jfloatArray,
						jfloat *, jint);
  void		(*ReleaseDoubleArrayElements)  (JNIEnv *, jdoubleArray,
						jdouble *, jint);

  void 		(*GetBooleanArrayRegion)   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*GetByteArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*GetCharArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*GetShortArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*GetIntArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*GetLongArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*GetFloatArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*GetDoubleArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);

  void 		(*SetBooleanArrayRegion)   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*SetByteArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*SetCharArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*SetShortArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*SetIntArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*SetLongArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*SetFloatArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);
  void 		(*SetDoubleArrayRegion)	   (JNIEnv *, jbooleanArray,
					    jsize, jsize, jboolean *);

  jint     (*RegisterNatives)              (JNIEnv *, jclass,
					    const JNINativeMethod *, jint);
  jint     (*UnregisterNatives)            (JNIEnv *, jclass);
  jint     (*MonitorEnter)                 (JNIEnv *, jobject);
  jint     (*MonitorExit)                  (JNIEnv *, jobject);
  jint     (*GetJavaVM)                    (JNIEnv *, JavaVM **);

  void	   (*GetStringRegion)	           (JNIEnv *, jstring, jsize,
					    jsize, jchar *);
  void     (*GetStringUTFRegion)	   (JNIEnv *, jstring, jsize,
					    jsize, char *);

  void * (*GetPrimitiveArrayCritical)      (JNIEnv *, jarray, jboolean *);
  void   (*ReleasePrimitiveArrayCritical)  (JNIEnv *, jarray, void *, jint);

  const jchar * (*GetStringCritical)       (JNIEnv *, jstring, jboolean *);
  void          (*ReleaseStringCritical)   (JNIEnv *, jstring, const jchar *);

  jweak  (*NewWeakGlobalRef)               (JNIEnv *, jobject);
  void   (*DeleteWeakGlobalRef)            (JNIEnv *, jweak);

  jboolean	(*ExceptionCheck)	   (JNIEnv *);
};

#ifdef __cplusplus

class _Jv_JNIEnv
{
public:
  /* The method table.  */
  struct JNINativeInterface *p;

  /* FIXME: this is really ugly.  */
#ifndef __GCJ_JNI_IMPL__
private:
#endif
  /* The current exception.  */
  jthrowable ex;

  /* The class of the current native method.  */
  jclass klass;

  /* The chain of local frames.  */
  struct _Jv_JNI_LocalFrame *locals;

public:
  jint GetVersion ()
  { return p->GetVersion (this); }

  jclass DefineClass (jobject obj0, const jbyte * val1, jsize val2)
  { return p->DefineClass (this, obj0, val1, val2); }

  jclass FindClass (const char * val0)
  { return p->FindClass (this, val0); }

  jmethodID FromReflectedMethod (jobject obj0)
  { return p->FromReflectedMethod (this, obj0); }

  jfieldID FromReflectedField (jobject obj0)
  { return p->FromReflectedField (this, obj0); }

  jobject ToReflectedMethod (jclass cl0, jmethodID meth1, jboolean val2)
  { return p->ToReflectedMethod (this, cl0, meth1, val2); }

  jclass GetSuperclass (jclass cl0)
  { return p->GetSuperclass (this, cl0); }

  jboolean IsAssignableFrom (jclass cl0, jclass cl1)
  { return p->IsAssignableFrom (this, cl0, cl1); }

  jobject ToReflectedField (jclass cl0, jfieldID fld1, jboolean val2)
  { return p->ToReflectedField (this, cl0, fld1, val2); }

  jint Throw (jthrowable val0)
  { return p->Throw (this, val0); }

  jint ThrowNew (jclass cl0, const char * val1)
  { return p->ThrowNew (this, cl0, val1); }

  jthrowable ExceptionOccurred ()
  { return p->ExceptionOccurred (this); }

  void ExceptionDescribe ()
  { p->ExceptionDescribe (this); }

  void ExceptionClear ()
  { p->ExceptionClear (this); }

  void FatalError (const char * val0)
  { p->FatalError (this, val0); }

  jint PushLocalFrame (jint val0)
  { return p->PushLocalFrame (this, val0); }

  jobject PopLocalFrame (jobject obj0)
  { return p->PopLocalFrame (this, obj0); }

  jobject NewGlobalRef (jobject obj0)
  { return p->NewGlobalRef (this, obj0); }

  void DeleteGlobalRef (jobject obj0)
  { p->DeleteGlobalRef (this, obj0); }

  void DeleteLocalRef (jobject obj0)
  { p->DeleteLocalRef (this, obj0); }

  jboolean IsSameObject (jobject obj0, jobject obj1)
  { return p->IsSameObject (this, obj0, obj1); }

  jobject NewLocalRef (jobject obj0)
  { return p->NewLocalRef (this, obj0); }

  jint EnsureLocalCapacity (jint val0)
  { return p->EnsureLocalCapacity (this, val0); }

  jobject AllocObject (jclass cl0)
  { return p->AllocObject (this, cl0); }

  jobject NewObject (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jobject result = p->NewObjectV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jobject NewObjectV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->NewObjectV (this, cl0, meth1, val2); }

  jobject NewObjectA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->NewObjectA (this, cl0, meth1, val2); }

  jclass GetObjectClass (jobject obj0)
  { return p->GetObjectClass (this, obj0); }

  jboolean IsInstanceOf (jobject obj0, jclass cl1)
  { return p->IsInstanceOf (this, obj0, cl1); }

  jmethodID GetMethodID (jclass cl0, const char * val1, const char * val2)
  { return p->GetMethodID (this, cl0, val1, val2); }

  jobject CallObjectMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jobject result = p->CallObjectMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jobject CallObjectMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallObjectMethodV (this, obj0, meth1, val2); }

  jobject CallObjectMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallObjectMethodA (this, obj0, meth1, val2); }

  jboolean CallBooleanMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jboolean result = p->CallBooleanMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jboolean CallBooleanMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallBooleanMethodV (this, obj0, meth1, val2); }

  jboolean CallBooleanMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallBooleanMethodA (this, obj0, meth1, val2); }

  jbyte CallByteMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jbyte result = p->CallByteMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jbyte CallByteMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallByteMethodV (this, obj0, meth1, val2); }

  jbyte CallByteMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallByteMethodA (this, obj0, meth1, val2); }

  jchar CallCharMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jchar result = p->CallCharMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jchar CallCharMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallCharMethodV (this, obj0, meth1, val2); }

  jchar CallCharMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallCharMethodA (this, obj0, meth1, val2); }

  jshort CallShortMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jshort result = p->CallShortMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jshort CallShortMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallShortMethodV (this, obj0, meth1, val2); }

  jshort CallShortMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallShortMethodA (this, obj0, meth1, val2); }

  jint CallIntMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jint result = p->CallIntMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jint CallIntMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallIntMethodV (this, obj0, meth1, val2); }

  jint CallIntMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallIntMethodA (this, obj0, meth1, val2); }

  jlong CallLongMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jlong result = p->CallLongMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jlong CallLongMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallLongMethodV (this, obj0, meth1, val2); }

  jlong CallLongMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallLongMethodA (this, obj0, meth1, val2); }

  jfloat CallFloatMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jfloat result = p->CallFloatMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jfloat CallFloatMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallFloatMethodV (this, obj0, meth1, val2); }

  jfloat CallFloatMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallFloatMethodA (this, obj0, meth1, val2); }

  jdouble CallDoubleMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jdouble result = p->CallDoubleMethodV (this, obj0, meth1, args);
    va_end (args);
    return result;
  }

  jdouble CallDoubleMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallDoubleMethodV (this, obj0, meth1, val2); }

  jdouble CallDoubleMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { return p->CallDoubleMethodA (this, obj0, meth1, val2); }

  void CallVoidMethod (jobject obj0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    p->CallVoidMethodV (this, obj0, meth1, args);
    va_end (args);
  }

  void CallVoidMethodV (jobject obj0, jmethodID meth1, _Jv_va_list val2)
  { p->CallVoidMethodV (this, obj0, meth1, val2); }

  void CallVoidMethodA (jobject obj0, jmethodID meth1, jvalue * val2)
  { p->CallVoidMethodA (this, obj0, meth1, val2); }

  jobject CallNonvirtualObjectMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jobject result = p->CallNonvirtualObjectMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jobject CallNonvirtualObjectMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualObjectMethodV (this, obj0, cl1, meth2, val3); }

  jobject CallNonvirtualObjectMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualObjectMethodA (this, obj0, cl1, meth2, val3); }

  jboolean CallNonvirtualBooleanMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jboolean result = p->CallNonvirtualBooleanMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jboolean CallNonvirtualBooleanMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualBooleanMethodV (this, obj0, cl1, meth2, val3); }

  jboolean CallNonvirtualBooleanMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualBooleanMethodA (this, obj0, cl1, meth2, val3); }

  jbyte CallNonvirtualByteMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jbyte result = p->CallNonvirtualByteMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jbyte CallNonvirtualByteMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualByteMethodV (this, obj0, cl1, meth2, val3); }

  jbyte CallNonvirtualByteMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualByteMethodA (this, obj0, cl1, meth2, val3); }

  jchar CallNonvirtualCharMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jchar result = p->CallNonvirtualCharMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jchar CallNonvirtualCharMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualCharMethodV (this, obj0, cl1, meth2, val3); }

  jchar CallNonvirtualCharMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualCharMethodA (this, obj0, cl1, meth2, val3); }

  jshort CallNonvirtualShortMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jshort result = p->CallNonvirtualShortMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jshort CallNonvirtualShortMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualShortMethodV (this, obj0, cl1, meth2, val3); }

  jshort CallNonvirtualShortMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualShortMethodA (this, obj0, cl1, meth2, val3); }

  jint CallNonvirtualIntMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jint result = p->CallNonvirtualIntMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jint CallNonvirtualIntMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualIntMethodV (this, obj0, cl1, meth2, val3); }

  jint CallNonvirtualIntMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualIntMethodA (this, obj0, cl1, meth2, val3); }

  jlong CallNonvirtualLongMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jlong result = p->CallNonvirtualLongMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jlong CallNonvirtualLongMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualLongMethodV (this, obj0, cl1, meth2, val3); }

  jlong CallNonvirtualLongMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualLongMethodA (this, obj0, cl1, meth2, val3); }

  jfloat CallNonvirtualFloatMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jfloat result = p->CallNonvirtualFloatMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jfloat CallNonvirtualFloatMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualFloatMethodV (this, obj0, cl1, meth2, val3); }

  jfloat CallNonvirtualFloatMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualFloatMethodA (this, obj0, cl1, meth2, val3); }

  jdouble CallNonvirtualDoubleMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    jdouble result = p->CallNonvirtualDoubleMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
    return result;
  }

  jdouble CallNonvirtualDoubleMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { return p->CallNonvirtualDoubleMethodV (this, obj0, cl1, meth2, val3); }

  jdouble CallNonvirtualDoubleMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { return p->CallNonvirtualDoubleMethodA (this, obj0, cl1, meth2, val3); }

  void CallNonvirtualVoidMethod (jobject obj0, jclass cl1, jmethodID meth2, ...)
  {
    _Jv_va_list args;
    va_start (args, meth2);
    p->CallNonvirtualVoidMethodV (this, obj0, cl1, meth2, args);
    va_end (args);
  }

  void CallNonvirtualVoidMethodV (jobject obj0, jclass cl1, jmethodID meth2, _Jv_va_list val3)
  { p->CallNonvirtualVoidMethodV (this, obj0, cl1, meth2, val3); }

  void CallNonvirtualVoidMethodA (jobject obj0, jclass cl1, jmethodID meth2, jvalue * val3)
  { p->CallNonvirtualVoidMethodA (this, obj0, cl1, meth2, val3); }

  jfieldID GetFieldID (jclass cl0, const char * val1, const char * val2)
  { return p->GetFieldID (this, cl0, val1, val2); }

  jobject GetObjectField (jobject obj0, jfieldID fld1)
  { return p->GetObjectField (this, obj0, fld1); }

  jboolean GetBooleanField (jobject obj0, jfieldID fld1)
  { return p->GetBooleanField (this, obj0, fld1); }

  jbyte GetByteField (jobject obj0, jfieldID fld1)
  { return p->GetByteField (this, obj0, fld1); }

  jchar GetCharField (jobject obj0, jfieldID fld1)
  { return p->GetCharField (this, obj0, fld1); }

  jshort GetShortField (jobject obj0, jfieldID fld1)
  { return p->GetShortField (this, obj0, fld1); }

  jint GetIntField (jobject obj0, jfieldID fld1)
  { return p->GetIntField (this, obj0, fld1); }

  jlong GetLongField (jobject obj0, jfieldID fld1)
  { return p->GetLongField (this, obj0, fld1); }

  jfloat GetFloatField (jobject obj0, jfieldID fld1)
  { return p->GetFloatField (this, obj0, fld1); }

  jdouble GetDoubleField (jobject obj0, jfieldID fld1)
  { return p->GetDoubleField (this, obj0, fld1); }

  void SetObjectField (jobject obj0, jfieldID fld1, jobject obj2)
  { p->SetObjectField (this, obj0, fld1, obj2); }

  void SetBooleanField (jobject obj0, jfieldID fld1, jboolean val2)
  { p->SetBooleanField (this, obj0, fld1, val2); }

  void SetByteField (jobject obj0, jfieldID fld1, jbyte val2)
  { p->SetByteField (this, obj0, fld1, val2); }

  void SetCharField (jobject obj0, jfieldID fld1, jchar val2)
  { p->SetCharField (this, obj0, fld1, val2); }

  void SetShortField (jobject obj0, jfieldID fld1, jshort val2)
  { p->SetShortField (this, obj0, fld1, val2); }

  void SetIntField (jobject obj0, jfieldID fld1, jint val2)
  { p->SetIntField (this, obj0, fld1, val2); }

  void SetLongField (jobject obj0, jfieldID fld1, jlong val2)
  { p->SetLongField (this, obj0, fld1, val2); }

  void SetFloatField (jobject obj0, jfieldID fld1, jfloat val2)
  { p->SetFloatField (this, obj0, fld1, val2); }

  void SetDoubleField (jobject obj0, jfieldID fld1, jdouble val2)
  { p->SetDoubleField (this, obj0, fld1, val2); }

  jmethodID GetStaticMethodID (jclass cl0, const char * val1, const char * val2)
  { return p->GetStaticMethodID (this, cl0, val1, val2); }

  jobject CallStaticObjectMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jobject result = p->CallStaticObjectMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jobject CallStaticObjectMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticObjectMethodV (this, cl0, meth1, val2); }

  jobject CallStaticObjectMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticObjectMethodA (this, cl0, meth1, val2); }

  jboolean CallStaticBooleanMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jboolean result = p->CallStaticBooleanMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jboolean CallStaticBooleanMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticBooleanMethodV (this, cl0, meth1, val2); }

  jboolean CallStaticBooleanMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticBooleanMethodA (this, cl0, meth1, val2); }

  jbyte CallStaticByteMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jbyte result = p->CallStaticByteMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jbyte CallStaticByteMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticByteMethodV (this, cl0, meth1, val2); }

  jbyte CallStaticByteMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticByteMethodA (this, cl0, meth1, val2); }

  jchar CallStaticCharMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jchar result = p->CallStaticCharMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jchar CallStaticCharMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticCharMethodV (this, cl0, meth1, val2); }

  jchar CallStaticCharMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticCharMethodA (this, cl0, meth1, val2); }

  jshort CallStaticShortMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jshort result = p->CallStaticShortMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jshort CallStaticShortMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticShortMethodV (this, cl0, meth1, val2); }

  jshort CallStaticShortMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticShortMethodA (this, cl0, meth1, val2); }

  jint CallStaticIntMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jint result = p->CallStaticIntMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jint CallStaticIntMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticIntMethodV (this, cl0, meth1, val2); }

  jint CallStaticIntMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticIntMethodA (this, cl0, meth1, val2); }

  jlong CallStaticLongMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jlong result = p->CallStaticLongMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jlong CallStaticLongMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticLongMethodV (this, cl0, meth1, val2); }

  jlong CallStaticLongMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticLongMethodA (this, cl0, meth1, val2); }

  jfloat CallStaticFloatMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jfloat result = p->CallStaticFloatMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jfloat CallStaticFloatMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticFloatMethodV (this, cl0, meth1, val2); }

  jfloat CallStaticFloatMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticFloatMethodA (this, cl0, meth1, val2); }

  jdouble CallStaticDoubleMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    jdouble result = p->CallStaticDoubleMethodV (this, cl0, meth1, args);
    va_end (args);
    return result;
  }

  jdouble CallStaticDoubleMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { return p->CallStaticDoubleMethodV (this, cl0, meth1, val2); }

  jdouble CallStaticDoubleMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { return p->CallStaticDoubleMethodA (this, cl0, meth1, val2); }

  void CallStaticVoidMethod (jclass cl0, jmethodID meth1, ...)
  {
    _Jv_va_list args;
    va_start (args, meth1);
    p->CallStaticVoidMethodV (this, cl0, meth1, args);
    va_end (args);
  }

  void CallStaticVoidMethodV (jclass cl0, jmethodID meth1, _Jv_va_list val2)
  { p->CallStaticVoidMethodV (this, cl0, meth1, val2); }

  void CallStaticVoidMethodA (jclass cl0, jmethodID meth1, jvalue * val2)
  { p->CallStaticVoidMethodA (this, cl0, meth1, val2); }

  jfieldID GetStaticFieldID (jclass cl0, const char * val1, const char * val2)
  { return p->GetStaticFieldID (this, cl0, val1, val2); }

  jobject GetStaticObjectField (jclass cl0, jfieldID fld1)
  { return p->GetStaticObjectField (this, cl0, fld1); }

  jboolean GetStaticBooleanField (jclass cl0, jfieldID fld1)
  { return p->GetStaticBooleanField (this, cl0, fld1); }

  jbyte GetStaticByteField (jclass cl0, jfieldID fld1)
  { return p->GetStaticByteField (this, cl0, fld1); }

  jchar GetStaticCharField (jclass cl0, jfieldID fld1)
  { return p->GetStaticCharField (this, cl0, fld1); }

  jshort GetStaticShortField (jclass cl0, jfieldID fld1)
  { return p->GetStaticShortField (this, cl0, fld1); }

  jint GetStaticIntField (jclass cl0, jfieldID fld1)
  { return p->GetStaticIntField (this, cl0, fld1); }

  jlong GetStaticLongField (jclass cl0, jfieldID fld1)
  { return p->GetStaticLongField (this, cl0, fld1); }

  jfloat GetStaticFloatField (jclass cl0, jfieldID fld1)
  { return p->GetStaticFloatField (this, cl0, fld1); }

  jdouble GetStaticDoubleField (jclass cl0, jfieldID fld1)
  { return p->GetStaticDoubleField (this, cl0, fld1); }

  void SetStaticObjectField (jclass cl0, jfieldID fld1, jobject obj2)
  { p->SetStaticObjectField (this, cl0, fld1, obj2); }

  void SetStaticBooleanField (jclass cl0, jfieldID fld1, jboolean val2)
  { p->SetStaticBooleanField (this, cl0, fld1, val2); }

  void SetStaticByteField (jclass cl0, jfieldID fld1, jbyte val2)
  { p->SetStaticByteField (this, cl0, fld1, val2); }

  void SetStaticCharField (jclass cl0, jfieldID fld1, jchar val2)
  { p->SetStaticCharField (this, cl0, fld1, val2); }

  void SetStaticShortField (jclass cl0, jfieldID fld1, jshort val2)
  { p->SetStaticShortField (this, cl0, fld1, val2); }

  void SetStaticIntField (jclass cl0, jfieldID fld1, jint val2)
  { p->SetStaticIntField (this, cl0, fld1, val2); }

  void SetStaticLongField (jclass cl0, jfieldID fld1, jlong val2)
  { p->SetStaticLongField (this, cl0, fld1, val2); }

  void SetStaticFloatField (jclass cl0, jfieldID fld1, jfloat val2)
  { p->SetStaticFloatField (this, cl0, fld1, val2); }

  void SetStaticDoubleField (jclass cl0, jfieldID fld1, jdouble val2)
  { p->SetStaticDoubleField (this, cl0, fld1, val2); }

  jstring NewString (const jchar * val0, jsize val1)
  { return p->NewString (this, val0, val1); }

  jint GetStringLength (jstring val0)
  { return p->GetStringLength (this, val0); }

  const jchar * GetStringChars (jstring val0, jboolean * val1)
  { return p->GetStringChars (this, val0, val1); }

  void ReleaseStringChars (jstring val0, const jchar * val1)
  { p->ReleaseStringChars (this, val0, val1); }

  jstring NewStringUTF (const char * val0)
  { return p->NewStringUTF (this, val0); }

  jsize GetStringUTFLength (jstring val0)
  { return p->GetStringUTFLength (this, val0); }

  const char * GetStringUTFChars (jstring val0, jboolean * val1)
  { return p->GetStringUTFChars (this, val0, val1); }

  void ReleaseStringUTFChars (jstring val0, const char * val1)
  { p->ReleaseStringUTFChars (this, val0, val1); }

  jsize GetArrayLength (jarray val0)
  { return p->GetArrayLength (this, val0); }

  jarray NewObjectArray (jsize val0, jclass cl1, jobject obj2)
  { return p->NewObjectArray (this, val0, cl1, obj2); }

  jobject GetObjectArrayElement (jobjectArray val0, jsize val1)
  { return p->GetObjectArrayElement (this, val0, val1); }

  void SetObjectArrayElement (jobjectArray val0, jsize val1, jobject obj2)
  { p->SetObjectArrayElement (this, val0, val1, obj2); }

  jbooleanArray NewBooleanArray (jsize val0)
  { return p->NewBooleanArray (this, val0); }

  jbyteArray NewByteArray (jsize val0)
  { return p->NewByteArray (this, val0); }

  jcharArray NewCharArray (jsize val0)
  { return p->NewCharArray (this, val0); }

  jshortArray NewShortArray (jsize val0)
  { return p->NewShortArray (this, val0); }

  jintArray NewIntArray (jsize val0)
  { return p->NewIntArray (this, val0); }

  jlongArray NewLongArray (jsize val0)
  { return p->NewLongArray (this, val0); }

  jfloatArray NewFloatArray (jsize val0)
  { return p->NewFloatArray (this, val0); }

  jdoubleArray NewDoubleArray (jsize val0)
  { return p->NewDoubleArray (this, val0); }

  jboolean * GetBooleanArrayElements (jbooleanArray val0, jboolean * val1)
  { return p->GetBooleanArrayElements (this, val0, val1); }

  jbyte * GetByteArrayElements (jbyteArray val0, jboolean * val1)
  { return p->GetByteArrayElements (this, val0, val1); }

  jchar * GetCharArrayElements (jcharArray val0, jboolean * val1)
  { return p->GetCharArrayElements (this, val0, val1); }

  jshort * GetShortArrayElements (jshortArray val0, jboolean * val1)
  { return p->GetShortArrayElements (this, val0, val1); }

  jint * GetIntArrayElements (jintArray val0, jboolean * val1)
  { return p->GetIntArrayElements (this, val0, val1); }

  jlong * GetLongArrayElements (jlongArray val0, jboolean * val1)
  { return p->GetLongArrayElements (this, val0, val1); }

  jfloat * GetFloatArrayElements (jfloatArray val0, jboolean * val1)
  { return p->GetFloatArrayElements (this, val0, val1); }

  jdouble * GetDoubleArrayElements (jdoubleArray val0, jboolean * val1)
  { return p->GetDoubleArrayElements (this, val0, val1); }

  void ReleaseBooleanArrayElements (jbooleanArray val0, jboolean * val1, jint val2)
  { p->ReleaseBooleanArrayElements (this, val0, val1, val2); }

  void ReleaseByteArrayElements (jbyteArray val0, jbyte * val1, jint val2)
  { p->ReleaseByteArrayElements (this, val0, val1, val2); }

  void ReleaseCharArrayElements (jcharArray val0, jchar * val1, jint val2)
  { p->ReleaseCharArrayElements (this, val0, val1, val2); }

  void ReleaseShortArrayElements (jshortArray val0, jshort * val1, jint val2)
  { p->ReleaseShortArrayElements (this, val0, val1, val2); }

  void ReleaseIntArrayElements (jintArray val0, jint * val1, jint val2)
  { p->ReleaseIntArrayElements (this, val0, val1, val2); }

  void ReleaseLongArrayElements (jlongArray val0, jlong * val1, jint val2)
  { p->ReleaseLongArrayElements (this, val0, val1, val2); }

  void ReleaseFloatArrayElements (jfloatArray val0, jfloat * val1, jint val2)
  { p->ReleaseFloatArrayElements (this, val0, val1, val2); }

  void ReleaseDoubleArrayElements (jdoubleArray val0, jdouble * val1, jint val2)
  { p->ReleaseDoubleArrayElements (this, val0, val1, val2); }

  void GetBooleanArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetBooleanArrayRegion (this, val0, val1, val2, val3); }

  void GetByteArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetByteArrayRegion (this, val0, val1, val2, val3); }

  void GetCharArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetCharArrayRegion (this, val0, val1, val2, val3); }

  void GetShortArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetShortArrayRegion (this, val0, val1, val2, val3); }

  void GetIntArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetIntArrayRegion (this, val0, val1, val2, val3); }

  void GetLongArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetLongArrayRegion (this, val0, val1, val2, val3); }

  void GetFloatArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetFloatArrayRegion (this, val0, val1, val2, val3); }

  void GetDoubleArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->GetDoubleArrayRegion (this, val0, val1, val2, val3); }

  void SetBooleanArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetBooleanArrayRegion (this, val0, val1, val2, val3); }

  void SetByteArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetByteArrayRegion (this, val0, val1, val2, val3); }

  void SetCharArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetCharArrayRegion (this, val0, val1, val2, val3); }

  void SetShortArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetShortArrayRegion (this, val0, val1, val2, val3); }

  void SetIntArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetIntArrayRegion (this, val0, val1, val2, val3); }

  void SetLongArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetLongArrayRegion (this, val0, val1, val2, val3); }

  void SetFloatArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetFloatArrayRegion (this, val0, val1, val2, val3); }

  void SetDoubleArrayRegion (jbooleanArray val0, jsize val1, jsize val2, jboolean * val3)
  { p->SetDoubleArrayRegion (this, val0, val1, val2, val3); }

  jint RegisterNatives (jclass cl0, const JNINativeMethod * val1, jint val2)
  { return p->RegisterNatives (this, cl0, val1, val2); }

  jint UnregisterNatives (jclass cl0)
  { return p->UnregisterNatives (this, cl0); }

  jint MonitorEnter (jobject obj0)
  { return p->MonitorEnter (this, obj0); }

  jint MonitorExit (jobject obj0)
  { return p->MonitorExit (this, obj0); }

  jint GetJavaVM (JavaVM ** val0)
  { return p->GetJavaVM (this, val0); }

  void GetStringRegion (jstring val0, jsize val1, jsize val2, jchar * val3)
  { p->GetStringRegion (this, val0, val1, val2, val3); }

  void GetStringUTFRegion (jstring val0, jsize val1, jsize val2, char * val3)
  { p->GetStringUTFRegion (this, val0, val1, val2, val3); }

  void * GetPrimitiveArrayCritical (jarray val0, jboolean * val1)
  { return p->GetPrimitiveArrayCritical (this, val0, val1); }

  void ReleasePrimitiveArrayCritical (jarray val0, void * val1, jint val2)
  { p->ReleasePrimitiveArrayCritical (this, val0, val1, val2); }

  const jchar * GetStringCritical (jstring val0, jboolean * val1)
  { return p->GetStringCritical (this, val0, val1); }

  void ReleaseStringCritical (jstring val0, const jchar * val1)
  { p->ReleaseStringCritical (this, val0, val1); }

  jweak NewWeakGlobalRef (jobject obj0)
  { return p->NewWeakGlobalRef (this, obj0); }

  void DeleteWeakGlobalRef (jweak val0)
  { p->DeleteWeakGlobalRef (this, val0); }

  jboolean ExceptionCheck ()
  { return p->ExceptionCheck (this); }
};
#endif /* __cplusplus */

/*
 * Invocation API.
 */

struct JNIInvokeInterface
{
  _Jv_func reserved0;
  _Jv_func reserved1;
  _Jv_func reserved2;

  jint (*DestroyJavaVM)         (JavaVM *);
  jint (*AttachCurrentThread)   (JavaVM *, void **, void *);
  jint (*DetachCurrentThread)   (JavaVM *);
  jint (*GetEnv)                (JavaVM *, void **, jint);
};

#ifdef __cplusplus

class _Jv_JavaVM
{
public:
  const struct JNIInvokeInterface *functions;

private:
  /* FIXME: other fields.  */

public:
  jint DestroyJavaVM ()
  { return functions->DestroyJavaVM (this); }

  jint AttachCurrentThread (void **penv, void *args)
  { return functions->AttachCurrentThread (this, penv, args); }

  jint DetachCurrentThread ()
  { return functions->DetachCurrentThread (this); }

  jint GetEnv (void **penv, jint version)
  { return functions->GetEnv (this, penv, version); }
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

#endif /* __GCJ_JNI_H__ */
