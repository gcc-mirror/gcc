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

/* FIXME: this is just a placeholder.  */
typedef int JavaVM;

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
  jclass GetSuperclass (jclass cl)
  { return p->GetSuperclass (this, cl); }

  jsize GetStringLength (jstring str)
  { return p->GetStringLength (this, str); }
};

#endif /* __cplusplus */

#endif /* __GCJ_JNI_H__ */
