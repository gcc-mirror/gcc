/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_JNI_H__
#define __GCJ_JNI_H__

#include <stdarg.h>
#define _Jv_va_list va_list

#ifdef __cplusplus

// This is wrong, because it pollutes the name-space too much!
#include <gcj/javaprims.h>

typedef struct _Jv_JNIEnv JNIEnv;
#else

typedef int    jbyte  __attribute__((__mode__(__QI__)));
typedef int    jshort __attribute__((__mode__(__HI__)));
typedef int    jint   __attribute__((__mode__(__SI__)));
typedef int    jlong  __attribute__((__mode__(__DI__)));
typedef bool   jboolean __attribute__((__mode__(__QI__)));
typedef unsigned short jchar __attribute__((__mode__(__HI__)));
typedef float  jfloat;
typedef double jdouble;
typedef jint jsize;

typedef const struct JNINativeInterface *JNIEnv;
#endif

typedef union jvalue
{
  jboolean z;
  jbyte    b;
  jchar    c;
  jshort  s;
  jint    i;
  jlong    j;
  jfloat  f;
  jdouble  d;
  jobject l;
} jvalue;

typedef void * (*_Jv_func)(...);

struct JNINativeInterface
{
  _Jv_func reserved0;
  _Jv_func reserved1;
  _Jv_func reserved2;
  _Jv_func reserved3;
  _Jv_func GetVersion;
  _Jv_func DefineClass;
  _Jv_func FindClass;
  _Jv_func reserved4;
  _Jv_func reserved5;
  _Jv_func reserved6;
  jclass   (*GetSuperclass)                (JNIEnv*, jclass);
  jboolean (*IsAssignableFrom)             (JNIEnv*, jclass, jclass);
  _Jv_func reserved7;
  jint     (*Throw)                        (JNIEnv*, jthrowable);
  _Jv_func ThrowNew;
  _Jv_func ExceptionOccurred;
  _Jv_func ExceptionDescribe;
  _Jv_func ExceptionClear;
  _Jv_func FatalError;
  _Jv_func reserved8;
  _Jv_func reserved9;
  _Jv_func NewGlobalRef;
  _Jv_func DeleteGlobalRef;
  _Jv_func DeleteLocalRef;
  _Jv_func IsSameObject;
  _Jv_func reserved10;
  _Jv_func reserved11;
  _Jv_func AllocObject;
  _Jv_func NewObject;
  _Jv_func NewObjectV;
  _Jv_func NewObjectA;
  _Jv_func GetObjectClass;
  _Jv_func IsInstanceOf;
  _Jv_func GetMethodID;
  _Jv_func CallObjectMethod;
  _Jv_func CallObjectMethodV;
  _Jv_func CallObjectMethodA;
  _Jv_func CallBooleanMethod;
  _Jv_func CallBooleanMethodV;
  _Jv_func CallBooleanMethodA;
  _Jv_func CallByteMethod;
  _Jv_func CallByteMethodV;
  _Jv_func CallByteMethodA;
  _Jv_func CallCharMethod;
  _Jv_func CallCharMethodV;
  _Jv_func CallCharMethodA;
  _Jv_func CallShortMethod;
  _Jv_func CallShortMethodV;
  _Jv_func CallShortMethodA;
  _Jv_func CallIntMethod;
  _Jv_func CallIntMethodV;
  _Jv_func CallIntMethodA;
  _Jv_func CallLongMethod;
  _Jv_func CallLongMethodV;
  _Jv_func CallLongMethodA;
  _Jv_func CallFloatMethod;
  _Jv_func CallFloatMethodV;
  _Jv_func CallFloatMethodA;
  _Jv_func CallDoubleMethod;
  _Jv_func CallDoubleMethodV;
  _Jv_func CallDoubleMethodA;
  _Jv_func CallVoidMethod;
  _Jv_func CallVoidMethodV;
  _Jv_func CallVoidMethodA;
  _Jv_func CallNonvirtualObjectMethod;
  _Jv_func CallNonvirtualObjectMethodV;
  _Jv_func CallNonvirtualObjectMethodA;
  jboolean (*CallNonvirtualBooleanMethod)  (JNIEnv*, jobject, jclass, jmethodID, ...);
  jboolean (*CallNonvirtualBooleanMethodV) (JNIEnv*, jobject, jclass, jmethodID, _Jv_va_list);
  jboolean (*CallNonvirtualBooleanMethodA) (JNIEnv*, jobject, jclass, jmethodID, jvalue*);
  _Jv_func CallNonvirtualByteMethod;
  _Jv_func CallNonvirtualByteMethodV;
  _Jv_func CallNonvirtualByteMethodA;
  _Jv_func CallNonvirtualCharMethod;
  _Jv_func CallNonvirtualCharMethodV;
  _Jv_func CallNonvirtualCharMethodA;
  _Jv_func CallNonvirtualShortMethod;
  _Jv_func CallNonvirtualShortMethodV;
  _Jv_func CallNonvirtualShortMethodA;
  _Jv_func CallNonvirtualIntMethod;
  _Jv_func CallNonvirtualIntMethodV;
  _Jv_func CallNonvirtualIntMethodA;
  _Jv_func CallNonvirtualLongMethod;
  _Jv_func CallNonvirtualLongMethodV;
  _Jv_func CallNonvirtualLongMethodA;
  _Jv_func CallNonvirtualFloatMethod;
  _Jv_func CallNonvirtualFloatMethodV;
  _Jv_func CallNonvirtualFloatMethodA;
  _Jv_func CallNonvirtualDoubleMethod;
  jdouble  (*CallNonvirtualDoubleMethodV)  (JNIEnv*, jobject, jclass, jmethodID, _Jv_va_list);
  _Jv_func CallNonvirtualDoubleMethodA;
  _Jv_func CallNonvirtualVoidMethod;
  _Jv_func CallNonvirtualVoidMethodV;
  _Jv_func CallNonvirtualVoidMethodA;
  _Jv_func GetFieldID;
  jobject  (*GetObjectField)               (JNIEnv*, jobject, jfieldID);
  jboolean (*GetBooleanField)              (JNIEnv*, jobject, jfieldID);
  jbyte    (*GetByteField)                 (JNIEnv*, jobject, jfieldID);
  jchar    (*GetCharField)                 (JNIEnv*, jobject, jfieldID);
  jshort   (*GetShortField)                (JNIEnv*, jobject, jfieldID);
  jint     (*GetIntField)                  (JNIEnv*, jobject, jfieldID);
  jlong    (*GetLongField)                 (JNIEnv*, jobject, jfieldID);
  jfloat   (*GetFloatField)                (JNIEnv*, jobject, jfieldID);
  jdouble  (*GetDoubleField)               (JNIEnv*, jobject, jfieldID);
  _Jv_func SetObjectField;
  _Jv_func SetBooleanField;
  _Jv_func SetByteField;
  _Jv_func SetCharField;
  _Jv_func SetShortField;
  _Jv_func SetIntField;
  _Jv_func SetLongField;
  _Jv_func SetFloatField;
  _Jv_func SetDoubleField;
  _Jv_func GetStaticMethodID;
  _Jv_func CallStaticObjectMethod;
  _Jv_func CallStaticObjectMethodV;
  _Jv_func CallStaticObjectMethodA;
  _Jv_func CallStaticBooleanMethod;
  _Jv_func CallStaticBooleanMethodV;
  _Jv_func CallStaticBooleanMethodA;
  _Jv_func CallStaticByteMethod;
  _Jv_func CallStaticByteMethodV;
  _Jv_func CallStaticByteMethodA;
  _Jv_func CallStaticCharMethod;
  _Jv_func CallStaticCharMethodV;
  _Jv_func CallStaticCharMethodA;
  _Jv_func CallStaticShortMethod;
  _Jv_func CallStaticShortMethodV;
  _Jv_func CallStaticShortMethodA;
  _Jv_func CallStaticIntMethod;
  _Jv_func CallStaticIntMethodV;
  _Jv_func CallStaticIntMethodA;
  _Jv_func CallStaticLongMethod;
  _Jv_func CallStaticLongMethodV;
  _Jv_func CallStaticLongMethodA;
  _Jv_func CallStaticFloatMethod;
  _Jv_func CallStaticFloatMethodV;
  _Jv_func CallStaticFloatMethodA;
  _Jv_func CallStaticDoubleMethod;
  _Jv_func CallStaticDoubleMethodV;
  _Jv_func CallStaticDoubleMethodA;
  _Jv_func CallStaticVoidMethod;
  _Jv_func CallStaticVoidMethodV;
  _Jv_func CallStaticVoidMethodA;
  _Jv_func GetStaticFieldID;
  _Jv_func GetStaticObjectField;
  _Jv_func GetStaticBooleanField;
  _Jv_func GetStaticByteField;
  _Jv_func GetStaticCharField;
  _Jv_func GetStaticShortField;
  _Jv_func GetStaticIntField;
  _Jv_func GetStaticLongField;
  _Jv_func GetStaticFloatField;
  _Jv_func GetStaticDoubleField;
  _Jv_func SetStaticObjectField;
  _Jv_func SetStaticBooleanField;
  _Jv_func SetStaticByteField;
  _Jv_func SetStaticCharField;
  _Jv_func SetStaticShortField;
  _Jv_func SetStaticIntField;
  _Jv_func SetStaticLongField;
  _Jv_func SetStaticFloatField;
  _Jv_func SetStaticDoubleField;
  _Jv_func NewString;
  jint     (*GetStringLength)              (JNIEnv*, jstring);
  _Jv_func GetStringChars;
  _Jv_func ReleaseStringChars;
  _Jv_func NewStringUTF;
  _Jv_func GetStringUTFLength;
  _Jv_func GetStringUTFChars;
  _Jv_func ReleaseStringUTFChars;
  _Jv_func GetArrayLength;
  _Jv_func NewObjectArray;
  _Jv_func GetObjectArrayElement;
  _Jv_func SetObjectArrayElement;
  _Jv_func NewBooleanArray;
  _Jv_func NewByteArray;
  _Jv_func NewCharArray;
  _Jv_func NewShortArray;
  _Jv_func NewIntArray;
  _Jv_func NewLongArray;
  _Jv_func NewFloatArray;
  _Jv_func NewDoubleArray;
  _Jv_func GetBooleanArrayElements;
  _Jv_func GetByteArrayElements;
  _Jv_func GetCharArrayElements;
  _Jv_func GetShortArrayElements;
  _Jv_func GetIntArrayElements;
  _Jv_func GetLongArrayElements;
  _Jv_func GetFloatArrayElements;
  _Jv_func GetDoubleArrayElements;
  _Jv_func ReleaseBooleanArrayElements;
  _Jv_func ReleaseByteArrayElements;
  _Jv_func ReleaseCharArrayElements;
  _Jv_func ReleaseShortArrayElements;
  _Jv_func ReleaseIntArrayElements;
  _Jv_func ReleaseLongArrayElements;
  _Jv_func ReleaseFloatArrayElements;
  _Jv_func ReleaseDoubleArrayElements;
  _Jv_func GetBooleanArrayRegion;
  _Jv_func GetByteArrayRegion;
  _Jv_func GetCharArrayRegion;
  _Jv_func GetShortArrayRegion;
  _Jv_func GetIntArrayRegion;
  _Jv_func GetLongArrayRegion;
  _Jv_func GetFloatArrayRegion;
  _Jv_func GetDoubleArrayRegion;
  _Jv_func SetBooleanArrayRegion;
  _Jv_func SetByteArrayRegion;
  _Jv_func SetCharArrayRegion;
  _Jv_func SetShortArrayRegion;
  _Jv_func SetIntArrayRegion;
  _Jv_func SetLongArrayRegion;
  _Jv_func SetFloatArrayRegion;
  _Jv_func SetDoubleArrayRegion;
  _Jv_func RegisterNatives;
  _Jv_func UnregisterNatives;
  _Jv_func MonitorEnter;
  _Jv_func MonitorExit;
  _Jv_func GetJavaVM;
};

#ifdef __cplusplus

struct _Jv_JNIEnv
{
  struct JNINativeInterface *p;

  jclass GetSuperclass (jclass cl);
  jsize GetStringLength (jstring str);

};

extern inline jclass
_Jv_JNIEnv::GetSuperclass (jclass cl)
{ return p->GetSuperclass (this, cl); }

extern inline jsize
_Jv_JNIEnv::GetStringLength (jstring str)
{ return p->GetStringLength (this, str); }

#endif

#endif /* __GCJ_JNI_H__ */
