// jni.cc - JNI implementation, including the jump table.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stddef.h>

#include <jni.h>
#include <gcj/field.h>

static jclass
_Jv_JNI_GetSuperclass (JNIEnv *, jclass clazz)
{
  return clazz->getSuperclass ();
}

static jboolean
IsAssignableFrom(JNIEnv *, jclass clazz1, jclass clazz2)
{
  return clazz1->isAssignableFrom (clazz2);
}

static jobject
_Jv_JNI_GetObjectField (JNIEnv *, jobject obj, jfieldID field) 
{
  return _Jv_GetObjectField (obj, field);
}

static jbyte
_Jv_JNI_GetByteField (JNIEnv *, jobject obj, jfieldID field) 
{
  return _Jv_GetByteField (obj, field);
}

static jshort
_Jv_JNI_GetShortField (JNIEnv *, jobject obj, jfieldID field) 
{
  return _Jv_GetShortField (obj, field);
}

static jint
_Jv_JNI_GetIntField (JNIEnv *, jobject obj, jfieldID field) 
{
  return _Jv_GetIntField (obj, field);
}

static jlong
_Jv_JNI_GetLongField (JNIEnv *, jobject obj, jfieldID field) 
{
  return _Jv_GetLongField (obj, field);
}

static jsize
_Jv_JNI_GetStringLength (JNIEnv *, jstring string)
{
  return string->length();
}

// JDK 1.2
jobject
_Jv_JNI_ToReflectedField (JNIEnv *, jclass cls, jfieldID fieldID)
{
  java::lang::reflect::Field *field = new java::lang::reflect::Field();
  field->declaringClass = cls;
  field->offset = (char*) fieldID - (char *) cls->fields;
  field->name = _Jv_NewStringUtf8Const (fieldID->getNameUtf8Const (cls));
  return field;
}

// JDK 1.2
jfieldID
_Jv_JNI_FromReflectedField (JNIEnv *, java::lang::reflect::Field *field)
{
  return _Jv_FromReflectedField (field);
}

#define NOT_IMPL NULL

struct JNINativeInterface _Jv_JNIFunctions =
{
  NULL,
  NULL,
  NULL,
  NULL,
  NOT_IMPL /* GetVersion */,
  NOT_IMPL /* DefineClass */,
  NOT_IMPL /* FindClass */,
  NULL,
  NULL,
  NULL,
  _Jv_JNI_GetSuperclass,
  IsAssignableFrom,
  NULL,
  NOT_IMPL /* Throw */,
  NOT_IMPL /* ThrowNew */,
  NOT_IMPL /* ExceptionOccurred */,
  NOT_IMPL /* ExceptionDescribe */,
  NOT_IMPL /* ExceptionClear */,
  NOT_IMPL /* FatalError */,
  NOT_IMPL /* NULL */,
  NOT_IMPL /* NULL */,
  NOT_IMPL /* NewGlobalRef */,
  NOT_IMPL /* DeleteGlobalRef */,
  NOT_IMPL /* DeleteLocalRef */,
  NOT_IMPL /* IsSameObject */,
  NOT_IMPL /* NULL */,
  NOT_IMPL /* NULL */,
  NOT_IMPL /* AllocObject */,
  NOT_IMPL /* NewObject */,
  NOT_IMPL /* NewObjectV */,
  NOT_IMPL /* NewObjectA */,
  NOT_IMPL /* GetObjectClass */,
  NOT_IMPL /* IsInstanceOf */,
  NOT_IMPL /* GetMethodID */,
  NOT_IMPL /* CallObjectMethod */,
  NOT_IMPL /* CallObjectMethodV */,
  NOT_IMPL /* CallObjectMethodA */,
  NOT_IMPL /* CallBooleanMethod */,
  NOT_IMPL /* CallBooleanMethodV */,
  NOT_IMPL /* CallBooleanMethodA */,
  NOT_IMPL /* CallByteMethod */,
  NOT_IMPL /* CallByteMethodV */,
  NOT_IMPL /* CallByteMethodA */,
  NOT_IMPL /* CallCharMethod */,
  NOT_IMPL /* CallCharMethodV */,
  NOT_IMPL /* CallCharMethodA */,
  NOT_IMPL /* CallShortMethod */,
  NOT_IMPL /* CallShortMethodV */,
  NOT_IMPL /* CallShortMethodA */,
  NOT_IMPL /* CallIntMethod */,
  NOT_IMPL /* CallIntMethodV */,
  NOT_IMPL /* CallIntMethodA */,
  NOT_IMPL /* CallLongMethod */,
  NOT_IMPL /* CallLongMethodV */,
  NOT_IMPL /* CallLongMethodA */,
  NOT_IMPL /* CallFloatMethod */,
  NOT_IMPL /* CallFloatMethodV */,
  NOT_IMPL /* CallFloatMethodA */,
  NOT_IMPL /* CallDoubleMethod */,
  NOT_IMPL /* CallDoubleMethodV */,
  NOT_IMPL /* CallDoubleMethodA */,
  NOT_IMPL /* CallVoidMethod */,
  NOT_IMPL /* CallVoidMethodV */,
  NOT_IMPL /* CallVoidMethodA */,
  NOT_IMPL /* CallNonvirtualObjectMethod */,
  NOT_IMPL /* CallNonvirtualObjectMethodV */,
  NOT_IMPL /* CallNonvirtualObjectMethodA */,
  NOT_IMPL /* CallNonvirtualBooleanMethod */,
  NOT_IMPL /* CallNonvirtualBooleanMethodV */,
  NOT_IMPL /* CallNonvirtualBooleanMethodA */,
  NOT_IMPL /* CallNonvirtualByteMethod */,
  NOT_IMPL /* CallNonvirtualByteMethodV */,
  NOT_IMPL /* CallNonvirtualByteMethodA */,
  NOT_IMPL /* CallNonvirtualCharMethod */,
  NOT_IMPL /* CallNonvirtualCharMethodV */,
  NOT_IMPL /* CallNonvirtualCharMethodA */,
  NOT_IMPL /* CallNonvirtualShortMethod */,
  NOT_IMPL /* CallNonvirtualShortMethodV */,
  NOT_IMPL /* CallNonvirtualShortMethodA */,
  NOT_IMPL /* CallNonvirtualIntMethod */,
  NOT_IMPL /* CallNonvirtualIntMethodV */,
  NOT_IMPL /* CallNonvirtualIntMethodA */,
  NOT_IMPL /* CallNonvirtualLongMethod */,
  NOT_IMPL /* CallNonvirtualLongMethodV */,
  NOT_IMPL /* CallNonvirtualLongMethodA */,
  NOT_IMPL /* CallNonvirtualFloatMethod */,
  NOT_IMPL /* CallNonvirtualFloatMethodV */,
  NOT_IMPL /* CallNonvirtualFloatMethodA */,
  NOT_IMPL /* CallNonvirtualDoubleMethod */,
  NOT_IMPL /* CallNonvirtualDoubleMethodV */,
  NOT_IMPL /* CallNonvirtualDoubleMethodA */,
  NOT_IMPL /* CallNonvirtualVoidMethod */,
  NOT_IMPL /* CallNonvirtualVoidMethodV */,
  NOT_IMPL /* CallNonvirtualVoidMethodA */,
  NOT_IMPL /* GetFieldID */,
  _Jv_JNI_GetObjectField,
  NOT_IMPL /* GetBooleanField */,
  _Jv_JNI_GetByteField,
  NOT_IMPL /* GetCharField */,
  _Jv_JNI_GetShortField,
  _Jv_JNI_GetIntField,
  _Jv_JNI_GetLongField,
  NOT_IMPL /* GetFloatField */,
  NOT_IMPL /* GetDoubleField */,
  NOT_IMPL /* SetObjectField */,
  NOT_IMPL /* SetBooleanField */,
  NOT_IMPL /* SetByteField */,
  NOT_IMPL /* SetCharField */,
  NOT_IMPL /* SetShortField */,
  NOT_IMPL /* SetIntField */,
  NOT_IMPL /* SetLongField */,
  NOT_IMPL /* SetFloatField */,
  NOT_IMPL /* SetDoubleField */,
  NOT_IMPL /* GetStaticMethodID */,
  NOT_IMPL /* CallStaticObjectMethod */,
  NOT_IMPL /* CallStaticObjectMethodV */,
  NOT_IMPL /* CallStaticObjectMethodA */,
  NOT_IMPL /* CallStaticBooleanMethod */,
  NOT_IMPL /* CallStaticBooleanMethodV */,
  NOT_IMPL /* CallStaticBooleanMethodA */,
  NOT_IMPL /* CallStaticByteMethod */,
  NOT_IMPL /* CallStaticByteMethodV */,
  NOT_IMPL /* CallStaticByteMethodA */,
  NOT_IMPL /* CallStaticCharMethod */,
  NOT_IMPL /* CallStaticCharMethodV */,
  NOT_IMPL /* CallStaticCharMethodA */,
  NOT_IMPL /* CallStaticShortMethod */,
  NOT_IMPL /* CallStaticShortMethodV */,
  NOT_IMPL /* CallStaticShortMethodA */,
  NOT_IMPL /* CallStaticIntMethod */,
  NOT_IMPL /* CallStaticIntMethodV */,
  NOT_IMPL /* CallStaticIntMethodA */,
  NOT_IMPL /* CallStaticLongMethod */,
  NOT_IMPL /* CallStaticLongMethodV */,
  NOT_IMPL /* CallStaticLongMethodA */,
  NOT_IMPL /* CallStaticFloatMethod */,
  NOT_IMPL /* CallStaticFloatMethodV */,
  NOT_IMPL /* CallStaticFloatMethodA */,
  NOT_IMPL /* CallStaticDoubleMethod */,
  NOT_IMPL /* CallStaticDoubleMethodV */,
  NOT_IMPL /* CallStaticDoubleMethodA */,
  NOT_IMPL /* CallStaticVoidMethod */,
  NOT_IMPL /* CallStaticVoidMethodV */,
  NOT_IMPL /* CallStaticVoidMethodA */,
  NOT_IMPL /* GetStaticFieldID */,
  NOT_IMPL /* GetStaticObjectField */,
  NOT_IMPL /* GetStaticBooleanField */,
  NOT_IMPL /* GetStaticByteField */,
  NOT_IMPL /* GetStaticCharField */,
  NOT_IMPL /* GetStaticShortField */,
  NOT_IMPL /* GetStaticIntField */,
  NOT_IMPL /* GetStaticLongField */,
  NOT_IMPL /* GetStaticFloatField */,
  NOT_IMPL /* GetStaticDoubleField */,
  NOT_IMPL /* SetStaticObjectField */,
  NOT_IMPL /* SetStaticBooleanField */,
  NOT_IMPL /* SetStaticByteField */,
  NOT_IMPL /* SetStaticCharField */,
  NOT_IMPL /* SetStaticShortField */,
  NOT_IMPL /* SetStaticIntField */,
  NOT_IMPL /* SetStaticLongField */,
  NOT_IMPL /* SetStaticFloatField */,
  NOT_IMPL /* SetStaticDoubleField */,
  NOT_IMPL /* NewString */,
  _Jv_JNI_GetStringLength,
  NOT_IMPL /* GetStringChars */,
  NOT_IMPL /* ReleaseStringChars */,
  NOT_IMPL /* NewStringUTF */,
  NOT_IMPL /* GetStringUTFLength */,
  NOT_IMPL /* GetStringUTFChars */,
  NOT_IMPL /* ReleaseStringUTFChars */,
  NOT_IMPL /* GetArrayLength */,
  NOT_IMPL /* NewObjectArray */,
  NOT_IMPL /* GetObjectArrayElement */,
  NOT_IMPL /* SetObjectArrayElement */,
  NOT_IMPL /* NewBooleanArray */,
  NOT_IMPL /* NewByteArray */,
  NOT_IMPL /* NewCharArray */,
  NOT_IMPL /* NewShortArray */,
  NOT_IMPL /* NewIntArray */,
  NOT_IMPL /* NewLongArray */,
  NOT_IMPL /* NewFloatArray */,
  NOT_IMPL /* NewDoubleArray */,
  NOT_IMPL /* GetBooleanArrayElements */,
  NOT_IMPL /* GetByteArrayElements */,
  NOT_IMPL /* GetCharArrayElements */,
  NOT_IMPL /* GetShortArrayElements */,
  NOT_IMPL /* GetIntArrayElements */,
  NOT_IMPL /* GetLongArrayElements */,
  NOT_IMPL /* GetFloatArrayElements */,
  NOT_IMPL /* GetDoubleArrayElements */,
  NOT_IMPL /* ReleaseBooleanArrayElements */,
  NOT_IMPL /* ReleaseByteArrayElements */,
  NOT_IMPL /* ReleaseCharArrayElements */,
  NOT_IMPL /* ReleaseShortArrayElements */,
  NOT_IMPL /* ReleaseIntArrayElements */,
  NOT_IMPL /* ReleaseLongArrayElements */,
  NOT_IMPL /* ReleaseFloatArrayElements */,
  NOT_IMPL /* ReleaseDoubleArrayElements */,
  NOT_IMPL /* GetBooleanArrayRegion */,
  NOT_IMPL /* GetByteArrayRegion */,
  NOT_IMPL /* GetCharArrayRegion */,
  NOT_IMPL /* GetShortArrayRegion */,
  NOT_IMPL /* GetIntArrayRegion */,
  NOT_IMPL /* GetLongArrayRegion */,
  NOT_IMPL /* GetFloatArrayRegion */,
  NOT_IMPL /* GetDoubleArrayRegion */,
  NOT_IMPL /* SetBooleanArrayRegion */,
  NOT_IMPL /* SetByteArrayRegion */,
  NOT_IMPL /* SetCharArrayRegion */,
  NOT_IMPL /* SetShortArrayRegion */,
  NOT_IMPL /* SetIntArrayRegion */,
  NOT_IMPL /* SetLongArrayRegion */,
  NOT_IMPL /* SetFloatArrayRegion */,
  NOT_IMPL /* SetDoubleArrayRegion */,
  NOT_IMPL /* RegisterNatives */,
  NOT_IMPL /* UnregisterNatives */,
  NOT_IMPL /* MonitorEnter */,
  NOT_IMPL /* MonitorExit */,
  NOT_IMPL /* GetJavaVM */,
};
