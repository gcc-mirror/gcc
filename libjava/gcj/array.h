// array.h - Header file for CNI arrays.  -*- c++ -*-

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_ARRAY_H__
#define __GCJ_ARRAY_H__

#include <java/lang/Object.h>

extern "Java" {

class __JArray : public java::lang::Object
{
public:
  int length;
  friend jsize JvGetArrayLength (__JArray*);
};

template<class T>
class JArray : public __JArray
{
  T data[0];
public:
  friend T* elements<>(JArray<T>& x);
  friend T* elements<>(JArray<T>* x);
  // T* getData() { return data; }
  // T& operator[](jint i) { return data[i]; }
};

template<class T>
T* elements(JArray<T>& x) { return x.data; }
template<class T>
T* elements(JArray<T>* x) { return x->data; }

}; // end extern "Java"

/* These typesdefs match those in JNI. */
typedef __JArray *jarray;
typedef JArray<jobject> *jobjectArray;
typedef JArray<jboolean> *jbooleanArray;
typedef JArray<jbyte> *jbyteArray;
typedef JArray<jchar> *jcharArray;
typedef JArray<jshort> *jshortArray;
typedef JArray<jint> *jintArray;
typedef JArray<jlong> *jlongArray;
typedef JArray<jfloat> *jfloatArray;
typedef JArray<jdouble> *jdoubleArray;
typedef JArray<jstring> *jstringArray;

extern "C" jbooleanArray JvNewBooleanArray (jint length);
extern "C" jbyteArray JvNewByteArray (jint length);
extern "C" jcharArray JvNewCharArray (jint length);
extern "C" jshortArray JvNewShortArray (jint length);
extern "C" jintArray JvNewIntArray (jint length);
extern "C" jlongArray JvNewLongArray (jint length);
extern "C" jfloatArray JvNewFloatArray (jint length);
extern "C" jdoubleArray JvNewDoubleArray (jint length);
extern "C" jobjectArray _Jv_NewObjectArray(jsize length, jclass, jobject init);

inline jobjectArray JvNewObjectArray (jsize length, jclass cls, jobject init)
{ return _Jv_NewObjectArray (length, cls, init); }

extern "C" jstringArray JvConvertArgv(int argc, const char **argv);

inline jsize JvGetArrayLength (jarray array) { return array->length; }

#endif /* __GCJ_ARRAY_H__ */
