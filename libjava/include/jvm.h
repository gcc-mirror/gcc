// jvm.h - Header file for private implementation information. -*- c++ -*-

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_JVM_H__
#define __JAVA_JVM_H__

#include <java-assert.h>
#include <java-field.h>

/* Structure of the virtual table.  */
struct _Jv_VTable
{
  jclass clas;
  void *method[1];
};

/* Extract a character from a Java-style Utf8 string.
 * PTR points to the current character.
 * LIMIT points to the end of the Utf8 string.
 * PTR is incremented to point after the character thta gets returns.
 * On an error, -1 is returned. */
#define UTF8_GET(PTR, LIMIT) \
  ((PTR) >= (LIMIT) ? -1 \
   : *(PTR) < 128 ? *(PTR)++ \
   : (*(PTR)&0xE0) == 0xC0 && ((PTR)+=2)<=(LIMIT) && ((PTR)[-1]&0xC0) == 0x80 \
   ? (((PTR)[-2] & 0x1F) << 6) + ((PTR)[-1] & 0x3F) \
   : (*(PTR) & 0xF0) == 0xE0 && ((PTR) += 3) <= (LIMIT) \
   && ((PTR)[-2] & 0xC0) == 0x80 && ((PTR)[-1] & 0xC0) == 0x80 \
   ? (((PTR)[-3]&0x0F) << 12) + (((PTR)[-2]&0x3F) << 6) + ((PTR)[-1]&0x3F) \
   : ((PTR)++, -1))

extern int _Jv_strLengthUtf8(char* str, int len);

typedef struct _Jv_Utf8Const Utf8Const;
_Jv_Utf8Const *_Jv_makeUtf8Const (char *s, int len);
_Jv_Utf8Const *_Jv_makeUtf8Const (jstring string);
extern jboolean _Jv_equalUtf8Consts (_Jv_Utf8Const *, _Jv_Utf8Const *);
extern jboolean _Jv_equal (_Jv_Utf8Const *, jstring, jint);

#define StringClass _CL_Q34java4lang6String
extern java::lang::Class StringClass;

/* Type of pointer used as finalizer.  */
typedef void _Jv_FinalizerFunc (jobject);

/* Allocate space for a new Java object.  */
void *_Jv_AllocObj (jsize size);
/* Allocate space for an array of Java objects.  */
void *_Jv_AllocArray (jsize size);
/* Allocate space that is known to be pointer-free.  */
void *_Jv_AllocBytes (jsize size);
/* Initialize the GC.  */
void _Jv_InitGC (void);
/* Register a finalizer.  */
void _Jv_RegisterFinalizer (void *object, _Jv_FinalizerFunc *method);

/* Run finalizers for objects ready to be finalized..  */
void _Jv_RunFinalizers (void);
/* Run all finalizers.  Should be called only before exit.  */
void _Jv_RunAllFinalizers (void);
/* Perform a GC.  */
void _Jv_RunGC (void);

/* Return approximation of total size of heap.  */
long _Jv_GCTotalMemory (void);
/* Return approximation of total free memory.  */
long _Jv_GCFreeMemory (void);

/* Allocate some unscanned bytes.  Throw exception if out of memory.  */
void *_Jv_AllocBytesChecked (jsize size);

// This function is used to determine the hash code of an object.
inline jint
_Jv_HashCode (jobject obj)
{
  return (jint) obj;
}

extern "C" void _Jv_ThrowBadArrayIndex (jint bad_index);
extern "C" jobject _Jv_NewArray (jint type, jint size);
extern "C" jobject _Jv_NewMultiArray (jclass klass, jint dims, ...);
extern "C" void *_Jv_CheckCast (jclass klass, jobject obj);
extern "C" void *_Jv_LookupInterfaceMethod (jclass klass, Utf8Const *name,
					    Utf8Const *signature);
extern "C" void _Jv_CheckArrayStore (jobject array, jobject obj);
extern "C" void _Jv_RegisterClass (jclass klass);
extern "C" void _Jv_RegisterClasses (jclass *classes);
extern void _Jv_UnregisterClass (_Jv_Utf8Const*, java::lang::ClassLoader*);

extern jclass _Jv_FindClass (_Jv_Utf8Const *name,
			     java::lang::ClassLoader *loader);
extern jclass _Jv_FindClassFromSignature (char *,
					  java::lang::ClassLoader *loader);

extern jobject _Jv_NewMultiArray (jclass, jint ndims, jint* dims);

/* Checked divide subroutines. */
extern "C"
{
  jint _Jv_divI (jint, jint);
  jint _Jv_remI (jint, jint);
  jlong _Jv_divJ (jlong, jlong);
  jlong _Jv_remJ (jlong, jlong);
}

#endif /* __JAVA_JVM_H__ */
