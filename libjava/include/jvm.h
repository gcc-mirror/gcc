// jvm.h - Header file for private implementation information. -*- c++ -*-

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_JVM_H__
#define __JAVA_JVM_H__

#include <gcj/javaprims.h>

#include <java-assert.h>
#include <java-threads.h>
// Must include java-gc.h before Object.h for the implementation.
#include <java-gc.h>

#include <java/lang/Object.h>

// Include cni.h before field.h to enable all definitions.  FIXME.
#include <gcj/cni.h>
#include <gcj/field.h>

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
extern jboolean _Jv_equaln (_Jv_Utf8Const *, jstring, jint);

#define StringClass _CL_Q34java4lang6String
extern java::lang::Class StringClass;

/* Type of pointer used as finalizer.  */
typedef void _Jv_FinalizerFunc (jobject);

/* Allocate space for a new Java object.  */
void *_Jv_AllocObj (jsize size) __attribute__((__malloc__));
/* Allocate space for an array of Java objects.  */
void *_Jv_AllocArray (jsize size) __attribute__((__malloc__));
/* Allocate space that is known to be pointer-free.  */
void *_Jv_AllocBytes (jsize size) __attribute__((__malloc__));
/* Initialize the GC.  */
void _Jv_InitGC (void);
/* Register a finalizer.  */
void _Jv_RegisterFinalizer (void *object, _Jv_FinalizerFunc *method);

/* Allocate some unscanned, unmoveable memory.  Return NULL if out of
   memory.  */
void *_Jv_MallocUnchecked (jsize size) __attribute__((__malloc__));

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

/* Set initial heap size.  If SIZE==0, ignore.  Should be run before
   _Jv_InitGC.  Not required to have any actual effect.  */
void _Jv_GCSetInitialHeapSize (size_t size);

/* Set maximum heap size.  If SIZE==0, unbounded.  Should be run
   before _Jv_InitGC.  Not required to have any actual effect.  */
void _Jv_GCSetMaximumHeapSize (size_t size);

/* External interface to setting the heap size.  Parses ARG (a number
   which can optionally have "k" or "m" appended and calls
   _Jv_GCSetInitialHeapSize.  */
void _Jv_SetInitialHeapSize (const char *arg);

/* External interface to setting the maximum heap size.  Parses ARG (a
   number which can optionally have "k" or "m" appended and calls
   _Jv_GCSetMaximumHeapSize.  */
void _Jv_SetMaximumHeapSize (const char *arg);

/* Allocate some unscanned bytes.  Throw exception if out of memory.  */
void *_Jv_AllocBytesChecked (jsize size) __attribute__((__malloc__));

extern "C" void JvRunMain (jclass klass, int argc, const char **argv);
void _Jv_RunMain (const char* name, int argc, const char **argv);

// This function is used to determine the hash code of an object.
inline jint
_Jv_HashCode (jobject obj)
{
  return (jint) obj;
}

// Return a raw pointer to the elements of an array given the array
// and its element type.  You might think we could just pick a single
// array type and use elements() on it, but we can't because we must
// account for alignment of the element type.
inline char *
_Jv_GetArrayElementFromElementType (jobject array,
				    jclass element_type)
{
  char *elts;
  if (element_type == JvPrimClass (byte))
    elts = (char *) elements ((jbyteArray) array);
  else if (element_type == JvPrimClass (short))
    elts = (char *) elements ((jshortArray) array);
  else if (element_type == JvPrimClass (int))
    elts = (char *) elements ((jintArray) array);
  else if (element_type == JvPrimClass (long))
    elts = (char *) elements ((jlongArray) array);
  else if (element_type == JvPrimClass (boolean))
    elts = (char *) elements ((jbooleanArray) array);
  else if (element_type == JvPrimClass (char))
    elts = (char *) elements ((jcharArray) array);
  else if (element_type == JvPrimClass (float))
    elts = (char *) elements ((jfloatArray) array);
  else if (element_type == JvPrimClass (double))
    elts = (char *) elements ((jdoubleArray) array);
  else
    elts = (char *) elements ((jobjectArray) array);
  return elts;
}

extern "C" void _Jv_ThrowBadArrayIndex (jint bad_index);
extern "C" jobject _Jv_NewArray (jint type, jint size)
  __attribute__((__malloc__));
extern "C" jobject _Jv_NewMultiArray (jclass klass, jint dims, ...)
  __attribute__((__malloc__));
extern "C" void *_Jv_CheckCast (jclass klass, jobject obj);
extern "C" void *_Jv_LookupInterfaceMethod (jclass klass, Utf8Const *name,
                                           Utf8Const *signature);
extern "C" void *_Jv_LookupInterfaceMethodIdx (jclass klass, jclass iface, 
                                               int meth_idx);
extern "C" void _Jv_CheckArrayStore (jobject array, jobject obj);
extern "C" void _Jv_RegisterClass (jclass klass);
extern "C" void _Jv_RegisterClasses (jclass *classes);
extern void _Jv_UnregisterClass (_Jv_Utf8Const*, java::lang::ClassLoader*);

extern jclass _Jv_FindClass (_Jv_Utf8Const *name,
			     java::lang::ClassLoader *loader);
extern jclass _Jv_FindClassFromSignature (char *,
					  java::lang::ClassLoader *loader);
extern void _Jv_GetTypesFromSignature (jmethodID method,
				       jclass declaringClass,
				       JArray<jclass> **arg_types_out,
				       jclass *return_type_out);

extern jobject _Jv_CallAnyMethodA (jobject obj, jclass return_type,
				   jmethodID meth, jboolean is_constructor,
				   JArray<jclass> *parameter_types,
				   jobjectArray args);

union jvalue;
extern jthrowable _Jv_CallAnyMethodA (jobject obj,
				      jclass return_type,
				      jmethodID meth,
				      jboolean is_constructor,
				      JArray<jclass> *parameter_types,
				      jvalue *args,
				      jvalue *result);

extern jobject _Jv_NewMultiArray (jclass, jint ndims, jint* dims)
  __attribute__((__malloc__));

/* Checked divide subroutines. */
extern "C"
{
  jint _Jv_divI (jint, jint);
  jint _Jv_remI (jint, jint);
  jlong _Jv_divJ (jlong, jlong);
  jlong _Jv_remJ (jlong, jlong);
}

/* get/set the name of the running executable. */
extern char *_Jv_ThisExecutable (void);
extern void _Jv_ThisExecutable (const char *);

/* Return a pointer to a symbol in executable or loaded library.  */
void *_Jv_FindSymbolInExecutable (const char *);

/* Initialize JNI.  */
extern void _Jv_JNI_Init (void);

/* Get or set the per-thread JNIEnv used by the invocation API.  */
_Jv_JNIEnv *_Jv_GetCurrentJNIEnv ();
void _Jv_SetCurrentJNIEnv (_Jv_JNIEnv *);

struct _Jv_JavaVM;
_Jv_JavaVM *_Jv_GetJavaVM ();

#endif /* __JAVA_JVM_H__ */
