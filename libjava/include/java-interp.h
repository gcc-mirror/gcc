// java-interp.h - Header file for the bytecode interpreter.  -*- c++ -*-

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_INTERP_H__
#define __JAVA_INTERP_H__

#include <jvm.h>
#include <java-cpool.h>
#include <gnu/gcj/runtime/NameFinder.h>

#ifdef INTERPRETER

#pragma interface

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/reflect/Modifier.h>

extern "C" {
#include <ffi.h>
}

struct _Jv_ResolvedMethod;

void _Jv_InitInterpreter ();
void _Jv_DefineClass (jclass, jbyteArray, jint, jint,
		      java::security::ProtectionDomain *);

void _Jv_InitField (jobject, jclass, int);
void * _Jv_AllocMethodInvocation (jsize size);
int  _Jv_count_arguments (_Jv_Utf8Const *signature,
			  jboolean staticp = true);
void _Jv_VerifyMethod (_Jv_InterpMethod *method);

/* the interpreter is written in C++, primarily because it makes it easy for
 * the entire thing to be "friend" with class Class. */

class _Jv_InterpClass;
class _Jv_InterpMethod;

// Before a method is "compiled" we store values as the bytecode PC,
// an int.  Afterwards we store them as pointers into the prepared
// code itself.
union _Jv_InterpPC
{
  int i;
  void *p;
};

class _Jv_InterpException
{
  _Jv_InterpPC start_pc;
  _Jv_InterpPC end_pc;
  _Jv_InterpPC handler_pc;
  _Jv_InterpPC handler_type;

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethod;
  friend class _Jv_BytecodeVerifier;
};

// Base class for method representations.  Subclasses are interpreted
// and JNI methods.
class _Jv_MethodBase
{
protected:
  // The class which defined this method.
  jclass defining_class;

  // The method description.
  _Jv_Method *self;

  // Size of raw arguments.
  _Jv_ushort args_raw_size;

  friend class _Jv_InterpreterEngine;

public:
  _Jv_Method *get_method ()
  {
    return self;
  }
};

class _Jv_InterpMethod : public _Jv_MethodBase
{
  _Jv_ushort       max_stack;
  _Jv_ushort       max_locals;
  int              code_length;

  _Jv_ushort       exc_count;

  void *prepared;

  unsigned char* bytecode () 
  {
    return 
      ((unsigned char*)this) 
      + ROUND((sizeof (_Jv_InterpMethod)
	       + exc_count*sizeof (_Jv_InterpException)), 4);
  }

  _Jv_InterpException * exceptions ()
  {
    return (_Jv_InterpException*) (this+1);
  }

  static size_t size (int exc_count, int code_length)
  {
    return 
      ROUND ((sizeof (_Jv_InterpMethod) 
	      + (exc_count * sizeof (_Jv_InterpException))), 4)
      + code_length;
  }

  // return the method's invocation pointer (a stub).
  void *ncode ();
  void compile (const void * const *);

  static void run_normal (ffi_cif*, void*, ffi_raw*, void*);
  static void run_synch_object (ffi_cif*, void*, ffi_raw*, void*);
  static void run_class (ffi_cif*, void*, ffi_raw*, void*);
  static void run_synch_class (ffi_cif*, void*, ffi_raw*, void*);

  void run (void*, ffi_raw *);

 public:
  static void dump_object(jobject o);

  friend class _Jv_ClassReader;
  friend class _Jv_BytecodeVerifier;
  friend class gnu::gcj::runtime::NameFinder;
  friend class gnu::gcj::runtime::StackTrace;
  friend class _Jv_InterpreterEngine;
  

#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif
};

class _Jv_InterpClass
{
  _Jv_MethodBase **interpreted_methods;
  _Jv_ushort        *field_initializers;

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethod;
  friend class _Jv_InterpreterEngine;
  friend void  _Jv_InitField (jobject, jclass, int);
#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif

  friend _Jv_MethodBase ** _Jv_GetFirstMethod (_Jv_InterpClass *klass);
};

extern inline _Jv_MethodBase **
_Jv_GetFirstMethod (_Jv_InterpClass *klass)
{
  return klass->interpreted_methods;
}

struct _Jv_ResolvedMethod {
  jint            stack_item_count;	
  jint            vtable_index;	
  jclass          klass;
  _Jv_Method*     method;

  // a resolved method holds the cif in-line, so that _Jv_MarkObj just needs
  // to mark the resolved method to hold on to the cif.  Some memory could be
  // saved by keeping a cache of cif's, since many will be the same.
  ffi_cif         cif;
  ffi_type *      arg_types[0];
};

class _Jv_JNIMethod : public _Jv_MethodBase
{
  // The underlying function.  If NULL we have to look for the
  // function.
  void *function;

  // This is the CIF used by the JNI function.
  ffi_cif jni_cif;

  // These are the argument types used by the JNI function.
  ffi_type **jni_arg_types;

  // This function is used when making a JNI call from the interpreter.
  static void call (ffi_cif *, void *, ffi_raw *, void *);

  void *ncode ();

  friend class _Jv_ClassReader;
  friend class _Jv_InterpreterEngine;

#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif

public:
  // FIXME: this is ugly.
  void set_function (void *f)
  {
    function = f;
  }
};

// A structure of this type is used to link together interpreter
// invocations on the stack.
struct _Jv_MethodChain
{
  const _Jv_InterpMethod *self;
  _Jv_MethodChain **ptr;
  _Jv_MethodChain *next;

  _Jv_MethodChain (const _Jv_InterpMethod *s, _Jv_MethodChain **n)
  {
    self = s;
    ptr = n;
    next = *n;
    *n = this;
  }

  ~_Jv_MethodChain ()
  {
    *ptr = next;
  }
};

#endif /* INTERPRETER */

#endif /* __JAVA_INTERP_H__ */
