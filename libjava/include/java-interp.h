// java-interp.h - Header file for the bytecode interpreter.  -*- c++ -*-

/* Copyright (C) 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_INTERP_H__
#define __JAVA_INTERP_H__

#include <jvm.h>
#include <java-cpool.h>

#ifdef INTERPRETER

#pragma interface

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <gnu/gcj/runtime/MethodInvocation.h>

extern "C" {
#include <ffi.h>
}

extern inline jboolean
_Jv_IsInterpretedClass (jclass c)
{
  return (c->loader != 0);
}

struct _Jv_ResolvedMethod;

void _Jv_VerifyFieldSignature (_Jv_Utf8Const*sig);
void _Jv_VerifyMethodSignature (_Jv_Utf8Const*sig);
void _Jv_VerifyClassName (unsigned char* ptr, _Jv_ushort length);
void _Jv_VerifyClassName (_Jv_Utf8Const *name);
void _Jv_VerifyIdentifier (_Jv_Utf8Const *);
bool _Jv_ClassNameSamePackage (_Jv_Utf8Const *name1, _Jv_Utf8Const *name2);
void _Jv_DefineClass (jclass, jbyteArray, jint, jint);
void _Jv_ResolveField (_Jv_Field *, java::lang::ClassLoader*);

void _Jv_InitField (jobject, jclass, int);
void * _Jv_AllocMethodInvocation (jsize size);

/* FIXME: this should really be defined in some more generic place */
#define ROUND(V, A) (((((unsigned) (V))-1) | ((A)-1))+1)

/* the interpreter is written in C++, primarily because it makes it easy for
 * the entire thing to be "friend" with class Class. */

class _Jv_InterpClass;
class _Jv_InterpMethod;
class _Jv_InterpMethodInvocation;

class _Jv_InterpException {
  int  start_pc;
  int  end_pc;
  int  handler_pc;
  int  handler_type;

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethod;
};

class _Jv_InterpMethod {

  _Jv_ushort       max_stack;
  _Jv_ushort       max_locals;
  int              code_length;

  _Jv_ushort       exc_count;
  _Jv_ushort       args_raw_size;

  _Jv_InterpClass *defining_class;
  _Jv_Method      *self;

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
  void continue1 (_Jv_InterpMethodInvocation *inv);

  static void run_normal (ffi_cif*, void*, ffi_raw*, void*);
  static void run_synch_object (ffi_cif*, void*, ffi_raw*, void*);
  static void run_synch_class (ffi_cif*, void*, ffi_raw*, void*);

  inline jobject run (ffi_cif*, void*, ffi_raw*, 
		      _Jv_InterpMethodInvocation*);

  bool find_exception (jobject ex,
		       _Jv_InterpMethodInvocation *inv);

 public:
  static void dump_object(jobject o);

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethodInvocation;
  friend class gnu::gcj::runtime::MethodInvocation;

  friend void _Jv_PrepareClass(jclass);

  // This function is used when making a JNI call from the interpreter.
  friend void _Jv_JNI_conversion_call (ffi_cif *, void *, ffi_raw *, void *);
};

class _Jv_InterpMethodInvocation {
  _Jv_InterpMethod *running;
  _Jv_word         *sp;
  unsigned char    *pc;
  _Jv_word          state[0];

  _Jv_word*         stack_base () { return &state[0]; }
  _Jv_word*         local_base () { return &state[running->max_stack]; }

  friend class _Jv_InterpMethod;
};
  
class _Jv_InterpClass : public java::lang::Class
{
  _Jv_InterpMethod **interpreted_methods;
  _Jv_ushort        *field_initializers;

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethod;
  friend void  _Jv_PrepareClass(jclass);
  friend void  _Jv_InitField (jobject, jclass, int);
  friend void* _Jv_MarkObj (void *, void *, void *, void *);
};

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

#endif /* INTERPRETER */

#endif /* __JAVA_INTERP_H__ */
