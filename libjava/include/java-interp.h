// java-interp.h - Header file for the bytecode interpreter.  -*- c++ -*-

/* Copyright (C) 1999, 2000, 2001  Free Software Foundation

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

extern "C" {
#include <ffi.h>
}

extern inline jboolean
_Jv_IsInterpretedClass (jclass c)
{
  return (c->loader != 0);
}

struct _Jv_ResolvedMethod;

void _Jv_DefineClass (jclass, jbyteArray, jint, jint);

void _Jv_InitField (jobject, jclass, int);
void * _Jv_AllocMethodInvocation (jsize size);
int  _Jv_count_arguments (_Jv_Utf8Const *signature,
			  jboolean staticp = true);
void _Jv_VerifyMethod (_Jv_InterpMethod *method);

/* FIXME: this should really be defined in some more generic place */
#define ROUND(V, A) (((((unsigned) (V))-1) | ((A)-1))+1)

/* the interpreter is written in C++, primarily because it makes it easy for
 * the entire thing to be "friend" with class Class. */

class _Jv_InterpClass;
class _Jv_InterpMethod;
class _Jv_InterpMethodInvocation;

class _Jv_InterpException
{
  int  start_pc;
  int  end_pc;
  int  handler_pc;
  int  handler_type;

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
  _Jv_InterpClass *defining_class;

  // The method description.
  _Jv_Method *self;

  // Size of raw arguments.
  _Jv_ushort args_raw_size;

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
  friend class _Jv_BytecodeVerifier;

  friend void _Jv_PrepareClass(jclass);
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
  _Jv_MethodBase **interpreted_methods;
  _Jv_ushort        *field_initializers;

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethod;
  friend void  _Jv_PrepareClass(jclass);
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
  friend void _Jv_PrepareClass(jclass);

public:
  // FIXME: this is ugly.
  void set_function (void *f)
  {
    function = f;
  }
};

#endif /* INTERPRETER */

#endif /* __JAVA_INTERP_H__ */
