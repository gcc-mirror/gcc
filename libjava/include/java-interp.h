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

// Define this to get the direct-threaded interpreter.  If undefined,
// we revert to a basic bytecode interpreter.  The former is faster
// but uses more memory.
#define DIRECT_THREADED

extern "C" {
#include <ffi.h>
}

struct _Jv_ResolvedMethod;

void _Jv_InitInterpreter ();
void _Jv_DefineClass (jclass, jbyteArray, jint, jint,
		      java::security::ProtectionDomain *,
		      _Jv_Utf8Const **);

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

// The type of the PC depends on whether we're doing direct threading
// or a more ordinary bytecode interpreter.
#ifdef DIRECT_THREADED
// Slot in the "compiled" form of the bytecode.
union insn_slot
{
  // Address of code.
  void *insn;
  // An integer value used by an instruction.
  jint int_val;
  // A pointer value used by an instruction.
  void *datum;
};

typedef insn_slot *pc_t;
#else
typedef unsigned char *pc_t;
#endif


// This structure holds the bytecode pc and corresponding source code
// line number.  An array (plus length field) of this structure is put
// in each _Jv_InterpMethod and used to resolve the (internal) program
// counter of the interpreted method to an actual java source file
// line.
struct  _Jv_LineTableEntry
{
  union
  {
    pc_t pc;
    int bytecode_pc;
  };
  int line;
};

class _Jv_InterpMethod : public _Jv_MethodBase
{
  _Jv_ushort       max_stack;
  _Jv_ushort       max_locals;
  int              code_length;

  _Jv_ushort       exc_count;
  bool             is_15;

  // Length of the line_table - when this is zero then line_table is NULL.
  int line_table_len;  
  _Jv_LineTableEntry *line_table;

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

  static void run (void*, ffi_raw *, _Jv_InterpMethod *);

  // Returns source file line number for given PC value, or -1 if line
  // number info is unavailable.
  int get_source_line(pc_t mpc);

 public:
  static void dump_object(jobject o);

  friend class _Jv_ClassReader;
  friend class _Jv_BytecodeVerifier;
  friend class _Jv_StackTrace;
  friend class _Jv_InterpreterEngine;

#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif
};

class _Jv_InterpClass
{
  _Jv_MethodBase **interpreted_methods;
  _Jv_ushort     *field_initializers;
  jstring source_file_name;

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethod;
  friend class _Jv_StackTrace;
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

struct _Jv_ResolvedMethod
{
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

// The interpreted call stack, represented by a linked list of frames.
struct _Jv_InterpFrame
{
  _Jv_InterpMethod *self;
  _Jv_InterpFrame **ptr;
  _Jv_InterpFrame *next;
  pc_t pc;

  _Jv_InterpFrame (_Jv_InterpMethod *s, _Jv_InterpFrame **n)
  {
    self = s;
    ptr = n;
    next = *n;
    *n = this;
    pc = NULL;
  }

  ~_Jv_InterpFrame ()
  {
    *ptr = next;
  }
};

#endif /* INTERPRETER */

#endif /* __JAVA_INTERP_H__ */
