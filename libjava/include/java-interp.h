// java-interp.h - Header file for the bytecode interpreter.  -*- c++ -*-

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_INTERP_H__
#define __JAVA_INTERP_H__

#include <jvm.h>
#include <java-cpool.h>
#include <gnu/gcj/runtime/NameFinder.h>

enum _Jv_FrameType
{
  frame_native,
  frame_interpreter,
  frame_proxy
};

#ifdef INTERPRETER

#pragma interface

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/Thread.h>
#include <gnu/gcj/RawData.h>

// Define this to get the direct-threaded interpreter.  If undefined,
// we revert to a basic bytecode interpreter.  The former is faster
// but uses more memory.
#define DIRECT_THREADED

#include <ffi.h>

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
void _Jv_CompileMethod (_Jv_InterpMethod* method);
int _Jv_init_cif (_Jv_Utf8Const* signature,
		  int arg_count,
		  jboolean staticp,
		  ffi_cif *cif,
		  ffi_type **arg_types,
		  ffi_type **rtype_p);

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

// This structure holds local variable information.
// Like _Jv_LineTableEntry above, it is remapped when the method is
// compiled for direct threading.
struct _Jv_LocalVarTableEntry
{
  // First PC value at which variable is live
  union
  {
    pc_t pc;
    int bytecode_pc;
  };

  // length of visibility of variable
  int length;

  // variable name
  char *name;

  // type description
  char *descriptor;

  // stack slot number (long and double occupy slot and slot + 1)
  int slot;
};

class _Jv_InterpMethod : public _Jv_MethodBase
{
  // Breakpoint instruction
  static pc_t breakpoint_insn;
#ifdef DIRECT_THREADED
  static insn_slot bp_insn_slot;

public:
  // Mutex to prevent a data race between threads when rewriting
  // instructions.  See interpret-run.cc for an explanation of its use.
  static _Jv_Mutex_t rewrite_insn_mutex;

  // The count of threads executing this method.
  long thread_count;

private:

#else
  static unsigned char bp_insn_opcode;
#endif

  _Jv_ushort       max_stack;
  _Jv_ushort       max_locals;
  int              code_length;

  _Jv_ushort       exc_count;
  bool             is_15;

  // Length of the line_table - when this is zero then line_table is NULL.
  int line_table_len;  
  _Jv_LineTableEntry *line_table;
  
  // The local variable table length and the table itself
  int local_var_table_len;
  _Jv_LocalVarTableEntry *local_var_table;

  pc_t prepared;
  int number_insn_slots;

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
  void *ncode (jclass);
  void compile (const void * const *);

#if FFI_NATIVE_RAW_API
#  define INTERP_FFI_RAW_TYPE ffi_raw
#else
#  define INTERP_FFI_RAW_TYPE ffi_java_raw
#endif

  static void run_normal (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*, void*);
  static void run_synch_object (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*, void*);
  static void run_class (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*, void*);
  static void run_synch_class (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*, void*);
  
  static void run_normal_debug (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*, void*);
  static void run_synch_object_debug (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*,
				      void*);
  static void run_class_debug (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*, void*);
  static void run_synch_class_debug (ffi_cif*, void*, INTERP_FFI_RAW_TYPE*,
				     void*);

  static void run (void *, INTERP_FFI_RAW_TYPE *, _Jv_InterpMethod *);
  static void run_debug (void *, INTERP_FFI_RAW_TYPE *, _Jv_InterpMethod *);
  

  
  // Returns source file line number for given PC value, or -1 if line
  // number info is unavailable.
  int get_source_line(pc_t mpc);

   public:

  // Convenience function for indexing bytecode PC/insn slots in
  // line tables for JDWP
  jlong insn_index (pc_t pc);
  
  // Helper function used to check if there is a handler for an exception
  // present at this code index
  jboolean check_handler (pc_t *pc, _Jv_InterpMethod *meth,
                     java::lang::Throwable *ex);
   
  /* Get the line table for this method.
   * start  is the lowest index in the method
   * end    is the  highest index in the method
   * line_numbers is an array to hold the list of source line numbers
   * code_indices is an array to hold the corresponding list of code indices
   */
  void get_line_table (jlong& start, jlong& end, jintArray& line_numbers,
		       jlongArray& code_indices);
  
  int get_max_locals ()
  {
    return static_cast<int> (max_locals);
  }
  
  /* Get info for a local variable of this method.
   * If there is no loca_var_table for this method it will return -1.
   * table_slot  indicates which slot in the local_var_table to get, if there is
   * no variable at this location it will return 0.
   * Otherwise, it will return the number of table slots after the selected
   * slot, indexed from 0.
   * 
   * Example: there are 5 slots in the table, you request slot 0 so it will
   * return 4.
   */
  int get_local_var_table (char **name, char **sig, char **generic_sig,
                           jlong *startloc, jint *length, jint *slot,
                           int table_slot);

  /* Installs a break instruction at the given code index. Returns
     the pc_t of the breakpoint or NULL if index is invalid. */
  pc_t install_break (jlong index);

  // Gets the instruction at the given index
  pc_t get_insn (jlong index);

  /* Writes the given instruction at the given code index. Returns
     the insn or NULL if index is invalid. */
  pc_t set_insn (jlong index, pc_t insn);

  // Is the given location in this method a breakpoint?
  bool breakpoint_at (jlong index);

#ifdef DIRECT_THREADED
  friend void _Jv_CompileMethod (_Jv_InterpMethod*);
#endif
  
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
  _Jv_ClosureList **closures;

  friend class _Jv_ClassReader;
  friend class _Jv_InterpMethod;
  friend class _Jv_StackTrace;
  friend class _Jv_InterpreterEngine;

  friend void  _Jv_InitField (jobject, jclass, int);
#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif

  friend _Jv_MethodBase ** _Jv_GetFirstMethod (_Jv_InterpClass *klass);
  friend jstring _Jv_GetInterpClassSourceFile (jclass);
};

extern inline _Jv_MethodBase **
_Jv_GetFirstMethod (_Jv_InterpClass *klass)
{
  return klass->interpreted_methods;
}

struct _Jv_ResolvedMethod
{
  jint            stack_item_count;	
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
  static void call (ffi_cif *, void *, INTERP_FFI_RAW_TYPE *, void *);

  void *ncode (jclass);

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

//  The composite call stack as represented by a linked list of frames
class _Jv_Frame
{
public:
  java::lang::Thread *thread;

  union
  {
    _Jv_MethodBase *self;
    void *meth;
    _Jv_Method *proxyMethod;
  };
  
  //The full list of frames, JNI and interpreted
  _Jv_Frame *next;
  _Jv_FrameType frame_type;
  
  _Jv_Frame (_Jv_MethodBase *s, java::lang::Thread *thr, _Jv_FrameType type)
  {
    self = s;
    frame_type = type;
    next = (_Jv_Frame *) thr->frame;
    thr->frame = (gnu::gcj::RawData *) this;
    thread = thr;
  }

  ~_Jv_Frame ()
  {
    thread->frame = (gnu::gcj::RawData *) next;
  }

  int depth ()
  {
    int depth = 0;
    struct _Jv_Frame *f;
    for (f = this; f != NULL; f = f->next)
      ++depth;

    return depth;
  }
};

// An interpreted frame in the call stack
class _Jv_InterpFrame : public _Jv_Frame
{
public:
  
  // Keep the purely interpreted list around so as not to break backtraces
  _Jv_InterpFrame *next_interp;
  
  union
  {
    pc_t pc;
    jclass proxyClass;
  };
  
  // Pointer to the actual pc value.
  pc_t *pc_ptr;

  //Debug info for local variables.
  _Jv_word *locals;
  char *locals_type;

  // Object pointer for this frame ("this")
  jobject obj_ptr;

  _Jv_InterpFrame (void *meth, java::lang::Thread *thr, jclass proxyCls = NULL,
                   pc_t *pc = NULL, 
		   _Jv_FrameType frame_type = frame_interpreter)
  : _Jv_Frame (reinterpret_cast<_Jv_MethodBase *> (meth), thr,
	             frame_type)
  {
    next_interp = (_Jv_InterpFrame *) thr->interp_frame;
    proxyClass = proxyCls;
    thr->interp_frame = (gnu::gcj::RawData *) this;
    obj_ptr = NULL;
    pc_ptr = pc;
  }

  ~_Jv_InterpFrame ()
  {
    thread->interp_frame = (gnu::gcj::RawData *) next_interp;
  }

  jobject get_this_ptr ()
  {
    return obj_ptr;
  }
  
  pc_t get_pc ()
  {
    pc_t pc;
    
    // If the PC_PTR is NULL, we are not debugging.
    if (pc_ptr == NULL)
      pc = 0;
    else
      pc = *pc_ptr - 1;
    
    return pc;
  }
};

// A native frame in the call stack really just a placeholder
class _Jv_NativeFrame : public _Jv_Frame
{
public:

  _Jv_NativeFrame (_Jv_JNIMethod *s, java::lang::Thread *thr)
  : _Jv_Frame (s, thr, frame_native)
  {
  }
};

#ifdef DIRECT_THREADED
// This class increments and decrements the thread_count field in an
// interpreted method.  On entry to the interpreter a
// ThreadCountAdjuster is created when increments the thread_count in
// the current method and uses the next_interp field in the frame to
// find the previous method and decrement its thread_count.
class ThreadCountAdjuster
{

  // A class used to handle the rewrite_insn_mutex while we're
  // adjusting the thread_count in a method.  Unlocking the mutex in a
  // destructor ensures that it's unlocked even if (for example) a
  // segfault occurs in the critical section.
  class MutexLock
  {
  private:
    _Jv_Mutex_t *mutex;
  public:
    MutexLock (_Jv_Mutex_t *m)
    {
      mutex = m;
      _Jv_MutexLock (mutex);
    }
    ~MutexLock ()
    {
      _Jv_MutexUnlock (mutex);
    }
  };

  _Jv_InterpMethod *method;
  _Jv_InterpMethod *next_method;

public:

  ThreadCountAdjuster (_Jv_InterpMethod *m, _Jv_InterpFrame *fr)
  {
    MutexLock lock (&::_Jv_InterpMethod::rewrite_insn_mutex);

    method = m;
    next_method = NULL;

    _Jv_InterpFrame *next_interp = fr->next_interp;

    // Record the fact that we're executing this method and that
    // we're no longer executing the method that called us.
    method->thread_count++;

    if (next_interp && next_interp->frame_type == frame_interpreter)
      {
	next_method 
	  = reinterpret_cast<_Jv_InterpMethod *> (next_interp->meth);
	next_method->thread_count--;
      }
  }

  ~ThreadCountAdjuster ()
  {
    MutexLock lock (&::_Jv_InterpMethod::rewrite_insn_mutex);

    // We're going to return to the method that called us, so bump its
    // thread_count and decrement our own.

    method->thread_count--;

    if (next_method)
      next_method->thread_count++;
  }
};
#endif // DIRECT_THREADED

#endif /* INTERPRETER */

#endif /* __JAVA_INTERP_H__ */
