// execution.h - Execution engines. -*- c++ -*-

/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_EXECUTION_H__
#define __JAVA_EXECUTION_H__

// This represents one execution engine.  Note that we use function
// pointers and not virtual methods to avoid calls to
// __cxa_call_unexpected and the like.
struct _Jv_ExecutionEngine
{
 public:

  void (*unregister) (jclass);
  // FIXME: probably should handle this elsewhere, see how
  // interpreter does it.
  bool (*need_resolve_string_fields) ();
  void (*verify) (jclass);
  void (*allocate_static_fields) (jclass, int);
  void (*create_ncode) (jclass);
  _Jv_ResolvedMethod *(*resolve_method) (_Jv_Method *, jclass,
					 jboolean, jint);
  void (*post_miranda_hook) (jclass);
};

// This handles all gcj-compiled code, including BC ABI.
struct _Jv_CompiledEngine : public _Jv_ExecutionEngine
{
 public:

  static void do_unregister (jclass)
  {
  }

  static bool do_need_resolve_string_fields ()
  {
    return true;
  }

  static void do_verify (jclass klass)
  {
    _Jv_Linker::verify_type_assertions (klass);
  }

  static _Jv_ResolvedMethod *do_resolve_method (_Jv_Method *, jclass,
						jboolean, jint)
  {
    return NULL;
  }

  static void do_allocate_static_fields (jclass, int)
  {
    // Compiled classes don't need this.
  }

  static void do_create_ncode (jclass)
  {
    // Not needed.
  }

  static void do_post_miranda_hook (jclass)
  {
    // Not needed.
  }

  _Jv_CompiledEngine ()
  {
    unregister = do_unregister;
    need_resolve_string_fields = do_need_resolve_string_fields;
    verify = do_verify;
    allocate_static_fields = do_allocate_static_fields;
    create_ncode = do_create_ncode;
    resolve_method = do_resolve_method;
    post_miranda_hook = do_post_miranda_hook;
  }

  // These operators make it so we don't have to link in libstdc++.
  void *operator new (size_t bytes)
  {
    return _Jv_Malloc(bytes);
  }

  void operator delete (void *mem)
  {
    _Jv_Free(mem);
  }
};

// This handles interpreted code.
class _Jv_InterpreterEngine : public _Jv_ExecutionEngine
{
 public:

  static void do_verify (jclass);
  static void do_allocate_static_fields (jclass, int);
  static void do_create_ncode (jclass);
  static _Jv_ResolvedMethod *do_resolve_method (_Jv_Method *, jclass,
						jboolean, jint);

  static bool do_need_resolve_string_fields ()
  {
    return false;
  }

  static void do_unregister(jclass klass)
  {
    _Jv_UnregisterClass(klass);
  }

  static void do_post_miranda_hook (jclass);

  _Jv_InterpreterEngine ()
  {
    unregister = do_unregister;
    need_resolve_string_fields = do_need_resolve_string_fields;
    verify = do_verify;
    allocate_static_fields = do_allocate_static_fields;
    create_ncode = do_create_ncode;
    resolve_method = do_resolve_method;
    post_miranda_hook = do_post_miranda_hook;
  }

  // These operators make it so we don't have to link in libstdc++.
  void *operator new (size_t bytes)
  {
    return _Jv_Malloc(bytes);
  }

  void operator delete (void *mem)
  {
    _Jv_Free(mem);
  }
};


extern _Jv_InterpreterEngine _Jv_soleInterpreterEngine;
extern _Jv_CompiledEngine _Jv_soleCompiledEngine;

#endif // __JAVA_EXECUTION_H__
