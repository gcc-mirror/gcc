// execution.h - Execution engines. -*- c++ -*-

/* Copyright (C) 2004, 2006, 2007  Free Software Foundation

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
  void (*allocate_static_fields) (jclass, int, int);
  void (*allocate_field_initializers) (jclass);
  void (*create_ncode) (jclass);
  _Jv_ResolvedMethod *(*resolve_method) (_Jv_Method *, jclass,
					 jboolean);
  void (*post_miranda_hook) (jclass);
  _Jv_ClosureList **(*get_closure_list) (jclass);
};

// This handles gcj-compiled code except that compiled with
// -findirect-classes.
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
						jboolean)
  {
    return NULL;
  }

  static void do_allocate_static_fields (jclass,
					 int,
					 int)
  {
  }

  static void do_allocate_field_initializers (jclass)
  {
  }

  static void do_create_ncode (jclass)
  {
    // Not needed.
  }

  static void do_post_miranda_hook (jclass)
  {
    // Not needed.
  }

  static _Jv_ClosureList **do_get_closure_list (jclass)
  {
    return NULL;
  }

  _Jv_CompiledEngine ()
  {
    unregister = do_unregister;
    need_resolve_string_fields = do_need_resolve_string_fields;
    verify = do_verify;
    allocate_static_fields = do_allocate_static_fields;
    allocate_field_initializers = do_allocate_field_initializers;
    create_ncode = do_create_ncode;
    resolve_method = do_resolve_method;
    post_miranda_hook = do_post_miranda_hook;
    get_closure_list = do_get_closure_list;
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

class _Jv_IndirectCompiledClass
{
public:
  void **field_initializers;
  _Jv_ClosureList **closures;
};

// This handles gcj-compiled code compiled with -findirect-classes.
struct _Jv_IndirectCompiledEngine : public _Jv_CompiledEngine
{
  _Jv_IndirectCompiledEngine () : _Jv_CompiledEngine ()
  {
    allocate_static_fields = do_allocate_static_fields;
    allocate_field_initializers = do_allocate_field_initializers;
    get_closure_list = do_get_closure_list;
  }
  
  static _Jv_IndirectCompiledClass *get_aux_info (jclass klass)
  {
    _Jv_IndirectCompiledClass *aux =
      (_Jv_IndirectCompiledClass*)klass->aux_info;
    if (!aux)
      {
	aux = (_Jv_IndirectCompiledClass*)
	  _Jv_AllocRawObj (sizeof (_Jv_IndirectCompiledClass));
	klass->aux_info = aux;
      }

    return aux;
  }

  static void do_allocate_field_initializers (jclass klass)
  {
    _Jv_IndirectCompiledClass *aux = get_aux_info (klass);
    if (!aux)
      {
	aux = (_Jv_IndirectCompiledClass*)
	  _Jv_AllocRawObj (sizeof (_Jv_IndirectCompiledClass));
	klass->aux_info = aux;
      }

    aux->field_initializers = (void **)_Jv_Malloc (klass->field_count 
						   * sizeof (void*));    

    for (int i = 0; i < klass->field_count; i++)
      {
	_Jv_Field *field = &klass->fields[i];
	if (field->flags & java::lang::reflect::Modifier::STATIC)
	  {
	    aux->field_initializers[i] = field->u.addr;
	    field->u.addr = NULL; 
	  }
      }
  }

  static void do_allocate_static_fields (jclass klass,
					 int pointer_size,
					 int other_size)
  {
    // Splitting the allocations here lets us scan reference fields
    // and avoid scanning non-reference fields.
    char *reference_fields = (char *) _Jv_AllocRawObj (pointer_size);
    char *non_reference_fields = (char *) _Jv_AllocBytes (other_size);

    _Jv_IndirectCompiledClass *aux 
      =  (_Jv_IndirectCompiledClass*)klass->aux_info;

    for (int i = 0; i < klass->field_count; i++)
      {
	_Jv_Field *field = &klass->fields[i];

	if ((field->flags & java::lang::reflect::Modifier::STATIC) == 0)
	  continue;

	char *base = field->isRef() ? reference_fields : non_reference_fields;
	field->u.addr  = base + field->u.boffset;

	if (aux->field_initializers[i])
	  {
	    int field_size;
	    if (! field->isRef ())
	      field_size = field->type->size ();
	    else 
	      field_size = sizeof (jobject);

	    memcpy (field->u.addr, aux->field_initializers[i], field_size);
	  }
      } 
    _Jv_Free (aux->field_initializers);
  }

#ifdef INTERPRETER
  static _Jv_ClosureList **do_get_closure_list (jclass klass)
  {
    _Jv_IndirectCompiledClass *aux = get_aux_info (klass);

    if (!aux->closures)
      aux->closures = _Jv_ClosureListFinalizer ();

    return aux->closures;
  }
#endif
};

#ifdef INTERPRETER

// This handles interpreted code.
class _Jv_InterpreterEngine : public _Jv_ExecutionEngine
{
 public:

  static void do_verify (jclass);
  static void do_allocate_static_fields (jclass, int, int);
  static void do_create_ncode (jclass);
  static _Jv_ResolvedMethod *do_resolve_method (_Jv_Method *, jclass,
						jboolean);

  static bool do_need_resolve_string_fields ()
  {
    return false;
  }

  static void do_unregister(jclass klass)
  {
    _Jv_UnregisterClass(klass);
  }

  static void do_allocate_field_initializers (jclass)
  {
  }

  static void do_post_miranda_hook (jclass);

  static _Jv_ClosureList **do_get_closure_list (jclass klass);

  _Jv_InterpreterEngine ()
  {
    unregister = do_unregister;
    need_resolve_string_fields = do_need_resolve_string_fields;
    verify = do_verify;
    allocate_static_fields = do_allocate_static_fields;
    allocate_field_initializers = do_allocate_field_initializers;
    create_ncode = do_create_ncode;
    resolve_method = do_resolve_method;
    post_miranda_hook = do_post_miranda_hook;
    get_closure_list = do_get_closure_list;
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
#endif // INTERPRETER

extern _Jv_CompiledEngine _Jv_soleCompiledEngine;
extern _Jv_IndirectCompiledEngine _Jv_soleIndirectCompiledEngine;
#endif // __JAVA_EXECUTION_H__
