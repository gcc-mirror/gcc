// boehm.cc - interface between libjava and Boehm GC.

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>

#include <jvm.h>
#include <gcj/cni.h>

#include <java/lang/Class.h>
#include <java/lang/reflect/Modifier.h>
#include <java-interp.h>

// More nastiness: the GC wants to define TRUE and FALSE.  We don't
// need the Java definitions (themselves a hack), so we undefine them.
#undef TRUE
#undef FALSE

extern "C"
{
#include <private/gc_pmark.h>
#include <gc_gcj.h>

#ifdef THREAD_LOCAL_ALLOC
# define GC_REDIRECT_TO_LOCAL
# include <gc_local_alloc.h>
#endif

  // These aren't declared in any Boehm GC header.
  void GC_finalize_all (void);
  ptr_t GC_debug_generic_malloc (size_t size, int k, GC_EXTRA_PARAMS);
};

// We must check for plausibility ourselves.
#define MAYBE_MARK(Obj, Top, Limit, Source, Exit)  \
	Top=GC_MARK_AND_PUSH((GC_PTR)Obj, Top, Limit, (GC_PTR *)Source)

// `kind' index used when allocating Java arrays.
static int array_kind_x;

// Freelist used for Java arrays.
static ptr_t *array_free_list;

// Lock used to protect access to Boehm's GC_enable/GC_disable functions.
static _Jv_Mutex_t disable_gc_mutex;



// This is called by the GC during the mark phase.  It marks a Java
// object.  We use `void *' arguments and return, and not what the
// Boehm GC wants, to avoid pollution in our headers.
void *
_Jv_MarkObj (void *addr, void *msp, void *msl, void * /* env */)
{
  mse *mark_stack_ptr = (mse *) msp;
  mse *mark_stack_limit = (mse *) msl;
  jobject obj = (jobject) addr;

  // FIXME: if env is 1, this object was allocated through the debug
  // interface, and addr points to the beginning of the debug header.
  // In that case, we should really add the size of the header to addr.

  _Jv_VTable *dt = *(_Jv_VTable **) addr;
  // The object might not yet have its vtable set, or it might
  // really be an object on the freelist.  In either case, the vtable slot
  // will either be 0, or it will point to a cleared object.
  // This assumes Java objects have size at least 3 words,
  // including the header.   But this should remain true, since this
  // should only be used with debugging allocation or with large objects.
  if (__builtin_expect (! dt || !(dt -> get_finalizer()), false))
    return mark_stack_ptr;
  jclass klass = dt->clas;
  ptr_t p;

# ifndef JV_HASH_SYNCHRONIZATION
    // Every object has a sync_info pointer.
    p = (ptr_t) obj->sync_info;
    MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, obj, o1label);
# endif
  // Mark the object's class.
  p = (ptr_t) klass;
  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, obj, o2label);

  if (__builtin_expect (klass == &java::lang::Class::class$, false))
    {
      // Currently we allocate some of the memory referenced from class objects
      // as pointerfree memory, and then mark it more intelligently here.
      // We ensure that the ClassClass mark descriptor forces invocation of
      // this procedure.
      // Correctness of this is subtle, but it looks OK to me for now.  For the incremental
      // collector, we need to make sure that the class object is written whenever
      // any of the subobjects are altered and may need rescanning.  This may be tricky
      // during construction, and this may not be the right way to do this with
      // incremental collection.
      // If we overflow the mark stack, we will rescan the class object, so we should
      // be OK.  The same applies if we redo the mark phase because win32 unmapped part
      // of our root set.		- HB
      jclass c = (jclass) addr;

      p = (ptr_t) c->name;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c3label);
      p = (ptr_t) c->superclass;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c4label);
      for (int i = 0; i < c->constants.size; ++i)
	{
	  /* FIXME: We could make this more precise by using the tags -KKT */
	  p = (ptr_t) c->constants.data[i].p;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c5label);
	}

#ifdef INTERPRETER
      if (_Jv_IsInterpretedClass (c))
	{
	  p = (ptr_t) c->constants.tags;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c5alabel);
	  p = (ptr_t) c->constants.data;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c5blabel);
	  p = (ptr_t) c->vtable;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c5clabel);
	}
#endif

      // If the class is an array, then the methods field holds a
      // pointer to the element class.  If the class is primitive,
      // then the methods field holds a pointer to the array class.
      p = (ptr_t) c->methods;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c6label);


      if (! c->isArray() && ! c->isPrimitive())
	{
	  // Scan each method in the cases where `methods' really
	  // points to a methods structure.
	  for (int i = 0; i < c->method_count; ++i)
	    {
	      p = (ptr_t) c->methods[i].name;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c,
			     cm1label);
	      p = (ptr_t) c->methods[i].signature;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c,
			     cm2label);

	      // FIXME: `ncode' entry?

#ifdef INTERPRETER
	      // The interpreter installs a heap-allocated
	      // trampoline here, so we'll mark it. 
	      if (_Jv_IsInterpretedClass (c))
		  {
		      p = (ptr_t) c->methods[i].ncode;
		      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c,
				  cm3label);
		  }
#endif
	    }
	}

      // Mark all the fields.
      p = (ptr_t) c->fields;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c8label);
      for (int i = 0; i < c->field_count; ++i)
	{
	  _Jv_Field* field = &c->fields[i];

#ifndef COMPACT_FIELDS
	  p = (ptr_t) field->name;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c8alabel);
#endif
	  p = (ptr_t) field->type;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c8blabel);

	  // For the interpreter, we also need to mark the memory
	  // containing static members
	  if ((field->flags & java::lang::reflect::Modifier::STATIC))
	    {
	      p = (ptr_t) field->u.addr;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c8clabel);

	      // also, if the static member is a reference,
	      // mark also the value pointed to.  We check for isResolved
	      // since marking can happen before memory is allocated for
	      // static members.
	      if (JvFieldIsRef (field) && field->isResolved()) 
		{
		  jobject val = *(jobject*) field->u.addr;
		  p = (ptr_t) val;
		  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit,
			      c, c8elabel);
		}
	    }
	}

      p = (ptr_t) c->vtable;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, c9label);
      p = (ptr_t) c->interfaces;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, cAlabel);
      for (int i = 0; i < c->interface_count; ++i)
	{
	  p = (ptr_t) c->interfaces[i];
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, cClabel);
	}
      p = (ptr_t) c->loader;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, cBlabel);
      p = (ptr_t) c->arrayclass;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c, cDlabel);

#ifdef INTERPRETER
      if (_Jv_IsInterpretedClass (c))
	{
	  _Jv_InterpClass* ic = (_Jv_InterpClass*)c;

	  p = (ptr_t) ic->interpreted_methods;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, ic, cElabel);

	  for (int i = 0; i < c->method_count; i++)
	    {
	      p = (ptr_t) ic->interpreted_methods[i];
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, ic, \
			  cFlabel);
	    }

	  p = (ptr_t) ic->field_initializers;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, ic, cGlabel);
	  
	}
#endif

    }
  else
    {
      // NOTE: each class only holds information about the class
      // itself.  So we must do the marking for the entire inheritance
      // tree in order to mark all fields.  FIXME: what about
      // interfaces?  We skip Object here, because Object only has a
      // sync_info, and we handled that earlier.
      // Note: occasionally `klass' can be null.  For instance, this
      // can happen if a GC occurs between the point where an object
      // is allocated and where the vtbl slot is set.
      while (klass && klass != &java::lang::Object::class$)
	{
	  jfieldID field = JvGetFirstInstanceField (klass);
	  jint max = JvNumInstanceFields (klass);

	  for (int i = 0; i < max; ++i)
	    {
	      if (JvFieldIsRef (field))
		{
		  jobject val = JvGetObjectField (obj, field);
		  p = (ptr_t) val;
		  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit,
			      obj, elabel);
		}
	      field = field->getNextField ();
	    }
	  klass = klass->getSuperclass();
	}
    }

  return mark_stack_ptr;
}

// This is called by the GC during the mark phase.  It marks a Java
// array (of objects).  We use `void *' arguments and return, and not
// what the Boehm GC wants, to avoid pollution in our headers.
void *
_Jv_MarkArray (void *addr, void *msp, void *msl, void * /*env*/)
{
  mse *mark_stack_ptr = (mse *) msp;
  mse *mark_stack_limit = (mse *) msl;
  jobjectArray array = (jobjectArray) addr;

  _Jv_VTable *dt = *(_Jv_VTable **) addr;
  // Assumes size >= 3 words.  That's currently true since arrays have
  // a vtable, sync pointer, and size.  If the sync pointer goes away,
  // we may need to round up the size.
  if (__builtin_expect (! dt || !(dt -> get_finalizer()), false))
    return mark_stack_ptr;
  jclass klass = dt->clas;
  ptr_t p;

# ifndef JV_HASH_SYNCHRONIZATION
    // Every object has a sync_info pointer.
    p = (ptr_t) array->sync_info;
    MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, array, e1label);
# endif
  // Mark the object's class.
  p = (ptr_t) klass;
  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, &(dt -> clas), o2label);

  for (int i = 0; i < JvGetArrayLength (array); ++i)
    {
      jobject obj = elements (array)[i];
      p = (ptr_t) obj;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, array, e2label);
    }

  return mark_stack_ptr;
}

// Generate a GC marking descriptor for a class.
//
// We assume that the gcj mark proc has index 0.  This is a dubious assumption,
// since another one could be registered first.  But the compiler also
// knows this, so in that case everything else will break, too.
#define GCJ_DEFAULT_DESCR GC_MAKE_PROC(GC_GCJ_RESERVED_MARK_PROC_INDEX,0)
void *
_Jv_BuildGCDescr(jclass)
{
  /* FIXME: We should really look at the class and build the descriptor. */
  return (void *)(GCJ_DEFAULT_DESCR);
}

// Allocate some space that is known to be pointer-free.
void *
_Jv_AllocBytes (jsize size)
{
  void *r = GC_MALLOC_ATOMIC (size);
  // We have to explicitly zero memory here, as the GC doesn't
  // guarantee that PTRFREE allocations are zeroed.  Note that we
  // don't have to do this for other allocation types because we set
  // the `ok_init' flag in the type descriptor.
  memset (r, 0, size);
  return r;
}

// Allocate space for a new Java array.
// Used only for arrays of objects.
void *
_Jv_AllocArray (jsize size, jclass klass)
{
  void *obj;
  const jsize min_heap_addr = 16*1024;
  // A heuristic.  If size is less than this value, the size
  // stored in the array can't possibly be misinterpreted as
  // a pointer.   Thus we lose nothing by scanning the object
  // completely conservatively, since no misidentification can
  // take place.
  
#ifdef GC_DEBUG
  // There isn't much to lose by scanning this conservatively.
  // If we didn't, the mark proc would have to understand that
  // it needed to skip the header.
  obj = GC_MALLOC(size);
#else
  if (size < min_heap_addr) 
    obj = GC_MALLOC(size);
  else 
    obj = GC_generic_malloc (size, array_kind_x);
#endif
  *((_Jv_VTable **) obj) = klass->vtable;
  return obj;
}

/* Allocate space for a new non-Java object, which does not have the usual 
   Java object header but may contain pointers to other GC'ed objects. */
void *
_Jv_AllocRawObj (jsize size)
{
  return (void *) GC_MALLOC (size);
}

static void
call_finalizer (GC_PTR obj, GC_PTR client_data)
{
  _Jv_FinalizerFunc *fn = (_Jv_FinalizerFunc *) client_data;
  jobject jobj = (jobject) obj;

  (*fn) (jobj);
}

void
_Jv_RegisterFinalizer (void *object, _Jv_FinalizerFunc *meth)
{
  GC_REGISTER_FINALIZER_NO_ORDER (object, call_finalizer, (GC_PTR) meth,
				  NULL, NULL);
}

void
_Jv_RunFinalizers (void)
{
  GC_invoke_finalizers ();
}

void
_Jv_RunAllFinalizers (void)
{
  GC_finalize_all ();
}

void
_Jv_RunGC (void)
{
  GC_gcollect ();
}

long
_Jv_GCTotalMemory (void)
{
  return GC_get_heap_size ();
}

long
_Jv_GCFreeMemory (void)
{
  return GC_get_free_bytes ();
}

void
_Jv_GCSetInitialHeapSize (size_t size)
{
  size_t current = GC_get_heap_size ();
  if (size > current)
    GC_expand_hp (size - current);
}

void
_Jv_GCSetMaximumHeapSize (size_t size)
{
  GC_set_max_heap_size ((GC_word) size);
}

// From boehm's misc.c 
extern "C" void GC_enable();
extern "C" void GC_disable();

void
_Jv_DisableGC (void)
{
  _Jv_MutexLock (&disable_gc_mutex); 
  GC_disable();
  _Jv_MutexUnlock (&disable_gc_mutex); 
}

void
_Jv_EnableGC (void)
{
  _Jv_MutexLock (&disable_gc_mutex); 
  GC_enable();
  _Jv_MutexUnlock (&disable_gc_mutex); 
}

static void * handle_out_of_memory(size_t)
{
  _Jv_ThrowNoMemory();
}

void
_Jv_InitGC (void)
{
  int proc;

  // Ignore pointers that do not point to the start of an object.
  GC_all_interior_pointers = 0;

  // Configure the collector to use the bitmap marking descriptors that we
  // stash in the class vtable.
  GC_init_gcj_malloc (0, (void *) _Jv_MarkObj);  

  // Cause an out of memory error to be thrown from the allocators,
  // instead of returning 0.  This is cheaper than checking on allocation.
  GC_oom_fn = handle_out_of_memory;

  GC_java_finalization = 1;

  // We use a different mark procedure for object arrays. This code 
  // configures a different object `kind' for object array allocation and
  // marking. FIXME: see above.
  array_free_list = (ptr_t *) GC_generic_malloc_inner ((MAXOBJSZ + 1)
						       * sizeof (ptr_t),
						       PTRFREE);
  memset (array_free_list, 0, (MAXOBJSZ + 1) * sizeof (ptr_t));

  proc = GC_n_mark_procs++;
  GC_mark_procs[proc] = (GC_mark_proc) _Jv_MarkArray;

  array_kind_x = GC_n_kinds++;
  GC_obj_kinds[array_kind_x].ok_freelist = array_free_list;
  GC_obj_kinds[array_kind_x].ok_reclaim_list = 0;
  GC_obj_kinds[array_kind_x].ok_descriptor = GC_MAKE_PROC (proc, 0);
  GC_obj_kinds[array_kind_x].ok_relocate_descr = FALSE;
  GC_obj_kinds[array_kind_x].ok_init = TRUE;

  _Jv_MutexInit (&disable_gc_mutex);
}

#ifdef JV_HASH_SYNCHRONIZATION
// Allocate an object with a fake vtable pointer, which causes only
// the first field (beyond the fake vtable pointer) to be traced.
// Eventually this should probably be generalized.

static _Jv_VTable trace_one_vtable = {
    0, 			// class pointer
    (void *)(2 * sizeof(void *)),
			// descriptor; scan 2 words incl. vtable ptr.
			// Least significant bits must be zero to
			// identify this as a length descriptor
    {0}			// First method
};

void *
_Jv_AllocTraceOne (jsize size /* includes vtable slot */) 
{
  return GC_GCJ_MALLOC (size, &trace_one_vtable);
}

// Ditto for two words.
// the first field (beyond the fake vtable pointer) to be traced.
// Eventually this should probably be generalized.

static _Jv_VTable trace_two_vtable =
{
  0, 			// class pointer
  (void *)(3 * sizeof(void *)),
			// descriptor; scan 3 words incl. vtable ptr.
  {0}			// First method
};

void *
_Jv_AllocTraceTwo (jsize size /* includes vtable slot */) 
{
  return GC_GCJ_MALLOC (size, &trace_two_vtable);
}

#endif /* JV_HASH_SYNCHRONIZATION */

void
_Jv_GCInitializeFinalizers (void (*notifier) (void))
{
  GC_finalize_on_demand = 1;
  GC_finalizer_notifier = notifier;
}

void
_Jv_GCRegisterDisappearingLink (jobject *objp)
{
  GC_general_register_disappearing_link ((GC_PTR *) objp, (GC_PTR) *objp);
}

jboolean
_Jv_GCCanReclaimSoftReference (jobject)
{
  // For now, always reclaim soft references.  FIXME.
  return true;
}
