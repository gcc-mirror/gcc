// boehm.cc - interface between libjava and Boehm GC.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>
#include <limits.h>

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
#include <gc_config.h>

// Set GC_DEBUG before including gc.h!
#ifdef LIBGCJ_GC_DEBUG
# define GC_DEBUG
#endif

#include <gc_mark.h>
#include <gc_gcj.h>
#include <javaxfc.h>  // GC_finalize_all declaration.  

#ifdef THREAD_LOCAL_ALLOC
# define GC_REDIRECT_TO_LOCAL
# include <gc_local_alloc.h>
#endif

  // From boehm's misc.c 
  void GC_enable();
  void GC_disable();
};

#define MAYBE_MARK(Obj, Top, Limit, Source)  \
	Top=GC_MARK_AND_PUSH((GC_PTR) Obj, Top, Limit, (GC_PTR *) Source)

// `kind' index used when allocating Java arrays.
static int array_kind_x;

// Freelist used for Java arrays.
static void **array_free_list;



// This is called by the GC during the mark phase.  It marks a Java
// object.  We use `void *' arguments and return, and not what the
// Boehm GC wants, to avoid pollution in our headers.
void *
_Jv_MarkObj (void *addr, void *msp, void *msl, void *env)
{
  struct GC_ms_entry *mark_stack_ptr = (struct GC_ms_entry *)msp;
  struct GC_ms_entry *mark_stack_limit = (struct GC_ms_entry *)msl;

  if (env == (void *)1) /* Object allocated with debug allocator.	*/
    addr = (GC_PTR)GC_USR_PTR_FROM_BASE(addr);
  jobject obj = (jobject) addr;

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
  GC_PTR p;

# ifndef JV_HASH_SYNCHRONIZATION
    // Every object has a sync_info pointer.
    p = (GC_PTR) obj->sync_info;
    MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, obj);
# endif
  // Mark the object's class.
  p = (GC_PTR) klass;
  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, obj);

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

      p = (GC_PTR) c->name;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      p = (GC_PTR) c->superclass;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      for (int i = 0; i < c->constants.size; ++i)
	{
	  /* FIXME: We could make this more precise by using the tags -KKT */
	  p = (GC_PTR) c->constants.data[i].p;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
	}

#ifdef INTERPRETER
      if (_Jv_IsInterpretedClass (c))
	{
	  p = (GC_PTR) c->constants.tags;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
	  p = (GC_PTR) c->constants.data;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
	}
#endif

      // The vtable might be allocated even for compiled code.
      p = (GC_PTR) c->vtable;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

      // If the class is an array, then the methods field holds a
      // pointer to the element class.  If the class is primitive,
      // then the methods field holds a pointer to the array class.
      p = (GC_PTR) c->methods;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

      // The vtable might have been set, but the rest of the class
      // could still be uninitialized.  If this is the case, then
      // c.isArray will SEGV.  We check for this, and if it is the
      // case we just return.
      if (__builtin_expect (c->name == NULL, false))
	return mark_stack_ptr;

      if (! c->isArray() && ! c->isPrimitive())
	{
	  // Scan each method in the cases where `methods' really
	  // points to a methods structure.
	  for (int i = 0; i < c->method_count; ++i)
	    {
	      p = (GC_PTR) c->methods[i].name;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
	      p = (GC_PTR) c->methods[i].signature;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

	      // Note that we don't have to mark each individual throw
	      // separately, as these are stored in the constant pool.
	      p = (GC_PTR) c->methods[i].throws;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
	    }
	}

      // Mark all the fields.
      p = (GC_PTR) c->fields;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      for (int i = 0; i < c->field_count; ++i)
	{
	  _Jv_Field* field = &c->fields[i];

	  p = (GC_PTR) field->name;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
	  p = (GC_PTR) field->type;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

	  // For the interpreter, we also need to mark the memory
	  // containing static members
	  if ((field->flags & java::lang::reflect::Modifier::STATIC))
	    {
	      p = (GC_PTR) field->u.addr;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

	      // also, if the static member is a reference,
	      // mark also the value pointed to.  We check for isResolved
	      // since marking can happen before memory is allocated for
	      // static members.
	      // Note that field->u.addr may be null if the class c is
	      // JV_STATE_LOADED but not JV_STATE_PREPARED (initialized).
	      if (JvFieldIsRef (field) && p && field->isResolved()) 
		{
		  jobject val = *(jobject*) p;
		  p = (GC_PTR) val;
		  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
		}
	    }
	}

      p = (GC_PTR) c->vtable;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      p = (GC_PTR) c->interfaces;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      for (int i = 0; i < c->interface_count; ++i)
	{
	  p = (GC_PTR) c->interfaces[i];
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
	}
      p = (GC_PTR) c->loader;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

      // The dispatch tables can be allocated at runtime.
      p = (GC_PTR) c->ancestors;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      if (c->idt)
	{
	  p = (GC_PTR) c->idt;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

	  if (c->isInterface())
	    {
	      p = (GC_PTR) c->idt->iface.ioffsets;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c->idt);
	    }
	  else if (! c->isPrimitive())
	    {
	      // This field is only valid for ordinary classes.
	      p = (GC_PTR) c->idt->cls.itable;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c->idt);
	    }
	}

      p = (GC_PTR) c->arrayclass;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      p = (GC_PTR) c->protectionDomain;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      p = (GC_PTR) c->hack_signers;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);
      p = (GC_PTR) c->aux_info;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

#ifdef INTERPRETER
      if (_Jv_IsInterpretedClass (c) && c->aux_info)
	{
	  _Jv_InterpClass* ic = (_Jv_InterpClass*) c->aux_info;

	  p = (GC_PTR) ic->interpreted_methods;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, ic);

	  for (int i = 0; i < c->method_count; i++)
	    {
	      // The interpreter installs a heap-allocated trampoline
	      // here, so we'll mark it.
	      p = (GC_PTR) c->methods[i].ncode;
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, c);

	      using namespace java::lang::reflect;

	      // Mark the direct-threaded code.  Note a subtlety here:
	      // when we add Miranda methods to a class, we don't
	      // resize its interpreted_methods array.  If we try to
	      // reference one of these methods, we may crash.
	      // However, we know these are all abstract, and we know
	      // that abstract methods have nothing useful in this
	      // array.  So, we skip all abstract methods to avoid the
	      // problem.  FIXME: this is pretty obscure, it may be
	      // better to add a methods to the execution engine and
	      // resize the array.
	      if ((c->methods[i].accflags & Modifier::ABSTRACT) != 0)
		continue;

	      p = (GC_PTR) ic->interpreted_methods[i];
	      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, ic);

	      if ((c->methods[i].accflags & Modifier::NATIVE) != 0)
		{
		  _Jv_JNIMethod *jm
		    = (_Jv_JNIMethod *) ic->interpreted_methods[i];
		  if (jm)
		    {
		      p = (GC_PTR) jm->jni_arg_types;
		      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, p);
		    }
		}
	      else
		{
		  _Jv_InterpMethod *im
		    = (_Jv_InterpMethod *) ic->interpreted_methods[i];
		  if (im)
		    {
		      p = (GC_PTR) im->prepared;
		      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, ic);
		    }
		}
	    }

	  p = (GC_PTR) ic->field_initializers;
	  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, ic);
	  
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
		  p = (GC_PTR) val;
		  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, obj);
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
_Jv_MarkArray (void *addr, void *msp, void *msl, void *env)
{
  struct GC_ms_entry *mark_stack_ptr = (struct GC_ms_entry *)msp;
  struct GC_ms_entry *mark_stack_limit = (struct GC_ms_entry *)msl;

  if (env == (void *)1) /* Object allocated with debug allocator.	*/
    addr = (void *)GC_USR_PTR_FROM_BASE(addr);
  jobjectArray array = (jobjectArray) addr;

  _Jv_VTable *dt = *(_Jv_VTable **) addr;
  // Assumes size >= 3 words.  That's currently true since arrays have
  // a vtable, sync pointer, and size.  If the sync pointer goes away,
  // we may need to round up the size.
  if (__builtin_expect (! dt || !(dt -> get_finalizer()), false))
    return mark_stack_ptr;
  jclass klass = dt->clas;
  GC_PTR p;

# ifndef JV_HASH_SYNCHRONIZATION
    // Every object has a sync_info pointer.
    p = (GC_PTR) array->sync_info;
    MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, array);
# endif
  // Mark the object's class.
  p = (GC_PTR) klass;
  MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, &(dt -> clas));

  for (int i = 0; i < JvGetArrayLength (array); ++i)
    {
      jobject obj = elements (array)[i];
      p = (GC_PTR) obj;
      MAYBE_MARK (p, mark_stack_ptr, mark_stack_limit, array);
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
_Jv_BuildGCDescr(jclass self)
{
  jlong desc = 0;
  jint bits_per_word = CHAR_BIT * sizeof (void *);

  // Note: for now we only consider a bitmap mark descriptor.  We
  // could also handle the case where the first N fields of a type are
  // references.  However, this is not very likely to be used by many
  // classes, and it is easier to compute things this way.

  // The vtable pointer.
  desc |= 1ULL << (bits_per_word - 1);
#ifndef JV_HASH_SYNCHRONIZATION
  // The sync_info field.
  desc |= 1ULL << (bits_per_word - 2);
#endif

  for (jclass klass = self; klass != NULL; klass = klass->getSuperclass())
    {
      jfieldID field = JvGetFirstInstanceField(klass);
      int count = JvNumInstanceFields(klass);

      for (int i = 0; i < count; ++i)
	{
	  if (field->isRef())
	    {
	      unsigned int off = field->getOffset();
	      // If we run into a weird situation, we bail.
	      if (off % sizeof (void *) != 0)
		return (void *) (GCJ_DEFAULT_DESCR);
	      off /= sizeof (void *);
	      // If we find a field outside the range of our bitmap,
	      // fall back to procedure marker. The bottom 2 bits are
	      // reserved.
	      if (off >= (unsigned) bits_per_word - 2)
		return (void *) (GCJ_DEFAULT_DESCR);
	      desc |= 1ULL << (bits_per_word - off - 1);
	    }

	  field = field->getNextField();
	}
    }

  // For bitmap mark type, bottom bits are 01.
  desc |= 1;
  // Bogus warning avoidance (on many platforms).
  return (void *) (unsigned long) desc;
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

#ifdef LIBGCJ_GC_DEBUG

void *
_Jv_AllocObj (jsize size, jclass klass)
{
  return GC_GCJ_MALLOC (size, klass->vtable);
}

void *
_Jv_AllocPtrFreeObj (jsize size, jclass klass)
{
#ifdef JV_HASH_SYNCHRONIZATION
  void * obj = GC_MALLOC_ATOMIC(size);
  *((_Jv_VTable **) obj) = klass->vtable;
#else
  void * obj = GC_GCJ_MALLOC(size, klass->vtable);
#endif
  return obj;
}

#endif /* LIBGCJ_GC_DEBUG */
// In the non-debug case, the above two functions are defined
// as inline functions in boehm-gc.h.  In the debug case we
// really want to take advantage of the definitions in gc_gcj.h.

// Allocate space for a new Java array.
// Used only for arrays of objects.
void *
_Jv_AllocArray (jsize size, jclass klass)
{
  void *obj;

#ifdef LIBGCJ_GC_DEBUG
  // There isn't much to lose by scanning this conservatively.
  // If we didn't, the mark proc would have to understand that
  // it needed to skip the header.
  obj = GC_MALLOC(size);
#else
  const jsize min_heap_addr = 16*1024;
  // A heuristic.  If size is less than this value, the size
  // stored in the array can't possibly be misinterpreted as
  // a pointer.   Thus we lose nothing by scanning the object
  // completely conservatively, since no misidentification can
  // take place.
  
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

void
_Jv_DisableGC (void)
{
  GC_disable();
}

void
_Jv_EnableGC (void)
{
  GC_enable();
}

static void * handle_out_of_memory(size_t)
{
  _Jv_ThrowNoMemory();
}

static void
gcj_describe_type_fn(void *obj, char *out_buf)
{
  _Jv_VTable *dt = *(_Jv_VTable **) obj;

  if (! dt /* Shouldn't happen */)
    {
      strcpy(out_buf, "GCJ (bad)");
      return;
    }
  jclass klass = dt->clas;
  if (!klass /* shouldn't happen */)
    {
      strcpy(out_buf, "GCJ (bad)");
      return;
    }
  jstring name = klass -> getName();
  size_t len = name -> length();
  if (len >= GC_TYPE_DESCR_LEN) len = GC_TYPE_DESCR_LEN - 1;
  JvGetStringUTFRegion (name, 0, len, out_buf);
  out_buf[len] = '\0';
}

void
_Jv_InitGC (void)
{
  int proc;

  // Ignore pointers that do not point to the start of an object.
  GC_all_interior_pointers = 0;

  // Configure the collector to use the bitmap marking descriptors that we
  // stash in the class vtable.
  // We always use mark proc descriptor 0, since the compiler knows
  // about it.
  GC_init_gcj_malloc (0, (void *) _Jv_MarkObj);  

  // Cause an out of memory error to be thrown from the allocators,
  // instead of returning 0.  This is cheaper than checking on allocation.
  GC_oom_fn = handle_out_of_memory;

  GC_java_finalization = 1;

  // We use a different mark procedure for object arrays. This code 
  // configures a different object `kind' for object array allocation and
  // marking.
  array_free_list = GC_new_free_list();
  proc = GC_new_proc((GC_mark_proc)_Jv_MarkArray);
  array_kind_x = GC_new_kind(array_free_list, GC_MAKE_PROC (proc, 0), 0, 1);

  // Arrange to have the GC print Java class names in backtraces, etc.
  GC_register_describe_type_fn(GC_gcj_kind, gcj_describe_type_fn);
  GC_register_describe_type_fn(GC_gcj_debug_kind, gcj_describe_type_fn);
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
  // This test helps to ensure that we meet a precondition of
  // GC_general_register_disappearing_link, viz. "Obj must be a
  // pointer to the first word of an object we allocated."
  if (GC_base(*objp))
    GC_general_register_disappearing_link ((GC_PTR *) objp, (GC_PTR) *objp);
}

jboolean
_Jv_GCCanReclaimSoftReference (jobject)
{
  // For now, always reclaim soft references.  FIXME.
  return true;
}
