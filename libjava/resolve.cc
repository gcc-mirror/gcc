// resolve.cc - Code for linking and resolving classes and pool entries.

/* Copyright (C) 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>

#include <java-interp.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <string.h>
#include <java-cpool.h>
#include <java/lang/Class.h>
#include <java/lang/String.h>
#include <java/lang/Thread.h>
#include <java/lang/InternalError.h>
#include <java/lang/VirtualMachineError.h>
#include <java/lang/NoSuchFieldError.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/IllegalAccessError.h>
#include <java/lang/AbstractMethodError.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/reflect/Modifier.h>

#ifdef INTERPRETER

static void throw_internal_error (char *msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (char *msg)
	__attribute__ ((__noreturn__));

#define ClassObject _CL_Q34java4lang6Object
extern java::lang::Class ClassObject;
#define ObjectClass _CL_Q34java4lang6Object
extern java::lang::Class ObjectClass;


static int get_alignment_from_class (jclass);

static _Jv_ResolvedMethod* 
_Jv_BuildResolvedMethod (_Jv_Method*,
			 jclass,
			 jboolean,
			 jint);


// We need to know the name of a constructor.
static _Jv_Utf8Const *init_name = _Jv_makeUtf8Const ("<init>", 6);

static void throw_incompatible_class_change_error (jstring msg)
{
  JvThrow (new java::lang::IncompatibleClassChangeError (msg));
}

_Jv_word
_Jv_ResolvePoolEntry (jclass klass, int index)
{
  using namespace java::lang::reflect;

  _Jv_Constants *pool = &klass->constants;

  if ((pool->tags[index] & JV_CONSTANT_ResolvedFlag) != 0)
    return pool->data[index];

  switch (pool->tags[index]) {
  case JV_CONSTANT_Class:
    {
      _Jv_Utf8Const *name = pool->data[index].utf8;

      jclass found;
      if (name->data[0] == '[')
	found = _Jv_FindClassFromSignature (&name->data[0],
					    klass->loader);
      else
	found = _Jv_FindClass (name, klass->loader);

      if (! found)
	{
	  jstring str = _Jv_NewStringUTF (name->data);
	  JvThrow (new java::lang::ClassNotFoundException (str));
	}

      if ((found->accflags & Modifier::PUBLIC) == Modifier::PUBLIC
	  || (_Jv_ClassNameSamePackage (found->name,
					klass->name)))
	{
	  pool->data[index].clazz = found;
	  pool->tags[index] |= JV_CONSTANT_ResolvedFlag;
	}
      else
	{
	  JvThrow (new java::lang::IllegalAccessError (found->getName()));
	}
    }
    break;

  case JV_CONSTANT_String:
    {
      jstring str;
      str = _Jv_NewStringUtf8Const (pool->data[index].utf8);
      pool->data[index].o = str;
      pool->tags[index] |= JV_CONSTANT_ResolvedFlag;
    }
    break;


  case JV_CONSTANT_Fieldref:
    {
      _Jv_ushort class_index, name_and_type_index;
      _Jv_loadIndexes (&pool->data[index],
		       class_index,
		       name_and_type_index);
      jclass owner = (_Jv_ResolvePoolEntry (klass, class_index)).clazz;

      if (owner != klass)
	_Jv_InitClass (owner);

      _Jv_ushort name_index, type_index;
      _Jv_loadIndexes (&pool->data[name_and_type_index],
		       name_index,
		       type_index);

      _Jv_Utf8Const *field_name = pool->data[name_index].utf8;
      _Jv_Utf8Const *field_type_name = pool->data[type_index].utf8;

      // FIXME: The implementation of this function
      // (_Jv_FindClassFromSignature) will generate an instance of
      // _Jv_Utf8Const for each call if the field type is a class name
      // (Lxx.yy.Z;).  This may be too expensive to do for each and
      // every fieldref being resolved.  For now, we fix the problem by
      // only doing it when we have a loader different from the class
      // declaring the field.

      jclass field_type = 0;

      if (owner->loader != klass->loader)
	field_type = _Jv_FindClassFromSignature (field_type_name->data,
						 klass->loader);
      
      _Jv_Field* the_field = 0;

      for (jclass cls = owner; cls != 0; cls = cls->getSuperclass ())
	{
	  for (int i = 0;  i < cls->field_count;  i++)
	    {
	      _Jv_Field *field = &cls->fields[i];
	      if (! _Jv_equalUtf8Consts (field->name, field_name))
		continue;

	      // now, check field access. 

	      if (   (cls == klass)
		  || ((field->flags & Modifier::PUBLIC) != 0)
		  || (((field->flags & Modifier::PROTECTED) != 0)
		      && cls->isAssignableFrom (klass))
		  || (((field->flags & Modifier::PRIVATE) == 0)
		      && _Jv_ClassNameSamePackage (cls->name,
						   klass->name)))
		{
		  /* resove the field using the class' own loader
		     if necessary */

		  if (!field->isResolved ())
		    _Jv_ResolveField (field, cls->loader);

		  if (field_type != 0 && field->type != field_type)
		    JvThrow
		      (new java::lang::LinkageError
		       (JvNewStringLatin1 
			("field type mismatch with different loaders")));

		  the_field = field;
		  goto end_of_field_search;
		}
	      else
		{
		  JvThrow (new java::lang::IllegalAccessError);
		}
	    }
	}

    end_of_field_search:
      if (the_field == 0)
	{
	  jstring msg = JvNewStringLatin1 ("field ");
	  msg = msg->concat (owner->getName ());
	  msg = msg->concat (JvNewStringLatin1("."));
	  msg = msg->concat (_Jv_NewStringUTF (field_name->data));
	  msg = msg->concat (JvNewStringLatin1(" was not found."));
	  throw_incompatible_class_change_error (msg);
	}

      pool->data[index].field = the_field;
      pool->tags[index] |= JV_CONSTANT_ResolvedFlag;
    }
    break;

  case JV_CONSTANT_Methodref:
  case JV_CONSTANT_InterfaceMethodref:
    {
      _Jv_ushort class_index, name_and_type_index;
      _Jv_loadIndexes (&pool->data[index],
		       class_index,
		       name_and_type_index);
      jclass owner = (_Jv_ResolvePoolEntry (klass, class_index)).clazz;

      if (owner != klass)
	_Jv_InitClass (owner);

      _Jv_ushort name_index, type_index;
      _Jv_loadIndexes (&pool->data[name_and_type_index],
		       name_index,
		       type_index);

      _Jv_Utf8Const *method_name = pool->data[name_index].utf8;
      _Jv_Utf8Const *method_signature = pool->data[type_index].utf8;

      int vtable_index = -1;
      _Jv_Method *the_method = 0;
      jclass found_class = 0;

      // we make a loop here, because methods are allowed to be moved to
      // a super class, and still be visible.. (binary compatibility).

      for (jclass cls = owner; cls != 0; cls = cls->getSuperclass ())
	{
	  for (int i = 0;  i < cls->method_count;  i++)
	    {
	      _Jv_Method *method = &cls->methods[i];
	      if (   (!_Jv_equalUtf8Consts (method->name,
					    method_name))
		  || (!_Jv_equalUtf8Consts (method->signature,
					    method_signature)))
		continue;

	      if (cls == klass 
		  || ((method->accflags & Modifier::PUBLIC) != 0)
		  || (((method->accflags & Modifier::PROTECTED) != 0)
		      && cls->isAssignableFrom (klass))
		  || (((method->accflags & Modifier::PRIVATE) == 0)
		      && _Jv_ClassNameSamePackage (cls->name,
						   klass->name)))
		{
		  // FIXME: if (cls->loader != klass->loader), then we
		  // must actually check that the types of arguments
		  // correspond.  That is, for each argument type, and
		  // the return type, doing _Jv_FindClassFromSignature
		  // with either loader should produce the same result,
		  // i.e., exactly the same jclass object. JVMS 5.4.3.3

		  the_method = method;
		  found_class = cls;

		  
		  if (pool->tags[index] == JV_CONSTANT_InterfaceMethodref)
		    vtable_index = -1;
		  else
		    vtable_index = _Jv_DetermineVTableIndex
		      (cls, method_name, method_signature);

		  if (vtable_index == 0)
		    throw_incompatible_class_change_error
		      (JvNewStringLatin1 ("method not found"));

		  goto end_of_method_search;
		}
	      else
		{
		  JvThrow (new java::lang::IllegalAccessError);
		}
	    }
	}

    end_of_method_search:
      if (the_method == 0)
	{
	  jstring msg = JvNewStringLatin1 ("method ");
	  msg = msg->concat (owner->getName ());
	  msg = msg->concat (JvNewStringLatin1("."));
	  msg = msg->concat (_Jv_NewStringUTF (method_name->data));
	  msg = msg->concat (JvNewStringLatin1(" was not found."));
	  JvThrow(new java::lang::NoSuchFieldError (msg));
	}
      
      pool->data[index].rmethod = 
	_Jv_BuildResolvedMethod(the_method,
				found_class,
				(the_method->accflags & Modifier::STATIC) != 0,
				vtable_index);
      pool->tags[index] |= JV_CONSTANT_ResolvedFlag;
    }
    break;

  }

  return pool->data[index];
}


void
_Jv_ResolveField (_Jv_Field *field, java::lang::ClassLoader *loader)
{
  if (! field->isResolved ())
    {
      _Jv_Utf8Const *sig = (_Jv_Utf8Const*)field->type;
      field->type = _Jv_FindClassFromSignature (sig->data, loader);
      field->flags &= ~_Jv_FIELD_UNRESOLVED_FLAG;
    }
}

/** FIXME: this is a terribly inefficient algorithm!  It would improve
    things if compiled classes to know vtable offset, and _Jv_Method had
    a field for this.

    Returns 0  if this class does not declare the given method.
    Returns -1 if the given method does not appear in the vtable.
               i.e., it is static, private, final or a constructor.
    Otherwise, returns the vtable index.  */
int 
_Jv_DetermineVTableIndex (jclass klass,
			  _Jv_Utf8Const *name,
			  _Jv_Utf8Const *signature)
{
  using namespace java::lang::reflect;

  jclass super_class = klass->getSuperclass ();

  if (super_class != NULL)
    {
      int prev = _Jv_DetermineVTableIndex (super_class,
					   name,
					   signature);
      if (prev != 0)
	return prev;
    }

  /* at this point, we know that the super-class does not declare
   * the method.  Otherwise, the above call would have found it, and
   * determined the result of this function (-1 or some positive
   * number).
   */

  _Jv_Method *meth = _Jv_GetMethodLocal (klass, name, signature);

  /* now, if we do not declare this method, return zero */
  if (meth == NULL)
    return 0;

  /* so now, we know not only that the super class does not declare the
   * method, but we do!  So, this is a first declaration of the method. */

  /* now, the checks for things that are declared in this class, but do
   * not go into the vtable.  There are three cases.  
   * 1) the method is static, private or final
   * 2) the class itself is final, or
   * 3) it is the method <init>
   */

  if ((meth->accflags & (Modifier::STATIC
			 | Modifier::PRIVATE
			 | Modifier::FINAL)) != 0
      || (klass->accflags & Modifier::FINAL) != 0
      || _Jv_equalUtf8Consts (name, init_name))
    return -1;

  /* reaching this point, we know for sure, that the method in question
   * will be in the vtable.  The question is where. */

  /* the base offset, is where we will start assigning vtable
   * indexes for this class.  It is 1 for base classes
   * (vtable->method[0] is unused), and for non-base classes it is the
   * number of entries in the super class' vtable plus 1. */

  int base_offset;
  if (super_class == 0)
    base_offset = 1;
  else
    base_offset = super_class->vtable_method_count+1;

  /* we will consider methods 0..this_method_index-1.  And for each one,
   * determine if it is new (i.e., if it appears in the super class),
   * and if it should go in the vtable.  If so, increment base_offset */

  int this_method_index = meth - (&klass->methods[0]);

  for (int i = 0; i < this_method_index; i++)
    {
      _Jv_Method *m = &klass->methods[i];

      /* fist some checks for things that surely do not go in the
       * vtable */

      if ((m->accflags & (Modifier::STATIC | Modifier::PRIVATE)) != 0)
	continue;
      if (_Jv_equalUtf8Consts (m->name, init_name))
	continue;
      
      /* Then, we need to know if this method appears in the
         superclass. (This is where this function gets expensive) */
      _Jv_Method *sm = _Jv_LookupDeclaredMethod (super_class,
						 m->name,
						 m->signature);
      
      /* if it was somehow declared in the superclass, skip this */
      if (sm != NULL)
	continue;

      /* but if it is final, and not declared in the super class,
       * then we also skip it */
      if ((m->accflags & Modifier::FINAL) != 0)
	continue;

      /* finally, we can assign the index of this method */
      /* m->vtable_index = base_offset */
      base_offset += 1;
    }

  return base_offset;
}

/* this is installed in place of abstract methods */
static void
_Jv_abstractMethodError ()
{
  JvThrow (new java::lang::AbstractMethodError);
}

void 
_Jv_PrepareClass(jclass klass)
{
  using namespace java::lang::reflect;

 /*
  * The job of this function is to: 1) assign storage to fields, and 2)
  * build the vtable.  static fields are assigned real memory, instance
  * fields are assigned offsets.
  *
  * NOTE: we have a contract with the garbage collector here.  Static
  * reference fields must not be resolved, until after they have storage
  * assigned which is the check used by the collector to see if it
  * should indirect the static field reference and mark the object
  * pointed to. 
  *
  * Most fields are resolved lazily (i.e. have their class-type
  * assigned) when they are accessed the first time by calling as part
  * of _Jv_ResolveField, which is allways called after _Jv_PrepareClass.
  * Static fields with initializers are resolved as part of this
  * function, as are fields with primitive types.
  */

  if (! _Jv_IsInterpretedClass (klass))
    return;

  if (klass->state >= JV_STATE_PREPARED)
    return;

  // make sure super-class is linked.  This involves taking a lock on
  // the super class, so we use the Java method resolveClass, which will
  // unlock it properly, should an exception happen.

  java::lang::ClassLoader::resolveClass0 (klass->superclass);

  _Jv_InterpClass *clz = (_Jv_InterpClass*)klass;

  /************ PART ONE: OBJECT LAYOUT ***************/

  int instance_size;
  int static_size;

  // java.lang.Object is never interpreted!
  instance_size = clz->superclass->size ();
  static_size   = 0;

  for (int i = 0; i < clz->field_count; i++)
    {
      int field_size;
      int field_align;

      _Jv_Field *field = &clz->fields[i];

      if (! field->isRef ())
	{
	  // it's safe to resolve the field here, since it's 
	  // a primitive class, which does not cause loading to happen.
	  _Jv_ResolveField (field, clz->loader);

	  field_size = field->type->size ();
	  field_align = get_alignment_from_class (field->type);
	}
      else 
	{
	  field_size = sizeof (jobject);
	  field_align = __alignof__ (jobject);
	}

#ifndef COMPACT_FIELDS
      field->bsize = field_size;
#endif

      if (field->flags & Modifier::STATIC)
	{
	  /* this computes an offset into a region we'll allocate 
	     shortly, and then add this offset to the start address */

	  static_size        = ROUND (static_size, field_align);
	  field->u.boffset   = static_size;
	  static_size       += field_size;
	}
      else
	{
	  instance_size      = ROUND (instance_size, field_align);
	  field->u.boffset   = instance_size;
	  instance_size     += field_size;
	}
    }

  // set the instance size for the class
  clz->size_in_bytes = instance_size;
    
  // allocate static memory
  if (static_size != 0)
    {
      char *static_data = (char*)_Jv_AllocBytesChecked (static_size);

      memset (static_data, 0, static_size);

      for (int i = 0; i < clz->field_count; i++)
	{
	  _Jv_Field *field = &clz->fields[i];

	  if ((field->flags & Modifier::STATIC) != 0)
	    {
	      field->u.addr  = static_data + field->u.boffset;
			    
	      if (clz->field_initializers[i] != 0)
		{
		  _Jv_ResolveField (field, clz->loader);
		  _Jv_InitField (0, clz, i);
		}
	    }
	}

      // now we don't need the field_initializers anymore, so let the
      // collector get rid of it!

      clz->field_initializers = 0;
    }

  /************ PART TWO: VTABLE LAYOUT ***************/

  /* preparation: build the vtable stubs (even interfaces can)
     have code -- for static constructors. */
  for (int i = 0; i < clz->method_count; i++)
    {
      _Jv_MethodBase *imeth = clz->interpreted_methods[i];

      if ((clz->methods[i].accflags & Modifier::NATIVE) != 0)
	{
	  // You might think we could use a virtual `ncode' method in
	  // the _Jv_MethodBase and unify the native and non-native
	  // cases.  Well, we can't, because we don't allocate these
	  // objects using `new', and thus they don't get a vtable.
	  _Jv_JNIMethod *jnim = reinterpret_cast<_Jv_JNIMethod *> (imeth);
	  clz->methods[i].ncode = jnim->ncode ();
	}
      else if (imeth != 0)		// it could be abstract
	{
	  _Jv_InterpMethod *im = reinterpret_cast<_Jv_InterpMethod *> (imeth);
	  clz->methods[i].ncode = im->ncode ();
	}
    }

  if (clz->accflags & Modifier::INTERFACE)
    {
      clz->state = JV_STATE_PREPARED;
      clz->notifyAll ();
      return;
    }

  /* Now onto the actual job: vtable layout.  First, count how many new
     methods we have */
  int new_method_count = 0;

  jclass super_class = clz->getSuperclass ();

  if (super_class == 0)
    throw_internal_error ("cannot handle interpreted base classes");

  for (int i = 0; i < clz->method_count; i++)
    {
      _Jv_Method *this_meth = &clz->methods[i];

      if ((this_meth->accflags & (Modifier::STATIC | Modifier::PRIVATE)) != 0
	  || _Jv_equalUtf8Consts (this_meth->name, init_name))
	{
	  /* skip this, it doesn't go in the vtable */
	  continue;
	}
	  
      _Jv_Method *orig_meth = _Jv_LookupDeclaredMethod (super_class,
							this_meth->name,
							this_meth->signature);

      if (orig_meth == 0)
	{
	  // new methods that are final, also don't go in the vtable
	  if ((this_meth->accflags & Modifier::FINAL) != 0)
	    continue;

	  new_method_count += 1;
	  continue;
	}

      if ((orig_meth->accflags & (Modifier::STATIC
				  | Modifier::PRIVATE
				  | Modifier::FINAL)) != 0
	  || ((orig_meth->accflags & Modifier::ABSTRACT) == 0
	      && (this_meth->accflags & Modifier::ABSTRACT) != 0
	      && (klass->accflags & Modifier::ABSTRACT) == 0))
	{
	  clz->state = JV_STATE_ERROR;
	  clz->notifyAll ();
	  JvThrow (new java::lang::IncompatibleClassChangeError 
		           (clz->getName ()));
	}

      /* FIXME: At this point, if (loader != super_class->loader), we
       * need to "impose class loader constraints" for the types
       * involved in the signature of this method */
    }
  
  /* determine size */
  int vtable_count = (super_class->vtable_method_count) + new_method_count;
  clz->vtable_method_count = vtable_count;

  /* allocate vtable structure */
  _Jv_VTable *vtable = (_Jv_VTable*) 
    _Jv_AllocBytesChecked (sizeof (_Jv_VTable) 
			   + (sizeof (void*) * (vtable_count)));
  vtable->clas = clz;

  {
    jclass effective_superclass = super_class;

    /* If super_class is abstract or an interface it has no vtable.
       We need to find a real one... */
    while (effective_superclass && effective_superclass->vtable == NULL)
      effective_superclass = effective_superclass->superclass;

    /* copy super class' vtable entries (index 0 goes unused). */
    if (effective_superclass && effective_superclass->vtable)
      memcpy ((void*)&vtable->method[1],
	      (void*)&effective_superclass->vtable->method[1],
	      sizeof (void*) * effective_superclass->vtable_method_count);
  }

  /* now, install our own vtable entries, reprise... */
  for (int i = 0; i < clz->method_count; i++)
    {
      _Jv_Method *this_meth = &clz->methods[i];

      int index = _Jv_DetermineVTableIndex (clz, 
					    this_meth->name,
					    this_meth->signature);

      if (index == 0)
	throw_internal_error ("method now found in own class");

      if (index != -1)
	{
	  if (index > clz->vtable_method_count+1)
	    throw_internal_error ("vtable problem...");

	  if (clz->interpreted_methods[i] == 0)
	    vtable->method[index] = (void*)&_Jv_abstractMethodError;
	  else
	    vtable->method[index] = this_meth->ncode;
	}
    }

  /* finally, assign the vtable! */
  clz->vtable = vtable;

  /* wooha! we're done. */
  clz->state = JV_STATE_PREPARED;
  clz->notifyAll ();
}

/** Do static initialization for fields with a constant initializer */
void
_Jv_InitField (jobject obj, jclass klass, int index)
{
  using namespace java::lang::reflect;

  if (obj != 0 && klass == 0)
    klass = obj->getClass ();

  if (!_Jv_IsInterpretedClass (klass))
    return;

  _Jv_InterpClass *clz = (_Jv_InterpClass*)klass;

  _Jv_Field * field = (&clz->fields[0]) + index;

  if (index > clz->field_count)
    throw_internal_error ("field out of range");

  int init = clz->field_initializers[index];
  if (init == 0)
    return;

  _Jv_Constants *pool = &clz->constants;
  int tag = pool->tags[init];

  if (! field->isResolved ())
    throw_internal_error ("initializing unresolved field");

  if (obj==0 && ((field->flags & Modifier::STATIC) == 0))
    throw_internal_error ("initializing non-static field with no object");

  void *addr = 0;

  if ((field->flags & Modifier::STATIC) != 0)
    addr = (void*) field->u.addr;
  else
    addr = (void*) (((char*)obj) + field->u.boffset);

  switch (tag)
    {
    case JV_CONSTANT_String:
      {
	_Jv_MonitorEnter (clz);
	jstring str;
	str = _Jv_NewStringUtf8Const (pool->data[init].utf8);
	pool->data[init].string = str;
	pool->tags[init] = JV_CONSTANT_ResolvedString;
	_Jv_MonitorExit (clz);
      }
      /* fall through */

    case JV_CONSTANT_ResolvedString:
      if (! (field->type == &StringClass || field->type == &ObjectClass))
	throw_class_format_error ("string initialiser to non-string field");

      *(jstring*)addr = pool->data[init].string;
      break;

    case JV_CONSTANT_Integer:
      {
	int value = pool->data[init].i;

	if (field->type == JvPrimClass (boolean))
	  *(jboolean*)addr = (jboolean)value;
	
	else if (field->type == JvPrimClass (byte))
	  *(jbyte*)addr = (jbyte)value;
	
	else if (field->type == JvPrimClass (char))
	  *(jchar*)addr = (jchar)value;

	else if (field->type == JvPrimClass (short))
	  *(jshort*)addr = (jshort)value;
	
	else if (field->type == JvPrimClass (int))
	  *(jint*)addr = (jint)value;

	else
	  throw_class_format_error ("erroneous field initializer");
      }  
      break;

    case JV_CONSTANT_Long:
      if (field->type != JvPrimClass (long))
	throw_class_format_error ("erroneous field initializer");

      *(jlong*)addr = _Jv_loadLong (&pool->data[init]);
      break;

    case JV_CONSTANT_Float:
      if (field->type != JvPrimClass (float))
	throw_class_format_error ("erroneous field initializer");

      *(jfloat*)addr = pool->data[init].f;
      break;

    case JV_CONSTANT_Double:
      if (field->type != JvPrimClass (double))
	throw_class_format_error ("erroneous field initializer");

      *(jdouble*)addr = _Jv_loadDouble (&pool->data[init]);
      break;

    default:
      throw_class_format_error ("erroneous field initializer");
    }
}

static int
get_alignment_from_class (jclass klass)
{
  if (klass == JvPrimClass (byte))
    return  __alignof__ (jbyte);
  else if (klass == JvPrimClass (short))
    return  __alignof__ (jshort);
  else if (klass == JvPrimClass (int)) 
    return  __alignof__ (jint);
  else if (klass == JvPrimClass (long))
    return  __alignof__ (jlong);
  else if (klass == JvPrimClass (boolean))
    return  __alignof__ (jboolean);
  else if (klass == JvPrimClass (char))
    return  __alignof__ (jchar);
  else if (klass == JvPrimClass (float))
    return  __alignof__ (jfloat);
  else if (klass == JvPrimClass (double))
    return  __alignof__ (jdouble);
  else
    return __alignof__ (jobject);
}


inline static unsigned char*
skip_one_type (unsigned char* ptr)
{
  int ch = *ptr++;

  while (ch == '[')
    { 
      ch = *ptr++;
    }
  
  if (ch == 'L')
    {
      do { ch = *ptr++; } while (ch != ';');
    }

  return ptr;
}

static ffi_type*
get_ffi_type_from_signature (unsigned char* ptr)
{
  switch (*ptr) 
    {
    case 'L':
    case '[':
      return &ffi_type_pointer;
      break;

    case 'Z':
      // On some platforms a bool is a byte, on others an int.
      if (sizeof (jboolean) == sizeof (jbyte))
	return &ffi_type_sint8;
      else
	{
	  JvAssert (sizeof (jbyte) == sizeof (jint));
	  return &ffi_type_sint32;
	}
      break;

    case 'B':
      return &ffi_type_sint8;
      break;
      
    case 'C':
      return &ffi_type_uint16;
      break;
	  
    case 'S': 
      return &ffi_type_sint16;
      break;
	  
    case 'I':
      return &ffi_type_sint32;
      break;
	  
    case 'J':
      return &ffi_type_sint64;
      break;
	  
    case 'F':
      return &ffi_type_float;
      break;
	  
    case 'D':
      return &ffi_type_double;
      break;

    case 'V':
      return &ffi_type_void;
      break;
    }

  throw_internal_error ("unknown type in signature");
}

/* this function yields the number of actual arguments, that is, if the
 * function is non-static, then one is added to the number of elements
 * found in the signature */

static int 
count_arguments (_Jv_Utf8Const *signature,
		 jboolean staticp)
{
  unsigned char *ptr = (unsigned char*) signature->data;
  int arg_count = staticp ? 0 : 1;

  /* first, count number of arguments */

  // skip '('
  ptr++;

  // count args
  while (*ptr != ')')
    {
      ptr = skip_one_type (ptr);
      arg_count += 1;
    }

  return arg_count;
}

/* This beast will build a cif, given the signature.  Memory for
 * the cif itself and for the argument types must be allocated by the
 * caller.
 */

static int 
init_cif (_Jv_Utf8Const* signature,
	  int arg_count,
	  jboolean staticp,
	  ffi_cif *cif,
	  ffi_type **arg_types,
	  ffi_type **rtype_p)
{
  unsigned char *ptr = (unsigned char*) signature->data;

  int arg_index = 0;		// arg number
  int item_count = 0;		// stack-item count

  // setup receiver
  if (!staticp)
    {
      arg_types[arg_index++] = &ffi_type_pointer;
      item_count += 1;
    }

  // skip '('
  ptr++;

  // assign arg types
  while (*ptr != ')')
    {
      arg_types[arg_index++] = get_ffi_type_from_signature (ptr);

      if (*ptr == 'J' || *ptr == 'D')
	item_count += 2;
      else
	item_count += 1;

      ptr = skip_one_type (ptr);
    }

  // skip ')'
  ptr++;
  ffi_type *rtype = get_ffi_type_from_signature (ptr);

  ptr = skip_one_type (ptr);
  if (ptr != (unsigned char*)signature->data + signature->length)
    throw_internal_error ("did not find end of signature");

  if (ffi_prep_cif (cif, FFI_DEFAULT_ABI,
		    arg_count, rtype, arg_types) != FFI_OK)
    throw_internal_error ("ffi_prep_cif failed");

  if (rtype_p != NULL)
    *rtype_p = rtype;

  return item_count;
}


/* we put this one here, and not in interpret.cc because it
 * calls the utility routines count_arguments 
 * which are static to this module.  The following struct defines the
 * layout we use for the stubs, it's only used in the ncode method. */

typedef struct {
  ffi_raw_closure  closure;
  ffi_cif   cif;
  ffi_type *arg_types[0];
} ncode_closure;

typedef void (*ffi_closure_fun) (ffi_cif*,void*,ffi_raw*,void*);

void *
_Jv_InterpMethod::ncode ()
{
  using namespace java::lang::reflect;

  if (self->ncode != 0)
    return self->ncode;

  jboolean staticp = (self->accflags & Modifier::STATIC) != 0;
  int arg_count = count_arguments (self->signature, staticp);

  ncode_closure *closure =
    (ncode_closure*)_Jv_AllocBytesChecked (sizeof (ncode_closure)
					+ arg_count * sizeof (ffi_type*));

  init_cif (self->signature,
	    arg_count,
	    staticp,
	    &closure->cif,
	    &closure->arg_types[0],
	    NULL);

  ffi_closure_fun fun;

  args_raw_size = ffi_raw_size (&closure->cif);

  JvAssert ((self->accflags & Modifier::NATIVE) == 0);

  if ((self->accflags & Modifier::SYNCHRONIZED) != 0)
    {
      if (staticp)
	fun = (ffi_closure_fun)&_Jv_InterpMethod::run_synch_class;
      else
	fun = (ffi_closure_fun)&_Jv_InterpMethod::run_synch_object; 
    }
  else
    {
      fun = (ffi_closure_fun)&_Jv_InterpMethod::run_normal;
    }

  ffi_prep_raw_closure (&closure->closure,
			&closure->cif, 
			fun,
			(void*) this);

  self->ncode = (void*)closure;
  return self->ncode;
}


void *
_Jv_JNIMethod::ncode ()
{
  using namespace java::lang::reflect;

  if (self->ncode != 0)
    return self->ncode;

  jboolean staticp = (self->accflags & Modifier::STATIC) != 0;
  int arg_count = count_arguments (self->signature, staticp);

  ncode_closure *closure =
    (ncode_closure*)_Jv_AllocBytesChecked (sizeof (ncode_closure)
					+ arg_count * sizeof (ffi_type*));

  ffi_type *rtype;
  init_cif (self->signature,
	    arg_count,
	    staticp,
	    &closure->cif,
	    &closure->arg_types[0],
	    &rtype);

  ffi_closure_fun fun;

  args_raw_size = ffi_raw_size (&closure->cif);

  // Initialize the argument types and CIF that represent the actual
  // underlying JNI function.
  int extra_args = 1;
  if ((self->accflags & Modifier::STATIC))
    ++extra_args;
  jni_arg_types = (ffi_type **) _Jv_Malloc ((extra_args + arg_count)
					    * sizeof (ffi_type *));
  int offset = 0;
  jni_arg_types[offset++] = &ffi_type_pointer;
  if ((self->accflags & Modifier::STATIC))
    jni_arg_types[offset++] = &ffi_type_pointer;
  memcpy (&jni_arg_types[offset], &closure->arg_types[0],
	  arg_count * sizeof (ffi_type *));

  if (ffi_prep_cif (&jni_cif, FFI_DEFAULT_ABI,
		    extra_args + arg_count, rtype,
		    jni_arg_types) != FFI_OK)
    throw_internal_error ("ffi_prep_cif failed for JNI function");

  JvAssert ((self->accflags & Modifier::NATIVE) != 0);

  // FIXME: for now we assume that all native methods for
  // interpreted code use JNI.
  fun = (ffi_closure_fun) &_Jv_JNIMethod::call;

  ffi_prep_raw_closure (&closure->closure,
			&closure->cif, 
			fun,
			(void*) this);

  self->ncode = (void *) closure;
  return self->ncode;
}


/* A _Jv_ResolvedMethod is what is put in the constant pool for a
 * MethodRef or InterfacemethodRef.  */
static _Jv_ResolvedMethod*
_Jv_BuildResolvedMethod (_Jv_Method* method,
			 jclass      klass,
			 jboolean staticp,
			 jint vtable_index)
{
  int arg_count = count_arguments (method->signature, staticp);

  _Jv_ResolvedMethod* result = (_Jv_ResolvedMethod*)
    _Jv_AllocBytesChecked (sizeof (_Jv_ResolvedMethod)
			   + arg_count*sizeof (ffi_type*));

  result->stack_item_count
    = init_cif (method->signature,
		arg_count,
		staticp,
		&result->cif,
		&result->arg_types[0],
		NULL);

  result->vtable_index        = vtable_index;
  result->method              = method;
  result->klass               = klass;

  return result;
}


static void
throw_class_format_error (jstring msg)
{
  if (msg == 0)
    JvThrow (new java::lang::ClassFormatError);
  else
    JvThrow (new java::lang::ClassFormatError (msg));
}

static void
throw_class_format_error (char *msg)
{
  throw_class_format_error (JvNewStringLatin1 (msg));
}

static void
throw_internal_error (char *msg)
{
  JvThrow 
    (new java::lang::InternalError (JvNewStringLatin1 (msg)));
}


#endif /* INTERPRETER */
