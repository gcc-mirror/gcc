// resolve.cc - Code for linking and resolving classes and pool entries.

/* Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>
#include <platform.h>

#include <java-interp.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <string.h>
#include <java-cpool.h>
#include <java/lang/Class.h>
#include <java/lang/String.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/Thread.h>
#include <java/lang/InternalError.h>
#include <java/lang/VirtualMachineError.h>
#include <java/lang/NoSuchFieldError.h>
#include <java/lang/NoSuchMethodError.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/IllegalAccessError.h>
#include <java/lang/AbstractMethodError.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/VMClassLoader.h>
#include <java/lang/reflect/Modifier.h>

using namespace gcj;

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

#ifdef INTERPRETER

static void throw_internal_error (char *msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (char *msg)
	__attribute__ ((__noreturn__));

static int get_alignment_from_class (jclass);

static _Jv_ResolvedMethod* 
_Jv_BuildResolvedMethod (_Jv_Method*,
			 jclass,
			 jboolean,
			 jint);


static void throw_incompatible_class_change_error (jstring msg)
{
  throw new java::lang::IncompatibleClassChangeError (msg);
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
	  // This exception is specified in JLS 2nd Ed, section 5.1.
	  throw new java::lang::NoClassDefFoundError (str);
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
	  throw new java::lang::IllegalAccessError (found->getName());
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

	      if (_Jv_CheckAccess (klass, cls, field->flags))
		{
		  /* resove the field using the class' own loader
		     if necessary */

		  if (!field->isResolved ())
		    _Jv_ResolveField (field, cls->loader);

		  if (field_type != 0 && field->type != field_type)
		    throw new java::lang::LinkageError
		      (JvNewStringLatin1 
		       ("field type mismatch with different loaders"));

		  the_field = field;
		  goto end_of_field_search;
		}
	      else
		{
		  throw new java::lang::IllegalAccessError;
		}
	    }
	}

    end_of_field_search:
      if (the_field == 0)
	{
	  java::lang::StringBuffer *sb = new java::lang::StringBuffer();
	  sb->append(JvNewStringLatin1("field "));
	  sb->append(owner->getName());
	  sb->append(JvNewStringLatin1("."));
	  sb->append(_Jv_NewStringUTF(field_name->data));
	  sb->append(JvNewStringLatin1(" was not found."));
	  throw_incompatible_class_change_error(sb->toString());
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

      _Jv_Method *the_method = 0;
      jclass found_class = 0;

      // First search the class itself.
      the_method = _Jv_SearchMethodInClass (owner, klass, 
					    method_name, method_signature);

      if (the_method != 0)
        {
	  found_class = owner;
          goto end_of_method_search;
	}

      // If we are resolving an interface method, search the
      // interface's superinterfaces (A superinterface is not an
      // interface's superclass - a superinterface is implemented by
      // the interface).
      if (pool->tags[index] == JV_CONSTANT_InterfaceMethodref)
        {
	  _Jv_ifaces ifaces;
	  ifaces.count = 0;
	  ifaces.len = 4;
	  ifaces.list = (jclass *) _Jv_Malloc (ifaces.len * sizeof (jclass *));

	  _Jv_GetInterfaces (owner, &ifaces);	  

	  for (int i = 0; i < ifaces.count; i++)
	    {
	      jclass cls = ifaces.list[i];
	      the_method = _Jv_SearchMethodInClass (cls, klass, method_name, 
	                                            method_signature);
	      if (the_method != 0)
	        {
		  found_class = cls;
                  break;
		}
	    }

	  _Jv_Free (ifaces.list);

	  if (the_method != 0)
	    goto end_of_method_search;
	}

      // Finally, search superclasses. 
      for (jclass cls = owner->getSuperclass (); cls != 0; 
           cls = cls->getSuperclass ())
	{
	  the_method = _Jv_SearchMethodInClass (cls, klass, 
						method_name, method_signature);
          if (the_method != 0)
	    {
	      found_class = cls;
	      break;
	    }
	}

    end_of_method_search:
    
      // FIXME: if (cls->loader != klass->loader), then we
      // must actually check that the types of arguments
      // correspond.  That is, for each argument type, and
      // the return type, doing _Jv_FindClassFromSignature
      // with either loader should produce the same result,
      // i.e., exactly the same jclass object. JVMS 5.4.3.3    
    
      if (the_method == 0)
	{
	  java::lang::StringBuffer *sb = new java::lang::StringBuffer();
	  sb->append(JvNewStringLatin1("method "));
	  sb->append(owner->getName());
	  sb->append(JvNewStringLatin1("."));
	  sb->append(_Jv_NewStringUTF(method_name->data));
	  sb->append(JvNewStringLatin1(" was not found."));
	  throw new java::lang::NoSuchMethodError (sb->toString());
	}
      
      int vtable_index = -1;
      if (pool->tags[index] != JV_CONSTANT_InterfaceMethodref)
	vtable_index = (jshort)the_method->index;

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

// Find a method declared in the cls that is referenced from klass and
// perform access checks.
_Jv_Method *
_Jv_SearchMethodInClass (jclass cls, jclass klass, 
                         _Jv_Utf8Const *method_name, 
			 _Jv_Utf8Const *method_signature)
{
  using namespace java::lang::reflect;

  for (int i = 0;  i < cls->method_count;  i++)
    {
      _Jv_Method *method = &cls->methods[i];
      if (   (!_Jv_equalUtf8Consts (method->name,
				    method_name))
	  || (!_Jv_equalUtf8Consts (method->signature,
				    method_signature)))
	continue;

      if (_Jv_CheckAccess (klass, cls, method->accflags))
	return method;
      else
	throw new java::lang::IllegalAccessError;
    }
  return 0;
}

// A helper for _Jv_PrepareClass.  This adds missing `Miranda methods'
// to a class.
void
_Jv_PrepareMissingMethods (jclass base2, jclass iface_class)
{
  _Jv_InterpClass *base = reinterpret_cast<_Jv_InterpClass *> (base2);
  for (int i = 0; i < iface_class->interface_count; ++i)
    {
      for (int j = 0; j < iface_class->interfaces[i]->method_count; ++j)
	{
	  _Jv_Method *meth = &iface_class->interfaces[i]->methods[j];
	  // Don't bother with <clinit>.
	  if (meth->name->data[0] == '<')
	    continue;
	  _Jv_Method *new_meth = _Jv_LookupDeclaredMethod (base, meth->name,
							   meth->signature);
	  if (! new_meth)
	    {
	      // We assume that such methods are very unlikely, so we
	      // just reallocate the method array each time one is
	      // found.  This greatly simplifies the searching --
	      // otherwise we have to make sure that each such method
	      // found is really unique among all superinterfaces.
	      int new_count = base->method_count + 1;
	      _Jv_Method *new_m
		= (_Jv_Method *) _Jv_AllocBytes (sizeof (_Jv_Method)
						 * new_count);
	      memcpy (new_m, base->methods,
		      sizeof (_Jv_Method) * base->method_count);

	      // Add new method.
	      new_m[base->method_count] = *meth;
	      new_m[base->method_count].index = (_Jv_ushort) -1;
	      new_m[base->method_count].accflags
		|= java::lang::reflect::Modifier::INVISIBLE;

	      _Jv_MethodBase **new_im
		= (_Jv_MethodBase **) _Jv_AllocBytes (sizeof (_Jv_MethodBase *)
						      * new_count);
	      memcpy (new_im, base->interpreted_methods,
		      sizeof (_Jv_MethodBase *) * base->method_count);

	      base->methods = new_m;
	      base->interpreted_methods = new_im;
	      base->method_count = new_count;
	    }
	}

      _Jv_PrepareMissingMethods (base, iface_class->interfaces[i]);
    }
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

  // Make sure super-class is linked.  This involves taking a lock on
  // the super class, so we use the Java method resolveClass, which
  // will unlock it properly, should an exception happen.  If there's
  // no superclass, do nothing -- Object will already have been
  // resolved.

  if (klass->superclass)
    java::lang::VMClassLoader::resolveClass (klass->superclass);

  _Jv_InterpClass *clz = (_Jv_InterpClass*)klass;

  /************ PART ONE: OBJECT LAYOUT ***************/

  // Compute the alignment for this type by searching through the
  // superclasses and finding the maximum required alignment.  We
  // could consider caching this in the Class.
  int max_align = __alignof__ (java::lang::Object);
  jclass super = clz->superclass;
  while (super != NULL)
    {
      int num = JvNumInstanceFields (super);
      _Jv_Field *field = JvGetFirstInstanceField (super);
      while (num > 0)
	{
	  int field_align = get_alignment_from_class (field->type);
	  if (field_align > max_align)
	    max_align = field_align;
	  ++field;
	  --num;
	}
      super = super->superclass;
    }

  int instance_size;
  int static_size = 0;

  // Although java.lang.Object is never interpreted, an interface can
  // have a null superclass.  Note that we have to lay out an
  // interface because it might have static fields.
  if (clz->superclass)
    instance_size = clz->superclass->size();
  else
    instance_size = java::lang::Object::class$.size();

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
	  if (field_align > max_align)
	    max_align = field_align;
	}
    }

  // Set the instance size for the class.  Note that first we round it
  // to the alignment required for this object; this keeps us in sync
  // with our current ABI.
  instance_size = ROUND (instance_size, max_align);
  clz->size_in_bytes = instance_size;

  // allocate static memory
  if (static_size != 0)
    {
      char *static_data = (char*)_Jv_AllocBytes (static_size);

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
	  _Jv_VerifyMethod (im);
	  clz->methods[i].ncode = im->ncode ();

	  // Resolve ctable entries pointing to this method.  See
	  // _Jv_Defer_Resolution.
	  void **code = (void **)imeth->deferred;
	  while (code)
	    {
	      void **target = (void **)*code;
	      *code = clz->methods[i].ncode;
	      code = target;
	    }
	}
    }

  if ((clz->accflags & Modifier::INTERFACE))
    {
      clz->state = JV_STATE_PREPARED;
      clz->notifyAll ();
      return;
    }

  // A class might have so-called "Miranda methods".  This is a method
  // that is declared in an interface and not re-declared in an
  // abstract class.  Some compilers don't emit declarations for such
  // methods in the class; this will give us problems since we expect
  // a declaration for any method requiring a vtable entry.  We handle
  // this here by searching for such methods and constructing new
  // internal declarations for them.  We only need to do this for
  // abstract classes.
  if ((clz->accflags & Modifier::ABSTRACT))
    _Jv_PrepareMissingMethods (clz, clz);

  clz->vtable_method_count = -1;
  _Jv_MakeVTable (clz);

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
      if (! (field->type == &StringClass
 	     || field->type == &java::lang::Class::class$))
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

template<typename T>
struct aligner
{
  T field;
};

#define ALIGNOF(TYPE) (__alignof__ (((aligner<TYPE> *) 0)->field))

// This returns the alignment of a type as it would appear in a
// structure.  This can be different from the alignment of the type
// itself.  For instance on x86 double is 8-aligned but struct{double}
// is 4-aligned.
static int
get_alignment_from_class (jclass klass)
{
  if (klass == JvPrimClass (byte))
    return ALIGNOF (jbyte);
  else if (klass == JvPrimClass (short))
    return ALIGNOF (jshort);
  else if (klass == JvPrimClass (int)) 
    return ALIGNOF (jint);
  else if (klass == JvPrimClass (long))
    return ALIGNOF (jlong);
  else if (klass == JvPrimClass (boolean))
    return ALIGNOF (jboolean);
  else if (klass == JvPrimClass (char))
    return ALIGNOF (jchar);
  else if (klass == JvPrimClass (float))
    return ALIGNOF (jfloat);
  else if (klass == JvPrimClass (double))
    return ALIGNOF (jdouble);
  else
    return ALIGNOF (jobject);
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

int 
_Jv_count_arguments (_Jv_Utf8Const *signature,
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

#if FFI_NATIVE_RAW_API
#   define FFI_PREP_RAW_CLOSURE ffi_prep_raw_closure
#   define FFI_RAW_SIZE ffi_raw_size
#else
#   define FFI_PREP_RAW_CLOSURE ffi_prep_java_raw_closure
#   define FFI_RAW_SIZE ffi_java_raw_size
#endif

/* we put this one here, and not in interpret.cc because it
 * calls the utility routines _Jv_count_arguments 
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
  int arg_count = _Jv_count_arguments (self->signature, staticp);

  ncode_closure *closure =
    (ncode_closure*)_Jv_AllocBytes (sizeof (ncode_closure)
					+ arg_count * sizeof (ffi_type*));

  init_cif (self->signature,
	    arg_count,
	    staticp,
	    &closure->cif,
	    &closure->arg_types[0],
	    NULL);

  ffi_closure_fun fun;

  args_raw_size = FFI_RAW_SIZE (&closure->cif);

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
      if (staticp)
	fun = (ffi_closure_fun)&_Jv_InterpMethod::run_class;
      else
	fun = (ffi_closure_fun)&_Jv_InterpMethod::run_normal;
    }

  FFI_PREP_RAW_CLOSURE (&closure->closure,
		        &closure->cif, 
		        fun,
		        (void*)this);

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
  int arg_count = _Jv_count_arguments (self->signature, staticp);

  ncode_closure *closure =
    (ncode_closure*)_Jv_AllocBytes (sizeof (ncode_closure)
				    + arg_count * sizeof (ffi_type*));

  ffi_type *rtype;
  init_cif (self->signature,
	    arg_count,
	    staticp,
	    &closure->cif,
	    &closure->arg_types[0],
	    &rtype);

  ffi_closure_fun fun;

  args_raw_size = FFI_RAW_SIZE (&closure->cif);

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

  if (ffi_prep_cif (&jni_cif, _Jv_platform_ffi_abi,
		    extra_args + arg_count, rtype,
		    jni_arg_types) != FFI_OK)
    throw_internal_error ("ffi_prep_cif failed for JNI function");

  JvAssert ((self->accflags & Modifier::NATIVE) != 0);

  // FIXME: for now we assume that all native methods for
  // interpreted code use JNI.
  fun = (ffi_closure_fun) &_Jv_JNIMethod::call;

  FFI_PREP_RAW_CLOSURE (&closure->closure,
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
  int arg_count = _Jv_count_arguments (method->signature, staticp);

  _Jv_ResolvedMethod* result = (_Jv_ResolvedMethod*)
    _Jv_AllocBytes (sizeof (_Jv_ResolvedMethod)
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
  throw (msg
	 ? new java::lang::ClassFormatError (msg)
	 : new java::lang::ClassFormatError);
}

static void
throw_class_format_error (char *msg)
{
  throw_class_format_error (JvNewStringLatin1 (msg));
}

static void
throw_internal_error (char *msg)
{
  throw new java::lang::InternalError (JvNewStringLatin1 (msg));
}


#endif /* INTERPRETER */
