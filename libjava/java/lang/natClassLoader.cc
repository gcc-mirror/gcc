// natClassLoader.cc - Implementation of java.lang.ClassLoader native methods.

/* Copyright (C) 1999, 2000, 2001, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>

#include <stdlib.h>
#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java-threads.h>
#include <java-interp.h>

#include <java/lang/Character.h>
#include <java/lang/Thread.h>
#include <java/lang/ClassLoader.h>
#include <gnu/gcj/runtime/VMClassLoader.h>
#include <java/lang/InternalError.h>
#include <java/lang/IllegalAccessError.h>
#include <java/lang/LinkageError.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/lang/ClassCircularityError.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/VirtualMachineError.h>
#include <java/lang/VMClassLoader.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/Runtime.h>
#include <java/lang/StringBuffer.h>
#include <java/io/Serializable.h>
#include <java/lang/Cloneable.h>

/////////// java.lang.ClassLoader native methods ////////////

java::lang::Class *
java::lang::ClassLoader::defineClass0 (jstring name,
				       jbyteArray data, 
				       jint offset,
				       jint length,
				       java::security::ProtectionDomain *pd)
{
#ifdef INTERPRETER
  jclass klass;
  klass = (jclass) JvAllocObject (&java::lang::Class::class$,
				  sizeof (_Jv_InterpClass));
  _Jv_InitNewClassFields (klass);

  // Synchronize on the class, so that it is not attempted initialized
  // until we're done loading.
  JvSynchronize sync (klass);

  // Record the defining loader.  For the system class loader, we
  // record NULL.
  if (this != java::lang::ClassLoader::getSystemClassLoader())
    klass->loader = this;

  if (name != 0)
    {
      _Jv_Utf8Const *name2 = _Jv_makeUtf8Const (name);

      if (! _Jv_VerifyClassName (name2))
	throw new java::lang::ClassFormatError
	  (JvNewStringLatin1 ("erroneous class name"));

      klass->name = name2;
    }

  try
    {
      _Jv_DefineClass (klass, data, offset, length);
    }
  catch (java::lang::Throwable *ex)
    {
      klass->state = JV_STATE_ERROR;
      klass->notifyAll ();

      _Jv_UnregisterClass (klass);

      // If EX is not a ClassNotFoundException, that's ok, because we
      // account for the possibility in defineClass().
      throw ex;
    }
    
  klass->protectionDomain = pd;

  // if everything proceeded sucessfully, we're loaded.
  JvAssert (klass->state == JV_STATE_LOADED);

  return klass;

#else // INTERPRETER

  return 0;
#endif
}

void
_Jv_WaitForState (jclass klass, int state)
{
  if (klass->state >= state)
    return;
  
  _Jv_MonitorEnter (klass) ;

  if (state == JV_STATE_LINKED)
    {
      // Must call _Jv_PrepareCompiledClass while holding the class
      // mutex.
      _Jv_PrepareCompiledClass (klass);
      _Jv_MonitorExit (klass);
      return;
    }
	
  java::lang::Thread *self = java::lang::Thread::currentThread();

  // this is similar to the strategy for class initialization.
  // if we already hold the lock, just leave.
  while (klass->state <= state
	 && klass->thread 
	 && klass->thread != self)
    klass->wait ();

  _Jv_MonitorExit (klass);

  if (klass->state == JV_STATE_ERROR)
    throw new java::lang::LinkageError;
}

// Finish linking a class.  Only called from ClassLoader::resolveClass.
void
java::lang::ClassLoader::linkClass0 (java::lang::Class *klass)
{
  if (klass->state >= JV_STATE_LINKED)
    return;

#ifdef INTERPRETER
  if (_Jv_IsInterpretedClass (klass))
    _Jv_PrepareClass (klass);
#endif

  _Jv_PrepareCompiledClass (klass);
}

void
java::lang::ClassLoader::markClassErrorState0 (java::lang::Class *klass)
{
  klass->state = JV_STATE_ERROR;
  klass->notifyAll ();
}

jclass
java::lang::VMClassLoader::defineClass (java::lang::ClassLoader *cl, 
					jstring name,
					jbyteArray data, 
					jint offset,
					jint length)
{
  return cl->defineClass (name, data, offset, length);
}

jclass
java::lang::VMClassLoader::getPrimitiveClass (jchar type)
{
  char sig[2];
  sig[0] = (char) type;
  sig[1] = '\0';
  return _Jv_FindClassFromSignature (sig, NULL);
}

/** This function does class-preparation for compiled classes.  
    NOTE: It contains replicated functionality from
    _Jv_ResolvePoolEntry, and this is intentional, since that function
    lives in resolve.cc which is entirely conditionally compiled.
 */
void
_Jv_PrepareCompiledClass (jclass klass)
{
  if (klass->state >= JV_STATE_LINKED)
    return;

  // Short-circuit, so that mutually dependent classes are ok.
  klass->state = JV_STATE_LINKED;

  _Jv_Constants *pool = &klass->constants;
  for (int index = 1; index < pool->size; ++index)
    {
      if (pool->tags[index] == JV_CONSTANT_Class)
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
	      throw new java::lang::NoClassDefFoundError (str);
	    }

	  pool->data[index].clazz = found;
	  pool->tags[index] |= JV_CONSTANT_ResolvedFlag;
	}
      else if (pool->tags[index] == JV_CONSTANT_String)
	{
	  jstring str;

	  str = _Jv_NewStringUtf8Const (pool->data[index].utf8);
	  pool->data[index].o = str;
	  pool->tags[index] |= JV_CONSTANT_ResolvedFlag;
	}
    }

#ifdef INTERPRETER
  // FIXME: although the comment up top says that this function is
  // only called for compiled classes, it is actually called for every
  // class.
  if (! _Jv_IsInterpretedClass (klass))
    {
#endif /* INTERPRETER */
      jfieldID f = JvGetFirstStaticField (klass);
      for (int n = JvNumStaticFields (klass); n > 0; --n)
	{
	  int mod = f->getModifiers ();
	  // If we have a static String field with a non-null initial
	  // value, we know it points to a Utf8Const.
	  if (f->getClass () == &java::lang::String::class$
	      && java::lang::reflect::Modifier::isStatic (mod))
	    {
	      jstring *strp = (jstring *) f->u.addr;
	      if (*strp)
		*strp = _Jv_NewStringUtf8Const ((_Jv_Utf8Const *) *strp);
	    }
	  f = f->getNextField ();
	}
#ifdef INTERPRETER
    }
#endif /* INTERPRETER */

  if (klass->vtable == NULL)
    _Jv_MakeVTable(klass);

  if (klass->otable != NULL && klass->otable->state == 0)
    _Jv_LinkOffsetTable(klass);

  klass->notifyAll ();

  _Jv_PushClass (klass);
}


//
//  A single class can have many "initiating" class loaders,
//  and a single "defining" class loader.  The Defining
//  class loader is what is returned from Class.getClassLoader()
//  and is used when loading dependent classes during resolution.
//  The set of initiating class loaders are used to ensure
//  safety of linking, and is maintained in the hash table
//  "initiated_classes".  A defining classloader is by definition also
//  initiating, so we only store classes in this table, if they have more
//  than one class loader associated.
//


// Size of local hash table.
#define HASH_LEN 1013

// Hash function for Utf8Consts.
#define HASH_UTF(Utf) (((Utf)->hash) % HASH_LEN)

struct _Jv_LoaderInfo
{
  _Jv_LoaderInfo          *next;
  java::lang::Class       *klass;
  java::lang::ClassLoader *loader;
};

static _Jv_LoaderInfo *initiated_classes[HASH_LEN];
static jclass loaded_classes[HASH_LEN];

// This is the root of a linked list of classes



jclass
_Jv_FindClassInCache (_Jv_Utf8Const *name, java::lang::ClassLoader *loader)
{
  JvSynchronize sync (&java::lang::Class::class$);
  jint hash = HASH_UTF (name);

  if (loader && loader == java::lang::ClassLoader::getSystemClassLoader())
    loader = NULL;

  // first, if LOADER is a defining loader, then it is also initiating
  jclass klass;
  for (klass = loaded_classes[hash]; klass; klass = klass->next)
    {
      if (loader == klass->loader && _Jv_equalUtf8Consts (name, klass->name))
	break;
    }

  // otherwise, it may be that the class in question was defined
  // by some other loader, but that the loading was initiated by 
  // the loader in question.
  if (!klass)
    {
      _Jv_LoaderInfo *info;
      for (info = initiated_classes[hash]; info; info = info->next)
	{
	  if (loader == info->loader
	      && _Jv_equalUtf8Consts (name, info->klass->name))
	    {
	      klass = info->klass;
	      break;
	    }
	}
    }

  return klass;
}

void
_Jv_UnregisterClass (jclass the_class)
{
  JvSynchronize sync (&java::lang::Class::class$);
  jint hash = HASH_UTF(the_class->name);

  jclass *klass = &(loaded_classes[hash]);
  for ( ; *klass; klass = &((*klass)->next))
    {
      if (*klass == the_class)
	{
	  *klass = (*klass)->next;
	  break;
	}
    }

  _Jv_LoaderInfo **info = &(initiated_classes[hash]);
  for ( ; ; info = &((*info)->next))
    {
      while (*info && (*info)->klass == the_class)
	{
	  _Jv_LoaderInfo *old = *info;
	  *info = (*info)->next;
	  _Jv_Free (old);
	}

      if (*info == NULL)
	break;
    }
}

void
_Jv_RegisterInitiatingLoader (jclass klass, java::lang::ClassLoader *loader)
{
  if (loader && loader == java::lang::ClassLoader::getSystemClassLoader())
    loader = NULL;

  // This information can't be visible to the GC.
  _Jv_LoaderInfo *info
    = (_Jv_LoaderInfo *) _Jv_Malloc (sizeof(_Jv_LoaderInfo));
  jint hash = HASH_UTF(klass->name);

  JvSynchronize sync (&java::lang::Class::class$);
  info->loader = loader;
  info->klass  = klass;
  info->next   = initiated_classes[hash];
  initiated_classes[hash] = info;
}

// This function is called many times during startup, before main() is
// run.  At that point in time we know for certain we are running 
// single-threaded, so we don't need to lock when adding classes to the 
// class chain.  At all other times, the caller should synchronize on
// Class::class$.
void
_Jv_RegisterClasses (jclass *classes)
{
  for (; *classes; ++classes)
    {
      jclass klass = *classes;

      (*_Jv_RegisterClassHook) (klass);

      // registering a compiled class causes
      // it to be immediately "prepared".  
      if (klass->state == JV_STATE_NOTHING)
	klass->state = JV_STATE_COMPILED;
    }
}

void
_Jv_RegisterClassHookDefault (jclass klass)
{
  jint hash = HASH_UTF (klass->name);

  jclass check_class = loaded_classes[hash];

  // If the class is already registered, don't re-register it.
  while (check_class != NULL)
    {
      if (check_class == klass)
	{
	  // If you get this, it means you have the same class in two
	  // different libraries.
#define TEXT "Duplicate class registration: "
	  // We size-limit MESSAGE so that you can't trash the stack.
	  char message[200];
	  strcpy (message, TEXT);
	  strncpy (message + sizeof (TEXT) - 1, klass->name->data,
		   sizeof (message) - sizeof (TEXT));
	  message[sizeof (message) - 1] = '\0';
	  if (! gcj::runtimeInitialized)
	    JvFail (message);
	  else
	    {
	      java::lang::String *str = JvNewStringLatin1 (message);
	      throw new java::lang::VirtualMachineError (str);
	    }
	}

      check_class = check_class->next;
    }

  klass->next = loaded_classes[hash];
  loaded_classes[hash] = klass;
}

// A pointer to a function that actually registers a class.
// Normally _Jv_RegisterClassHookDefault, but could be some other function
// that registers the class in e.g. a ClassLoader-local table.
// Should synchronize on Class:class$ while setting/restore this variable.

void (*_Jv_RegisterClassHook) (jclass cl) = _Jv_RegisterClassHookDefault;

void
_Jv_RegisterClass (jclass klass)
{
  jclass classes[2];
  classes[0] = klass;
  classes[1] = NULL;
  _Jv_RegisterClasses (classes);
}

jclass
_Jv_FindClass (_Jv_Utf8Const *name, java::lang::ClassLoader *loader)
{
  jclass klass = _Jv_FindClassInCache (name, loader);

  if (! klass)
    {
      jstring sname = _Jv_NewStringUTF (name->data);

      java::lang::ClassLoader *sys
	= java::lang::ClassLoader::getSystemClassLoader ();

      if (loader)
	{
	  // Load using a user-defined loader, jvmspec 5.3.2
	  klass = loader->loadClass(sname, false);

	  // If "loader" delegated the loadClass operation to another
	  // loader, explicitly register that it is also an initiating
	  // loader of the given class.
	  java::lang::ClassLoader *delegate = (loader == sys
					       ? NULL
					       : loader);
	  if (klass && klass->getClassLoaderInternal () != delegate)
	    _Jv_RegisterInitiatingLoader (klass, loader);
	}
      else 
	{
	  // Load using the bootstrap loader jvmspec 5.3.1.
	  klass = sys->loadClass (sname, false); 

	  // Register that we're an initiating loader.
	  if (klass)
	    _Jv_RegisterInitiatingLoader (klass, 0);
	}
    }
  else
    {
      // we need classes to be in the hash while
      // we're loading, so that they can refer to themselves. 
      _Jv_WaitForState (klass, JV_STATE_LOADED);
    }

  return klass;
}

void
_Jv_InitNewClassFields (jclass ret)
{
  ret->next = NULL;
  ret->name = NULL;
  ret->accflags = 0;
  ret->superclass = NULL;
  ret->constants.size = 0;
  ret->constants.tags = NULL;
  ret->constants.data = NULL;
  ret->methods = NULL;
  ret->method_count = 0;
  ret->vtable_method_count = 0;
  ret->fields = NULL;
  ret->size_in_bytes = 0;
  ret->field_count = 0;
  ret->static_field_count = 0;
  ret->vtable = NULL;
  ret->interfaces = NULL;
  ret->loader = NULL;
  ret->interface_count = 0;
  ret->state = JV_STATE_NOTHING;
  ret->thread = NULL;
  ret->depth = 0;
  ret->ancestors = NULL;
  ret->idt = NULL;
  ret->arrayclass = NULL;
  ret->protectionDomain = NULL;
  ret->chain = NULL;
}

jclass
_Jv_NewClass (_Jv_Utf8Const *name, jclass superclass,
	      java::lang::ClassLoader *loader)
{
  jclass ret = (jclass) JvAllocObject (&java::lang::Class::class$);
  _Jv_InitNewClassFields (ret);
  ret->name = name;
  ret->superclass = superclass;
  ret->loader = loader;

  _Jv_RegisterClass (ret);

  return ret;
}

static _Jv_IDispatchTable *array_idt = NULL;
static jshort array_depth = 0;
static jclass *array_ancestors = NULL;

// Create a class representing an array of ELEMENT and store a pointer to it
// in element->arrayclass. LOADER is the ClassLoader which _initiated_ the 
// instantiation of this array. ARRAY_VTABLE is the vtable to use for the new 
// array class. This parameter is optional.
void
_Jv_NewArrayClass (jclass element, java::lang::ClassLoader *loader,
		   _Jv_VTable *array_vtable)
{
  JvSynchronize sync (element);

  _Jv_Utf8Const *array_name;
  int len;

  if (element->arrayclass)
    return;

  if (element->isPrimitive())
    {
      if (element == JvPrimClass (void))
	throw new java::lang::ClassNotFoundException ();
      len = 3;
    }
  else
    len = element->name->length + 5;

  {
    char signature[len];
    int index = 0;
    signature[index++] = '[';
    // Compute name of array class.
    if (element->isPrimitive())
      {
	signature[index++] = (char) element->method_count;
      }
    else
      {
	size_t length = element->name->length;
	const char *const name = element->name->data;
	if (name[0] != '[')
	  signature[index++] = 'L';
	memcpy (&signature[index], name, length);
	index += length;
	if (name[0] != '[')
	  signature[index++] = ';';
      }      
    array_name = _Jv_makeUtf8Const (signature, index);
  }

  // Create new array class.
  jclass array_class = _Jv_NewClass (array_name, &java::lang::Object::class$,
  				     element->loader);

  // Note that `vtable_method_count' doesn't include the initial
  // gc_descr slot.
  JvAssert (java::lang::Object::class$.vtable_method_count
	    == NUM_OBJECT_METHODS);
  int dm_count = java::lang::Object::class$.vtable_method_count;

  // Create a new vtable by copying Object's vtable.
  _Jv_VTable *vtable;
  if (array_vtable)
    vtable = array_vtable;
  else
    vtable = _Jv_VTable::new_vtable (dm_count);
  vtable->clas = array_class;
  vtable->gc_descr = java::lang::Object::class$.vtable->gc_descr;
  for (int i = 0; i < dm_count; ++i)
    vtable->set_method (i, java::lang::Object::class$.vtable->get_method (i));

  array_class->vtable = vtable;
  array_class->vtable_method_count
    = java::lang::Object::class$.vtable_method_count;

  // Stash the pointer to the element type.
  array_class->methods = (_Jv_Method *) element;

  // Register our interfaces.
  static jclass interfaces[] =
    {
      &java::lang::Cloneable::class$,
      &java::io::Serializable::class$
    };
  array_class->interfaces = interfaces;
  array_class->interface_count = sizeof interfaces / sizeof interfaces[0];

  // Since all array classes have the same interface dispatch table, we can 
  // cache one and reuse it. It is not necessary to synchronize this.
  if (!array_idt)
    {
      _Jv_PrepareConstantTimeTables (array_class);
      array_idt = array_class->idt;
      array_depth = array_class->depth;
      array_ancestors = array_class->ancestors;
    }
  else
    {
      array_class->idt = array_idt;
      array_class->depth = array_depth;
      array_class->ancestors = array_ancestors;
    }

  using namespace java::lang::reflect;
  {
    // Array classes are "abstract final"...
    _Jv_ushort accflags = Modifier::FINAL | Modifier::ABSTRACT;
    // ... and inherit accessibility from element type, per vmspec 5.3.3.2
    accflags |= (element->accflags & Modifier::PUBLIC);
    accflags |= (element->accflags & Modifier::PROTECTED);
    accflags |= (element->accflags & Modifier::PRIVATE);      
    array_class->accflags = accflags;
  }

  // An array class has no visible instance fields. "length" is invisible to 
  // reflection.

  // say this class is initialized and ready to go!
  array_class->state = JV_STATE_DONE;

  // vmspec, section 5.3.3 describes this
  if (element->loader != loader)
    _Jv_RegisterInitiatingLoader (array_class, loader);

  element->arrayclass = array_class;
}

static jclass stack_head;

// These two functions form a stack of classes.   When a class is loaded
// it is pushed onto the stack by the class loader; this is so that
// StackTrace can quickly determine which classes have been loaded.

jclass
_Jv_PopClass (void)
{
  JvSynchronize sync (&java::lang::Class::class$);
  if (stack_head)
    {
      jclass tmp = stack_head;
      stack_head = tmp->chain;
      return tmp;
    }
  return NULL;
}

void
_Jv_PushClass (jclass k)
{
  JvSynchronize sync (&java::lang::Class::class$);
  jclass tmp = stack_head;
  stack_head = k;
  k->chain = tmp;
}
