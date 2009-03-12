// natClassLoader.cc - Implementation of java.lang.ClassLoader native methods.

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <execution.h>

#include <java-threads.h>
#include <java-interp.h>

#include <java/lang/Character.h>
#include <java/lang/Thread.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/InternalError.h>
#include <java/lang/IllegalAccessError.h>
#include <java/lang/LinkageError.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/lang/ClassCircularityError.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/VirtualMachineError.h>
#include <java/lang/VMClassLoader.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/Runtime.h>
#include <java/lang/StringBuffer.h>
#include <java/io/Serializable.h>
#include <java/lang/Cloneable.h>
#include <java/lang/ref/WeakReference.h>
#include <java/util/HashMap.h>
#include <gnu/gcj/runtime/BootClassLoader.h>
#include <gnu/gcj/runtime/SystemClassLoader.h>

// Size of local hash table.
#define HASH_LEN 1013

// Hash function for Utf8Consts.
#define HASH_UTF(Utf) ((Utf)->hash16() % HASH_LEN)

// This records classes which will be registered with the system class
// loader when it is initialized.
static jclass system_class_list;

// This is used as the value of system_class_list after we have
// initialized the system class loader; it lets us know that we should
// no longer pay attention to the system abi flag.
#define SYSTEM_LOADER_INITIALIZED ((jclass) -1)

static jclass loaded_classes[HASH_LEN];

// This is the root of a linked list of classes
static jclass stack_head;

// While bootstrapping we keep a list of classes we found, so that we
// can register their packages.  There aren't many of these so we
// just keep a small buffer here and abort if we overflow.
#define BOOTSTRAP_CLASS_LIST_SIZE 20
static jclass bootstrap_class_list[BOOTSTRAP_CLASS_LIST_SIZE];
static int bootstrap_index;




jclass
java::lang::ClassLoader::loadClassFromSig(jstring name)
{
  int len = _Jv_GetStringUTFLength (name);
  char sig[len + 1];
  _Jv_GetStringUTFRegion (name, 0, name->length(), sig);
  jclass result = _Jv_FindClassFromSignature(sig, this);
  if (result == NULL)
    throw new ClassNotFoundException(name);
  return result;
}



// This tries to find a class in our built-in cache.  This cache is
// used only for classes which are linked in to the executable or
// loaded via dlopen().
jclass
_Jv_FindClassInCache (_Jv_Utf8Const *name)
{
  JvSynchronize sync (&java::lang::Class::class$);
  jint hash = HASH_UTF (name);

  jclass klass;
  for (klass = loaded_classes[hash]; klass; klass = klass->next_or_version)
    {
      if (_Jv_equalUtf8Consts (name, klass->name))
	break;
    }

  return klass;
}

void
_Jv_UnregisterClass (jclass the_class)
{
  // This can happen if the class could not be defined properly.
  if (! the_class->name)
    return;

  JvSynchronize sync (&java::lang::Class::class$);
  jint hash = HASH_UTF(the_class->name);

  jclass *klass = &(loaded_classes[hash]);
  for ( ; *klass; klass = &((*klass)->next_or_version))
    {
      if (*klass == the_class)
	{
	  *klass = (*klass)->next_or_version;
	  break;
	}
    }
}

// Register an initiating class loader for a given class.
void
_Jv_RegisterInitiatingLoader (jclass klass, java::lang::ClassLoader *loader)
{
  if (! loader)
    loader = java::lang::VMClassLoader::bootLoader;
  if (! loader)
    {
      // Very early in the bootstrap process, the Bootstrap classloader may 
      // not exist yet.
      // FIXME: We could maintain a list of these and come back and register
      // them later.
      return;
    }

  JvSynchronize sync (loader->loadingConstraints);

  using namespace java::lang::ref;

  jstring name = klass->getName();
  WeakReference *ref = (WeakReference *) loader->loadingConstraints->get (name);
  if (ref)
    {
      jclass constraint = (jclass) ref->get();
      if (constraint && constraint != klass)
	throw new java::lang::LinkageError(JvNewStringLatin1("loading constraint violated"));
    }
  loader->loadingConstraints->put(name, new WeakReference(klass));
  loader->loadedClasses->put(name, klass);
}

// If we found an error while defining an interpreted class, we must
// go back and unregister it.
void
_Jv_UnregisterInitiatingLoader (jclass klass, java::lang::ClassLoader *loader)
{
  if (! loader)
    loader = java::lang::VMClassLoader::bootLoader;
  loader->loadedClasses->remove(klass->name->toString());
}

// Check a loading constraint.  In particular check that, if there is
// a constraint for the name of KLASS in LOADER, that it maps to
// KLASS.  If there is no such constraint, make a new one.  If the
// constraint is violated, throw an exception.  Do nothing for
// primitive types.
void
_Jv_CheckOrCreateLoadingConstraint (jclass klass,
				    java::lang::ClassLoader *loader)
{
  // Strip arrays.
  while (klass->isArray())
    klass = klass->getComponentType();
  // Ignore primitive types.
  if (klass->isPrimitive())
    return;

  if (! loader)
    loader = java::lang::VMClassLoader::bootLoader;
  jstring name = klass->getName();

  JvSynchronize sync (loader->loadingConstraints);

  using namespace java::lang::ref;

  WeakReference *ref = (WeakReference *) loader->loadingConstraints->get (name);
  if (ref)
    {
      jclass constraint = (jclass) ref->get();
      if (constraint)
	{
	  if (klass != constraint)
	    throw new java::lang::LinkageError(JvNewStringLatin1("loading constraint violated"));
	  // Otherwise, all is ok.
	  return;
	}
    }
  // No constraint (or old constraint GC'd).  Make a new one.
  loader->loadingConstraints->put(name, new WeakReference(klass));
}


// Class registration.
//
// There are two kinds of functions that register classes.  
//
// Type 1:
//
// These take the address of a class that is in an object file.
// Because these classes are not allocated on the heap, It is also
// necessary to register the address of the object for garbage
// collection.  This is used with the "old" C++ ABI and with
// -findirect-dispatch -fno-indirect-classes.
//
// Type 2:
//
// These take an initializer struct, create the class, and return the
// address of the newly created class to their caller.  These are used
// with -findirect-dispatch.
//
// _Jv_RegisterClasses() and _Jv_RegisterClasses_Counted() are
// functions of Type 1, and _Jv_NewClassFromInitializer() and
// _Jv_RegisterNewClasses() are of Type 2.


// Check that the file we're trying to load has been compiled with a
// compatible version of gcj.  In previous versions of libgcj we
// silently failed to register classes of an incompatible ABI version,
// but this was totally bogus.
void
_Jv_CheckABIVersion (unsigned long value)
{
  // We are compatible with GCJ 4.0.0 BC-ABI classes. This release used a
  // different format for the version ID string.
   if (value == OLD_GCJ_40_BC_ABI_VERSION)
     return;
     
  // The 20 low-end bits are used for the version number.
  unsigned long version = value & 0xfffff;

  if (value & FLAG_BINARYCOMPAT_ABI)
    {
      int abi_rev = version % 100;
      int abi_ver = version - abi_rev;
      // We are compatible with abi_rev 0 and 1.
      if (abi_ver == GCJ_40_BC_ABI_VERSION && abi_rev <= 1)
        return;
    }
  else
    {
      // C++ ABI
      if (version == GCJ_CXX_ABI_VERSION)
	return;

      // If we've loaded a library that uses the C++ ABI, and this
      // library is an incompatible version, then we're dead.  There's
      // no point throwing an exception: that will crash.
      JvFail ("gcj linkage error.\n"
	      "Incorrect library ABI version detected.  Aborting.\n");
    }

  throw new ::java::lang::ClassFormatError
    (JvNewStringLatin1 ("Library compiled with later ABI version than"
			" this version of libgcj supports"));
}

// This function is called many times during startup, before main() is
// run.  At that point in time we know for certain we are running 
// single-threaded, so we don't need to lock when adding classes to the 
// class chain.  At all other times, the caller should synchronize on
// Class::class$.
void
_Jv_RegisterClasses (const jclass *classes)
{
  _Jv_RegisterLibForGc (classes);

  for (; *classes; ++classes)
    {
      jclass klass = *classes;

      _Jv_CheckABIVersion ((unsigned long) klass->next_or_version);
      (*_Jv_RegisterClassHook) (klass);
    }
}

// This is a version of _Jv_RegisterClasses that takes a count.
void
_Jv_RegisterClasses_Counted (const jclass * classes, size_t count)
{
  size_t i;

  _Jv_RegisterLibForGc (classes);

  for (i = 0; i < count; i++)
    {
      jclass klass = classes[i];

      _Jv_CheckABIVersion ((unsigned long) klass->next_or_version);
      (*_Jv_RegisterClassHook) (klass);
    }
}

// Create a class on the heap from an initializer struct.
inline jclass
_Jv_NewClassFromInitializer (const char *class_initializer)
{
  const unsigned long version 
    = ((unsigned long) 
       ((::java::lang::Class *)class_initializer)->next_or_version);
  _Jv_CheckABIVersion (version);
  
  /* We create an instance of java::lang::Class and copy all of its
     fields except the first word (the vtable pointer) from
     CLASS_INITIALIZER.  This first word is pre-initialized by
     _Jv_AllocObj, and we don't want to overwrite it.  */
  
  jclass new_class
    = (jclass)_Jv_AllocObj (sizeof (::java::lang::Class),
			    &::java::lang::Class::class$);
  const char *src = class_initializer + sizeof (void*);
  char *dst = (char*)new_class + sizeof (void*);
  size_t len = (::java::lang::Class::initializerSize (version) 
		- sizeof (void*));
  memcpy (dst, src, len);
  
  new_class->engine = &_Jv_soleIndirectCompiledEngine;

  /* FIXME:  Way back before the dawn of time, we overloaded the
     SYNTHETIC class access modifier to mean INTERPRETED.  This was a
     Bad Thing, but it didn't matter then because classes were never
     marked synthetic.  However, it is possible to redeem the
     situation: _Jv_NewClassFromInitializer is only called from
     compiled classes, so we clear the INTERPRETED flag.  This is a
     kludge!  */
  new_class->accflags &= ~java::lang::reflect::Modifier::INTERPRETED;

  (*_Jv_RegisterClassHook) (new_class);
  
  return new_class;
}

// Called by compiler-generated code at DSO initialization.  CLASSES
// is an array of pairs: the first item of each pair is a pointer to
// the initialized data that is a class initializer in a DSO, and the
// second is a pointer to a class reference.
// _Jv_NewClassFromInitializer() creates the new class (on the Java
// heap) and we write the address of the new class into the address
// pointed to by the second word.
void
_Jv_RegisterNewClasses (char **classes)
{
  _Jv_InitGC ();

  const char *initializer;

  while ((initializer = *classes++))
    {
      jclass *class_ptr = (jclass *)*classes++;
      *class_ptr = _Jv_NewClassFromInitializer (initializer);
    }      
}
  
void
_Jv_RegisterClassHookDefault (jclass klass)
{
  // This is bogus, but there doesn't seem to be a better place to do
  // it.
  if (! klass->engine)
    klass->engine = &_Jv_soleCompiledEngine;

  /* FIXME:  Way back before the dawn of time, we overloaded the
     SYNTHETIC class access modifier to mean INTERPRETED.  This was a
     Bad Thing, but it didn't matter then because classes were never
     marked synthetic.  However, it is possible to redeem the
     situation: _Jv_RegisterClassHookDefault is only called from
     compiled classes, so we clear the INTERPRETED flag.  This is a
     kludge!  */
  klass->accflags &= ~java::lang::reflect::Modifier::INTERPRETED;

  if (system_class_list != SYSTEM_LOADER_INITIALIZED)
    {
      unsigned long abi = (unsigned long) klass->next_or_version;
      if (! _Jv_ClassForBootstrapLoader (abi))
	{
	  klass->next_or_version = system_class_list;
	  system_class_list = klass;
	  return;
	}
    }

  jint hash = HASH_UTF (klass->name);

  // If the class is already registered, don't re-register it.
  for (jclass check_class = loaded_classes[hash];
       check_class != NULL;
       check_class = check_class->next_or_version)
    {
      if (check_class == klass)
	{
	  // If you get this, it means you have the same class in two
	  // different libraries.
#define TEXT "Duplicate class registration: "
	  // We size-limit MESSAGE so that you can't trash the stack.
	  char message[200];
	  strcpy (message, TEXT);
	  strncpy (message + sizeof (TEXT) - 1, klass->name->chars(),
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
    }

  klass->next_or_version = loaded_classes[hash];
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

// This is used during initialization to register all compiled-in
// classes that are not part of the core with the system class loader.
void
_Jv_CopyClassesToSystemLoader (gnu::gcj::runtime::SystemClassLoader *loader)
{
  for (jclass klass = system_class_list;
       klass;
       klass = klass->next_or_version)
    {
      klass->loader = loader;
      loader->addClass(klass);
    }
  system_class_list = SYSTEM_LOADER_INITIALIZED;
}

// An internal variant of _Jv_FindClass which simply swallows a
// NoClassDefFoundError or a ClassNotFoundException. This gives the
// caller a chance to evaluate the situation and behave accordingly.
jclass
_Jv_FindClassNoException (_Jv_Utf8Const *name, java::lang::ClassLoader *loader)
{
  jclass klass;

  try
    {
      klass = _Jv_FindClass(name, loader);
    }
  catch ( java::lang::NoClassDefFoundError *ncdfe )
    {
      return NULL;
    }
  catch ( java::lang::ClassNotFoundException *cnfe )
    {
      return NULL;
    }

  return klass;
}

jclass
_Jv_FindClass (_Jv_Utf8Const *name, java::lang::ClassLoader *loader)
{
  // See if the class was already loaded by this loader.  This handles
  // initiating loader checks, as we register classes with their
  // initiating loaders.

  java::lang::ClassLoader *boot = java::lang::VMClassLoader::bootLoader;
  java::lang::ClassLoader *real = loader;
  if (! real)
    real = boot;
  jstring sname = name->toString();
  // We might still be bootstrapping the VM, in which case there
  // won't be a bootstrap class loader yet.
  jclass klass = real ? real->findLoadedClass (sname) : NULL;

  if (! klass)
    {
      if (loader)
	{
	  // Load using a user-defined loader, jvmspec 5.3.2.
	  // Note that we explicitly must call the single-argument form.
	  klass = loader->loadClass(sname);

	  // If "loader" delegated the loadClass operation to another
	  // loader, explicitly register that it is also an initiating
	  // loader of the given class.
	  java::lang::ClassLoader *delegate = (loader == boot
					       ? NULL
					       : loader);
	  if (klass && klass->getClassLoaderInternal () != delegate)
	    _Jv_RegisterInitiatingLoader (klass, loader);
	}
      else if (boot)
	{
	  // Load using the bootstrap loader jvmspec 5.3.1.
	  klass = java::lang::VMClassLoader::loadClass (sname, false); 

	  // Register that we're an initiating loader.
	  if (klass)
	    _Jv_RegisterInitiatingLoader (klass, 0);
	}
      else
	{
	  // Not even a bootstrap loader, try the built-in cache.
	  klass = _Jv_FindClassInCache (name);

	  if (klass)
	    {
	      bool found = false;
	      for (int i = 0; i < bootstrap_index; ++i)
		{
		  if (bootstrap_class_list[i] == klass)
		    {
		      found = true;
		      break;
		    }
		}
	      if (! found)
		{
		  if (bootstrap_index == BOOTSTRAP_CLASS_LIST_SIZE)
		    abort ();
		  bootstrap_class_list[bootstrap_index++] = klass;
		}
	    }
	}
    }

  return klass;
}

void
_Jv_RegisterBootstrapPackages ()
{
  for (int i = 0; i < bootstrap_index; ++i)
    java::lang::VMClassLoader::definePackageForNative(bootstrap_class_list[i]->getName());
}

jclass
_Jv_NewClass (_Jv_Utf8Const *name, jclass superclass,
	      java::lang::ClassLoader *loader)
{
  jclass ret = (jclass) _Jv_AllocObject (&java::lang::Class::class$);
  ret->name = name;
  ret->superclass = superclass;
  ret->loader = loader;

  _Jv_RegisterInitiatingLoader (ret, loader);

  return ret;
}

static _Jv_IDispatchTable *array_idt = NULL;
static jshort array_depth = 0;
static jclass *array_ancestors = NULL;

static jclass interfaces[] =
{
  &java::lang::Cloneable::class$,
  &java::io::Serializable::class$
};

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
    len = element->name->len() + 5;

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
	size_t length = element->name->len();
	const char *const name = element->name->chars();
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
  array_class->element_type = element;

  // Register our interfaces.
  array_class->interfaces = interfaces;
  array_class->interface_count = sizeof interfaces / sizeof interfaces[0];

  // Since all array classes have the same interface dispatch table, we can 
  // cache one and reuse it. It is not necessary to synchronize this.
  if (!array_idt)
    {
      _Jv_Linker::wait_for_state(array_class, JV_STATE_PREPARED);
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
    // Array classes are "abstract final" and inherit accessibility
    // from element type, per vmspec 5.3.3.2
    _Jv_ushort accflags = (Modifier::FINAL | Modifier::ABSTRACT
			   | (element->accflags
			      & (Modifier::PUBLIC | Modifier::PROTECTED
				 | Modifier::PRIVATE)));
    array_class->accflags = accflags;
  }

  // An array class has no visible instance fields. "length" is invisible to 
  // reflection.

  // Say this class is initialized and ready to go!
  array_class->state = JV_STATE_DONE;

  // vmspec, section 5.3.3 describes this
  if (element->loader != loader)
    _Jv_RegisterInitiatingLoader (array_class, loader);

  element->arrayclass = array_class;
}

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
