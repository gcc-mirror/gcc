// natClassLoader.cc - Implementation of java.lang.ClassLoader native methods.

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005  Free Software Foundation

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
#include <java/util/HashMap.h>
#include <gnu/gcj/runtime/BootClassLoader.h>

// Size of local hash table.
#define HASH_LEN 1013

// Hash function for Utf8Consts.
#define HASH_UTF(Utf) ((Utf)->hash16() % HASH_LEN)

static jclass loaded_classes[HASH_LEN];

// This records classes which will be registered with the system class
// loader when it is initialized.
static jclass system_class_list;

// This is used as the value of system_class_list after we have
// initialized the system class loader; it lets us know that we should
// no longer pay attention to the system abi flag.
#define SYSTEM_LOADER_INITIALIZED ((jclass) -1)

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
  return _Jv_FindClassFromSignature(sig, this);
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
  loader->loadedClasses->put(klass->name->toString(), klass);
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

// This function is called many times during startup, before main() is
// run.  At that point in time we know for certain we are running 
// single-threaded, so we don't need to lock when adding classes to the 
// class chain.  At all other times, the caller should synchronize on
// Class::class$.
void
_Jv_RegisterClasses (const jclass *classes)
{
  for (; *classes; ++classes)
    {
      jclass klass = *classes;

      if (_Jv_CheckABIVersion ((unsigned long) klass->next_or_version))
	(*_Jv_RegisterClassHook) (klass);
    }
}

// This is a version of _Jv_RegisterClasses that takes a count.
void
_Jv_RegisterClasses_Counted (const jclass * classes, size_t count)
{
  size_t i;
  for (i = 0; i < count; i++)
    {
      jclass klass = classes[i];

      if (_Jv_CheckABIVersion ((unsigned long) klass->next_or_version))
	(*_Jv_RegisterClassHook) (klass);
    }
}

void
_Jv_RegisterClassHookDefault (jclass klass)
{
  // This is bogus, but there doesn't seem to be a better place to do
  // it.
  if (! klass->engine)
    klass->engine = &_Jv_soleCompiledEngine;

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
_Jv_CopyClassesToSystemLoader (java::lang::ClassLoader *loader)
{
  for (jclass klass = system_class_list;
       klass;
       klass = klass->next_or_version)
    {
      klass->loader = loader;
      loader->loadedClasses->put(klass->name->toString(), klass);
    }
  system_class_list = SYSTEM_LOADER_INITIALIZED;
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
