// natClassLoader.cc - Implementation of java.lang.ClassLoader native methods.

/* Copyright (C) 1999, 2000  Red Hat, Inc.

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
#include <java/lang/reflect/Modifier.h>
#include <java/lang/Runtime.h>

#define CloneableClass _CL_Q34java4lang9Cloneable
extern java::lang::Class CloneableClass;
#define ObjectClass _CL_Q34java4lang6Object
extern java::lang::Class ObjectClass;
#define ClassClass _CL_Q34java4lang5Class
extern java::lang::Class ClassClass;
#define VMClassLoaderClass _CL_Q34java4lang17VMClassLoader
extern java::lang::Class VMClassLoader;
#define ClassLoaderClass _CL_Q34java4lang11ClassLoader
extern java::lang::Class ClassLoaderClass;

/////////// java.lang.ClassLoader native methods ////////////

java::lang::ClassLoader *
java::lang::ClassLoader::getSystemClassLoader (void)
{
  JvSynchronize sync (&ClassLoaderClass);
  if (! system)
    system = gnu::gcj::runtime::VMClassLoader::getVMClassLoader ();
  return system;
}

void
java::lang::ClassLoader::defineClass2 (jclass klass, jbyteArray data,
				       jint offset, jint length)
{
#ifdef INTERPRETER
  _Jv_DefineClass (klass, data, offset, length);
#endif
}

java::lang::Class *
java::lang::ClassLoader::defineClass0 (jstring name,
				       jbyteArray data, 
				       jint offset,
				       jint length)
{
#ifdef INTERPRETER
  jclass klass;
  klass = (jclass) JvAllocObject (&ClassClass, sizeof (_Jv_InterpClass));

  // synchronize on the class, so that it is not
  // attempted initialized until we're done loading.
  _Jv_MonitorEnter (klass);

  // record which is the defining loader
  klass->loader = this;

  // register that we are the initiating loader...
  if (name != 0)
    {
      _Jv_Utf8Const *   name2 = _Jv_makeUtf8Const (name);

      _Jv_VerifyClassName (name2);

      klass->name = name2;
    }

  // this will do the magic.  loadInto also operates
  // as an exception trampoline for now...
  Throwable *ex = defineClass1 (klass, data, offset, length);
    
  if (ex)  // we failed to load it
    {
      klass->state = JV_STATE_ERROR;
      klass->notifyAll ();

      _Jv_UnregisterClass (klass);

      _Jv_MonitorExit (klass);
	  
      // FIXME: Here we may want to test that EX does
      // indeed represent a valid exception.  That is,
      // anything but ClassNotFoundException, 
      // or some kind of Error.
	  
      JvThrow (ex);
    }
    
  // if everything proceeded sucessfully, we're loaded.
  JvAssert (klass->state == JV_STATE_LOADED);

  // if an exception is generated, this is initially missed.
  // however, we come back here in handleException0 below...
  _Jv_MonitorExit (klass);

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
    {
      _Jv_Throw (new java::lang::LinkageError ());
    }
}

// Finish linking a class.  Only called from ClassLoader::resolveClass.
void
java::lang::ClassLoader::linkClass0 (java::lang::Class *klass)
{
  if (klass->state >= JV_STATE_LINKED)
    return;

#ifdef INTERPRETER
  if (_Jv_IsInterpretedClass (klass))
    {
      _Jv_PrepareClass (klass);
    }
#endif

  _Jv_PrepareCompiledClass (klass);
}

void
java::lang::ClassLoader::markClassErrorState0 (java::lang::Class *klass)
{
  klass->state = JV_STATE_ERROR;
  klass->notifyAll ();
}


/** this is the only native method in VMClassLoader, so 
    we define it here. */
jclass
gnu::gcj::runtime::VMClassLoader::findSystemClass (jstring name)
{
  _Jv_Utf8Const *name_u = _Jv_makeUtf8Const (name);
  jclass klass = _Jv_FindClassInCache (name_u, 0);

  if (! klass)
    {
      // Turn `gnu.pkg.quux' into `gnu-pkg-quux'.  Then search for a
      // module named (eg, on Linux) `gnu-pkg-quux.so', followed by
      // `gnu-pkg.so' and `gnu.so'.  If loading one of these causes
      // the class to appear in the cache, then use it.
      jstring so_base_name = name->replace ('.', '-');

      while (! klass && so_base_name && so_base_name->length() > 0)
	{
	  using namespace ::java::lang;
	  Runtime *rt = Runtime::getRuntime();
	  jboolean loaded = rt->loadLibraryInternal (so_base_name);

	  jint nd = so_base_name->lastIndexOf ('-');
	  if (nd == -1)
	    so_base_name = NULL;
	  else
	    so_base_name = so_base_name->substring (0, nd);

	  if (loaded)
	    klass = _Jv_FindClassInCache (name_u, 0);
	}
    }

  return klass;
}

jclass
java::lang::ClassLoader::findLoadedClass (jstring name)
{
  return _Jv_FindClassInCache (_Jv_makeUtf8Const (name), this);
}


/** This function does class-preparation for compiled classes.  
    NOTE: It contains replicated functionality from
    _Jv_ResolvePoolEntry, and this is intentional, since that function
    lives in resolve.cc which is entirely conditionally compiled.
 */
void
_Jv_PrepareCompiledClass(jclass klass)
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
	      JvThrow (new java::lang::ClassNotFoundException (str));
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

  klass->notifyAll ();
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

struct _Jv_LoaderInfo {
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
  _Jv_MonitorEnter (&ClassClass);
  jint hash = HASH_UTF (name);

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

  _Jv_MonitorExit (&ClassClass);

  return klass;
}

void
_Jv_UnregisterClass (jclass the_class)
{
  _Jv_MonitorEnter (&ClassClass);
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
	  *info = (*info)->next;
	}

      if (*info == NULL)
	break;
    }

  _Jv_MonitorExit (&ClassClass);
}

void
_Jv_RegisterInitiatingLoader (jclass klass, java::lang::ClassLoader *loader)
{
  _Jv_LoaderInfo *info = new _Jv_LoaderInfo; // non-gc alloc!
  jint hash = HASH_UTF(klass->name);

  _Jv_MonitorEnter (&ClassClass);
  info->loader = loader;
  info->klass  = klass;
  info->next   = initiated_classes[hash];
  initiated_classes[hash] = info;
  _Jv_MonitorExit (&ClassClass);
  
}

// This function is called many times during startup, before main() is
// run.  We do our runtime initialization here the very first time we
// are called.  At that point in time we know for certain we are
// running single-threaded, so we don't need to lock when modifying
// `init'.  CLASSES is NULL-terminated.
void
_Jv_RegisterClasses (jclass *classes)
{
  static bool init = false;

  if (! init)
    {
      init = true;
      _Jv_InitThreads ();
      _Jv_InitGC ();
      _Jv_InitializeSyncMutex ();
    }

  JvSynchronize sync (&ClassClass);
  for (; *classes; ++classes)
    {
      jclass klass = *classes;
      jint hash = HASH_UTF (klass->name);
      klass->next = loaded_classes[hash];
      loaded_classes[hash] = klass;

      // registering a compiled class causes
      // it to be immediately "prepared".  
      if (klass->state == JV_STATE_NOTHING)
	klass->state = JV_STATE_COMPILED;
    }
}

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

      if (loader)
	{
	  // Load using a user-defined loader, jvmspec 5.3.2
	  klass = loader->loadClass(sname, false);

	  // If "loader" delegated the loadClass operation to another
	  // loader, explicitly register that it is also an initiating
	  // loader of the given class.
	  if (klass && (klass->getClassLoader () != loader))
	    _Jv_RegisterInitiatingLoader (klass, loader);
	}
      else 
	{
	  java::lang::ClassLoader *sys = java::lang::ClassLoader::system;
	  if (sys == NULL)
	    {
	      _Jv_InitClass (&ClassLoaderClass);
	      sys = java::lang::ClassLoader::getSystemClassLoader ();
	    }

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

jclass
_Jv_NewClass (_Jv_Utf8Const *name, jclass superclass,
	      java::lang::ClassLoader *loader)
{
  jclass ret = (jclass) JvAllocObject (&ClassClass);

  ret->next = NULL;
  ret->name = name;
  ret->accflags = 0;
  ret->superclass = superclass;
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
  ret->loader = loader;
  ret->interface_count = 0;
  ret->state = JV_STATE_NOTHING;
  ret->thread = NULL;

  _Jv_RegisterClass (ret);

  return ret;
}

jclass
_Jv_FindArrayClass (jclass element, java::lang::ClassLoader *loader)
{
  _Jv_Utf8Const *array_name;
  int len;
  if (element->isPrimitive())
    {
      // For primitive types the array is cached in the class.
      jclass ret = (jclass) element->methods;
      if (ret)
	return ret;
      len = 3;
    }
  else
    len = element->name->length + 5;

  {
    char signature[len];
    int index = 0;
    signature[index++] = '[';
    // Compute name of array class to see if we've already cached it.
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

  jclass array_class = _Jv_FindClassInCache (array_name, element->loader);

  if (! array_class)
    {
      // Create new array class.
      array_class = _Jv_NewClass (array_name, &ObjectClass, element->loader);

      // Note that `vtable_method_count' doesn't include the initial
      // NULL slot.
      int dm_count = ObjectClass.vtable_method_count + 1;

      // Create a new vtable by copying Object's vtable (except the
      // class pointer, of course).  Note that we allocate this as
      // unscanned memory -- the vtables are handled specially by the
      // GC.
      int size = (sizeof (_Jv_VTable) +
		  ((dm_count - 1) * sizeof (void *)));
      _Jv_VTable *vtable = (_Jv_VTable *) _Jv_AllocBytes (size);
      vtable->clas = array_class;
      memcpy (vtable->method, ObjectClass.vtable->method,
	      dm_count * sizeof (void *));
      array_class->vtable = vtable;
      array_class->vtable_method_count = ObjectClass.vtable_method_count;

      // Stash the pointer to the element type.
      array_class->methods = (_Jv_Method *) element;

      // Register our interfaces.
      // FIXME: for JDK 1.2 we need Serializable.
      static jclass interfaces[] = { &CloneableClass };
      array_class->interfaces = interfaces;
      array_class->interface_count = 1;

      // as per vmspec 5.3.3.2
      array_class->accflags = element->accflags;

      // FIXME: initialize other Class instance variables,
      // e.g. `fields'.

      // say this class is initialized and ready to go!
      array_class->state = JV_STATE_DONE;

      // vmspec, section 5.3.3 describes this
      if (element->loader != loader)
	_Jv_RegisterInitiatingLoader (array_class, loader);
    }

  // For primitive types, point back at this array.
  if (element->isPrimitive())
    element->methods = (_Jv_Method *) array_class;

  return array_class;
}


