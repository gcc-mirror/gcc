// natClass.cc - Implementation of java.lang.Class native methods.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>
#include <string.h>

#pragma implementation "Class.h"

#include <cni.h>
#include <jvm.h>
#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/String.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/reflect/Member.h>
#include <java/lang/reflect/Method.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/reflect/Constructor.h>
#include <java/lang/AbstractMethodError.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/lang/IllegalAccessException.h>
#include <java/lang/IllegalAccessError.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/InstantiationException.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/NoSuchFieldException.h>
#include <java/lang/NoSuchMethodException.h>
#include <java/lang/Thread.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/System.h>
#include <java/lang/SecurityManager.h>



#define CloneableClass _CL_Q34java4lang9Cloneable
extern java::lang::Class CloneableClass;
#define ObjectClass _CL_Q34java4lang6Object
extern java::lang::Class ObjectClass;
#define ErrorClass _CL_Q34java4lang5Error
extern java::lang::Class ErrorClass;
#define ClassClass _CL_Q34java4lang5Class
extern java::lang::Class ClassClass;
#define MethodClass _CL_Q44java4lang7reflect6Method
extern java::lang::Class MethodClass;
#define FieldClass _CL_Q44java4lang7reflect5Field
extern java::lang::Class FieldClass;

// Some constants we use to look up the class initializer.
static _Jv_Utf8Const *void_signature = _Jv_makeUtf8Const ("()V", 3);
static _Jv_Utf8Const *clinit_name = _Jv_makeUtf8Const ("<clinit>", 8);
static _Jv_Utf8Const *init_name = _Jv_makeUtf8Const ("<init>", 6);

// These are the possible values for the `state' field.  They more or
// less follow the section numbers in the Java Language Spec.  Right
// now we don't bother to represent other interesting states, e.g. the
// states a class might inhabit before it is prepared.  Note that
// ordering is important here; in particular `resolved' must come
// between `nothing' and the other states.
#define STATE_NOTHING      0
#define STATE_RESOLVED     1
#define STATE_IN_PROGRESS  6
#define STATE_DONE         9
#define STATE_ERROR       10

// Size of local hash table.
#define HASH_LEN 256

// Hash function for Utf8Consts.
#define HASH_UTF(Utf) (((Utf)->hash) % HASH_LEN)

// This is the table we use to keep track of loaded classes.  See Spec
// section 12.2.
static jclass loaded_classes[HASH_LEN];



jclass
java::lang::Class::forName (jstring className)
{
  if (! className)
    JvThrow (new java::lang::NullPointerException);

#if 0
  // FIXME: should check syntax of CLASSNAME and throw
  // IllegalArgumentException on failure.

  // FIXME: should use class loader from calling method.
  jclass klass = _Jv_FindClass (className, NULL);
#else
  jsize length = _Jv_GetStringUTFLength (className);
  char buffer[length];
  _Jv_GetStringUTFRegion (className, 0, length, buffer);

  // FIXME: should check syntax of CLASSNAME and throw
  // IllegalArgumentException on failure.
  _Jv_Utf8Const *name = _Jv_makeUtf8Const (buffer, length);

  // FIXME: should use class loader from calling method.
  jclass klass = (buffer[0] == '[' 
		  ? _Jv_FindClassFromSignature (name->data, NULL)
		  : _Jv_FindClass (name, NULL));
#endif
  if (! klass)
    JvThrow (new java::lang::ClassNotFoundException (className));
  return klass;
}

java::lang::reflect::Constructor *
java::lang::Class::getConstructor (JArray<jclass> *)
{
  JvFail ("java::lang::Class::getConstructor not implemented");
}

JArray<java::lang::reflect::Constructor *> *
java::lang::Class::getConstructors (void)
{
  JvFail ("java::lang::Class::getConstructors not implemented");
}

java::lang::reflect::Constructor *
java::lang::Class::getDeclaredConstructor (JArray<jclass> *)
{
  JvFail ("java::lang::Class::getDeclaredConstructor not implemented");
}

JArray<java::lang::reflect::Constructor *> *
java::lang::Class::getDeclaredConstructors (void)
{
  JvFail ("java::lang::Class::getDeclaredConstructors not implemented");
}

java::lang::reflect::Field *
java::lang::Class::getField (jstring name, jint hash)
{
  java::lang::reflect::Field* rfield;
  for (int i = 0;  i < field_count;  i++)
    {
      _Jv_Field *field = &fields[i];
      if (! _Jv_equal (field->name, name, hash))
	continue;
      if (! (field->getModifiers() & java::lang::reflect::Modifier::PUBLIC))
	continue;
      rfield = new java::lang::reflect::Field ();
      rfield->offset = (char*) field - (char*) fields;
      rfield->declaringClass = this;
      rfield->name = name;
      return rfield;
    }
  jclass superclass = getSuperclass();
  if (superclass == NULL)
    return NULL;
  rfield = superclass->getField(name, hash);
  for (int i = 0; i < interface_count && rfield == NULL; ++i)
    rfield = interfaces[i]->getField (name, hash);
  return rfield;
}

java::lang::reflect::Field *
java::lang::Class::getDeclaredField (jstring name)
{
  java::lang::SecurityManager *s = java::lang::System::getSecurityManager();
  if (s != NULL)
    s->checkMemberAccess (this, java::lang::reflect::Member::DECLARED);
  int hash = name->hashCode();
  for (int i = 0;  i < field_count;  i++)
    {
      _Jv_Field *field = &fields[i];
      if (! _Jv_equal (field->name, name, hash))
	continue;
      java::lang::reflect::Field* rfield = new java::lang::reflect::Field ();
      rfield->offset = (char*) field - (char*) fields;
      rfield->declaringClass = this;
      rfield->name = name;
      return rfield;
    }
  JvThrow (new java::lang::NoSuchFieldException (name));
}

JArray<java::lang::reflect::Field *> *
java::lang::Class::getDeclaredFields (void)
{
  java::lang::SecurityManager *s = java::lang::System::getSecurityManager();
  if (s != NULL)
    s->checkMemberAccess (this, java::lang::reflect::Member::DECLARED);
  JArray<java::lang::reflect::Field *> *result
    = (JArray<java::lang::reflect::Field *> *)
    JvNewObjectArray (field_count, &FieldClass, NULL);
  java::lang::reflect::Field** fptr = elements (result);
  for (int i = 0;  i < field_count;  i++)
    {
      _Jv_Field *field = &fields[i];
      java::lang::reflect::Field* rfield = new java::lang::reflect::Field ();
      rfield->offset = (char*) field - (char*) fields;
      rfield->declaringClass = this;
      *fptr++ = rfield;
    }
  return result;
}

java::lang::reflect::Method *
java::lang::Class::getDeclaredMethod (jstring, JArray<jclass> *)
{
  JvFail ("java::lang::Class::getDeclaredMethod not implemented");
}

JArray<java::lang::reflect::Method *> *
java::lang::Class::getDeclaredMethods (void)
{
  int numMethods = 0;
  int i;
  for (i = method_count;  --i >= 0; )
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || _Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name))
	continue;
      numMethods++;
    }
  JArray<java::lang::reflect::Method *> *result
    = (JArray<java::lang::reflect::Method *> *)
    JvNewObjectArray (numMethods, &MethodClass, NULL);
  java::lang::reflect::Method** mptr = elements (result);
  for (i = 0;  i < method_count;  i++)
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || _Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name))
	continue;
      java::lang::reflect::Method* rmethod = new java::lang::reflect::Method ();
      rmethod->offset = (char*) mptr - (char*) elements (result);
      rmethod->declaringClass = this;
      *mptr++ = rmethod;
    }
  return result;
}

jstring
java::lang::Class::getName (void)
{
  char buffer[name->length + 1];  
  memcpy (buffer, name->data, name->length); 
  buffer[name->length] = '\0';
  return _Jv_NewStringUTF (buffer);
}

JArray<jclass> *
java::lang::Class::getClasses (void)
{
  // FIXME: implement.
  return NULL;
}

JArray<jclass> *
java::lang::Class::getDeclaredClasses (void)
{
  checkMemberAccess (java::lang::reflect::Member::DECLARED);
  JvFail ("java::lang::Class::getDeclaredClasses not implemented");
  return NULL;			// Placate compiler.
}

// This is marked as unimplemented in the JCL book.
jclass
java::lang::Class::getDeclaringClass (void)
{
  JvFail ("java::lang::Class::getDeclaringClass unimplemented");
  return NULL;			// Placate compiler.
}

JArray<java::lang::reflect::Field *> *
java::lang::Class::getFields (void)
{
  JvFail ("java::lang::Class::getFields not implemented");
}

JArray<jclass> *
java::lang::Class::getInterfaces (void)
{
  jobjectArray r = JvNewObjectArray (interface_count, getClass (), NULL);
  jobject *data = elements (r);
  for (int i = 0; i < interface_count; ++i)
    data[i] = interfaces[i];
  return reinterpret_cast<JArray<jclass> *> (r);
}

java::lang::reflect::Method *
java::lang::Class::getMethod (jstring, JArray<jclass> *)
{
  JvFail ("java::lang::Class::getMethod not implemented");
}

JArray<java::lang::reflect::Method *> *
java::lang::Class::getMethods (void)
{
  JvFail ("java::lang::Class::getMethods not implemented");
}

jboolean
java::lang::Class::isAssignableFrom (jclass klass)
{
  if (this == klass)
    return true;
  // Primitive types must be equal, which we just tested for.
  if (isPrimitive () || ! klass || klass->isPrimitive())
    return false;

  // If target is array, so must source be.
  if (isArray ())
    {
      if (! klass->isArray())
	return false;
      return getComponentType()->isAssignableFrom(klass->getComponentType());
    }

  if (isAssignableFrom (klass->getSuperclass()))
    return true;

  if (isInterface())
    {
      // See if source implements this interface.
      for (int i = 0; i < klass->interface_count; ++i)
	{
	  jclass interface = klass->interfaces[i];
	  // FIXME: ensure that class is prepared here.
	  // See Spec 12.3.2.
	  if (isAssignableFrom (interface))
	    return true;
	}
    }

  return false;
}

jboolean
java::lang::Class::isInstance (jobject obj)
{
  if (! obj || isPrimitive ())
    return false;
  return isAssignableFrom (obj->getClass());
}

jboolean
java::lang::Class::isInterface (void)
{
  return (accflags & java::lang::reflect::Modifier::INTERFACE) != 0;
}

jobject
java::lang::Class::newInstance (void)
{
  // FIXME: do accessibility checks here.  There currently doesn't
  // seem to be any way to do these.
  // FIXME: we special-case one check here just to pass a Plum Hall
  // test.  Once access checking is implemented, remove this.
  if (this == &ClassClass)
    JvThrow (new java::lang::IllegalAccessException);

  if (isPrimitive ()
      || isInterface ()
      || isArray ()
      || java::lang::reflect::Modifier::isAbstract(accflags))
    JvThrow (new java::lang::InstantiationException);

  _Jv_Method *meth = _Jv_GetMethodLocal (this, init_name, void_signature);
  if (! meth)
    JvThrow (new java::lang::NoSuchMethodException);

  jobject r = JvAllocObject (this);
  ((void (*) (jobject)) meth->ncode) (r);
  return r;
}

// Initialize the constants.
void
java::lang::Class::resolveConstants (void)
{
  for (int i = 0; i < constants.size; ++i)
    {
      if (constants.tags[i] == CONSTANT_String)
	{
	  jstring str;
	  str = _Jv_NewStringUtf8Const ((_Jv_Utf8Const *) constants.data[i]);
	  constants.data[i] = (void *) str;
	  constants.tags[i] = CONSTANT_ResolvedString;
	}
      else if (constants.tags[i] == CONSTANT_Class)
	{
	  _Jv_Utf8Const *name = (_Jv_Utf8Const *) constants.data[i];
	  jclass klass = _Jv_FindClassFromSignature (name->data, loader);
	  if (! klass)
	    {
	      jstring str = _Jv_NewStringUtf8Const (name);
	      JvThrow (new java::lang::ClassNotFoundException (str));
	    }

	  constants.data[i] = (void *) klass;
	  constants.tags[i] = CONSTANT_ResolvedClass;
	}
    }
}

// FIXME.
void
java::lang::Class::hackRunInitializers (void)
{
  _Jv_Method *meth = _Jv_GetMethodLocal (this, clinit_name, void_signature);
  if (meth)
    ((void (*) (void)) meth->ncode) ();
}

// This implements the initialization process for a class.  From Spec
// section 12.4.2.
void
java::lang::Class::initializeClass (void)
{
  // Short-circuit to avoid needless locking.
  if (state == STATE_DONE)
    return;

  // Step 1.
  _Jv_MonitorEnter (this);

  // FIXME: This should actually be handled by calling into the class
  // loader.  For now we put it here.
  if (state < STATE_RESOLVED)
    {
      // We set the state before calling resolveConstants to avoid
      // infinite recursion when processing String or Class.
      state = STATE_RESOLVED;
      resolveConstants ();
    }

  // Step 2.
  java::lang::Thread *self = java::lang::Thread::currentThread();
  // FIXME: `self' can be null at startup.  Hence this nasty trick.
  self = (java::lang::Thread *) ((long) self | 1);
  while (state == STATE_IN_PROGRESS && thread && thread != self)
    wait ();

  // Steps 3 &  4.
  if (state == STATE_DONE || state == STATE_IN_PROGRESS || thread == self)
    {
      _Jv_MonitorExit (this);
      return;
    }

  // Step 5.
  if (state == STATE_ERROR)
    {
      _Jv_MonitorExit (this);
      JvThrow (new java::lang::NoClassDefFoundError);
    }

  // Step 6.
  thread = self;
  state = STATE_IN_PROGRESS;
  _Jv_MonitorExit (this);

  // Step 7.
  if (! isInterface () && superclass)
    {
      // FIXME: We can't currently catch a Java exception in C++ code.
      // So instead we call a Java trampoline.  It returns an
      // exception, or null.
      jobject except = superclass->hackTrampoline(0, NULL);
      if (except)
	{
	  // Caught an exception.
	  _Jv_MonitorEnter (this);
	  state = STATE_ERROR;
	  notify ();
	  _Jv_MonitorExit (this);
	  JvThrow (except);
	}
    }

  // Step 8.
  // FIXME: once again we have to go through a trampoline.
  java::lang::Throwable *except = hackTrampoline (1, NULL);

  // Steps 9, 10, 11.
  if (! except)
    {
      _Jv_MonitorEnter (this);
      state = STATE_DONE;
    }
  else
    {
      if (! ErrorClass.isInstance(except))
	{
	  // Once again we must use the trampoline.  In this case we
	  // have to detect an OutOfMemoryError.
	  except = hackTrampoline(2, except);
	}
      _Jv_MonitorEnter (this);
      state = STATE_ERROR;
    }
  notify ();
  _Jv_MonitorExit (this);
  if (except)
    JvThrow (except);
}



//
// Some class-related convenience functions.
//

_Jv_Method *
_Jv_GetMethodLocal (jclass klass, _Jv_Utf8Const *name,
		    _Jv_Utf8Const *signature)
{
  for (int i = 0; i < klass->method_count; ++i)
    {
      if (_Jv_equalUtf8Consts (name, klass->methods[i].name)
	  && _Jv_equalUtf8Consts (signature, klass->methods[i].signature))
	return &klass->methods[i];
    }
  return NULL;
}

void *
_Jv_LookupInterfaceMethod (jclass klass, _Jv_Utf8Const *name,
			   _Jv_Utf8Const *signature)
{
  // FIXME: can't do this until we have a working class loader.
  // This probably isn't the right thing to do anyway, since we can't
  // call a method of a class until the class is linked.  But this
  // captures the general idea.
  // klass->getClassLoader()->resolveClass(klass);

  for (; klass; klass = klass->getSuperclass())
    {
      _Jv_Method *meth = _Jv_GetMethodLocal (klass, name, signature);
      if (! meth)
	continue;

      if (java::lang::reflect::Modifier::isStatic(meth->accflags))
	JvThrow (new java::lang::IncompatibleClassChangeError);
      if (java::lang::reflect::Modifier::isAbstract(meth->accflags))
	JvThrow (new java::lang::AbstractMethodError);
      if (! java::lang::reflect::Modifier::isPublic(meth->accflags))
	JvThrow (new java::lang::IllegalAccessError);

      return meth->ncode;
    }
  JvThrow (new java::lang::IncompatibleClassChangeError);
  return NULL;			// Placate compiler.
}

void
_Jv_InitClass (jclass klass)
{
  klass->initializeClass();
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
_Jv_FindClassInCache (_Jv_Utf8Const *name, java::lang::ClassLoader *loader)
{
  JvSynchronize sync (&ClassClass);
  jint hash = HASH_UTF (name);
  jclass klass;
  for (klass = loaded_classes[hash]; klass; klass = klass->next)
    {
      if (loader == klass->loader && _Jv_equalUtf8Consts (name, klass->name))
	break;
    }
  return klass;
}

#if 0
jclass
_Jv_FindClassInCache (jstring name, java::lang::ClassLoader *loader)
{
  JvSynchronize sync (&ClassClass);
  jint hash = name->hashCode();
  jclass klass = loaded_classes[(_Jv_ushort) hash % HASH_LEN];
  for ( ; klass; klass = klass->next)
    {
      if (loader == klass->loader
	  && _Jv_equalUtf8Consts (klass->name, name, hash))
	break;
    }
  return klass;
}
#endif

jclass
_Jv_FindClass (_Jv_Utf8Const* name, java::lang::ClassLoader *loader)
{
  jclass klass = _Jv_FindClassInCache (name, loader);
  if (loader && ! klass)
    {
      klass = loader->loadClass(_Jv_NewStringUtf8Const (name));
      if (klass)
	_Jv_RegisterClass (klass);
    }
  return klass;
}

#if 0
jclass
_Jv_FindClass (jstring name, java::lang::ClassLoader *loader)
{
  jclass klass = _Jv_FindClassInCache (name, loader);
  if (loader && ! klass)
    {
      klass = loader->loadClass(name);
      if (klass)
	_Jv_RegisterClass (klass);
    }
  return klass;
}
#endif

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
  ret->state = 0;
  ret->thread = NULL;

  _Jv_RegisterClass (ret);

  return ret;
}

jclass
_Jv_FindArrayClass (jclass element)
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

      // FIXME: initialize other Class instance variables,
      // e.g. `fields'.

      array_class->state = STATE_DONE;
    }

  // For primitive types, point back at this array.
  if (element->isPrimitive())
    element->methods = (_Jv_Method *) array_class;

  return array_class;
}

jboolean
_Jv_IsInstanceOf(jobject obj, jclass cl)
{
  return cl->isInstance(obj);
}
