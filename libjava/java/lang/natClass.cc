// natClass.cc - Implementation of java.lang.Class native methods.

/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>
#include <string.h>

#pragma implementation "Class.h"

#include <gcj/cni.h>
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
#include <java/lang/ExceptionInInitializerError.h>
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
#include <java/lang/StringBuffer.h>
#include <gcj/method.h>

#include <java-cpool.h>



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
#define ConstructorClass _CL_Q44java4lang7reflect11Constructor
extern java::lang::Class ConstructorClass;

// Some constants we use to look up the class initializer.
static _Jv_Utf8Const *void_signature = _Jv_makeUtf8Const ("()V", 3);
static _Jv_Utf8Const *clinit_name = _Jv_makeUtf8Const ("<clinit>", 8);
static _Jv_Utf8Const *init_name = _Jv_makeUtf8Const ("<init>", 6);
static _Jv_Utf8Const *finit_name = _Jv_makeUtf8Const ("$finit$", 7);



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
java::lang::Class::getConstructor (JArray<jclass> *param_types)
{
  jstring partial_sig = getSignature (param_types, true);
  jint hash = partial_sig->hashCode ();

  int i = isPrimitive () ? 0 : method_count;
  while (--i >= 0)
    {
      // FIXME: access checks.
      if (_Jv_equalUtf8Consts (methods[i].name, init_name)
	  && _Jv_equal (methods[i].signature, partial_sig, hash))
	{
	  // Found it.  For getConstructor, the constructor must be
	  // public.
	  using namespace java::lang::reflect;
	  if (! Modifier::isPublic(methods[i].accflags))
	    break;
	  Constructor *cons = new Constructor ();
	  cons->offset = (char *) (&methods[i]) - (char *) methods;
	  cons->declaringClass = this;
	  return cons;
	}
    }
  JvThrow (new java::lang::NoSuchMethodException);
}

JArray<java::lang::reflect::Constructor *> *
java::lang::Class::_getConstructors (jboolean declared)
{
  // FIXME: this method needs access checks.

  int numConstructors = 0;
  int max = isPrimitive () ? 0 : method_count;
  int i;
  for (i = max; --i >= 0; )
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || ! _Jv_equalUtf8Consts (method->name, init_name))
	continue;
      if (! declared
	  && ! java::lang::reflect::Modifier::isPublic(method->accflags))
	continue;
      numConstructors++;
    }
  JArray<java::lang::reflect::Constructor *> *result
    = (JArray<java::lang::reflect::Constructor *> *)
    JvNewObjectArray (numConstructors, &ConstructorClass, NULL);
  java::lang::reflect::Constructor** cptr = elements (result);
  for (i = 0;  i < max;  i++)
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || ! _Jv_equalUtf8Consts (method->name, init_name))
	continue;
      if (! declared
	  && ! java::lang::reflect::Modifier::isPublic(method->accflags))
	continue;
      java::lang::reflect::Constructor *cons
	= new java::lang::reflect::Constructor ();
      cons->offset = (char *) method - (char *) methods;
      cons->declaringClass = this;
      *cptr++ = cons;
    }
  return result;
}

java::lang::reflect::Constructor *
java::lang::Class::getDeclaredConstructor (JArray<jclass> *param_types)
{
  jstring partial_sig = getSignature (param_types, true);
  jint hash = partial_sig->hashCode ();

  int i = isPrimitive () ? 0 : method_count;
  while (--i >= 0)
    {
      // FIXME: access checks.
      if (_Jv_equalUtf8Consts (methods[i].name, init_name)
	  && _Jv_equal (methods[i].signature, partial_sig, hash))
	{
	  // Found it.
	  using namespace java::lang::reflect;
	  Constructor *cons = new Constructor ();
	  cons->offset = (char *) (&methods[i]) - (char *) methods;
	  cons->declaringClass = this;
	  return cons;
	}
    }
  JvThrow (new java::lang::NoSuchMethodException);
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

void
java::lang::Class::getSignature (java::lang::StringBuffer *buffer)
{
  if (isPrimitive())
    buffer->append((jchar) method_count);
  else
    {
      jstring name = getName();
      if (name->charAt(0) != '[')
	buffer->append((jchar) 'L');
      buffer->append(name);
      if (name->charAt(0) != '[')
	buffer->append((jchar) ';');
    }
}

// This doesn't have to be native.  It is an implementation detail
// only called from the C++ code, though, so maybe this is clearer.
jstring
java::lang::Class::getSignature (JArray<jclass> *param_types,
				 jboolean is_constructor)
{
  java::lang::StringBuffer *buf = new java::lang::StringBuffer ();
  buf->append((jchar) '(');
  jclass *v = elements (param_types);
  for (int i = 0; i < param_types->length; ++i)
    v[i]->getSignature(buf);
  buf->append((jchar) ')');
  if (is_constructor)
    buf->append((jchar) 'V');
  return buf->toString();
}

java::lang::reflect::Method *
java::lang::Class::getDeclaredMethod (jstring name,
				      JArray<jclass> *param_types)
{
  jstring partial_sig = getSignature (param_types, false);
  jint p_len = partial_sig->length();
  _Jv_Utf8Const *utf_name = _Jv_makeUtf8Const (name);
  int i = isPrimitive () ? 0 : method_count;
  while (--i >= 0)
    {
      // FIXME: access checks.
      if (_Jv_equalUtf8Consts (methods[i].name, utf_name)
	  && _Jv_equaln (methods[i].signature, partial_sig, p_len))
	{
	  // Found it.
	  using namespace java::lang::reflect;
	  Method *rmethod = new Method ();
	  rmethod->offset = (char*) (&methods[i]) - (char*) methods;
	  rmethod->declaringClass = this;
	  return rmethod;
	}
    }
  JvThrow (new java::lang::NoSuchMethodException);
}

JArray<java::lang::reflect::Method *> *
java::lang::Class::getDeclaredMethods (void)
{
  int numMethods = 0;
  int max = isPrimitive () ? 0 : method_count;
  int i;
  for (i = max; --i >= 0; )
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || _Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name)
	  || _Jv_equalUtf8Consts (method->name, finit_name))
	continue;
      numMethods++;
    }
  JArray<java::lang::reflect::Method *> *result
    = (JArray<java::lang::reflect::Method *> *)
    JvNewObjectArray (numMethods, &MethodClass, NULL);
  java::lang::reflect::Method** mptr = elements (result);
  for (i = 0;  i < max;  i++)
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || _Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name)
	  || _Jv_equalUtf8Consts (method->name, finit_name))
	continue;
      java::lang::reflect::Method* rmethod
	= new java::lang::reflect::Method ();
      rmethod->offset = (char*) method - (char*) methods;
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
  // Until we have inner classes, it always makes sense to return an
  // empty array.
  JArray<jclass> *result
    = (JArray<jclass> *) JvNewObjectArray (0, &ClassClass, NULL);
  return result;
}

JArray<jclass> *
java::lang::Class::getDeclaredClasses (void)
{
  checkMemberAccess (java::lang::reflect::Member::DECLARED);
  // Until we have inner classes, it always makes sense to return an
  // empty array.
  JArray<jclass> *result
    = (JArray<jclass> *) JvNewObjectArray (0, &ClassClass, NULL);
  return result;
}

jclass
java::lang::Class::getDeclaringClass (void)
{
  // Until we have inner classes, it makes sense to always return
  // NULL.
  return NULL;
}

jint
java::lang::Class::_getFields (JArray<java::lang::reflect::Field *> *result,
			       jint offset)
{
  int count = 0;
  for (int i = 0;  i < field_count;  i++)
    {
      _Jv_Field *field = &fields[i];
      if (! (field->getModifiers() & java::lang::reflect::Modifier::PUBLIC))
	continue;
      ++count;

      if (result != NULL)
	{
	  java::lang::reflect::Field *rfield
	    = new java::lang::reflect::Field ();
	  rfield->offset = (char *) field - (char *) fields;
	  rfield->declaringClass = this;
	  rfield->name = _Jv_NewStringUtf8Const (field->name);
	  (elements (result))[offset + i] = rfield;
	}
    }
  jclass superclass = getSuperclass();
  if (superclass != NULL)
    {
      int s_count = superclass->_getFields (result, offset);
      count += s_count;
      offset += s_count;
    }
  for (int i = 0; i < interface_count; ++i)
    {
      int f_count = interfaces[i]->_getFields (result, offset);
      count += f_count;
      offset += f_count;
    }
  return count;
}

JArray<java::lang::reflect::Field *> *
java::lang::Class::getFields (void)
{
  using namespace java::lang::reflect;

  int count = _getFields (NULL, 0);

  JArray<java::lang::reflect::Field *> *result
    = ((JArray<java::lang::reflect::Field *> *)
       JvNewObjectArray (count, &FieldClass, NULL));

  _getFields (result, 0);

  return result;
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
java::lang::Class::getMethod (jstring name, JArray<jclass> *param_types)
{
  jstring partial_sig = getSignature (param_types, false);
  jint p_len = partial_sig->length();
  _Jv_Utf8Const *utf_name = _Jv_makeUtf8Const (name);
  for (Class *klass = this; klass; klass = klass->getSuperclass())
    {
      int i = klass->isPrimitive () ? 0 : klass->method_count;
      while (--i >= 0)
	{
	  // FIXME: access checks.
	  if (_Jv_equalUtf8Consts (klass->methods[i].name, utf_name)
	      && _Jv_equaln (klass->methods[i].signature, partial_sig, p_len))
	    {
	      // Found it.
	      using namespace java::lang::reflect;

	      // Method must be public.
	      if (! Modifier::isPublic (klass->methods[i].accflags))
		break;

	      Method *rmethod = new Method ();
	      rmethod->offset = ((char *) (&klass->methods[i])
				 - (char *) klass->methods);
	      rmethod->declaringClass = klass;
	      return rmethod;
	    }
	}
    }
  JvThrow (new java::lang::NoSuchMethodException);
}

// This is a very slow implementation, since it re-scans all the
// methods we've already listed to make sure we haven't duplicated a
// method.  It also over-estimates the required size, so we have to
// shrink the result array later.
jint
java::lang::Class::_getMethods (JArray<java::lang::reflect::Method *> *result,
				jint offset)
{
  jint count = 0;

  // First examine all local methods
  for (int i = isPrimitive () ? 0 : method_count; --i >= 0; )
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || _Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name)
	  || _Jv_equalUtf8Consts (method->name, finit_name))
	continue;
      // Only want public methods.
      if (! java::lang::reflect::Modifier::isPublic (method->accflags))
	continue;

      // This is where we over-count the slots required if we aren't
      // filling the result for real.
      if (result != NULL)
	{
	  jboolean add = true;
	  java::lang::reflect::Method **mp = elements (result);
	  // If we already have a method with this name and signature,
	  // then ignore this one.  This can happen with virtual
	  // methods.
	  for (int j = 0; j < offset; ++j)
	    {
	      _Jv_Method *meth_2 = _Jv_FromReflectedMethod (mp[j]);
	      if (_Jv_equalUtf8Consts (method->name, meth_2->name)
		  && _Jv_equalUtf8Consts (method->signature,
					  meth_2->signature))
		{
		  add = false;
		  break;
		}
	    }
	  if (! add)
	    continue;
	}

      if (result != NULL)
	{
	  using namespace java::lang::reflect;
	  Method *rmethod = new Method ();
	  rmethod->offset = (char *) method - (char *) methods;
	  rmethod->declaringClass = this;
	  Method **mp = elements (result);
	  mp[offset + count] = rmethod;
	}
      ++count;
    }
  offset += count;

  // Now examine superclasses.
  if (getSuperclass () != NULL)
    {
      jint s_count = getSuperclass()->_getMethods (result, offset);
      offset += s_count;
      count += s_count;
    }

  // Finally, examine interfaces.
  for (int i = 0; i < interface_count; ++i)
    {
      int f_count = interfaces[i]->_getMethods (result, offset);
      count += f_count;
      offset += f_count;
    }

  return count;
}

JArray<java::lang::reflect::Method *> *
java::lang::Class::getMethods (void)
{
  using namespace java::lang::reflect;

  // FIXME: security checks.

  // This will overestimate the size we need.
  jint count = _getMethods (NULL, 0);

  JArray<Method *> *result
    = ((JArray<Method *> *) JvNewObjectArray (count, &MethodClass, NULL));

  // When filling the array for real, we get the actual count.  Then
  // we resize the array.
  jint real_count = _getMethods (result, 0);

  if (real_count != count)
    {
      JArray<Method *> *r2
	= ((JArray<Method *> *) JvNewObjectArray (real_count, &MethodClass,
						  NULL));
      
      Method **destp = elements (r2);
      Method **srcp = elements (result);

      for (int i = 0; i < real_count; ++i)
	*destp++ = *srcp++;

      result = r2;
    }

  return result;
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

  _Jv_InitClass (this);

  _Jv_Method *meth = _Jv_GetMethodLocal (this, init_name, void_signature);
  if (! meth)
    JvThrow (new java::lang::NoSuchMethodException);

  jobject r = JvAllocObject (this);
  ((void (*) (jobject)) meth->ncode) (r);
  return r;
}

void
java::lang::Class::finalize (void)
{
#ifdef INTERPRETER
  JvAssert (_Jv_IsInterpretedClass (this));
  _Jv_UnregisterClass (this);
#endif
}

// This implements the initialization process for a class.  From Spec
// section 12.4.2.
void
java::lang::Class::initializeClass (void)
{
  // Short-circuit to avoid needless locking.
  if (state == JV_STATE_DONE)
    return;

  // do this before we enter the monitor below, since this can cause
  // exceptions.  Here we assume, that reading "state" is an atomic
  // operation, I pressume that is true? --Kresten
  if (state < JV_STATE_LINKED)
    {
#ifdef INTERPRETER
      if (_Jv_IsInterpretedClass (this))
	{
	  java::lang::ClassLoader::resolveClass0 (this);

	  // Step 1.
	  _Jv_MonitorEnter (this);
	}
      else
#endif
        {
          // Step 1.
	  _Jv_MonitorEnter (this);
	  _Jv_PrepareCompiledClass (this);
	}
    }
  else
    {
      // Step 1.
      _Jv_MonitorEnter (this);
    }

  // Step 2.
  java::lang::Thread *self = java::lang::Thread::currentThread();
  // FIXME: `self' can be null at startup.  Hence this nasty trick.
  self = (java::lang::Thread *) ((long) self | 1);
  while (state == JV_STATE_IN_PROGRESS && thread && thread != self)
    wait ();

  // Steps 3 &  4.
  if (state == JV_STATE_DONE || state == JV_STATE_IN_PROGRESS || thread == self)
    {
      _Jv_MonitorExit (this);
      return;
    }

  // Step 5.
  if (state == JV_STATE_ERROR)
    {
      _Jv_MonitorExit (this);
      JvThrow (new java::lang::NoClassDefFoundError);
    }

  // Step 6.
  thread = self;
  state = JV_STATE_IN_PROGRESS;
  _Jv_MonitorExit (this);

  // Step 7.
  if (! isInterface () && superclass)
    {
      try
	{
	  superclass->initializeClass ();
	}
      catch (java::lang::Throwable *except)
	{
	  // Caught an exception.
	  _Jv_MonitorEnter (this);
	  state = JV_STATE_ERROR;
	  notifyAll ();
	  _Jv_MonitorExit (this);
	  throw except;
	}
    }

  // Steps 8, 9, 10, 11.
  try
    {
      _Jv_Method *meth = _Jv_GetMethodLocal (this, clinit_name,
					     void_signature);
      if (meth)
	((void (*) (void)) meth->ncode) ();
    }
  catch (java::lang::Throwable *except)
    {
      if (! ErrorClass.isInstance(except))
	{
	  try
	    {
	      except = new ExceptionInInitializerError (except);
	    }
	  catch (java::lang::Throwable *t)
	    {
	      except = t;
	    }
	}
      _Jv_MonitorEnter (this);
      state = JV_STATE_ERROR;
      notifyAll ();
      _Jv_MonitorExit (this);
      JvThrow (except);
    }

  _Jv_MonitorEnter (this);
  state = JV_STATE_DONE;
  notifyAll ();
  _Jv_MonitorExit (this);
}



//
// Some class-related convenience functions.
//

// Find a method declared in the class.  If it is not declared locally
// (or if it is inherited), return NULL.
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

_Jv_Method *
_Jv_LookupDeclaredMethod (jclass klass, _Jv_Utf8Const *name,
			_Jv_Utf8Const *signature)
{
  for (; klass; klass = klass->getSuperclass())
    {
      _Jv_Method *meth = _Jv_GetMethodLocal (klass, name, signature);

      if (meth)
	return meth;
    }

  return NULL;
}

// NOTE: MCACHE_SIZE should be a power of 2 minus one.
#define MCACHE_SIZE 1023

struct _Jv_mcache
{
  jclass klass;
  _Jv_Method *method;
};

static _Jv_mcache method_cache[MCACHE_SIZE + 1];

static void *
_Jv_FindMethodInCache (jclass klass,
		       _Jv_Utf8Const *name,
		       _Jv_Utf8Const *signature)
{
  int index = name->hash & MCACHE_SIZE;
  _Jv_mcache *mc = method_cache + index;
  _Jv_Method *m = mc->method;

  if (mc->klass == klass
      && m != NULL		// thread safe check
      && _Jv_equalUtf8Consts (m->name, name)
      && _Jv_equalUtf8Consts (m->signature, signature))
    return mc->method->ncode;
  return NULL;
}

static void
_Jv_AddMethodToCache (jclass klass,
			_Jv_Method *method)
{
  _Jv_MonitorEnter (&ClassClass); 

  int index = method->name->hash & MCACHE_SIZE;

  method_cache[index].method = method;
  method_cache[index].klass = klass;

  _Jv_MonitorExit (&ClassClass);
}

void *
_Jv_LookupInterfaceMethod (jclass klass, _Jv_Utf8Const *name,
			   _Jv_Utf8Const *signature)
{
  void *ncode = _Jv_FindMethodInCache (klass, name, signature);
  if (ncode != 0)
    return ncode;

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

      _Jv_AddMethodToCache (klass, meth);

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

jboolean
_Jv_IsInstanceOf(jobject obj, jclass cl)
{
  return cl->isInstance(obj);
}
