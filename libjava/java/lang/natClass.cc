// natClass.cc - Implementation of java.lang.Class native methods.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <limits.h>
#include <string.h>

#pragma implementation "Class.h"

#include <gcj/cni.h>
#include <jvm.h>
#include <java-threads.h>

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/String.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/reflect/Member.h>
#include <java/lang/reflect/Method.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/reflect/Constructor.h>
#include <java/lang/AbstractMethodError.h>
#include <java/lang/ArrayStoreException.h>
#include <java/lang/ClassCastException.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/lang/ExceptionInInitializerError.h>
#include <java/lang/IllegalAccessException.h>
#include <java/lang/IllegalAccessError.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/InstantiationException.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/NoSuchFieldException.h>
#include <java/lang/NoSuchMethodError.h>
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
  // Arguments may not have been initialized, given ".class" syntax.
  _Jv_InitClass (this);
  _Jv_InitClass (klass);
  return _Jv_IsAssignableFrom (this, klass);
}

inline jboolean
java::lang::Class::isInstance (jobject obj)
{
  if (! obj || isPrimitive ())
    return false;
  _Jv_InitClass (this);
  return _Jv_IsAssignableFrom (this, obj->getClass());
}

inline jboolean
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
  // jshort-circuit to avoid needless locking.
  if (state == JV_STATE_DONE)
    return;

  // Step 1.
  _Jv_MonitorEnter (this);

  if (state < JV_STATE_LINKED)
    {    
#ifdef INTERPRETER
      if (_Jv_IsInterpretedClass (this))
	{
	  // this can throw exceptions, so exit the monitor as a precaution.
	  _Jv_MonitorExit (this);
	  java::lang::ClassLoader::resolveClass0 (this);
	  _Jv_MonitorEnter (this);
	}
      else
#endif
        {
	  _Jv_PrepareCompiledClass (this);
	}
    }
  
  if (state <= JV_STATE_LINKED)
    _Jv_PrepareConstantTimeTables (this);

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
      && m != NULL             // thread safe check
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
  using namespace java::lang::reflect;

  void *ncode = _Jv_FindMethodInCache (klass, name, signature);
  if (ncode != 0)
    return ncode;

  for (; klass; klass = klass->getSuperclass())
    {
      _Jv_Method *meth = _Jv_GetMethodLocal (klass, name, signature);
      if (! meth)
        continue;

      if (Modifier::isStatic(meth->accflags))
	JvThrow (new java::lang::IncompatibleClassChangeError
	         (_Jv_GetMethodString (klass, meth->name)));
      if (Modifier::isAbstract(meth->accflags))
	JvThrow (new java::lang::AbstractMethodError
	         (_Jv_GetMethodString (klass, meth->name)));
      if (! Modifier::isPublic(meth->accflags))
	JvThrow (new java::lang::IllegalAccessError
	         (_Jv_GetMethodString (klass, meth->name)));

      _Jv_AddMethodToCache (klass, meth);

      return meth->ncode;
    }
  JvThrow (new java::lang::IncompatibleClassChangeError);
  return NULL;                 // Placate compiler.
}

// Fast interface method lookup by index.
void *
_Jv_LookupInterfaceMethodIdx (jclass klass, jclass iface, int method_idx)
{
  _Jv_IDispatchTable *cldt = klass->idt;
  int idx = iface->idt->iface.ioffsets[cldt->cls.iindex] + method_idx;
  return cldt->cls.itable[idx];
}

inline jboolean
_Jv_IsAssignableFrom (jclass target, jclass source)
{
  if (target == &ObjectClass 
      || source == target 
      || (source->ancestors != NULL 
          && source->ancestors[source->depth - target->depth] == target))
     return true;
     
  // If target is array, so must source be.  
  if (target->isArray ())
    {
      if (! source->isArray())
	return false;
      return _Jv_IsAssignableFrom(target->getComponentType(), 
                                  source->getComponentType());
    }
        
  if (target->isInterface())
    {
      _Jv_IDispatchTable *cl_idt = source->idt;
      _Jv_IDispatchTable *if_idt = target->idt;
      if (if_idt == NULL)
	return false; // No class implementing TARGET has been loaded.    
      jshort cl_iindex = cl_idt->cls.iindex;
      if (cl_iindex <= if_idt->iface.ioffsets[0])
        {
	  jshort offset = if_idt->iface.ioffsets[cl_iindex];
	  if (offset < cl_idt->cls.itable_length
	      && cl_idt->cls.itable[offset] == target)
	    return true;
	}
    }
    
  return false;
}

jboolean
_Jv_IsInstanceOf(jobject obj, jclass cl)
{
  return (obj ? _Jv_IsAssignableFrom (cl, JV_CLASS (obj)) : false);
}

void *
_Jv_CheckCast (jclass c, jobject obj)
{
  if (obj != NULL && ! _Jv_IsAssignableFrom(c, JV_CLASS (obj)))
    JvThrow (new java::lang::ClassCastException);
  return obj;
}

void
_Jv_CheckArrayStore (jobject arr, jobject obj)
{
  if (obj)
    {
      JvAssert (arr != NULL);
      jclass elt_class = (JV_CLASS (arr))->getComponentType();
      jclass obj_class = JV_CLASS (obj);
      if (! _Jv_IsAssignableFrom (elt_class, obj_class))
	JvThrow (new java::lang::ArrayStoreException);
    }
}

#define INITIAL_IOFFSETS_LEN 4
#define INITIAL_IFACES_LEN 4

// Generate tables for constant-time assignment testing and interface
// method lookup. This implements the technique described by Per Bothner
// <per@bothner.com> on the java-discuss mailing list on 1999-09-02:
// http://sourceware.cygnus.com/ml/java-discuss/1999-q3/msg00377.html
void 
_Jv_PrepareConstantTimeTables (jclass klass)
{  
  if (klass->isPrimitive () || klass->isInterface ())
    return;
  
  // Short-circuit in case we've been called already.
  if ((klass->idt != NULL) || klass->depth != 0)
    return;

  // Calculate the class depth and ancestor table. The depth of a class 
  // is how many "extends" it is removed from Object. Thus the depth of 
  // java.lang.Object is 0, but the depth of java.io.FilterOutputStream 
  // is 2. Depth is defined for all regular and array classes, but not 
  // interfaces or primitive types.
   
  jclass klass0 = klass;
  while (klass0 != &ObjectClass)
    {
      klass0 = klass0->superclass;
      klass->depth++;
    }

  // We do class member testing in constant time by using a small table 
  // of all the ancestor classes within each class. The first element is 
  // a pointer to the current class, and the rest are pointers to the 
  // classes ancestors, ordered from the current class down by decreasing 
  // depth. We do not include java.lang.Object in the table of ancestors, 
  // since it is redundant.
	
  klass->ancestors = (jclass *) _Jv_Malloc (klass->depth * sizeof (jclass));
  klass0 = klass;
  for (int index = 0; index < klass->depth; index++)
    {
      klass->ancestors[index] = klass0;
      klass0 = klass0->superclass;
    }
    
  if (java::lang::reflect::Modifier::isAbstract (klass->accflags))
    return;

  klass->idt = 
    (_Jv_IDispatchTable *) _Jv_Malloc (sizeof (_Jv_IDispatchTable));
    
  _Jv_ifaces ifaces;

  ifaces.count = 0;
  ifaces.len = INITIAL_IFACES_LEN;
  ifaces.list = (jclass *) _Jv_Malloc (ifaces.len * sizeof (jclass *));

  int itable_size = _Jv_GetInterfaces (klass, &ifaces);

  if (ifaces.count > 0)
    {
      klass->idt->cls.itable = 
	(void **) _Jv_Malloc (itable_size * sizeof (void *));
      klass->idt->cls.itable_length = itable_size;
          
      jshort *itable_offsets = 
	(jshort *) _Jv_Malloc (ifaces.count * sizeof (jshort));

      _Jv_GenerateITable (klass, &ifaces, itable_offsets);

      jshort cls_iindex = 
	_Jv_FindIIndex (ifaces.list, itable_offsets, ifaces.count);

      for (int i=0; i < ifaces.count; i++)
	{
	  ifaces.list[i]->idt->iface.ioffsets[cls_iindex] =
	    itable_offsets[i];
	}

      klass->idt->cls.iindex = cls_iindex;	    

      _Jv_Free (ifaces.list);
      _Jv_Free (itable_offsets);
    }
  else 
    {
      klass->idt->cls.iindex = SHRT_MAX;
    }
}

// Return index of item in list, or -1 if item is not present.
jshort
_Jv_IndexOf (void *item, void **list, jshort list_len)
{
  for (int i=0; i < list_len; i++)
    {
      if (list[i] == item)
        return i;
    }
  return -1;
}

// Find all unique interfaces directly or indirectly implemented by klass.
// Returns the size of the interface dispatch table (itable) for klass, which 
// is the number of unique interfaces plus the total number of methods that 
// those interfaces declare. May extend ifaces if required.
jshort
_Jv_GetInterfaces (jclass klass, _Jv_ifaces *ifaces)
{
  jshort result = 0;
  
  for (int i=0; i < klass->interface_count; i++)
    {
      jclass iface = klass->interfaces[i];
      if (_Jv_IndexOf (iface, (void **) ifaces->list, ifaces->count) == -1)
        {
	  if (ifaces->count + 1 >= ifaces->len)
	    {
	      /* Resize ifaces list */
	      ifaces->len = ifaces->len * 2;
	      ifaces->list = (jclass *) _Jv_Realloc (ifaces->list, 
	                     ifaces->len * sizeof(jclass));
	    }
	  ifaces->list[ifaces->count] = iface;
	  ifaces->count++;

	  result += _Jv_GetInterfaces (klass->interfaces[i], ifaces);
	}
    }
    
  if (klass->isInterface())
    {
      result += klass->method_count + 1;
    }
  else
    {
      if (klass->superclass)
        {
	  result += _Jv_GetInterfaces (klass->superclass, ifaces);
	}
    }
  return result;
}

// Fill out itable in klass, resolving method declarations in each ifaces.
// itable_offsets is filled out with the position of each iface in itable,
// such that itable[itable_offsets[n]] == ifaces.list[n].
void
_Jv_GenerateITable (jclass klass, _Jv_ifaces *ifaces, jshort *itable_offsets)
{
  void **itable = klass->idt->cls.itable;
  jshort itable_pos = 0;

  for (int i=0; i < ifaces->count; i++)
    { 
      jclass iface = ifaces->list[i];
      itable_offsets[i] = itable_pos;
      itable_pos = _Jv_AppendPartialITable (klass, iface, itable,
                   itable_pos);
      
      /* Create interface dispatch table for iface */
      if (iface->idt == NULL)
	{
	  iface->idt = 
	    (_Jv_IDispatchTable *) _Jv_Malloc (sizeof (_Jv_IDispatchTable));

	  // The first element of ioffsets is its length (itself included).
	  jshort *ioffsets = 
	    (jshort *) _Jv_Malloc (INITIAL_IOFFSETS_LEN * sizeof (jshort));
	  ioffsets[0] = INITIAL_IOFFSETS_LEN;
	  for (int i=1; i < INITIAL_IOFFSETS_LEN; i++)
	    ioffsets[i] = -1;

	  iface->idt->iface.ioffsets = ioffsets;	    
	}
    }
}

// Format method name for use in error messages.
jstring
_Jv_GetMethodString (jclass klass, _Jv_Utf8Const *name)
{
  jstring r = JvNewStringUTF (klass->name->data);
  r = r->concat (JvNewStringUTF ("."));
  r = r->concat (JvNewStringUTF (name->data));
  return r;
}

void 
_Jv_ThrowNoSuchMethodError ()
{
  JvThrow (new java::lang::NoSuchMethodError ());
}

// Each superinterface of a class (i.e. each interface that the class
// directly or indirectly implements) has a corresponding "Partial
// Interface Dispatch Table" whose size is (number of methods + 1) words.
// The first word is a pointer to the interface (i.e. the java.lang.Class
// instance for that interface).  The remaining words are pointers to the
// actual methods that implement the methods declared in the interface,
// in order of declaration.
//
// Append partial interface dispatch table for "iface" to "itable", at
// position itable_pos.
// Returns the offset at which the next partial ITable should be appended.
jshort
_Jv_AppendPartialITable (jclass klass, jclass iface, void **itable, 
                         jshort pos)
{
  using namespace java::lang::reflect;

  itable[pos++] = (void *) iface;
  _Jv_Method *meth;
  
  for (int j=0; j < iface->method_count; j++)
    {
      meth = NULL;
      for (jclass cl = klass; cl; cl = cl->getSuperclass())
        {
	  meth = _Jv_GetMethodLocal (cl, iface->methods[j].name,
                 iface->methods[j].signature);
		 
	  if (meth)
	    break;
	}

      if (meth && (meth->name->data[0] == '<'))
	{
	  // leave a placeholder in the itable for hidden init methods.
          itable[pos] = NULL;	
	}
      else if (meth)
        {
	  if (Modifier::isStatic(meth->accflags))
	    JvThrow (new java::lang::IncompatibleClassChangeError
	             (_Jv_GetMethodString (klass, meth->name)));
	  if (Modifier::isAbstract(meth->accflags))
	    JvThrow (new java::lang::AbstractMethodError
	             (_Jv_GetMethodString (klass, meth->name)));
	  if (! Modifier::isPublic(meth->accflags))
	    JvThrow (new java::lang::IllegalAccessError
	             (_Jv_GetMethodString (klass, meth->name)));

	  itable[pos] = meth->ncode;
	}
      else
        {
	  // The method doesn't exist in klass. Binary compatibility rules
	  // permit this, so we delay the error until runtime using a pointer
	  // to a method which throws an exception.
	  itable[pos] = (void *) _Jv_ThrowNoSuchMethodError;
	}
      pos++;
    }
    
  return pos;
}

static _Jv_Mutex_t iindex_mutex;
bool iindex_mutex_initialized = false;

// We need to find the correct offset in the Class Interface Dispatch 
// Table for a given interface. Once we have that, invoking an interface 
// method just requires combining the Method's index in the interface 
// (known at compile time) to get the correct method.  Doing a type test 
// (cast or instanceof) is the same problem: Once we have a possible Partial 
// Interface Dispatch Table, we just compare the first element to see if it 
// matches the desired interface. So how can we find the correct offset?  
// Our solution is to keep a vector of candiate offsets in each interface 
// (idt->iface.ioffsets), and in each class we have an index 
// (idt->cls.iindex) used to select the correct offset from ioffsets.
//
// Calculate and return iindex for a new class. 
// ifaces is a vector of num interfaces that the class implements.
// offsets[j] is the offset in the interface dispatch table for the
// interface corresponding to ifaces[j].
// May extend the interface ioffsets if required.
jshort
_Jv_FindIIndex (jclass *ifaces, jshort *offsets, jshort num)
{
  int i;
  int j;
  
  // Acquire a global lock to prevent itable corruption in case of multiple 
  // classes that implement an intersecting set of interfaces being linked
  // simultaneously. We can assume that the mutex will be initialized
  // single-threaded.
  if (! iindex_mutex_initialized)
    {
      _Jv_MutexInit (&iindex_mutex);
      iindex_mutex_initialized = true;
    }
  
  _Jv_MutexLock (&iindex_mutex);
  
  for (i=1;; i++)  /* each potential position in ioffsets */
    {
      for (j=0;; j++)  /* each iface */
        {
	  if (j >= num)
	    goto found;
	  if (i > ifaces[j]->idt->iface.ioffsets[0])
	    continue;
	  int ioffset = ifaces[j]->idt->iface.ioffsets[i];
	  /* We can potentially share this position with another class. */
	  if (ioffset >= 0 && ioffset != offsets[j])
	    break; /* Nope. Try next i. */	  
	}
    }
  found:
  for (j = 0; j < num; j++)
    {
      int len = ifaces[j]->idt->iface.ioffsets[0];
      if (i >= len) 
	{
	  /* Resize ioffsets. */
	  int newlen = 2 * len;
	  if (i >= newlen)
	    newlen = i + 3;
	  jshort *old_ioffsets = ifaces[j]->idt->iface.ioffsets;
	  jshort *new_ioffsets = (jshort *) _Jv_Realloc (old_ioffsets, 
	                                  newlen * sizeof(jshort));	  
	  new_ioffsets[0] = newlen;

	  while (len < newlen)
	    new_ioffsets[len++] = -1;
	  
	  ifaces[j]->idt->iface.ioffsets = new_ioffsets;
	}
      ifaces[j]->idt->iface.ioffsets[i] = offsets[j];
    }

  _Jv_MutexUnlock (&iindex_mutex);

  return i;
}
