// natClass.cc - Implementation of java.lang.Class native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005  
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <limits.h>
#include <string.h>
#include <stddef.h>
#include <stdio.h>

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
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/NoSuchFieldError.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/InstantiationException.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/NoSuchFieldException.h>
#include <java/lang/NoSuchMethodError.h>
#include <java/lang/NoSuchMethodException.h>
#include <java/lang/Thread.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/RuntimePermission.h>
#include <java/lang/System.h>
#include <java/lang/SecurityManager.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/VMClassLoader.h>
#include <gcj/method.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/VerifyError.h>

#include <java-cpool.h>
#include <java-interp.h>
#include <java-assert.h>
#include <java-stack.h>
#include <execution.h>



using namespace gcj;

jclass
java::lang::Class::forName (jstring className, jboolean initialize,
                            java::lang::ClassLoader *loader)
{
  if (! className)
    throw new java::lang::NullPointerException;

  jsize length = _Jv_GetStringUTFLength (className);
  char buffer[length];
  _Jv_GetStringUTFRegion (className, 0, className->length(), buffer);

  _Jv_Utf8Const *name = _Jv_makeUtf8Const (buffer, length);

  if (! _Jv_VerifyClassName (name))
    throw new java::lang::ClassNotFoundException (className);

  jclass klass = (buffer[0] == '[' 
		  ? _Jv_FindClassFromSignature (name->chars(), loader)
		  : _Jv_FindClass (name, loader));

  if (klass == NULL)
    throw new java::lang::ClassNotFoundException (className);

  if (initialize)
    _Jv_InitClass (klass);

  return klass;
}

jclass
java::lang::Class::forName (jstring className)
{
  java::lang::ClassLoader *loader = NULL;

  jclass caller = _Jv_StackTrace::GetCallingClass (&Class::class$);
  if (caller)
    loader = caller->getClassLoaderInternal();

  return forName (className, true, loader);
}

java::lang::ClassLoader *
java::lang::Class::getClassLoader (void)
{
  java::lang::SecurityManager *s = java::lang::System::getSecurityManager();
  if (s != NULL)
    {
      jclass caller = _Jv_StackTrace::GetCallingClass (&Class::class$);
      ClassLoader *caller_loader = NULL;
      if (caller)
	caller_loader = caller->getClassLoaderInternal();

      // If the caller has a non-null class loader, and that loader
      // is not this class' loader or an ancestor thereof, then do a
      // security check.
      if (caller_loader != NULL && ! caller_loader->isAncestorOf(loader))
	s->checkPermission (new RuntimePermission (JvNewStringLatin1 ("getClassLoader")));
    }

  return loader;
}

java::lang::reflect::Constructor *
java::lang::Class::getConstructor (JArray<jclass> *param_types)
{
  memberAccessCheck(java::lang::reflect::Member::PUBLIC);

  jstring partial_sig = getSignature (param_types, true);
  jint hash = partial_sig->hashCode ();

  int i = isPrimitive () ? 0 : method_count;
  while (--i >= 0)
    {
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
  throw new java::lang::NoSuchMethodException (_Jv_NewStringUtf8Const (init_name));
}

JArray<java::lang::reflect::Constructor *> *
java::lang::Class::getDeclaredConstructors (jboolean publicOnly)
{
  int numConstructors = 0;
  int max = isPrimitive () ? 0 : method_count;
  int i;
  for (i = max; --i >= 0; )
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || ! _Jv_equalUtf8Consts (method->name, init_name))
	continue;
      if (publicOnly
	  && ! java::lang::reflect::Modifier::isPublic(method->accflags))
	continue;
      numConstructors++;
    }
  JArray<java::lang::reflect::Constructor *> *result
    = (JArray<java::lang::reflect::Constructor *> *)
    JvNewObjectArray (numConstructors,
		      &java::lang::reflect::Constructor::class$,
		      NULL);
  java::lang::reflect::Constructor** cptr = elements (result);
  for (i = 0;  i < max;  i++)
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || ! _Jv_equalUtf8Consts (method->name, init_name))
	continue;
      if (publicOnly
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
  memberAccessCheck(java::lang::reflect::Member::DECLARED);

  jstring partial_sig = getSignature (param_types, true);
  jint hash = partial_sig->hashCode ();

  int i = isPrimitive () ? 0 : method_count;
  while (--i >= 0)
    {
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
  throw new java::lang::NoSuchMethodException (_Jv_NewStringUtf8Const (init_name));
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
  memberAccessCheck(java::lang::reflect::Member::DECLARED);
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
  throw new java::lang::NoSuchFieldException (name);
}

JArray<java::lang::reflect::Field *> *
java::lang::Class::getDeclaredFields (jboolean public_only)
{
  int size;
  if (public_only)
    {
      size = 0;
      for (int i = 0; i < field_count; ++i)
	{
	  _Jv_Field *field = &fields[i];
	  if ((field->flags & java::lang::reflect::Modifier::PUBLIC))
	    ++size;
	}
    }
  else
    size = field_count;

  JArray<java::lang::reflect::Field *> *result
    = (JArray<java::lang::reflect::Field *> *)
    JvNewObjectArray (size, &java::lang::reflect::Field::class$, NULL);
  java::lang::reflect::Field** fptr = elements (result);
  for (int i = 0;  i < field_count;  i++)
    {
      _Jv_Field *field = &fields[i];
      if (public_only
	  && ! (field->flags & java::lang::reflect::Modifier::PUBLIC))
	continue;
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
  // A NULL param_types means "no parameters".
  if (param_types != NULL)
    {
      jclass *v = elements (param_types);
      for (int i = 0; i < param_types->length; ++i)
	v[i]->getSignature(buf);
    }
  buf->append((jchar) ')');
  if (is_constructor)
    buf->append((jchar) 'V');
  return buf->toString();
}

java::lang::reflect::Method *
java::lang::Class::_getDeclaredMethod (jstring name,
				       JArray<jclass> *param_types)
{
  jstring partial_sig = getSignature (param_types, false);
  jint p_len = partial_sig->length();
  _Jv_Utf8Const *utf_name = _Jv_makeUtf8Const (name);
  int i = isPrimitive () ? 0 : method_count;
  while (--i >= 0)
    {
      if (_Jv_equalUtf8Consts (methods[i].name, utf_name)
	  && _Jv_equaln (methods[i].signature, partial_sig, p_len)
	  && (methods[i].accflags
	      & java::lang::reflect::Modifier::INVISIBLE) == 0)
	{
	  // Found it.
	  using namespace java::lang::reflect;
	  Method *rmethod = new Method ();
	  rmethod->offset = (char*) (&methods[i]) - (char*) methods;
	  rmethod->declaringClass = this;
	  return rmethod;
	}
    }
  return NULL;
}

JArray<java::lang::reflect::Method *> *
java::lang::Class::getDeclaredMethods (void)
{
  memberAccessCheck(java::lang::reflect::Member::DECLARED);

  int numMethods = 0;
  int max = isPrimitive () ? 0 : method_count;
  int i;
  for (i = max; --i >= 0; )
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || _Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name)
	  || _Jv_equalUtf8Consts (method->name, finit_name)
	  || (methods[i].accflags
	      & java::lang::reflect::Modifier::INVISIBLE) != 0)
	continue;
      numMethods++;
    }
  JArray<java::lang::reflect::Method *> *result
    = (JArray<java::lang::reflect::Method *> *)
    JvNewObjectArray (numMethods, &java::lang::reflect::Method::class$, NULL);
  java::lang::reflect::Method** mptr = elements (result);
  for (i = 0;  i < max;  i++)
    {
      _Jv_Method *method = &methods[i];
      if (method->name == NULL
	  || _Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name)
	  || _Jv_equalUtf8Consts (method->name, finit_name)
	  || (methods[i].accflags
	      & java::lang::reflect::Modifier::INVISIBLE) != 0)
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
  return name->toString();
}

JArray<jclass> *
java::lang::Class::getDeclaredClasses (jboolean /*publicOnly*/)
{
  // Until we have inner classes, it always makes sense to return an
  // empty array.
  JArray<jclass> *result
    = (JArray<jclass> *) JvNewObjectArray (0, &java::lang::Class::class$,
					   NULL);
  return result;
}

jclass
java::lang::Class::getDeclaringClass (void)
{
  // Until we have inner classes, it makes sense to always return
  // NULL.
  return NULL;
}

JArray<jclass> *
java::lang::Class::getInterfaces (void)
{
  jobjectArray r = JvNewObjectArray (interface_count, getClass (), NULL);
  jobject *data = elements (r);
  for (int i = 0; i < interface_count; ++i)
    {
      typedef unsigned int uaddr __attribute__ ((mode (pointer)));
      data[i] = interfaces[i];
      if ((uaddr)data[i] < (uaddr)constants.size)
	fprintf (stderr, "ERROR !!!\n");
    }
  return reinterpret_cast<JArray<jclass> *> (r);
}

java::lang::reflect::Method *
java::lang::Class::_getMethod (jstring name, JArray<jclass> *param_types)
{
  jstring partial_sig = getSignature (param_types, false);
  jint p_len = partial_sig->length();
  _Jv_Utf8Const *utf_name = _Jv_makeUtf8Const (name);

   for (Class *klass = this; klass; klass = klass->getSuperclass())
    {
      int i = klass->isPrimitive () ? 0 : klass->method_count;
      while (--i >= 0)
	{
	  if (_Jv_equalUtf8Consts (klass->methods[i].name, utf_name)
	      && _Jv_equaln (klass->methods[i].signature, partial_sig, p_len)
	      && (klass->methods[i].accflags
		  & java::lang::reflect::Modifier::INVISIBLE) == 0)
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

  // If we haven't found a match, and this class is an interface, then
  // check all the superinterfaces.
  if (isInterface())
    {
      for (int i = 0; i < interface_count; ++i)
	{
	  using namespace java::lang::reflect;
	  Method *rmethod = interfaces[i]->_getMethod (name, param_types);
	  if (rmethod != NULL)
	    return rmethod;
	}
    }

  return NULL;
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
	  || _Jv_equalUtf8Consts (method->name, finit_name)
	  || (method->accflags
	      & java::lang::reflect::Modifier::INVISIBLE) != 0)
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

  memberAccessCheck(Member::PUBLIC);

  // This will overestimate the size we need.
  jint count = _getMethods (NULL, 0);

  JArray<Method *> *result
    = ((JArray<Method *> *) JvNewObjectArray (count,
					      &Method::class$,
					      NULL));

  // When filling the array for real, we get the actual count.  Then
  // we resize the array.
  jint real_count = _getMethods (result, 0);

  if (real_count != count)
    {
      JArray<Method *> *r2
	= ((JArray<Method *> *) JvNewObjectArray (real_count,
						  &Method::class$,
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
  return _Jv_IsAssignableFrom (klass, this);
}

jboolean
java::lang::Class::isInstance (jobject obj)
{
  if (! obj)
    return false;
  _Jv_InitClass (this);
  return _Jv_IsAssignableFrom (JV_CLASS (obj), this);
}

jobject
java::lang::Class::newInstance (void)
{
  memberAccessCheck(java::lang::reflect::Member::PUBLIC);

  if (isPrimitive ()
      || isInterface ()
      || isArray ()
      || java::lang::reflect::Modifier::isAbstract(accflags))
    throw new java::lang::InstantiationException (getName ());

  _Jv_InitClass (this);

  _Jv_Method *meth = _Jv_GetMethodLocal (this, init_name, void_signature);
  if (! meth)
    throw new java::lang::InstantiationException (getName());

  jobject r = _Jv_AllocObject (this);
  ((void (*) (jobject)) meth->ncode) (r);
  return r;
}

void
java::lang::Class::finalize (void)
{
  engine->unregister(this);
}

// This implements the initialization process for a class.  From Spec
// section 12.4.2.
void
java::lang::Class::initializeClass (void)
{
  // Short-circuit to avoid needless locking.
  if (state == JV_STATE_DONE)
    return;

  // Step 1.  We introduce a new scope so we can synchronize more
  // easily.
  {
    JvSynchronize sync (this);

    if (state < JV_STATE_LINKED)
      {
	try
	  {
	    _Jv_Linker::wait_for_state(this, JV_STATE_LINKED);
	  }
	catch (java::lang::Throwable *x)
	  {
	    // Turn into a NoClassDefFoundError.
	    java::lang::NoClassDefFoundError *result
	      = new java::lang::NoClassDefFoundError(getName());
	    result->initCause(x);
	    throw result;
	  }
      }

    // Step 2.
    java::lang::Thread *self = java::lang::Thread::currentThread();
    self = (java::lang::Thread *) ((long) self | 1);
    while (state == JV_STATE_IN_PROGRESS && thread && thread != self)
      wait ();

    // Steps 3 &  4.
    if (state == JV_STATE_DONE || state == JV_STATE_IN_PROGRESS)
      return;

    // Step 5.
    if (state == JV_STATE_ERROR)
      throw new java::lang::NoClassDefFoundError (getName());

    // Step 6.
    thread = self;
    _Jv_Linker::wait_for_state (this, JV_STATE_LINKED);
    state = JV_STATE_IN_PROGRESS;
  }

  // Step 7.
  if (! isInterface () && superclass)
    {
      try
	{
	  _Jv_InitClass (superclass);
	}
      catch (java::lang::Throwable *except)
	{
	  // Caught an exception.
	  JvSynchronize sync (this);
	  state = JV_STATE_ERROR;
	  notifyAll ();
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
      if (! java::lang::Error::class$.isInstance(except))
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

      JvSynchronize sync (this);
      state = JV_STATE_ERROR;
      notifyAll ();
      throw except;
    }

  JvSynchronize sync (this);
  state = JV_STATE_DONE;
  notifyAll ();
}

// Only used by serialization
java::lang::reflect::Field *
java::lang::Class::getPrivateField (jstring name)
{
  int hash = name->hashCode ();

  java::lang::reflect::Field* rfield;
  for (int i = 0;  i < field_count;  i++)
    {
      _Jv_Field *field = &fields[i];
      if (! _Jv_equal (field->name, name, hash))
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
  rfield = superclass->getPrivateField(name);
  for (int i = 0; i < interface_count && rfield == NULL; ++i)
    rfield = interfaces[i]->getPrivateField (name);
  return rfield;
}

// Only used by serialization
java::lang::reflect::Method *
java::lang::Class::getPrivateMethod (jstring name, JArray<jclass> *param_types)
{
  jstring partial_sig = getSignature (param_types, false);
  jint p_len = partial_sig->length();
  _Jv_Utf8Const *utf_name = _Jv_makeUtf8Const (name);
  for (Class *klass = this; klass; klass = klass->getSuperclass())
    {
      int i = klass->isPrimitive () ? 0 : klass->method_count;
      while (--i >= 0)
	{
	  if (_Jv_equalUtf8Consts (klass->methods[i].name, utf_name)
	      && _Jv_equaln (klass->methods[i].signature, partial_sig, p_len))
	    {
	      // Found it.
	      using namespace java::lang::reflect;

	      Method *rmethod = new Method ();
	      rmethod->offset = ((char *) (&klass->methods[i])
				 - (char *) klass->methods);
	      rmethod->declaringClass = klass;
	      return rmethod;
	    }
	}
    }
  throw new java::lang::NoSuchMethodException (name);
}

// Private accessor method for Java code to retrieve the protection domain.
java::security::ProtectionDomain *
java::lang::Class::getProtectionDomain0 ()
{
  return protectionDomain;
}

JArray<jobject> *
java::lang::Class::getSigners()
{
  return hack_signers;
}

void
java::lang::Class::setSigners(JArray<jobject> *s)
{
  hack_signers = s;
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
                          _Jv_Utf8Const *signature,
			  jclass *declarer_result)
{
  for (; klass; klass = klass->getSuperclass())
    {
      _Jv_Method *meth = _Jv_GetMethodLocal (klass, name, signature);

      if (meth)
	{
	  if (declarer_result)
	    *declarer_result = klass;
	  return meth;
	}
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
  int index = name->hash16 () & MCACHE_SIZE;
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
  _Jv_MonitorEnter (&java::lang::Class::class$); 

  int index = method->name->hash16 () & MCACHE_SIZE;

  method_cache[index].method = method;
  method_cache[index].klass = klass;

  _Jv_MonitorExit (&java::lang::Class::class$);
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
	throw new java::lang::IncompatibleClassChangeError
	  (_Jv_GetMethodString (klass, meth));
      if (Modifier::isAbstract(meth->accflags))
	throw new java::lang::AbstractMethodError
	  (_Jv_GetMethodString (klass, meth));
      if (! Modifier::isPublic(meth->accflags))
	throw new java::lang::IllegalAccessError
	  (_Jv_GetMethodString (klass, meth));

      _Jv_AddMethodToCache (klass, meth);

      return meth->ncode;
    }
  throw new java::lang::IncompatibleClassChangeError;
}

// Fast interface method lookup by index.
void *
_Jv_LookupInterfaceMethodIdx (jclass klass, jclass iface, int method_idx)
{
  _Jv_IDispatchTable *cldt = klass->idt;
  int idx = iface->idt->iface.ioffsets[cldt->cls.iindex] + method_idx;
  return cldt->cls.itable[idx];
}

jboolean
_Jv_IsAssignableFrom (jclass source, jclass target)
{
  if (source == target)
    return true;

  // If target is array, so must source be.  
  while (target->isArray ())
    {
      if (! source->isArray())
	return false;
      target = target->getComponentType();
      source = source->getComponentType();
    }

  if (target->isInterface())
    {
      // Abstract classes have no IDT, and IDTs provide no way to check
      // two interfaces for assignability.
      if (__builtin_expect 
          (source->idt == NULL || source->isInterface(), false))
        return _Jv_InterfaceAssignableFrom (source, target);

      _Jv_IDispatchTable *cl_idt = source->idt;
      _Jv_IDispatchTable *if_idt = target->idt;

      if (__builtin_expect ((if_idt == NULL), false))
	return false; // No class implementing TARGET has been loaded.    
      jshort cl_iindex = cl_idt->cls.iindex;
      if (cl_iindex < if_idt->iface.ioffsets[0])
        {
	  jshort offset = if_idt->iface.ioffsets[cl_iindex];
	  if (offset != -1 && offset < cl_idt->cls.itable_length
	      && cl_idt->cls.itable[offset] == target)
	    return true;
	}
      return false;
    }

  // Primitive TYPE classes are only assignable to themselves.
  if (__builtin_expect (target->isPrimitive() || source->isPrimitive(), false))
    return false;

  if (target == &java::lang::Object::class$)
    return true;
  else if (source->ancestors == NULL || target->ancestors == NULL)
    {
      // We need this case when either SOURCE or TARGET has not has
      // its constant-time tables prepared.

      // At this point we know that TARGET can't be Object, so it is
      // safe to use that as the termination point.
      while (source && source != &java::lang::Object::class$)
	{
	  if (source == target)
	    return true;
	  source = source->getSuperclass();
	}
    }
  else if (source->depth >= target->depth
	   && source->ancestors[source->depth - target->depth] == target)
    return true;

  return false;
}

// Interface type checking, the slow way. Returns TRUE if IFACE is a 
// superinterface of SOURCE. This is used when SOURCE is also an interface,
// or a class with no interface dispatch table.
jboolean
_Jv_InterfaceAssignableFrom (jclass source, jclass iface)
{
  for (int i = 0; i < source->interface_count; i++)
    {
      jclass interface = source->interfaces[i];
      if (iface == interface
          || _Jv_InterfaceAssignableFrom (interface, iface))
        return true;      
    }
    
  if (!source->isInterface()
      && source->superclass 
      && _Jv_InterfaceAssignableFrom (source->superclass, iface))
    return true;
        
  return false;
}

jboolean
_Jv_IsInstanceOf(jobject obj, jclass cl)
{
  if (__builtin_expect (!obj, false))
    return false;
  return _Jv_IsAssignableFrom (JV_CLASS (obj), cl);
}

void *
_Jv_CheckCast (jclass c, jobject obj)
{
  if (__builtin_expect 
      (obj != NULL && ! _Jv_IsAssignableFrom(JV_CLASS (obj), c), false))
    throw new java::lang::ClassCastException
      ((new java::lang::StringBuffer
	(obj->getClass()->getName()))->append
       (JvNewStringUTF(" cannot be cast to "))->append
       (c->getName())->toString());

  return obj;
}

void
_Jv_CheckArrayStore (jobject arr, jobject obj)
{
  if (obj)
    {
      JvAssert (arr != NULL);
      jclass elt_class = (JV_CLASS (arr))->getComponentType();
      if (elt_class == &java::lang::Object::class$)
	return;
      jclass obj_class = JV_CLASS (obj);
      if (__builtin_expect 
          (! _Jv_IsAssignableFrom (obj_class, elt_class), false))
	throw new java::lang::ArrayStoreException
		((new java::lang::StringBuffer
		 (JvNewStringUTF("Cannot store ")))->append
		 (obj_class->getName())->append
		 (JvNewStringUTF(" in array of type "))->append
		 (elt_class->getName())->toString());
    }
}

jboolean
_Jv_IsAssignableFromSlow (jclass source, jclass target)
{
  // First, strip arrays.
  while (target->isArray ())
    {
      // If target is array, source must be as well.
      if (! source->isArray ())
       return false;
      target = target->getComponentType ();
      source = source->getComponentType ();
    }

  // Quick success.
  if (target == &java::lang::Object::class$)
    return true;

  // Ensure that the classes have their supers installed.
  _Jv_Linker::wait_for_state (source, JV_STATE_LOADING);
  _Jv_Linker::wait_for_state (target, JV_STATE_LOADING);

  do
    {
      if (source == target)
       return true;

      if (target->isPrimitive () || source->isPrimitive ())
       return false;

      if (target->isInterface ())
       {
         for (int i = 0; i < source->interface_count; ++i)
           {
             // We use a recursive call because we also need to
             // check superinterfaces.
             if (_Jv_IsAssignableFromSlow (source->getInterface (i), target))
               return true;
           }
       }
      source = source->getSuperclass ();
    }
  while (source != NULL);

  return false;
}

// Lookup an interface method by name.  This is very similar to
// purpose to _getMethod, but the interfaces are quite different.  It
// might be a good idea for _getMethod to call this function.
//
// Return true of the method is found, with the class in FOUND_CLASS
// and the index in INDEX.
bool
_Jv_getInterfaceMethod (jclass search_class, jclass &found_class, int &index,
			const _Jv_Utf8Const *utf_name,  
			const _Jv_Utf8Const *utf_sig)
{
   for (jclass klass = search_class; klass; klass = klass->getSuperclass())
    {
      // FIXME: Throw an exception?
      if (!klass->isInterface ())
	return false;
      
      int i = klass->method_count;
      while (--i >= 0)
	{
	  if (_Jv_equalUtf8Consts (klass->methods[i].name, utf_name)
	      && _Jv_equalUtf8Consts (klass->methods[i].signature, utf_sig))
	    {
	      // Found it.
	      using namespace java::lang::reflect;

	      // FIXME: Method must be public.  Throw an exception?
	      if (! Modifier::isPublic (klass->methods[i].accflags))
		break;

	      found_class = klass;
	      // Interface method indexes count from 1.
	      index = i+1;
	      return true;
	    }
	}
    }

  // If we haven't found a match, and this class is an interface, then
  // check all the superinterfaces.
  if (search_class->isInterface())
    {
      for (int i = 0; i < search_class->interface_count; ++i)
	{
	  using namespace java::lang::reflect;
	  bool found = _Jv_getInterfaceMethod (search_class->interfaces[i], 
					   found_class, index,
					   utf_name, utf_sig);
	  if (found)
	    return true;
	}
    }

  return false;
}
