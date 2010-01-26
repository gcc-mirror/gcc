// natClass.cc - Implementation of java.lang.Class native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
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
#include <java/lang/reflect/Proxy.h>
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
#include <java/lang/SecurityException.h>
#include <java/lang/SecurityManager.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/VMClassLoader.h>
#include <gcj/method.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/VerifyError.h>
#include <java/lang/InternalError.h>
#include <java/lang/TypeNotPresentException.h>
#include <java/lang/Byte.h>
#include <java/lang/Short.h>
#include <java/lang/Integer.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/Long.h>
#include <java/lang/Character.h>
#include <java/lang/Boolean.h>
#include <java/lang/annotation/Annotation.h>
#include <java/util/HashMap.h>
#include <java/util/Map.h>
#include <sun/reflect/annotation/AnnotationInvocationHandler.h>
#include <java/lang/Enum.h>

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
      return getClassLoader (caller);
   }

  return loader;
}

java::lang::ClassLoader *
java::lang::Class::getClassLoader (jclass caller)
{
  java::lang::SecurityManager *s = java::lang::System::getSecurityManager();
  if (s != NULL)
    {
      ClassLoader *caller_loader = caller->getClassLoaderInternal();

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
  // This ensures we can at least look at their superclasses.
  _Jv_Linker::wait_for_state (this, JV_STATE_LOADING);
  _Jv_Linker::wait_for_state (klass, JV_STATE_LOADING);
  return _Jv_IsAssignableFrom (klass, this);
}

jboolean
java::lang::Class::isInstance (jobject obj)
{
  if (! obj)
    return false;
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

#ifdef INTERPRETER
void
_Jv_ClosureList::releaseClosures (_Jv_ClosureList **closures)
{
  if (!closures)
    return;

  while (_Jv_ClosureList *current = *closures)
    {
      *closures = current->next;
      ffi_closure_free (current->ptr);
    }
}

void
_Jv_ClosureList::registerClosure (jclass klass, void *ptr)
{
  _Jv_ClosureList **closures = klass->engine->get_closure_list (klass);
  if (closures)
    {
      this->ptr = ptr;
      this->next = *closures;
      *closures = this;
    }
}
#endif

// This implements the initialization process for a class.  From Spec
// section 12.4.2.
void
java::lang::Class::initializeClass (void)
{
  // Short-circuit to avoid needless locking (expression includes
  // JV_STATE_PHANTOM and JV_STATE_DONE).
  if (state >= JV_STATE_PHANTOM)
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
	catch (java::lang::SecurityException *x)
	  {
	    throw x;
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
      catch (java::lang::SecurityException *x)
	{
	  throw x;
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
  catch (java::lang::SecurityException *x)
    {
      throw x;
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



static unsigned char
read_u1 (unsigned char *&p)
{
  return *p++;
}

static unsigned char
read_u1 (unsigned char *&p, unsigned char *next)
{
  if (next - p < 1)
    throw new java::lang::InternalError();
  return *p++;
}

static unsigned int
read_u2 (unsigned char *&p)
{
  unsigned int b1 = *p++;
  unsigned int b2 = *p++;
  return (b1 << 8) | b2;
}

static unsigned int
read_u2 (unsigned char *&p, unsigned char *next)
{
  if (next - p < 2)
    throw new java::lang::InternalError();
  return read_u2 (p);
}

static int
read_4 (unsigned char *&p)
{
  int b1 = *p++;
  int b2 = *p++;
  int b3 = *p++;
  int b4 = *p++;
  return (b1 << 24) | (b2 << 16) | (b3 << 8) | b4;
}

jstring
java::lang::Class::getReflectionSignature (jint /*jv_attr_type*/ type,
					   jint obj_index)
{
  // We just re-parse the bytecode for this data each time.  If
  // necessary we can cache results, but I suspect this is not
  // performance sensitive.
  unsigned char *bytes = reflection_data;
  if (bytes == NULL)
    return NULL;
  while (true)
    {
      int kind = read_u1 (bytes);
      if (kind == JV_DONE_ATTR)
	return NULL;
      int len = read_4 (bytes);
      unsigned char *next = bytes + len;
      if (kind != type)
	{
	  bytes = next;
	  continue;
	}
      if (type != JV_CLASS_ATTR)
	{
	  unsigned short index = read_u2 (bytes, next);
	  if (index != obj_index)
	    {
	      bytes = next;
	      continue;
	    }
	}
      int nt = read_u1 (bytes, next);
      if (nt != JV_SIGNATURE_KIND)
	{
	  bytes = next;
	  continue;
	}
      unsigned int cpool_idx = read_u2 (bytes, next);
      if (cpool_idx >= (unsigned int) constants.size
	  || constants.tags[cpool_idx] != JV_CONSTANT_Utf8)
	{
	  // We just ignore errors for now.  It isn't clear what is
	  // best to do here, as an encoding error here means a bug
	  // either in the compiler or in defineclass.cc.
	  return NULL;
	}
      return _Jv_NewStringUtf8Const (constants.data[cpool_idx].utf8);
    }
}

jstring
java::lang::Class::getReflectionSignature (::java::lang::reflect::Constructor *c)
{
  _Jv_Method *meth = _Jv_FromReflectedConstructor (c);
  unsigned short meth_index = meth - methods;
  return getReflectionSignature (JV_METHOD_ATTR, meth_index);
}

jstring
java::lang::Class::getReflectionSignature (::java::lang::reflect::Method *m)
{
  _Jv_Method *meth = _Jv_FromReflectedMethod (m);
  unsigned short meth_index = meth - methods;
  return getReflectionSignature (JV_METHOD_ATTR, meth_index);
}

jstring
java::lang::Class::getReflectionSignature (::java::lang::reflect::Field *f)
{
  _Jv_Field *fld = _Jv_FromReflectedField (f);
  unsigned short fld_index = fld - fields;
  return getReflectionSignature (JV_FIELD_ATTR, fld_index);
}

jstring
java::lang::Class::getClassSignature()
{
  return getReflectionSignature (JV_CLASS_ATTR, 0);
}

jint
java::lang::Class::getEnclosingMethodData()
{
  unsigned char *bytes = reflection_data;
  if (bytes == NULL)
    return 0;
  while (true)
    {
      int kind = read_u1 (bytes);
      if (kind == JV_DONE_ATTR)
	return 0;
      int len = read_4 (bytes);
      unsigned char *next = bytes + len;
      if (kind != JV_CLASS_ATTR)
	{
	  bytes = next;
	  continue;
	}
      int type = read_u1 (bytes, next);
      if (type != JV_ENCLOSING_METHOD_KIND)
	{
	  bytes = next;
	  continue;
	}
      int class_index = read_u2 (bytes, next);
      int method_index = read_u2 (bytes, next);
      _Jv_word result;
      _Jv_storeIndexes (&result, class_index, method_index);
      return result.i;
    }
}

jclass
java::lang::Class::getEnclosingClass()
{
  _Jv_word indexes;
  indexes.i = getEnclosingMethodData();
  if (indexes.i == 0)
    // No enclosing method, but perhaps a member or anonymous class
    return getDeclaringClass();
  _Jv_ushort class_index, method_index;
  _Jv_loadIndexes (&indexes, class_index, method_index);
  return _Jv_Linker::resolve_pool_entry (this, class_index).clazz;
}

::java::lang::reflect::Method *
java::lang::Class::getEnclosingMethod()
{
  _Jv_word indexes;
  indexes.i = getEnclosingMethodData();
  if (indexes.i == 0)
    return NULL;
  _Jv_ushort class_index, method_index;
  _Jv_loadIndexes (&indexes, class_index, method_index);
  jclass found_class;
  _Jv_Method *method = _Jv_Linker::resolve_method_entry (this, found_class,
							 class_index,
							 method_index,
							 false, false);
  using namespace java::lang::reflect;
  Method *rmethod = new Method ();
  rmethod->offset = (char *) method - (char *) found_class->methods;
  rmethod->declaringClass = found_class;
  return rmethod;
}

::java::lang::reflect::Constructor *
java::lang::Class::getEnclosingConstructor()
{
  _Jv_word indexes;
  indexes.i = getEnclosingMethodData();
  if (indexes.i == 0)
    return NULL;
  _Jv_ushort class_index, method_index;
  _Jv_loadIndexes (&indexes, class_index, method_index);
  jclass found_class;
  _Jv_Method *method = _Jv_Linker::resolve_method_entry (this, found_class,
							 class_index,
							 method_index,
							 false, false);
  using namespace java::lang::reflect;
  Constructor *cons = new Constructor ();
  cons->offset = (char *) method - (char *) found_class->methods;
  cons->declaringClass = this;
  return cons;
}

static void
check_constant (_Jv_Constants *pool, jint cpool_index, jint type)
{
  if (cpool_index <= 0 || cpool_index >= pool->size)
    throw new InternalError(JvNewStringLatin1("invalid constant pool index"));
  if ((pool->tags[cpool_index] & 
	~(JV_CONSTANT_ResolvedFlag|JV_CONSTANT_LazyFlag)) != type)
    {
      ::java::lang::StringBuffer *sb = new ::java::lang::StringBuffer();
      sb->append(JvNewStringLatin1("expected pool constant "));
      sb->append(type);
      sb->append(JvNewStringLatin1(" but got "));
      sb->append(jint (pool->tags[cpool_index]));
      throw new InternalError(sb->toString());
    }
}

// Forward declaration
static ::java::lang::annotation::Annotation *
parseAnnotation(jclass klass, _Jv_Constants *pool,
		unsigned char *&bytes, unsigned char *last);

static jobject
parseAnnotationElement(jclass klass, _Jv_Constants *pool,
		       unsigned char *&bytes, unsigned char *last)
{
  int tag = read_u1 (bytes, last);
  jobject result;
  switch (tag)
    {
    case 'B':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Integer);
	result = Byte::valueOf (pool->data[cindex].i);
      }
      break;
    case 'C':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Integer);
	result = Character::valueOf (pool->data[cindex].i);
      }
      break;
    case 'S':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Integer);
	result = Short::valueOf (pool->data[cindex].i);
      }
      break;
    case 'Z':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Integer);
	result = Boolean::valueOf (jboolean (pool->data[cindex].i));
      }
      break;
    case 'I':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Integer);
	result = Integer::valueOf (pool->data[cindex].i);
      }
      break;
    case 'D':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Double);
	_Jv_word2 word;
	memcpy (&word, &pool->data[cindex], 2 * sizeof (_Jv_word));
	result = Double::valueOf (word.d);
      }
      break;
    case 'F':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Float);
	result = Float::valueOf (pool->data[cindex].f);
      }
      break;
    case 'J':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Long);
	_Jv_word2 word;
	memcpy (&word, &pool->data[cindex], 2 * sizeof (_Jv_word));
	result = Long::valueOf (word.l);
      }
      break;
    case 's':
      {
	int cindex = read_u2 (bytes, last);
	// Despite what the JVM spec says, compilers generate a Utf8
	// constant here, not a String.
	check_constant (pool, cindex, JV_CONSTANT_Utf8);
	result = pool->data[cindex].utf8->toString();
      }
      break;
    case 'e':
      {
	int type_name_index = read_u2 (bytes, last);
	check_constant (pool, type_name_index, JV_CONSTANT_Utf8);
 	int const_name_index = read_u2 (bytes, last);
	check_constant (pool, const_name_index, JV_CONSTANT_Utf8);

	_Jv_Utf8Const *u_name = pool->data[type_name_index].utf8;
	_Jv_Utf8Const *e_name = pool->data[const_name_index].utf8;

	// FIXME: throw correct exceptions at the correct times.
	jclass e_class = _Jv_FindClassFromSignature(u_name->chars(),
						    klass->getClassLoaderInternal());
	result = ::java::lang::Enum::valueOf(e_class, e_name->toString());
      }
      break;
    case 'c':
      {
	int cindex = read_u2 (bytes, last);
	check_constant (pool, cindex, JV_CONSTANT_Utf8);
	_Jv_Utf8Const *u_name = pool->data[cindex].utf8;
	jclass anno_class
	  = _Jv_FindClassFromSignatureNoException(u_name->chars(),
						  klass->getClassLoaderInternal());
	// FIXME: not correct: we should lazily do this when trying to
	// read the element.  This means that
	// AnnotationInvocationHandler needs to have a special case.
	if (! anno_class)
	  // FIXME: original exception...
	  throw new TypeNotPresentException(u_name->toString(), NULL);
	result = anno_class;
      }
      break;
    case '@':
      result = parseAnnotation (klass, pool, bytes, last);
      break;
    case '[':
      {
	int n_array_elts = read_u2 (bytes, last);
	jobjectArray aresult = _Jv_NewObjectArray (n_array_elts,
						   &Object::class$, NULL);
	jobject *elts = elements (aresult);
	for (int i = 0; i < n_array_elts; ++i)
	  elts[i] = parseAnnotationElement(klass, pool, bytes, last);
	result = aresult;
      }
      break;
    default:
      throw new java::lang::InternalError();
    }
  return result;
}

static ::java::lang::annotation::Annotation *
parseAnnotation(jclass klass, _Jv_Constants *pool,
		unsigned char *&bytes, unsigned char *last)
{
  int type_index = read_u2 (bytes, last);
  check_constant (pool, type_index, JV_CONSTANT_Utf8);

  _Jv_Utf8Const *u_name = pool->data[type_index].utf8;
  jclass anno_class = _Jv_FindClassFromSignatureNoException(u_name->chars(),
							    klass->getClassLoaderInternal());
  // FIXME: what to do if anno_class==NULL?

  ::java::util::HashMap *hmap = new ::java::util::HashMap();
  int npairs = read_u2 (bytes, last);
  for (int i = 0; i < npairs; ++i)
    {
      int name_index = read_u2 (bytes, last);
      check_constant (pool, name_index, JV_CONSTANT_Utf8);
      jstring name = _Jv_NewStringUtf8Const (pool->data[name_index].utf8);
      jobject value = parseAnnotationElement (klass, pool, bytes, last);
      // FIXME: any checks needed for name?
      hmap->put(name, value);
    }
  using namespace ::sun::reflect::annotation;
  return AnnotationInvocationHandler::create (anno_class,
					      (::java::util::Map *) hmap);
}

static jobjectArray
parseAnnotations(jclass klass, _Jv_Constants *pool,
		 unsigned char *&bytes, unsigned char *last)
{
  int num = read_u2 (bytes, last);
  jobjectArray result = _Jv_NewObjectArray (num,
					    &::java::lang::annotation::Annotation::class$,
					    NULL);
  jobject *elts = elements (result);
  for (int i = 0; i < num; ++i)
    elts[i] = parseAnnotation(klass, pool, bytes, last);
  return result;
}

static jobjectArray
parseParameterAnnotations(jclass klass, _Jv_Constants *pool,
			  unsigned char *&bytes, unsigned char *last)
{
  jclass anno = &::java::lang::annotation::Annotation::class$;
  jclass annoary = _Jv_GetArrayClass (anno, anno->getClassLoaderInternal());

  // FIXME: something should check the number of params versus the
  // method
  int n_params = read_u1 (bytes, last);
  jobjectArray result = _Jv_NewObjectArray (n_params, annoary, NULL);
  jobject *elts = elements (result);
  for (int i = 0; i < n_params; ++i)
    elts[i] = parseAnnotations(klass, pool, bytes, last);
  return result;
}

jobject
java::lang::Class::getMethodDefaultValue(::java::lang::reflect::Method *meth)
{
  // FIXME: could cache the value here...

  unsigned char *bytes = reflection_data;
  if (bytes == NULL)
    return 0;

  unsigned short meth_index = _Jv_FromReflectedMethod (meth) - methods;

  while (true)
    {
      int type = read_u1 (bytes);
      if (type == JV_DONE_ATTR)
	return NULL;
      int len = read_4 (bytes);
      unsigned char *next = bytes + len;
      if (type != JV_METHOD_ATTR)
	{
	  bytes = next;
	  continue;
	}
      int kind = read_u1 (bytes, next);
      if (kind != JV_ANNOTATION_DEFAULT_KIND)
	{
	  bytes = next;
	  continue;
	}
      int index = read_u2 (bytes, next);
      if (meth_index != index)
	{
	  bytes = next;
	  continue;
	}

      // FIXME: could cache here.  If we do then we have to clone any
      // array result.
      return parseAnnotationElement(this, &this->constants, bytes, next);
    }
}

jobjectArray
java::lang::Class::getDeclaredAnnotations(jint /* jv_attr_type */ member_type,
					  jint member_index,
					  jint /* jv_attr_kind */ kind_req)
{
  using namespace java::lang::annotation;
  jobjectArray result;

  unsigned char *bytes = reflection_data;
  if (bytes == NULL)
    return 0;

  if (loader == NULL)
    loader = (ClassLoader *)VMClassLoader::bootLoader;

  result = (loader->getDeclaredAnnotations
	    (this, member_type, member_index, kind_req));
  if (result)
    return result;

  for (;;)
    {
      int type = read_u1 (bytes);
      if (type == JV_DONE_ATTR)
	return NULL;
      int len = read_4 (bytes);
      unsigned char *next = bytes + len;
      if (type != member_type)
	{
	  bytes = next;
	  continue;
	}
      int kind = read_u1 (bytes, next);
      if (kind != kind_req)
	{
	  bytes = next;
	  continue;
	}
      if (member_type != JV_CLASS_ATTR)
	{
	  int index = read_u2 (bytes, next);
	  if (member_index != index)
	    {
	      bytes = next;
	      continue;
	    }
	}

      if (kind_req == JV_PARAMETER_ANNOTATIONS_KIND)
	result = ((parseParameterAnnotations 
		   (this, &this->constants, bytes, next)));
      else
	result = ((parseAnnotations (this, &this->constants, bytes, next)));
      break;
    }

  return (loader->putDeclaredAnnotations
	  (this, member_type, member_index, kind_req, result));
}

jobjectArray
java::lang::Class::getDeclaredAnnotations(::java::lang::reflect::Method *meth,
					  jboolean is_param)
{
  unsigned short meth_index = _Jv_FromReflectedMethod (meth) - methods;
  return getDeclaredAnnotations(JV_METHOD_ATTR, meth_index,
				(is_param
				 ? JV_PARAMETER_ANNOTATIONS_KIND
				 : JV_ANNOTATIONS_KIND));
}

jobjectArray
java::lang::Class::getDeclaredAnnotations(::java::lang::reflect::Constructor *cons,
					  jboolean is_param)
{
  unsigned short meth_index = _Jv_FromReflectedConstructor (cons) - methods;
  return getDeclaredAnnotations(JV_METHOD_ATTR, meth_index,
				(is_param
				 ? JV_PARAMETER_ANNOTATIONS_KIND
				 : JV_ANNOTATIONS_KIND));
}

jobjectArray
java::lang::Class::getDeclaredAnnotations(::java::lang::reflect::Field *fld)
{
  unsigned short field_index = _Jv_FromReflectedField (fld) - fields;
  return getDeclaredAnnotations(JV_FIELD_ATTR, field_index,
				JV_ANNOTATIONS_KIND);
}

JArray< ::java::lang::annotation::Annotation *> *
java::lang::Class::getDeclaredAnnotationsInternal()
{
  return (JArray< ::java::lang::annotation::Annotation *> *) getDeclaredAnnotations(JV_CLASS_ATTR, 0, JV_ANNOTATIONS_KIND);
}

static jclass
resolve_class_constant (jclass klass, _Jv_Constants *pool, int cpool_index)
{
  check_constant (pool, cpool_index, JV_CONSTANT_Class);
  // FIXME: what is the correct thing to do with an exception here?
  return _Jv_Linker::resolve_pool_entry (klass, cpool_index, false).clazz;
}

jint
java::lang::Class::findInnerClassAttribute()
{
  unsigned char *bytes = reflection_data;
  if (bytes == NULL)
    return -1;
  while (true)
    {
      int type = read_u1 (bytes);
      if (type == JV_DONE_ATTR)
	break;
      // After the type but before the length.
      unsigned char *save = bytes;
      int len = read_4 (bytes);
      unsigned char *next = bytes + len;
      if (type != JV_CLASS_ATTR)
	{
	  bytes = next;
	  continue;
	}
      int kind = read_u1 (bytes, next);
      if (kind != JV_INNER_CLASSES_KIND)
	{
	  bytes = next;
	  continue;
	}
      return save - reflection_data;
    }
  return -1;
}

jint
java::lang::Class::findDeclaredClasses(JArray<jclass> *result,
				       jboolean publicOnly,
				       jint offset)
{
  unsigned char *bytes = reflection_data + offset;
  int len = read_4 (bytes);
  unsigned char *next = bytes + len;
  // Skip a byte.
  read_u1 (bytes, next);
  int n_classes = read_u2 (bytes, next);
  int count = 0;
  for (int i = 0; i < n_classes; ++i)
    {
      int inner_class_index = read_u2 (bytes, next);
      int outer_class_index = read_u2 (bytes, next);
      /*int inner_name_index = */ read_u2 (bytes, next);
      int inner_flags = read_u2 (bytes, next);

      if (inner_class_index == 0 || outer_class_index == 0)
	continue;
      if (resolve_class_constant (this, &constants, outer_class_index) == this)
	{
	  jclass inner = resolve_class_constant (this, &constants,
						 inner_class_index);
	  if (! publicOnly
	      || ((inner_flags
		   & java::lang::reflect::Modifier::PUBLIC) != 0))
	    {
	      if (result)
		{
		  jclass *elts = elements (result);
		  elts[count] = inner;
		}
	      ++count;
	    }
	}
    }

  return count;
}

JArray<jclass> *
java::lang::Class::getDeclaredClasses (jboolean publicOnly)
{
  int offset = findInnerClassAttribute();
  int count;
  if (offset == -1)
    {
      // No InnerClasses attribute, so no declared classes.
      count = 0;
    }
  else
    count = findDeclaredClasses(NULL, publicOnly, offset);
  JArray<jclass> *result
    = (JArray<jclass> *) JvNewObjectArray (count, &java::lang::Class::class$,
					   NULL);
  if (count > 0)
    findDeclaredClasses(result, publicOnly, offset);
  return result;
}

jclass
java::lang::Class::getDeclaringClass (void)
{
  int offset = findInnerClassAttribute();
  if (offset == -1)
    return NULL;

  unsigned char *bytes = reflection_data + offset;
  int len = read_4 (bytes);
  unsigned char *next = bytes + len;
  // Skip a byte.
  read_u1 (bytes, next);
  int n_classes = read_u2 (bytes, next);
  for (int i = 0; i < n_classes; ++i)
    {
      int inner_class_index = read_u2 (bytes, next);
      int outer_class_index = read_u2 (bytes, next);
      /*int inner_name_index = */read_u2 (bytes, next);
      /*int inner_flags = */read_u2 (bytes, next);

      if (inner_class_index == 0 || outer_class_index == 0)
	continue;
      if (resolve_class_constant (this, &constants, inner_class_index) == this)
	return resolve_class_constant (this, &constants, outer_class_index);
    }

  return NULL;
}

jboolean
java::lang::Class::isAnonymousClass()
{
  int offset = findInnerClassAttribute();
  if (offset == -1)
    return false;

  unsigned char *bytes = reflection_data + offset;
  int len = read_4 (bytes);
  unsigned char *next = bytes + len;
  // Skip a byte.
  read_u1 (bytes, next);
  int n_classes = read_u2 (bytes, next);
  for (int i = 0; i < n_classes; ++i)
    {
      int inner_class_index = read_u2 (bytes, next);
      /*int outer_class_index = */read_u2 (bytes, next);
      int inner_name_index = read_u2 (bytes, next);
      /*int inner_flags = */read_u2 (bytes, next);

      if (inner_class_index == 0)
	continue;
      if (resolve_class_constant (this, &constants, inner_class_index) == this)
	return inner_name_index == 0;
    }

  return false;
}

jboolean
java::lang::Class::isLocalClass()
{
  _Jv_word indexes;
  indexes.i = getEnclosingMethodData();
  return indexes.i != 0;
}

jboolean
java::lang::Class::isMemberClass()
{
  // FIXME: is this correct?
  return !isLocalClass() && getDeclaringClass() != NULL;
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

java::lang::reflect::Method *
_Jv_GetReflectedMethod (jclass klass, _Jv_Utf8Const *name,
		       _Jv_Utf8Const *signature)
{
  for (; klass; klass = klass->getSuperclass())
    {
      _Jv_Method *meth = _Jv_GetMethodLocal (klass, name, signature);
      if (meth)
	{
	  using namespace java::lang::reflect;
	  Method *rmethod = new Method ();
	  rmethod->offset = (char*) meth - (char*) klass->methods;
	  rmethod->declaringClass = klass;
	  return rmethod;
	}
    }
  
  return NULL;
}

#ifdef HAVE_TLS

// NOTE: MCACHE_SIZE should be a power of 2 minus one.
#define MCACHE_SIZE 31

struct _Jv_mcache
{
  jclass klass;
  _Jv_Method *method;
};

static __thread _Jv_mcache *method_cache;
#endif // HAVE_TLS

static void *
_Jv_FindMethodInCache (jclass klass MAYBE_UNUSED,
		       _Jv_Utf8Const *name MAYBE_UNUSED,
		       _Jv_Utf8Const *signature MAYBE_UNUSED)
{
#ifdef HAVE_TLS
  _Jv_mcache *cache = method_cache;
  if (cache)
    {
      int index = name->hash16 () & MCACHE_SIZE;
      _Jv_mcache *mc = &cache[index];
      _Jv_Method *m = mc->method;

      if (mc->klass == klass
	  && _Jv_equalUtf8Consts (m->name, name)
	  && _Jv_equalUtf8Consts (m->signature, signature))
	return mc->method->ncode;
    }
#endif // HAVE_TLS
  return NULL;
}

static void
_Jv_AddMethodToCache (jclass klass MAYBE_UNUSED,
		      _Jv_Method *method MAYBE_UNUSED)
{
#ifdef HAVE_TLS
  if (method_cache == NULL)
    method_cache = (_Jv_mcache *) _Jv_MallocUnchecked((MCACHE_SIZE + 1)
						      * sizeof (_Jv_mcache));
  // If the allocation failed, just keep going.
  if (method_cache != NULL)
    {
      int index = method->name->hash16 () & MCACHE_SIZE;
      method_cache[index].method = method;
      method_cache[index].klass = klass;
    }
#endif // HAVE_TLS
}

// Free this thread's method cache.  We explicitly manage this memory
// as the GC does not yet know how to scan TLS on all platforms.
void
_Jv_FreeMethodCache ()
{
#ifdef HAVE_TLS
  if (method_cache != NULL)
    {
      _Jv_Free(method_cache);
      method_cache = NULL;
    }
#endif // HAVE_TLS
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
  int idx = iface->ioffsets[cldt->iindex] + method_idx;
  return cldt->itable[idx];
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

      if (__builtin_expect ((target->ioffsets == NULL), false))
	return false; // No class implementing TARGET has been loaded.    
      jshort cl_iindex = cl_idt->iindex;
      if (cl_iindex < target->ioffsets[0])
        {
	  jshort offset = target->ioffsets[cl_iindex];
	  if (offset != -1 && offset < cl_idt->itable_length
	      && cl_idt->itable[offset] == target)
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
      
      int max = klass->method_count;
      int offset = 0;
      for (int i = 0; i < max; ++i)
	{
	  // Skip <clinit> here, as it will not be in the IDT.
	  if (klass->methods[i].name->first() == '<')
	    continue;

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
	      index = offset + 1;
	      return true;
	    }

	  ++offset;
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

#ifdef INTERPRETER
_Jv_MethodBase *
_Jv_FindInterpreterMethod (jclass klass, jmethodID desired_method)
{
  using namespace java::lang::reflect;

  _Jv_InterpClass *iclass
    = reinterpret_cast<_Jv_InterpClass *> (klass->aux_info);
  _Jv_MethodBase **imethods = _Jv_GetFirstMethod (iclass);

  for (int i = 0; i < JvNumMethods (klass); ++i)
    {
      _Jv_MethodBase *imeth = imethods[i];
      if (imeth->get_method () == desired_method)
	return imeth;
    }

  return NULL;
}
#endif

// Return Utf8 name of a class. This function is here for code that
// can't access klass->name directly.
_Jv_Utf8Const*
_Jv_GetClassNameUtf8 (jclass klass)
{
  return klass->name;
}

jclass
_Jv_GetMethodDeclaringClass (jmethodID method)
{
  _Jv_StackTrace::UpdateNCodeMap ();
  jobject obj = reinterpret_cast<jobject> (method->ncode);
  return reinterpret_cast<jclass> (_Jv_StackTrace::ncodeMap->get (obj));
}

jbyte
_Jv_GetClassState (jclass klass)
{
  return klass->state;
}

#ifdef INTERPRETER
jstring
_Jv_GetInterpClassSourceFile (jclass klass)
{
  if (_Jv_IsInterpretedClass (klass))
    {
      _Jv_InterpClass *iclass =
	reinterpret_cast<_Jv_InterpClass *> (klass->aux_info);
      return iclass->source_file_name;
    }

  return NULL;
}
#endif
