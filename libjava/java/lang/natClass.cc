// natClass.cc - Implementation of java.lang.Class native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <limits.h>
#include <string.h>
#include <stddef.h>

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
#include <gnu/gcj/runtime/StackTrace.h>
#include <gcj/method.h>
#include <gnu/gcj/runtime/MethodRef.h>
#include <gnu/gcj/RawData.h>

#include <java-cpool.h>
#include <java-interp.h>


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
		  ? _Jv_FindClassFromSignature (name->data, loader)
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
  gnu::gcj::runtime::StackTrace *t 
    = new gnu::gcj::runtime::StackTrace(4);
  java::lang::Class *klass = NULL;
  try
    {
      for (int i = 1; !klass; i++)
	{
	  klass = t->classAt (i);
	}
      loader = klass->getClassLoaderInternal();
    }
  catch (::java::lang::ArrayIndexOutOfBoundsException *e)
    {
    }

  return forName (className, true, loader);
}

java::lang::ClassLoader *
java::lang::Class::getClassLoader (void)
{
  java::lang::SecurityManager *s = java::lang::System::getSecurityManager();
  if (s != NULL)
    {
      gnu::gcj::runtime::StackTrace *t 
	= new gnu::gcj::runtime::StackTrace(4);
      Class *caller = NULL;
      ClassLoader *caller_loader = NULL;
      try
	{
	  for (int i = 1; !caller; i++)
	    {
	      caller = t->classAt (i);
	    }
	  caller_loader = caller->getClassLoaderInternal();
	}
      catch (::java::lang::ArrayIndexOutOfBoundsException *e)
	{
	}

      // If the caller has a non-null class loader, and that loader
      // is not this class' loader or an ancestor thereof, then do a
      // security check.
      if (caller_loader != NULL && ! caller_loader->isAncestorOf(loader))
	s->checkPermission (new RuntimePermission (JvNewStringLatin1 ("getClassLoader")));
    }

  // The spec requires us to return `null' for primitive classes.  In
  // other cases we have the option of returning `null' for classes
  // loaded with the bootstrap loader.  All gcj-compiled classes which
  // are linked into the application used to return `null' here, but
  // that confuses some poorly-written applications.  It is a useful
  // and apparently harmless compatibility hack to simply never return
  // `null' instead.
  if (isPrimitive ())
    return NULL;
  return loader ? loader : ClassLoader::getSystemClassLoader ();
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
java::lang::Class::_getConstructors (jboolean declared)
{
  memberAccessCheck(java::lang::reflect::Member::PUBLIC);

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
  char buffer[name->length + 1];  
  memcpy (buffer, name->data, name->length); 
  buffer[name->length] = '\0';
  return _Jv_NewStringUTF (buffer);
}

JArray<jclass> *
java::lang::Class::getClasses (void)
{
  // FIXME: security checking.

  // Until we have inner classes, it always makes sense to return an
  // empty array.
  JArray<jclass> *result
    = (JArray<jclass> *) JvNewObjectArray (0, &java::lang::Class::class$,
					   NULL);
  return result;
}

JArray<jclass> *
java::lang::Class::getDeclaredClasses (void)
{
  memberAccessCheck (java::lang::reflect::Member::DECLARED);
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
    data[i] = interfaces[i];
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
  return _Jv_IsAssignableFrom (this, klass);
}

jboolean
java::lang::Class::isInstance (jobject obj)
{
  if (! obj)
    return false;
  _Jv_InitClass (this);
  return _Jv_IsAssignableFrom (this, JV_CLASS (obj));
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
  // short-circuit to avoid needless locking.
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
	  java::lang::VMClassLoader::resolveClass (this);
	  _Jv_MonitorEnter (this);
	}
      else
#endif
        {
	  _Jv_PrepareCompiledClass (this);
	}
    }

  // Step 2.
  java::lang::Thread *self = java::lang::Thread::currentThread();
  // FIXME: `self' can be null at startup.  Hence this nasty trick.
  self = (java::lang::Thread *) ((long) self | 1);
  while (state == JV_STATE_IN_PROGRESS && thread && thread != self)
    wait ();

  // Steps 3 &  4.
  if (state == JV_STATE_DONE)
    {
      _Jv_MonitorExit (this);
      return;
    }
  if (state == JV_STATE_IN_PROGRESS)
    {
      _Jv_MonitorExit (this);

      /* Initialization in progress.  The class is linked now,
         so ensure internal tables are built.  */
      _Jv_PrepareConstantTimeTables (this);
      _Jv_MakeVTable(this);
      _Jv_LinkSymbolTable(this);

      return;
    }

  // Step 5.
  if (state == JV_STATE_ERROR)
    {
      _Jv_MonitorExit (this);
      throw new java::lang::NoClassDefFoundError (getName());
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
	  _Jv_InitClass (superclass);
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

  _Jv_PrepareConstantTimeTables (this);

  if (vtable == NULL)
    _Jv_MakeVTable(this);

  if (otable || atable)
    _Jv_LinkSymbolTable(this);

  _Jv_linkExceptionClassTable (this);

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
      _Jv_MonitorEnter (this);
      state = JV_STATE_ERROR;
      notifyAll ();
      _Jv_MonitorExit (this);
      throw except;
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
  _Jv_MonitorEnter (&java::lang::Class::class$); 

  int index = method->name->hash & MCACHE_SIZE;

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
	  (_Jv_GetMethodString (klass, meth->name));
      if (Modifier::isAbstract(meth->accflags))
	throw new java::lang::AbstractMethodError
	  (_Jv_GetMethodString (klass, meth->name));
      if (! Modifier::isPublic(meth->accflags))
	throw new java::lang::IllegalAccessError
	  (_Jv_GetMethodString (klass, meth->name));

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
_Jv_IsAssignableFrom (jclass target, jclass source)
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
        return _Jv_InterfaceAssignableFrom (target, source);

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
_Jv_InterfaceAssignableFrom (jclass iface, jclass source)
{
  for (int i = 0; i < source->interface_count; i++)
    {
      jclass interface = source->interfaces[i];
      if (iface == interface
          || _Jv_InterfaceAssignableFrom (iface, interface))
        return true;      
    }
    
  if (!source->isInterface()
      && source->superclass 
      && _Jv_InterfaceAssignableFrom (iface, source->superclass))
    return true;
        
  return false;
}

jboolean
_Jv_IsInstanceOf(jobject obj, jclass cl)
{
  if (__builtin_expect (!obj, false))
    return false;
  return (_Jv_IsAssignableFrom (cl, JV_CLASS (obj)));
}

void *
_Jv_CheckCast (jclass c, jobject obj)
{
  if (__builtin_expect 
       (obj != NULL && ! _Jv_IsAssignableFrom(c, JV_CLASS (obj)), false))
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
          (! _Jv_IsAssignableFrom (elt_class, obj_class), false))
	throw new java::lang::ArrayStoreException
		((new java::lang::StringBuffer
		 (JvNewStringUTF("Cannot store ")))->append
		 (obj_class->getName())->append
		 (JvNewStringUTF(" in array of type "))->append
		 (elt_class->getName())->toString());
    }
}

#define INITIAL_IOFFSETS_LEN 4
#define INITIAL_IFACES_LEN 4

static _Jv_IDispatchTable null_idt = { {SHRT_MAX, 0, NULL} };

// Generate tables for constant-time assignment testing and interface
// method lookup. This implements the technique described by Per Bothner
// <per@bothner.com> on the java-discuss mailing list on 1999-09-02:
// http://gcc.gnu.org/ml/java/1999-q3/msg00377.html
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
  jboolean has_interfaces = 0;
  while (klass0 != &java::lang::Object::class$)
    {
      has_interfaces += klass0->interface_count;
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
  
  // Optimization: If class implements no interfaces, use a common
  // predefined interface table.
  if (!has_interfaces)
    {
      klass->idt = &null_idt;
      return;
    }

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
inline jshort
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

      /* Make sure interface is linked.  */
      _Jv_WaitForState(iface, JV_STATE_LINKED);

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
      itable_pos = _Jv_AppendPartialITable (klass, iface, itable, itable_pos);
      
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
  throw new java::lang::NoSuchMethodError;
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
	    throw new java::lang::IncompatibleClassChangeError
	      (_Jv_GetMethodString (klass, meth->name));
	  if (Modifier::isAbstract(meth->accflags))
	    throw new java::lang::AbstractMethodError
	      (_Jv_GetMethodString (klass, meth->name));
	  if (! Modifier::isPublic(meth->accflags))
	    throw new java::lang::IllegalAccessError
	      (_Jv_GetMethodString (klass, meth->name));

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
static bool iindex_mutex_initialized = false;

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
	  if (i >= ifaces[j]->idt->iface.ioffsets[0])
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

// Functions for indirect dispatch (symbolic virtual binding) support.

// There are two tables, atable and otable.  atable is an array of
// addresses, and otable is an array of offsets, and these are used
// for static and virtual members respectively.

// {a,o}table_syms is an array of _Jv_MethodSymbols.  Each such symbol
// is a tuple of {classname, member name, signature}.
// _Jv_LinkSymbolTable() scans these two arrays and fills in the
// corresponding atable and otable with the addresses of static
// members and the offsets of virtual members.

// The offset (in bytes) for each resolved method or field is placed
// at the corresponding position in the virtual method offset table
// (klass->otable). 

// The same otable and atable may be shared by many classes.

void
_Jv_LinkSymbolTable(jclass klass)
{
  //// FIXME: Need to lock the tables ////
  
  int index = 0;
  _Jv_MethodSymbol sym;
  if (klass->otable == NULL
      || klass->otable->state != 0)
    goto atable;
   
  klass->otable->state = 1;

  for (index = 0; sym = klass->otable_syms[index], sym.name != NULL; index++)
    {
      // FIXME: Why are we passing NULL as the class loader?
      jclass target_class = _Jv_FindClass (sym.class_name, NULL);
      _Jv_Method *meth = NULL;            

      const _Jv_Utf8Const *signature = sym.signature;

      {
	static char *bounce = (char *)_Jv_ThrowNoSuchMethodError;
	ptrdiff_t offset = (char *)(klass->vtable) - bounce;
	klass->otable->offsets[index] = offset;
      }

      if (target_class == NULL)
	continue;

      if (target_class->isInterface())
	{
	  // FIXME: This does not yet fully conform to binary compatibility
	  // rules. It will break if a declaration is moved into a 
	  // superinterface.
	  for (jclass cls = target_class; cls != 0; cls = cls->getSuperclass ())
	    {
	      for (int i=0; i < cls->method_count; i++)
		{
		  meth = &cls->methods[i];
		  if (_Jv_equalUtf8Consts (sym.name, meth->name)
		      && _Jv_equalUtf8Consts (signature, meth->signature))
		    {
		      klass->otable->offsets[index] = i + 1;
		      goto found;
		    }
		}
	    
	    }
	found:
	  continue;
	}

      // We're looking for a field or a method, and we can tell
      // which is needed by looking at the signature.
      if (signature->length >= 2
	  && signature->data[0] == '(')
	{
 	  // If the target class does not have a vtable_method_count yet, 
	  // then we can't tell the offsets for its methods, so we must lay 
	  // it out now.
	  if (target_class->vtable_method_count == -1)
	    {
	      JvSynchronize sync (target_class);
	      _Jv_LayoutVTableMethods (target_class);
	    }
		
	  meth = _Jv_LookupDeclaredMethod(target_class, sym.name, 
					  sym.signature);
		
	  if (meth != NULL)
	    {
	      klass->otable->offsets[index] = 
		_Jv_VTable::idx_to_offset (meth->index);	      
	    }

	  continue;
	}

      // try fields
      {
	_Jv_Field *the_field = NULL;

	for (jclass cls = target_class; cls != 0; cls = cls->getSuperclass ())
	  {
	    for (int i = 0; i < cls->field_count; i++)
	      {
		_Jv_Field *field = &cls->fields[i];
		if (! _Jv_equalUtf8Consts (field->name, sym.name))
		  continue;

		// FIXME: What access checks should we perform here?
// 		if (_Jv_CheckAccess (klass, cls, field->flags))
// 		  {

		if (!field->isResolved ())
		  _Jv_ResolveField (field, cls->loader);

// 		if (field_type != 0 && field->type != field_type)
// 		  throw new java::lang::LinkageError
// 		    (JvNewStringLatin1 
// 		     ("field type mismatch with different loaders"));

		the_field = field;
		goto end_of_field_search;
	      }
	  }
      end_of_field_search:
	if (the_field != NULL)
	  {
	    if (the_field->flags & 0x0008 /* Modifier::STATIC */)
	      {	      
		throw new java::lang::IncompatibleClassChangeError;
	      }
	    else
	      {
		klass->otable->offsets[index] = the_field->u.boffset;
	      }
	  }
	else
	  {
	    throw new java::lang::NoSuchFieldError
	      (_Jv_NewStringUtf8Const (sym.name));
	  }
      }
    }

 atable:
  if (klass->atable == NULL
      || klass->atable->state != 0)
    return;

  klass->atable->state = 1;

  for (index = 0; sym = klass->atable_syms[index], sym.name != NULL; index++)
    {
      // FIXME: Why are we passing NULL as the class loader?
      jclass target_class = _Jv_FindClass (sym.class_name, NULL);
      _Jv_Method *meth = NULL;            
      const _Jv_Utf8Const *signature = sym.signature;

      // ??? Setting this pointer to null will at least get us a
      // NullPointerException
      klass->atable->addresses[index] = NULL;
      
      if (target_class == NULL)
	continue;
      
      // We're looking for a static field or a static method, and we
      // can tell which is needed by looking at the signature.
      if (signature->length >= 2
	  && signature->data[0] == '(')
	{
 	  // If the target class does not have a vtable_method_count yet, 
	  // then we can't tell the offsets for its methods, so we must lay 
	  // it out now.
	  if (target_class->vtable_method_count == -1)
	    {
	      JvSynchronize sync (target_class);
	      _Jv_LayoutVTableMethods (target_class);
	    }
	  
	  meth = _Jv_LookupDeclaredMethod(target_class, sym.name, 
					  sym.signature);
	  
	  if (meth != NULL)
	    {
	      if (meth->ncode) // Maybe abstract?
		klass->atable->addresses[index] = meth->ncode;
#ifdef INTERPRETER
	      else if (_Jv_IsInterpretedClass (target_class))
		_Jv_Defer_Resolution (target_class, meth, 
				      &klass->atable->addresses[index]);
#endif
	    }
	  else
	    klass->atable->addresses[index] = (void *)_Jv_ThrowNoSuchMethodError;

	  continue;
	}

      // try fields
      {
	_Jv_Field *the_field = NULL;

	for (jclass cls = target_class; cls != 0; cls = cls->getSuperclass ())
	  {
	    for (int i = 0; i < cls->field_count; i++)
	      {
		_Jv_Field *field = &cls->fields[i];
		if (! _Jv_equalUtf8Consts (field->name, sym.name))
		  continue;

		// FIXME: What access checks should we perform here?
// 		if (_Jv_CheckAccess (klass, cls, field->flags))
// 		  {

		if (!field->isResolved ())
		  _Jv_ResolveField (field, cls->loader);
		
// 		if (field_type != 0 && field->type != field_type)
// 		  throw new java::lang::LinkageError
// 		    (JvNewStringLatin1 
// 		     ("field type mismatch with different loaders"));

		the_field = field;
		goto end_of_static_field_search;
	      }
	  }
      end_of_static_field_search:
	if (the_field != NULL)
	  {
	    if (the_field->flags & 0x0008 /* Modifier::STATIC */)
	      {	      
		klass->atable->addresses[index] = the_field->u.addr;
	      }
	    else
	      {
		throw new java::lang::IncompatibleClassChangeError;
	      }
	  }
	else
	  {
	    throw new java::lang::NoSuchFieldError
	      (_Jv_NewStringUtf8Const (sym.name));
	  }
      }
    }
}


// For each catch_record in the list of caught classes, fill in the
// address field.
void 
_Jv_linkExceptionClassTable (jclass self)
{
  struct _Jv_CatchClass *catch_record = self->catch_classes;
  if (!catch_record || catch_record->classname)
    return;  
  catch_record++;
  while (catch_record->classname)
    {
      jclass target_class = _Jv_FindClass (catch_record->classname,  
					   self->getClassLoaderInternal ());
      *catch_record->address = target_class;
      catch_record++;
    }
  self->catch_classes->classname = (_Jv_Utf8Const *)-1;
}
  
// This is put in empty vtable slots.
static void
_Jv_abstractMethodError (void)
{
  throw new java::lang::AbstractMethodError();
}

// Prepare virtual method declarations in KLASS, and any superclasses as 
// required, by determining their vtable index, setting method->index, and
// finally setting the class's vtable_method_count. Must be called with the
// lock for KLASS held.
void
_Jv_LayoutVTableMethods (jclass klass)
{
  if (klass->vtable != NULL || klass->isInterface() 
      || klass->vtable_method_count != -1)
    return;

  jclass superclass = klass->superclass;

  typedef unsigned int uaddr __attribute__ ((mode (pointer)));

  // If superclass looks like a constant pool entry,
  // resolve it now.
  if ((uaddr)superclass < (uaddr)klass->constants.size)
    {
      if (klass->state < JV_STATE_LINKED)
	{
	  _Jv_Utf8Const *name = klass->constants.data[(int)superclass].utf8;
	  superclass = _Jv_FindClass (name, klass->loader);
	  if (! superclass)
	    {
	      jstring str = _Jv_NewStringUTF (name->data);
	      throw new java::lang::NoClassDefFoundError (str);
	    }
	}
      else
	superclass = klass->constants.data[(int)superclass].clazz;
    }

  if (superclass != NULL && superclass->vtable_method_count == -1)
    {
      JvSynchronize sync (superclass);
      _Jv_LayoutVTableMethods (superclass);
    }

  int index = (superclass == NULL ? 0 : superclass->vtable_method_count);

  for (int i = 0; i < klass->method_count; ++i)
    {
      _Jv_Method *meth = &klass->methods[i];
      _Jv_Method *super_meth = NULL;

      if (! _Jv_isVirtualMethod (meth))
	continue;

      // FIXME: Must check that we don't override:
      // - Package-private method where superclass is in different package.
      // - Final or less-accessible declaration in superclass (check binary 
      //   spec, do we allocate new vtable entry or put throw node in vtable?)
      // - Static or private method in superclass.

      if (superclass != NULL)
	{
	  super_meth = _Jv_LookupDeclaredMethod (superclass, meth->name, 
						 meth->signature);
	}

      if (super_meth)
        meth->index = super_meth->index;
      else
	meth->index = index++;
    }

  klass->vtable_method_count = index;
}

// Set entries in VTABLE for virtual methods declared in KLASS. If
// KLASS has an immediate abstract parent, recursively do its methods
// first.  FLAGS is used to determine which slots we've actually set.
void
_Jv_SetVTableEntries (jclass klass, _Jv_VTable *vtable, jboolean *flags)
{
  using namespace java::lang::reflect;

  jclass superclass = klass->getSuperclass();

  if (superclass != NULL && (superclass->getModifiers() & Modifier::ABSTRACT))
    _Jv_SetVTableEntries (superclass, vtable, flags);

  for (int i = klass->method_count - 1; i >= 0; i--)
    {
      _Jv_Method *meth = &klass->methods[i];
      if (meth->index == (_Jv_ushort) -1)
	continue;
      if ((meth->accflags & Modifier::ABSTRACT))
	{
	  vtable->set_method(meth->index, (void *) &_Jv_abstractMethodError);
	  flags[meth->index] = false;
	}
      else
	{
	  vtable->set_method(meth->index, meth->ncode);
	  flags[meth->index] = true;
	}
    }
}

// Allocate and lay out the virtual method table for KLASS. This will also
// cause vtables to be generated for any non-abstract superclasses, and
// virtual method layout to occur for any abstract superclasses. Must be
// called with monitor lock for KLASS held.
void
_Jv_MakeVTable (jclass klass)
{
  using namespace java::lang::reflect;  

  if (klass->vtable != NULL || klass->isInterface() 
      || (klass->accflags & Modifier::ABSTRACT))
    return;

  //  out before we can create a vtable. 
  if (klass->vtable_method_count == -1)
    _Jv_LayoutVTableMethods (klass);

  // Allocate the new vtable.
  _Jv_VTable *vtable = _Jv_VTable::new_vtable (klass->vtable_method_count);
  klass->vtable = vtable;

  jboolean flags[klass->vtable_method_count];
  for (int i = 0; i < klass->vtable_method_count; ++i)
    flags[i] = false;

  // Copy the vtable of the closest non-abstract superclass.
  jclass superclass = klass->superclass;
  if (superclass != NULL)
    {
      while ((superclass->accflags & Modifier::ABSTRACT) != 0)
	superclass = superclass->superclass;

      if (superclass->vtable == NULL)
	{
	  JvSynchronize sync (superclass);
	  _Jv_MakeVTable (superclass);
	}

      for (int i = 0; i < superclass->vtable_method_count; ++i)
	{
	  vtable->set_method (i, superclass->vtable->get_method (i));
	  flags[i] = true;
	}
    }

  // Set the class pointer and GC descriptor.
  vtable->clas = klass;
  vtable->gc_descr = _Jv_BuildGCDescr (klass);

  // For each virtual declared in klass and any immediate abstract 
  // superclasses, set new vtable entry or override an old one.
  _Jv_SetVTableEntries (klass, vtable, flags);

  // It is an error to have an abstract method in a concrete class.
  if (! (klass->accflags & Modifier::ABSTRACT))
    {
      for (int i = 0; i < klass->vtable_method_count; ++i)
	if (! flags[i])
	  {
	    using namespace java::lang;
	    while (klass != NULL)
	      {
		for (int j = 0; j < klass->method_count; ++j)
		  {
		    if (klass->methods[i].index == i)
		      {
			StringBuffer *buf = new StringBuffer ();
			buf->append (_Jv_NewStringUtf8Const (klass->methods[i].name));
			buf->append ((jchar) ' ');
			buf->append (_Jv_NewStringUtf8Const (klass->methods[i].signature));
			throw new AbstractMethodError (buf->toString ());
		      }
		  }
		klass = klass->getSuperclass ();
	      }
	    // Couldn't find the name, which is weird.
	    // But we still must throw the error.
	    throw new AbstractMethodError ();
	  }
    }
}
