// natObjectInputStream.cc - Native part of ObjectInputStream class.

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

   This ObjectInputStream is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the ObjectInputStream "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <gcj/method.h>

#include <java/io/ObjectInputStream$GetField.h>
#include <java/io/ObjectInputStream.h>
#include <java/io/IOException.h>
#include <java/lang/Class.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/reflect/Method.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/SecurityManager.h>
#include <java/lang/reflect/Constructor.h>
#include <java/lang/reflect/Method.h>

#ifdef DEBUG
#include <java/lang/System.h>
#include <java/io/PrintStream.h>
#endif

jobject
java::io::ObjectInputStream::allocateObject (jclass klass, jclass,
  ::java::lang::reflect::Constructor *ctr)
{
  jobject obj = NULL;
  using namespace java::lang::reflect;

  try
    {
      JvAssert (klass && ! klass->isArray ());
      if (klass->isInterface() || Modifier::isAbstract(klass->getModifiers()))
	obj = NULL;	
      else
	{
	  obj = _Jv_AllocObject (klass);
	}	
    }
  catch (jthrowable t)
    {
      return NULL;
    }

  jmethodID meth = _Jv_FromReflectedConstructor (ctr);

  // This is a bit inefficient, and a bit of a hack, since we don't
  // actually use the Method and since what is returned isn't
  // technically a Method.  We can't use Method.invoke as it looks up
  // the declared method.
  JArray<jclass> *arg_types
    = (JArray<jclass> *) JvNewObjectArray (0, &java::lang::Class::class$,
					   NULL);

  // We lie about this being a constructor.  If we put `true' here
  // then _Jv_CallAnyMethodA would try to allocate the object for us.
  _Jv_CallAnyMethodA (obj, JvPrimClass (void), meth, false, arg_types, NULL);

  return obj;
}

java::lang::ClassLoader*
java::io::ObjectInputStream::currentClassLoader (::java::lang::SecurityManager *sm)
{
  return sm->currentClassLoader ();
}

