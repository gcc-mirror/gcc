// natConstructor.cc - Native code for Constructor class.

/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/reflect/Constructor.h>
#include <java/lang/reflect/Method.h>
#include <java/lang/reflect/InvocationTargetException.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/InstantiationException.h>
#include <gcj/method.h>

jint
java::lang::reflect::Constructor::getModifiers ()
{
  return _Jv_FromReflectedConstructor (this)->accflags;
}

void
java::lang::reflect::Constructor::getType ()
{
  _Jv_GetTypesFromSignature (_Jv_FromReflectedConstructor (this),
			     declaringClass,
			     &parameter_types,
			     NULL);
}

jobject
java::lang::reflect::Constructor::newInstance (jobjectArray args)
{
  if (parameter_types == NULL)
    getType ();

  using namespace java::lang::reflect;
  if (Modifier::isAbstract (declaringClass->getModifiers()))
    JvThrow (new InstantiationException);

  jmethodID meth = _Jv_FromReflectedConstructor (this);
  // In the constructor case the return type is the type of the
  // constructor.
  return _Jv_CallAnyMethodA (NULL, declaringClass, meth, true,
			     parameter_types, args);
}
