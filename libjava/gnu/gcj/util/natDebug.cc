// natDebug -- C++ side of Debug

/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <stddef.h>
#include <gcj/cni.h>
#include <gcj/field.h>
#include <gcj/javaprims.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/Class.h>
#include <java/lang/Byte.h>
#include <java/lang/Short.h>
#include <java/lang/Integer.h>
#include <java/lang/Long.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/Boolean.h>
#include <java/lang/Character.h>
#include <java/lang/IllegalArgumentException.h>

#include <gnu/gcj/util/Debug.h>

jlong 
gnu::gcj::util::Debug::getAddr (::java::lang::Object *o)
{
  return (jlong)(size_t)o;
}

JArray< ::java::lang::reflect::Field *> *
gnu::gcj::util::Debug::getDeclaredFields (::java::lang::Class *c)
{
  return c->getDeclaredFields (false);
}

static void *
getField (::java::lang::Object *obj, 
	  ::java::lang::reflect::Field *field)
{
  using namespace java::lang::reflect;
  
  jfieldID fld = _Jv_FromReflectedField (field);
  _Jv_ushort flags = fld->getModifiers();

  if (flags & Modifier::STATIC)
    {
      jclass fldClass = field->getDeclaringClass ();
      JvInitClass(fldClass);
      return (void*) fld->u.addr;
    }
  else
    {
      return (void*) ((char*) obj + fld->getOffset ());
    }
}

::java::lang::Object *
gnu::gcj::util::Debug::getField (::java::lang::Object *o, 
				   ::java::lang::reflect::Field *field)
{
  void *addr = ::getField (o, field);

  jclass type = field->getType();
  if (! type->isPrimitive ())
    return * (jobject*) addr;
  if (type == JvPrimClass (double))
    return new ::java::lang::Double (* (jdouble*) addr);
  if (type == JvPrimClass (float))
    return new ::java::lang::Float (* (jfloat*) addr);
  if (type == JvPrimClass (long))
    return new ::java::lang::Long (* (jlong*) addr);
  if (type == JvPrimClass (int))
    return new ::java::lang::Integer (* (jint*) addr);
  if (type == JvPrimClass (short))
    return new ::java::lang::Short (* (jshort*) addr);
  if (type == JvPrimClass (byte))
    return new ::java::lang::Byte (* (jbyte*) addr);
  if (type == JvPrimClass (char))
    return new ::java::lang::Character (* (jchar*) addr);
  if (type == JvPrimClass (boolean))
    {
      _Jv_InitClass (&::java::lang::Boolean::class$);
      if (* (jboolean*) addr)
	return ::java::lang::Boolean::TRUE;
      else
	return ::java::lang::Boolean::FALSE;
    }
  throw new ::java::lang::IllegalArgumentException;
}

/* A simple method of printing an object that can be called from a
   debugger.  */
extern "C"
void
_Jv_Debug (void *p)
{
  (new ::gnu::gcj::util::Debug ())->write ((jobject)p);
}

extern "C"
void
_Jv_DeepDebug (void *p, int depth)
{
  (new ::gnu::gcj::util::Debug (depth))->write ((jobject)p);
}

extern "C"
void
_Jv_StaticDeepDebug (void *p, int depth)
{
  (new ::gnu::gcj::util::Debug (depth, true))->write ((jobject)p);
}
