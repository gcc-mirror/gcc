// natField.cc - Implementation of java.lang.reflect.Field native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <jvm.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/IllegalAccessException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/Byte.h>
#include <java/lang/Short.h>
#include <java/lang/Integer.h>
#include <java/lang/Long.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/Boolean.h>
#include <java/lang/Character.h>

jint
java::lang::reflect::Field::getModifiers ()
{
  return _Jv_FromReflectedField (this)->getModifiers ();
}

jstring
java::lang::reflect::Field::getName ()
{
  if (name == NULL)
    name = _Jv_NewStringUtf8Const (_Jv_FromReflectedField (this)->name);
  return name;
}

jclass
java::lang::reflect::Field::getType ()
{
  jfieldID fld = _Jv_FromReflectedField (this);
  JvSynchronize sync (declaringClass);
  _Jv_ResolveField (fld, declaringClass->getClassLoaderInternal ());
  return fld->type;
}

static void*
getAddr (java::lang::reflect::Field* field, jclass caller, jobject obj)
{
  // FIXME: we know CALLER is NULL here.  At one point we planned to
  // have the compiler insert the caller as a hidden argument in some
  // calls.  However, we never implemented that, so we have to find
  // the caller by hand instead.
  
  using namespace java::lang::reflect;
  
  jfieldID fld = _Jv_FromReflectedField (field);
  _Jv_ushort flags = fld->getModifiers();
  
  // Check accessibility, if required.
  if (! (Modifier::isPublic (flags) || field->isAccessible()))
    {
      gnu::gcj::runtime::StackTrace *t 
	= new gnu::gcj::runtime::StackTrace(7);
      try
	{
	  // We want to skip all the frames on the stack from this class.
	  for (int i = 1; !caller || caller == &Field::class$; i++)
	    caller = t->classAt (i);
	}
      catch (::java::lang::ArrayIndexOutOfBoundsException *e)
	{
	}

      if (! _Jv_CheckAccess (caller, field->getDeclaringClass(), flags))
	throw new java::lang::IllegalAccessException;
    }

  if (flags & Modifier::STATIC)
    {
      jclass fldClass = field->getDeclaringClass ();
      JvInitClass(fldClass);
      return fld->u.addr;
    }
  else
    {
      if (obj == NULL)
	throw new java::lang::NullPointerException;
      if (! _Jv_IsInstanceOf (obj, field->getDeclaringClass()))
	throw new java::lang::IllegalArgumentException;
      return (void*) ((char*) obj + fld->getOffset ());
    }
}

static jboolean
getBoolean (jclass cls, void* addr)
{
  if (cls == JvPrimClass (boolean))
    return * (jboolean *) addr;
  throw new java::lang::IllegalArgumentException;
}

static jchar
getChar (jclass cls, void* addr)
{
  if (cls == JvPrimClass (char))
    return * (jchar *) addr;
  throw new java::lang::IllegalArgumentException;
}

static jbyte
getByte (jclass cls, void* addr)
{
  if (cls == JvPrimClass (byte))
    return * (jbyte *) addr;
  throw new java::lang::IllegalArgumentException;
}

static jshort
getShort (jclass cls, void* addr)
{
  if (cls == JvPrimClass (short))
    return * (jshort *) addr;
  if (cls == JvPrimClass (byte))
    return * (jbyte *) addr;
  throw new java::lang::IllegalArgumentException;
}

static jint
getInt (jclass cls, void* addr)
{
  if (cls == JvPrimClass (int))
    return * (jint *) addr;
  if (cls == JvPrimClass (short))
    return * (jshort *) addr;
  if (cls == JvPrimClass (char))
    return * (jchar *) addr;
  if (cls == JvPrimClass (byte))
    return * (jbyte *) addr;
  throw new java::lang::IllegalArgumentException;
}

static jlong
getLong (jclass cls, void* addr)
{
  if (cls == JvPrimClass (long))
    return * (jlong *) addr;
  return ::getInt(cls, addr);
}

static jfloat
getFloat (jclass cls, void* addr)
{
  if (cls == JvPrimClass (float))
    return * (jfloat *) addr;
  if (cls == JvPrimClass (long))
    return * (jlong *) addr;
  return ::getInt(cls, addr);
}

static jdouble
getDouble (jclass cls, void* addr)
{
  if (cls == JvPrimClass (double))
    return * (jdouble *) addr;
  if (cls == JvPrimClass (float))
    return * (jfloat *) addr;
  if (cls == JvPrimClass (long))
    return * (jlong *) addr;
  return ::getInt(cls, addr);
}

jboolean
java::lang::reflect::Field::getBoolean (jclass caller, jobject obj)
{
  return ::getBoolean (this->getType(), getAddr (this, caller, obj));
}

jchar
java::lang::reflect::Field::getChar (jclass caller, jobject obj)
{
  return ::getChar (this->getType(), getAddr (this, caller, obj));
}

jbyte
java::lang::reflect::Field::getByte (jclass caller, jobject obj)
{
  return ::getByte (this->getType(), getAddr (this, caller, obj));
}

jshort
java::lang::reflect::Field::getShort (jclass caller, jobject obj)
{
  return ::getShort (this->getType(), getAddr (this, caller, obj));
}

jint
java::lang::reflect::Field::getInt (jclass caller, jobject obj)
{
  return ::getInt (this->getType(), getAddr (this, caller, obj));
}

jlong
java::lang::reflect::Field::getLong (jclass caller, jobject obj)
{
  return ::getLong (this->getType(), getAddr (this, caller, obj));
}

jfloat
java::lang::reflect::Field::getFloat (jclass caller, jobject obj)
{
  return ::getFloat (this->getType(), getAddr (this, caller, obj));
}

jdouble
java::lang::reflect::Field::getDouble (jclass caller, jobject obj)
{
  return ::getDouble (this->getType(), getAddr (this, caller, obj));
}

jobject
java::lang::reflect::Field::get (jclass caller, jobject obj)
{
  jclass type = this->getType();
  void* addr = getAddr (this, caller, obj);
  if (! type->isPrimitive ())
    return * (jobject*) addr;
  if (type == JvPrimClass (double))
    return new java::lang::Double (* (jdouble*) addr);
  if (type == JvPrimClass (float))
    return new java::lang::Float (* (jfloat*) addr);
  if (type == JvPrimClass (long))
    return new java::lang::Long (* (jlong*) addr);
  if (type == JvPrimClass (int))
    return new java::lang::Integer (* (jint*) addr);
  if (type == JvPrimClass (short))
    return new java::lang::Short (* (jshort*) addr);
  if (type == JvPrimClass (byte))
    return new java::lang::Byte (* (jbyte*) addr);
  if (type == JvPrimClass (char))
    return new java::lang::Character (* (jchar*) addr);
  if (type == JvPrimClass (boolean))
    {
      _Jv_InitClass (&java::lang::Boolean::class$);
      if (* (jboolean*) addr)
	return java::lang::Boolean::TRUE;
      else
	return java::lang::Boolean::FALSE;
    }
  throw new java::lang::IllegalArgumentException;
}

static void*
setAddr (java::lang::reflect::Field* field, jclass caller, jobject obj)
{
  void *addr = getAddr(field, caller, obj);
  if  (!field->isAccessible()
	&& field->getModifiers() & java::lang::reflect::Modifier::FINAL)
    throw new java::lang::IllegalAccessException();
  return addr;
}

static void
setBoolean (jclass type, void *addr, jboolean value)
{
  if (type == JvPrimClass (boolean))
    * (jboolean *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

static void
setChar (jclass type, void *addr, jchar value)
{
  if (type == JvPrimClass (char))
    * (jchar *) addr = value;
  else if (type == JvPrimClass (int))
    * (jint *) addr = value;
  else if (type == JvPrimClass (long))
    * (jlong *) addr = value;
  else if (type == JvPrimClass (float))
    * (jfloat *) addr = value;
  else if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

static void
setByte (jclass type, void *addr, jbyte value)
{
  if (type == JvPrimClass (byte))
    * (jbyte *) addr = value;
  else if (type == JvPrimClass (short))
    * (jshort *) addr = value;
  else if (type == JvPrimClass (int))
    * (jint *) addr = value;
  else if (type == JvPrimClass (long))
    * (jlong *) addr = value;
  else if (type == JvPrimClass (float))
    * (jfloat *) addr = value;
  else if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

static void
setShort (jclass type, void *addr, jshort value)
{
  if (type == JvPrimClass (short))
    * (jshort *) addr = value;
  else if (type == JvPrimClass (int))
    * (jint *) addr = value;
  else if (type == JvPrimClass (long))
    * (jlong *) addr = value;
  else if (type == JvPrimClass (float))
    * (jfloat *) addr = value;
  else if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

static void
setInt (jclass type, void *addr, jint value)
{
  if (type == JvPrimClass (int))
    * (jint *) addr = value;
  else if (type == JvPrimClass (long))
    * (jlong *) addr = value;
  else if (type == JvPrimClass (float))
    * (jfloat *) addr = value;
  else if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

static void
setLong (jclass type, void *addr, jlong value)
{
  if (type == JvPrimClass (long))
    * (jlong *) addr = value;
  else if (type == JvPrimClass (float))
    * (jfloat *) addr = value;
  else if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

static void
setFloat (jclass type, void *addr, jfloat value)
{
  if (type == JvPrimClass (float))
    * (jfloat *) addr = value;
  else if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

static void
setDouble (jclass type, void *addr, jdouble value)
{
  if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Field::setBoolean (jclass caller, jobject obj, jboolean b)
{
  ::setBoolean (this->getType(), setAddr (this, caller, obj), b);
}

void
java::lang::reflect::Field::setChar (jclass caller, jobject obj, jchar c)
{
  ::setChar (this->getType(), setAddr (this, caller, obj), c);
}

void
java::lang::reflect::Field::setByte (jclass caller, jobject obj, jbyte b)
{
  ::setByte (this->getType(), setAddr (this, caller, obj), b);
}

void
java::lang::reflect::Field::setShort (jclass caller, jobject obj, jshort s)
{
  ::setShort (this->getType(), setAddr (this, caller, obj), s);
}

void
java::lang::reflect::Field::setInt (jclass caller, jobject obj, jint i)
{
  ::setInt (this->getType(), setAddr (this, caller, obj), i);
}

void
java::lang::reflect::Field::setLong (jclass caller, jobject obj, jlong l)
{
  ::setLong (this->getType(), setAddr (this, caller, obj), l);
}
void
java::lang::reflect::Field::setFloat (jclass caller, jobject obj, jfloat f)
{
  ::setFloat (this->getType(), setAddr (this, caller, obj), f);
}

void
java::lang::reflect::Field::setDouble (jclass caller, jobject obj, jdouble d)
{
  ::setDouble (this->getType(), setAddr (this, caller, obj), d);
}

void
java::lang::reflect::Field::set (jclass caller, jobject object, jobject value,
				 jclass type)
{
  void* addr = setAddr (this, caller, object);
  if (value != NULL && ! _Jv_IsInstanceOf (value, type))
    throw new java::lang::IllegalArgumentException;
  * (jobject*) addr = value;
}
