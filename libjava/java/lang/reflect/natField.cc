// natField.cc - Implementation of java.lang.reflect.Field native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <jvm.h>
#include <java-stack.h>
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

typedef JArray< ::java::lang::annotation::Annotation * > * anno_a_t;

jint
java::lang::reflect::Field::getModifiersInternal ()
{
  return _Jv_FromReflectedField (this)->flags;
}

jstring
java::lang::reflect::Field::getSignature()
{
  return declaringClass->getReflectionSignature (this);
}

anno_a_t
java::lang::reflect::Field::getDeclaredAnnotationsInternal()
{
  return (anno_a_t) declaringClass->getDeclaredAnnotations(this);
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
  if (type == NULL)
    {
      jfieldID fld = _Jv_FromReflectedField (this);
      JvSynchronize sync (declaringClass);
      _Jv_Linker::resolve_field (fld, declaringClass->getClassLoaderInternal ());
      type = fld->type;
    }
  return type;
}

static void*
getAddr (java::lang::reflect::Field* field, jclass caller, jobject obj,
         jboolean checkFinal)
{
  using namespace java::lang::reflect;
  
  jfieldID fld = _Jv_FromReflectedField (field);
  _Jv_ushort flags = fld->getModifiers();

  // Setting a final field is usually not allowed.
  if (checkFinal
      // As of 1.5, you can set a non-static final field if it is
      // accessible.
      && (! field->isAccessible()
	  || (field->getModifiers() & java::lang::reflect::Modifier::STATIC))
      && (field->getModifiers() & java::lang::reflect::Modifier::FINAL))
    throw new java::lang::IllegalAccessException(JvNewStringUTF 
      ("Field is final"));
  
  // Check accessibility, if required.
  if (! (Modifier::isPublic (flags) || field->isAccessible()))
    {
      if (! caller)
	caller = _Jv_StackTrace::GetCallingClass (&Field::class$);
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
  return ::getBoolean (this->getType(), getAddr (this, caller, obj, false));
}

jchar
java::lang::reflect::Field::getChar (jclass caller, jobject obj)
{
  return ::getChar (this->getType(), getAddr (this, caller, obj, false));
}

jbyte
java::lang::reflect::Field::getByte (jclass caller, jobject obj)
{
  return ::getByte (this->getType(), getAddr (this, caller, obj, false));
}

jshort
java::lang::reflect::Field::getShort (jclass caller, jobject obj)
{
  return ::getShort (this->getType(), getAddr (this, caller, obj, false));
}

jint
java::lang::reflect::Field::getInt (jclass caller, jobject obj)
{
  return ::getInt (this->getType(), getAddr (this, caller, obj, false));
}

jlong
java::lang::reflect::Field::getLong (jclass caller, jobject obj)
{
  return ::getLong (this->getType(), getAddr (this, caller, obj, false));
}

jfloat
java::lang::reflect::Field::getFloat (jclass caller, jobject obj)
{
  return ::getFloat (this->getType(), getAddr (this, caller, obj, false));
}

jdouble
java::lang::reflect::Field::getDouble (jclass caller, jobject obj)
{
  return ::getDouble (this->getType(), getAddr (this, caller, obj, false));
}

jobject
java::lang::reflect::Field::get (jclass caller, jobject obj)
{
  jclass type = this->getType();
  void* addr = getAddr (this, caller, obj, false);
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
java::lang::reflect::Field::setBoolean (jclass caller, jobject obj, jboolean b,
					jboolean checkFinal)
{
  ::setBoolean (this->getType(), getAddr (this, caller, obj, checkFinal), b);
}

void
java::lang::reflect::Field::setChar (jclass caller, jobject obj, jchar c,
				     jboolean checkFinal)
{
  ::setChar (this->getType(), getAddr (this, caller, obj, checkFinal), c);
}

void
java::lang::reflect::Field::setByte (jclass caller, jobject obj, jbyte b,
				     jboolean checkFinal)
{
  ::setByte (this->getType(), getAddr (this, caller, obj, checkFinal), b);
}

void
java::lang::reflect::Field::setShort (jclass caller, jobject obj, jshort s,
				      jboolean checkFinal)
{
  ::setShort (this->getType(), getAddr (this, caller, obj, checkFinal), s);
}

void
java::lang::reflect::Field::setInt (jclass caller, jobject obj, jint i,
				    jboolean checkFinal)
{
  ::setInt (this->getType(), getAddr (this, caller, obj, checkFinal), i);
}

void
java::lang::reflect::Field::setLong (jclass caller, jobject obj, jlong l,
				     jboolean checkFinal)
{
  ::setLong (this->getType(), getAddr (this, caller, obj, checkFinal), l);
}

void
java::lang::reflect::Field::setFloat (jclass caller, jobject obj, jfloat f,
				      jboolean checkFinal)
{
  ::setFloat (this->getType(), getAddr (this, caller, obj, checkFinal), f);
}

void
java::lang::reflect::Field::setDouble (jclass caller, jobject obj, jdouble d,
				       jboolean checkFinal)
{
  ::setDouble (this->getType(), getAddr (this, caller, obj, checkFinal), d);
}

void
java::lang::reflect::Field::set (jclass caller, jobject object, jobject value,
				 jclass type, jboolean checkFinal)
{
  void* addr = getAddr (this, caller, object, checkFinal);
  if (value != NULL && ! _Jv_IsInstanceOf (value, type))
    throw new java::lang::IllegalArgumentException;
  * (jobject*) addr = value;
}
