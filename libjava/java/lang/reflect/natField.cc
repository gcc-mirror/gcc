// natField.cc - Implementation of java.lang.reflect.Field native methods.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <jvm.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/IllegalArgumentException.h>
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
  if (! fld->isResolved())
    {
      JvSynchronize sync (declaringClass);
      if (! fld->isResolved())
	{
	  fld->type
	    = _Jv_FindClassFromSignature(((Utf8Const*) (fld->type))->data,
					 declaringClass->getClassLoader());
	  fld->flags &= ~_Jv_FIELD_UNRESOLVED_FLAG;
	}
    }
  return fld->type;
}

static void
_Jv_CheckFieldAccessibility (jfieldID /*fld*/, jclass /*caller*/)
{
#if 0
  if (caller == NULL)
    caller = getCaller();
#endif
#if 0
  _Jv_ushort flags = fld->getModifiers();
  check accesss;
#endif
}

static void*
getAddr (java::lang::reflect::Field* field, jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (field);
  _Jv_ushort flags = fld->getModifiers();
  if (! (flags & java::lang::reflect::Modifier::PUBLIC)
      && ! field->isAccessible ())
    _Jv_CheckFieldAccessibility (fld, caller);
  if (flags & java::lang::reflect::Modifier::STATIC)
    {
      jclass fldClass = field->getDeclaringClass ();
      JvInitClass(fldClass);
      return fld->u.addr;
    }
  else
    {
      if (obj == NULL)
	_Jv_Throw (new java::lang::NullPointerException ());
      if (! _Jv_IsInstanceOf (obj, field->getDeclaringClass()))
	JvThrow (new java::lang::IllegalArgumentException ());
      return (void*) ((char*) obj + fld->getOffset ());
    }
}

static jboolean
getBoolean (jclass cls, void* addr)
{
  if (cls == JvPrimClass (boolean))
    return * (jboolean *) addr;
  _Jv_Throw (new java::lang::IllegalArgumentException());
}

static jchar
getChar (jclass cls, void* addr)
{
  if (cls == JvPrimClass (char))
    return * (jchar *) addr;
  _Jv_Throw (new java::lang::IllegalArgumentException());
}

static jbyte
getByte (jclass cls, void* addr)
{
  if (cls == JvPrimClass (byte))
    return * (jbyte *) addr;
  _Jv_Throw (new java::lang::IllegalArgumentException());
}

static jshort
getShort (jclass cls, void* addr)
{
  if (cls == JvPrimClass (short))
    return * (jshort *) addr;
  if (cls == JvPrimClass (byte))
    return * (jbyte *) addr;
  _Jv_Throw (new java::lang::IllegalArgumentException());
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
  _Jv_Throw (new java::lang::IllegalArgumentException());
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
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getBoolean (fld->type, getAddr (this, caller, obj));
}

jchar
java::lang::reflect::Field::getChar (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getChar (fld->type, getAddr (this, caller, obj));
}

jbyte
java::lang::reflect::Field::getByte (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getByte (fld->type, getAddr (this, caller, obj));
}

jshort
java::lang::reflect::Field::getShort (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getShort (fld->type, getAddr (this, caller, obj));
}

jint
java::lang::reflect::Field::getInt (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getInt (fld->type, getAddr (this, caller, obj));
}

jlong
java::lang::reflect::Field::getLong (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getLong (fld->type, getAddr (this, caller, obj));
}

jfloat
java::lang::reflect::Field::getFloat (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getFloat (fld->type, getAddr (this, caller, obj));
}

jdouble
java::lang::reflect::Field::getDouble (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  return ::getDouble (fld->type, getAddr (this, caller, obj));
}

jobject
java::lang::reflect::Field::get (jclass caller, jobject obj)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  void* addr = getAddr (this, caller, obj);
  if (! fld->type->isPrimitive ())
    return * (jobject*) addr;
  if (fld->type == JvPrimClass (double))
    return new java::lang::Double (* (jdouble*) addr);
  if (fld->type == JvPrimClass (float))
    return new java::lang::Float (* (jfloat*) addr);
  if (fld->type == JvPrimClass (long))
    return new java::lang::Long (* (jlong*) addr);
  if (fld->type == JvPrimClass (int))
    return new java::lang::Integer (* (jint*) addr);
  if (fld->type == JvPrimClass (short))
    return new java::lang::Short (* (jshort*) addr);
  if (fld->type == JvPrimClass (byte))
    return new java::lang::Byte (* (jbyte*) addr);
  if (fld->type == JvPrimClass (char))
    return new java::lang::Character (* (jchar*) addr);
  if (fld->type == JvPrimClass (boolean))
    if (* (jboolean*) addr)
      return java::lang::Boolean::TRUE;
    else
      return java::lang::Boolean::FALSE;
  JvThrow (new java::lang::IllegalArgumentException());
}

static void
setBoolean (jclass type, void *addr, jboolean value)
{
  if (type == JvPrimClass (boolean))
    * (jboolean *) addr = value;
  else
    JvThrow (new java::lang::IllegalArgumentException());
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
    JvThrow (new java::lang::IllegalArgumentException());
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
    JvThrow (new java::lang::IllegalArgumentException());
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
    JvThrow (new java::lang::IllegalArgumentException());
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
    JvThrow (new java::lang::IllegalArgumentException());
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
    JvThrow (new java::lang::IllegalArgumentException());
}

static void
setFloat (jclass type, void *addr, jfloat value)
{
  if (type == JvPrimClass (float))
    * (jfloat *) addr = value;
  else if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    JvThrow (new java::lang::IllegalArgumentException());
}

static void
setDouble (jclass type, void *addr, jdouble value)
{
  if (type == JvPrimClass (double))
    * (jdouble *) addr = value;
  else
    JvThrow (new java::lang::IllegalArgumentException());
}

void
java::lang::reflect::Field::setBoolean (jclass caller, jobject obj, jboolean b)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setBoolean (fld->type, getAddr (this, caller, obj), b);
}

void
java::lang::reflect::Field::setChar (jclass caller, jobject obj, jchar c)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setChar (fld->type, getAddr (this, caller, obj), c);
}

void
java::lang::reflect::Field::setByte (jclass caller, jobject obj, jbyte b)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setByte (fld->type, getAddr (this, caller, obj), b);
}

void
java::lang::reflect::Field::setShort (jclass caller, jobject obj, jshort s)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setShort (fld->type, getAddr (this, caller, obj), s);
}

void
java::lang::reflect::Field::setInt (jclass caller, jobject obj, jint i)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setInt (fld->type, getAddr (this, caller, obj), i);
}

void
java::lang::reflect::Field::setLong (jclass caller, jobject obj, jlong l)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setLong (fld->type, getAddr (this, caller, obj), l);
}
void
java::lang::reflect::Field::setFloat (jclass caller, jobject obj, jfloat f)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setFloat (fld->type, getAddr (this, caller, obj), f);
}

void
java::lang::reflect::Field::setDouble (jclass caller, jobject obj, jdouble d)
{
  jfieldID fld = _Jv_FromReflectedField (this);
  ::setDouble (fld->type, getAddr (this, caller, obj), d);
}

void
java::lang::reflect::Field::set (jclass caller, jobject object, jobject value, jclass type)
{
  if (! _Jv_IsInstanceOf (value, type))
    JvThrow (new java::lang::IllegalArgumentException ());
  void* addr = getAddr (this, caller, object);
  * (jobject*) addr = value;
}
