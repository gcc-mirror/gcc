// natField.cc - Implementation of java.lang.reflect.Field native methods.

/* Copyright (C) 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <jvm.h>
#include <gcj/cni.h>
#include <java/lang/reflect/Array.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/Byte.h>
#include <java/lang/Short.h>
#include <java/lang/Integer.h>
#include <java/lang/Long.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/Boolean.h>
#include <java/lang/Character.h>

jobject
java::lang::reflect::Array::newInstance (jclass componentType, jint length)
{
  if (componentType->isPrimitive())
    {
      // We could check for this in _Jv_NewPrimArray, but that seems
      // like needless overhead when the only real route to this
      // problem is here.
      if (componentType == JvPrimClass (void))
	throw new java::lang::IllegalArgumentException ();
      return _Jv_NewPrimArray (componentType, length);
    }
  else
    return JvNewObjectArray (length, componentType, NULL);

}

jobject
java::lang::reflect::Array::newInstance (jclass componentType,
					 jintArray dimensions)
{
  jint ndims = dimensions->length;
  if (ndims == 0)
    throw new java::lang::IllegalArgumentException ();
  jint* dims = elements (dimensions);
  if (ndims == 1)
    return newInstance (componentType, dims[0]);
  jclass arrayType = componentType;
  for (int i = 0;  i < ndims;  i++)  // FIXME 2nd arg should 
                                     // be "current" loader
    arrayType = _Jv_GetArrayClass (arrayType, 0);

  return _Jv_NewMultiArray (arrayType, ndims, dims);
}

jint
java::lang::reflect::Array::getLength (jobject array)
{
  jclass arrayType = array->getClass();
  if (! arrayType->isArray ())
    throw new java::lang::IllegalArgumentException;
  return ((__JArray*) array)->length;
}

jclass
java::lang::reflect::Array::getElementType (jobject array, jint index)
{
  jclass arrayType = array->getClass();
  if (! arrayType->isArray ())
    throw new java::lang::IllegalArgumentException;
  jint length = ((__JArray*) array)->length;
  if ((_Jv_uint) index >= (_Jv_uint) length)
    _Jv_ThrowBadArrayIndex(index);
  return arrayType->getComponentType ();
}

jboolean
java::lang::reflect::Array::getBoolean (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (boolean))
    return elements ((jbooleanArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jchar
java::lang::reflect::Array::getChar (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (char))
    return elements ((jcharArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jbyte
java::lang::reflect::Array::getByte (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (byte))
    return elements ((jbyteArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jshort
java::lang::reflect::Array::getShort (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (short))
    return elements ((jshortArray) array) [index];
  if (elementType == JvPrimClass (byte))
    return elements ((jbyteArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jint
java::lang::reflect::Array::getInt (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (int))
    return elements ((jintArray) array) [index];
  if (elementType == JvPrimClass (short))
    return elements ((jshortArray) array) [index];
  if (elementType == JvPrimClass (byte))
    return elements ((jbyteArray) array) [index];
  if (elementType == JvPrimClass (char))
    return elements ((jcharArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jlong
java::lang::reflect::Array::getLong (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (long))
    return elements ((jlongArray) array) [index];
  if (elementType == JvPrimClass (int))
    return elements ((jintArray) array) [index];
  if (elementType == JvPrimClass (short))
    return elements ((jshortArray) array) [index];
  if (elementType == JvPrimClass (byte))
    return elements ((jbyteArray) array) [index];
  if (elementType == JvPrimClass (char))
    return elements ((jcharArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jfloat
java::lang::reflect::Array::getFloat (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (float))
    return elements ((jfloatArray) array) [index];
  if (elementType == JvPrimClass (long))
    return elements ((jlongArray) array) [index];
  if (elementType == JvPrimClass (int))
    return elements ((jintArray) array) [index];
  if (elementType == JvPrimClass (short))
    return elements ((jshortArray) array) [index];
  if (elementType == JvPrimClass (byte))
    return elements ((jbyteArray) array) [index];
  if (elementType == JvPrimClass (char))
    return elements ((jcharArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jdouble
java::lang::reflect::Array::getDouble (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (double))
    return elements ((jdoubleArray) array) [index];
  if (elementType == JvPrimClass (float))
    return elements ((jfloatArray) array) [index];
  if (elementType == JvPrimClass (long))
    return elements ((jlongArray) array) [index];
  if (elementType == JvPrimClass (int))
    return elements ((jintArray) array) [index];
  if (elementType == JvPrimClass (short))
    return elements ((jshortArray) array) [index];
  if (elementType == JvPrimClass (byte))
    return elements ((jbyteArray) array) [index];
  if (elementType == JvPrimClass (char))
    return elements ((jcharArray) array) [index];
  throw new java::lang::IllegalArgumentException;
}

jobject
java::lang::reflect::Array::get (jobject array, jint index)
{
  jclass elementType = getElementType (array, index);
  if (! elementType->isPrimitive ())
    return elements ((jobjectArray) array) [index];
  if (elementType == JvPrimClass (double))
    return new java::lang::Double (elements ((jdoubleArray) array) [index]);
  if (elementType == JvPrimClass (float))
    return new java::lang::Float (elements ((jfloatArray) array) [index]);
  if (elementType == JvPrimClass (long))
    return new java::lang::Long (elements ((jlongArray) array) [index]);
  if (elementType == JvPrimClass (int))
    return new java::lang::Integer (elements ((jintArray) array) [index]);
  if (elementType == JvPrimClass (short))
    return new java::lang::Short (elements ((jshortArray) array) [index]);
  if (elementType == JvPrimClass (byte))
    return new java::lang::Byte (elements ((jbyteArray) array) [index]);
  if (elementType == JvPrimClass (char))
    return new java::lang::Character (elements ((jcharArray) array) [index]);
  if (elementType == JvPrimClass (boolean))
    {
      _Jv_InitClass (&java::lang::Boolean::class$);
      if (elements ((jbooleanArray) array) [index])
	return java::lang::Boolean::TRUE;
      else
	return java::lang::Boolean::FALSE;
    }
  throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setChar (jobject array, jint index, jchar value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (char))
    elements ((jcharArray) array) [index] = value;
  else if (elementType == JvPrimClass (int))
    elements ((jintArray) array) [index] = value;
  else if (elementType == JvPrimClass (long))
    elements ((jlongArray) array) [index] = value;
  else if (elementType == JvPrimClass (float))
    elements ((jfloatArray) array) [index] = value;
  else if (elementType == JvPrimClass (double))
    elements ((jdoubleArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setByte (jobject array, jint index, jbyte value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (byte))
    elements ((jbyteArray) array) [index] = value;
  else if (elementType == JvPrimClass (short))
    elements ((jshortArray) array) [index] = value;
  else if (elementType == JvPrimClass (int))
    elements ((jintArray) array) [index] = value;
  else if (elementType == JvPrimClass (long))
    elements ((jlongArray) array) [index] = value;
  else if (elementType == JvPrimClass (float))
    elements ((jfloatArray) array) [index] = value;
  else if (elementType == JvPrimClass (double))
    elements ((jdoubleArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setShort (jobject array, jint index, jshort value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (short))
    elements ((jshortArray) array) [index] = value;
  else if (elementType == JvPrimClass (int))
    elements ((jintArray) array) [index] = value;
  else if (elementType == JvPrimClass (long))
    elements ((jlongArray) array) [index] = value;
  else if (elementType == JvPrimClass (float))
    elements ((jfloatArray) array) [index] = value;
  else if (elementType == JvPrimClass (double))
    elements ((jdoubleArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setInt (jobject array, jint index, jint value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (int))
    elements ((jintArray) array) [index] = value;
  else if (elementType == JvPrimClass (long))
    elements ((jlongArray) array) [index] = value;
  else if (elementType == JvPrimClass (float))
    elements ((jfloatArray) array) [index] = value;
  else if (elementType == JvPrimClass (double))
    elements ((jdoubleArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setLong (jobject array, jint index, jlong value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (long))
    elements ((jlongArray) array) [index] = value;
  else if (elementType == JvPrimClass (float))
    elements ((jfloatArray) array) [index] = value;
  else if (elementType == JvPrimClass (double))
    elements ((jdoubleArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setFloat (jobject array, jint index, jfloat value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (float))
    elements ((jfloatArray) array) [index] = value;
  else if (elementType == JvPrimClass (double))
    elements ((jdoubleArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setDouble (jobject array, jint index, jdouble value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (double))
    elements ((jdoubleArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::setBoolean (jobject array,
					jint index, jboolean value)
{
  jclass elementType = getElementType (array, index);
  if (elementType == JvPrimClass (boolean))
    elements ((jbooleanArray) array) [index] = value;
  else
    throw new java::lang::IllegalArgumentException;
}

void
java::lang::reflect::Array::set (jobject array, jint index,
				       jobject value, jclass elType)
{
  if (! _Jv_IsInstanceOf (value, elType))
    throw new java::lang::IllegalArgumentException;
  elements ((jobjectArray) array) [index] = value;
}
