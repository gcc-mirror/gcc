// natMethod.cc - Native code for Method class.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// This is about 90% done.  Search for FIXME to see what remains.

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/reflect/Method.h>
#include <java/lang/reflect/InvocationTargetException.h>
#include <java/lang/reflect/Modifier.h>

#include <java/lang/Void.h>
#include <java/lang/Byte.h>
#include <java/lang/Boolean.h>
#include <java/lang/Character.h>
#include <java/lang/Short.h>
#include <java/lang/Integer.h>
#include <java/lang/Long.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/Class.h>
#include <gcj/method.h>

#define ClassClass _CL_Q34java4lang5Class
extern java::lang::Class ClassClass;

#include <stdlib.h>

#if 0

#include <ffi.h>

#define VoidClass _CL_Q34java4lang4Void
extern java::lang::Class VoidClass;
#define ByteClass _CL_Q34java4lang4Byte
extern java::lang::Class ByteClass;
#define ShortClass _CL_Q34java4lang5Short
extern java::lang::Class ShortClass;
#define CharacterClass _CL_Q34java4lang9Character
extern java::lang::Class CharacterClass;
#define IntegerClass _CL_Q34java4lang7Integer
extern java::lang::Class IntegerClass;
#define LongClass _CL_Q34java4lang4Long
extern java::lang::Class LongClass;
#define FloatClass _CL_Q34java4lang5Float
extern java::lang::Class FloatClass;
#define DoubleClass _CL_Q34java4lang6Double
extern java::lang::Class DoubleClass;

struct cpair
{
  jclass prim;
  jclass wrap;
};

// This is used to determine when a primitive widening conversion is
// allowed.
static cpair primitives[] =
{
#define VOID 0
  { JvPrimClass (void), &VoidClass },
  { JvPrimClass (byte), &ByteClass },
#define SHORT 2
  { JvPrimClass (short), &ShortClass },
#define CHAR 3
  { JvPrimClass (char), &CharacterClass },
  { JvPrimClass (int), &IntegerClass },
  { JvPrimClass (long), &LongClass },
  { JvPrimClass (float), &FloatClass },
  { JvPrimClass (double), &DoubleClass },
  { NULL, NULL }
};

static jboolean
can_widen (jclass from, jclass to)
{
  int fromx = -1, tox = -1;

  for (int i = 0; primitives[i].prim; ++i)
    {
      if (primitives[i].wrap == from)
	fromx = i;
      if (primitives[i].prim == to)
	tox = i;
    }

  // Can't handle a miss.
  if (fromx == -1 || tox == -1)
    return false;
  // Can't handle Void arguments.
  if (fromx == VOID || tox == VOID)
    return false;
  // Special-case short/char conversions.
  if ((fromx == SHORT && tox == CHAR) || (fromx == CHAR && tox == SHORT))
    return false;

  return fromx <= tox;
}

static ffi_type *
get_ffi_type (jclass klass)
{
  // A special case.
  if (klass == NULL)
    return &ffi_type_pointer;

  ffi_type *r;
  if (klass == JvPrimClass (byte))
    r = &ffi_type_sint8;
  else if (klass == JvPrimClass (short))
    r = &ffi_type_sint16;
  else if (klass == JvPrimClass (int))
    r = &ffi_type_sint32;
  else if (klass == JvPrimClass (long))
    r = &ffi_type_sint64;
  else if (klass == JvPrimClass (float))
    r = &ffi_type_float;
  else if (klass == JvPrimClass (double))
    r = &ffi_type_double;
  else if (klass == JvPrimClass (boolean))
    {
      // FIXME.
      r = &ffi_type_sint8;
    }
  else if (klass == JvPrimClass (char))
    r = &ffi_type_uint16;
  else
    {
      JvAssert (! klass->isPrimitive());
      r = &ffi_type_pointer;
    }

  return r;
}

// FIXME: the body of this method should be a separate function so
// that Constructor can use it too.
jobject
java::lang::reflect::Method::invoke (jobject obj,
				     jobjectArray args)
{
  // FIXME: we need to be a friend of Class here.
  _Jv_Method *meth = decl_class->methods[index];
  if (! java::lang::reflect::Modifier::isStatic(modifiers))
    {
      jclass k = obj ? obj->getClass() : NULL;
      if (! obj || ! decl_class->isAssignableFrom(k))
	JvThrow (new java::lang::NullPointerException);
      // FIXME: access checks.
      meth = _Jv_LookupMethod (k, meth->name, meth->signature);
    }

  // FIXME: access checks.

  if (parameter_types->length != args->length)
    JvThrow (new java::lang::IllegalArgumentException);

  ffi_type *rtype = get_ffi_type (return_type);
  ffi_type **argtypes = (ffi_type **) alloca (parameter_types->length
					      * sizeof (ffi_type *));

  jobject *paramelts = elements (parameter_types);
  jobject *argelts = elements (args);

  int size = 0;
  for (int i = 0; i < parameter_types->length; ++i)
    {
      jclass k = argelts[i] ? argelts[i]->getClass() : NULL;
      argtypes[i] = get_ffi_type (k);
      if (paramelts[i]->isPrimitive())
	{
	  if (! argelts[i]
	      || ! k->isPrimitive ()
	      || ! can_widen (k, paramelts[i]))
	    JvThrow (new java::lang::IllegalArgumentException);
	  size += paramelts[i]->size();
	}
      else
	{
	  if (argelts[i] && ! paramelts[i]->isAssignableFrom (k))
	    JvThrow (new java::lang::IllegalArgumentException);
	  size += sizeof (jobject);
	}
    }

  ffi_cif cif;
  if (ffi_prep_cif (&cif, FFI_DEFAULT_ABI, parameter_types->length,
		    rtype, argtypes) != FFI_OK)
    {
      // FIXME: throw some kind of VirtualMachineError here.
    }

  char *values = (char *) alloca (size);
  char *p = values;

#define COPY(Where, What, Type) \
  do { \
    Type val = (What); \
    memcpy ((Where), &val, sizeof (Type)); \
    Where += sizeof (Type); \
  } while (0)

  for (int i = 0; i < parameter_types->length; ++i)
    {
      java::lang::Number *num = (java::lang::Number *) paramelts[i];
      if (paramelts[i] == JvPrimClass (byte))
	COPY (p, num->byteValue(), jbyte);
      else if (paramelts[i] == JvPrimClass (short))
	COPY (p, num->shortValue(), jshort);
      else if (paramelts[i] == JvPrimClass (int))
	COPY (p, num->intValue(), jint);
      else if (paramelts[i] == JvPrimClass (long))
	COPY (p, num->longValue(), jlong);
      else if (paramelts[i] == JvPrimClass (float))
	COPY (p, num->floatValue(), jfloat);
      else if (paramelts[i] == JvPrimClass (double))
	COPY (p, num->doubleValue(), jdouble);
      else if (paramelts[i] == JvPrimClass (boolean))
	COPY (p, ((java::lang::Boolean *) argelts[i])->booleanValue(), jboolean);
      else if (paramelts[i] == JvPrimClass (char))
	COPY (p, ((java::lang::Character *) argelts[i])->charValue(), jchar);
      else
	{
	  JvAssert (! paramelts[i]->isPrimitive());
	  COPY (p, argelts[i], jobject);
	}
    }

  // FIXME: exception handling.
  java::lang::Throwable *ex;
  jdouble ret_value;		// Largest possible value.  Hopefully
				// it is aligned!
  ex = TRAMP_CALL (ffi_call (&cif, meth->ncode, &ret_value, (void *) values));

  if (ex)
    JvThrow (new InvocationTargetException (ex));

  jobject r;
#define VAL(Wrapper, Type)  (new Wrapper (* (Type *) &ret_value))
  if (return_type == JvPrimClass (byte))
    r = VAL (java::lang::Byte, jbyte);
  else if (return_type == JvPrimClass (short))
    r = VAL (java::lang::Short, jshort);
  else if (return_type == JvPrimClass (int))
    r = VAL (java::lang::Integer, jint);
  else if (return_type == JvPrimClass (long))
    r = VAL (java::lang::Long, jlong);
  else if (return_type == JvPrimClass (float))
    r = VAL (java::lang::Float, jfloat);
  else if (return_type == JvPrimClass (double))
    r = VAL (java::lang::Double, jdouble);
  else if (return_type == JvPrimClass (boolean))
    r = VAL (java::lang::Boolean, jboolean);
  else if (return_type == JvPrimClass (char))
    r = VAL (java::lang::Character, jchar);
  else if (return_type == JvPrimClass (void))
    r = NULL;
  else
    {
      JvAssert (! return_type->isPrimitive());
      r = VAL (java::lang::Object, jobject);
    }

  return r;
}

#else /* 0 */

jobject
java::lang::reflect::Method::invoke (jobject, jobjectArray)
{
  JvFail ("not enabled yet");
}

#endif /* 0 */

jint
java::lang::reflect::Method::getModifiers ()
{
  return _Jv_FromReflectedMethod (this)->accflags;
}

jstring
java::lang::reflect::Method::getName ()
{
  if (name == NULL)
    name = _Jv_NewStringUtf8Const (_Jv_FromReflectedMethod (this)->name);
  return name;
}

/* Internal method to set return_type and parameter_types fields. */

void
java::lang::reflect::Method::getType ()
{
  _Jv_Utf8Const* sig = _Jv_FromReflectedMethod (this)->signature;
  java::lang::ClassLoader *loader = declaringClass->getClassLoader();
  char *ptr = sig->data;
  int numArgs = 0;
  /* First just count the number of parameters. */
  for (; ; ptr++)
    {
      switch (*ptr)
	{
	case 0:
	case ')':
	case 'V':
	  break;
	case '[':
	case '(':
	  continue;
	case 'B':
	case 'C':
	case 'D':
	case 'F':
	case 'S':
	case 'I':
	case 'J':
	case 'Z':
	  numArgs++;
	  continue;
	case 'L':
	  numArgs++;
	  do 
	    ptr++;
	  while (*ptr != ';' && ptr[1] != '\0');
	  continue;
	}
      break;
    }

  JArray<jclass> *args = (JArray<jclass> *)
    JvNewObjectArray (numArgs, &ClassClass, NULL);
  jclass* argPtr = elements (args);
  for (ptr = sig->data; *ptr != '\0'; ptr++)
    {
      int num_arrays = 0;
      jclass type;
      for (; *ptr == '[';  ptr++)
	num_arrays++;
      switch (*ptr)
	{
	default:
	  return;
	case ')':
	  argPtr = &return_type;
	  continue;
	case '(':
	  continue;
	case 'V':
	case 'B':
	case 'C':
	case 'D':
	case 'F':
	case 'S':
	case 'I':
	case 'J':
	case 'Z':
	  type = _Jv_FindClassFromSignature(ptr, loader);
	  break;
	case 'L':
	  type = _Jv_FindClassFromSignature(ptr, loader);
	  do 
	    ptr++;
	  while (*ptr != ';' && ptr[1] != '\0');
	  break;
	}

      // FIXME: 2'nd argument should be "current loader"
      while (--num_arrays >= 0)
	type = _Jv_FindArrayClass (type, 0);
      *argPtr++ = type;
    }
  parameter_types = args;
}
