// natMethod.cc - Native code for Method class.

/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/reflect/Method.h>
#include <java/lang/reflect/Constructor.h>
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
#include <gnu/gcj/RawData.h>

#define ObjectClass _CL_Q34java4lang6Object
extern java::lang::Class ObjectClass;
#define ClassClass _CL_Q34java4lang5Class
extern java::lang::Class ClassClass;

#include <stdlib.h>

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
      // On some platforms a bool is a byte, on others an int.
      if (sizeof (jboolean) == sizeof (jbyte))
	r = &ffi_type_sint8;
      else
	{
	  JvAssert (sizeof (jboolean) == sizeof (jint));
	  r = &ffi_type_sint32;
	}
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

// Actually perform an FFI call.
void
java::lang::reflect::Method::hack_call (gnu::gcj::RawData *rcif,
					gnu::gcj::RawData *rmethod,
					gnu::gcj::RawData *rret_value,
					gnu::gcj::RawData *rvalues)
{
  ffi_cif *cif = (ffi_cif *) rcif;
  void (*method) (...) = (void (*) (...)) rmethod;
  void *ret_value = (void *) rret_value;
  void **values = (void **) rvalues;

  ffi_call (cif, method, ret_value, values);
}

jobject
java::lang::reflect::Method::invoke (jobject obj, jobjectArray args)
{
  if (parameter_types == NULL)
    getType ();

  jmethodID meth = _Jv_FromReflectedMethod (this);
  if (! java::lang::reflect::Modifier::isStatic(meth->accflags))
    {
      jclass k = obj ? obj->getClass() : NULL;
      if (! obj)
	JvThrow (new java::lang::NullPointerException);
      if (! declaringClass->isAssignableFrom(k))
	JvThrow (new java::lang::IllegalArgumentException);
      // FIXME: access checks.

      // Find the possibly overloaded method based on the runtime type
      // of the object.
      meth = _Jv_LookupDeclaredMethod (k, meth->name, meth->signature);
    }

  return _Jv_CallNonvirtualMethodA (obj, return_type, meth, false,
				    parameter_types, args);
}

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
  _Jv_GetTypesFromSignature (_Jv_FromReflectedMethod (this),
			     declaringClass,
			     &parameter_types,
			     &return_type);
}

void
_Jv_GetTypesFromSignature (jmethodID method,
			   jclass declaringClass,
			   JArray<jclass> **arg_types_out,
			   jclass *return_type_out)
{

  _Jv_Utf8Const* sig = method->signature;
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
	  argPtr = return_type_out;
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
      // ARGPTR can be NULL if we are processing the return value of a
      // call from Constructor.
      if (argPtr)
	*argPtr++ = type;
    }
  *arg_types_out = args;
}

// This is a very rough analog of the JNI CallNonvirtual<type>MethodA
// functions.  It handles both Methods and Constructors, and it can
// handle any return type.  In the Constructor case, the `obj'
// argument is unused and should be NULL; also, the `return_type' is
// the class that the constructor will construct.
jobject
_Jv_CallNonvirtualMethodA (jobject obj,
			   jclass return_type,
			   jmethodID meth,
			   jboolean is_constructor,
			   JArray<jclass> *parameter_types,
			   jobjectArray args)
{
  JvAssert (! is_constructor || ! obj);
  JvAssert (! is_constructor || ! return_type);

  // FIXME: access checks.

  if (parameter_types->length != args->length)
    JvThrow (new java::lang::IllegalArgumentException);

  // See whether call needs an object as the first argument.  A
  // constructor does need a `this' argument, but it is one we create.
  jboolean needs_this = false;
  if (is_constructor
      || ! java::lang::reflect::Modifier::isStatic(meth->accflags))
    needs_this = true;

  int param_count = parameter_types->length;
  if (needs_this)
    ++param_count;

  ffi_type *rtype;
  // A constructor itself always returns void.
  if (is_constructor || return_type == JvPrimClass (void))
    rtype = &ffi_type_void;
  else
    rtype = get_ffi_type (return_type);
  ffi_type **argtypes = (ffi_type **) alloca (param_count
					      * sizeof (ffi_type *));

  jclass *paramelts = elements (parameter_types);
  jobject *argelts = elements (args);

  // FIXME: at some point the compiler is going to add extra arguments
  // to some functions.  In particular we are going to do this for
  // handling access checks in reflection.  We must add these hidden
  // arguments here.

  // Special case for the `this' argument of a constructor.  Note that
  // the JDK 1.2 docs specify that the new object must be allocated
  // before argument conversions are done.
  if (is_constructor)
    {
      // FIXME: must special-case String, arrays, maybe others here.
      obj = JvAllocObject (return_type);
    }

  int i = 0;
  int size = 0;
  if (needs_this)
    {
      // The `NULL' type is `Object'.
      argtypes[i++] = get_ffi_type (NULL);
      size += sizeof (jobject);
    }

  for (int arg = 0; i < param_count; ++i, ++arg)
    {
      jclass k = argelts[arg] ? argelts[arg]->getClass() : NULL;
      argtypes[i] = get_ffi_type (k);
      if (paramelts[arg]->isPrimitive())
	{
	  if (! argelts[arg]
	      || ! k
	      || ! can_widen (k, paramelts[arg]))
	    JvThrow (new java::lang::IllegalArgumentException);
	  size += paramelts[arg]->size();
	}
      else
	{
	  if (argelts[arg] && ! paramelts[arg]->isAssignableFrom (k))
	    JvThrow (new java::lang::IllegalArgumentException);
	  size += sizeof (jobject);
	}
    }

  ffi_cif cif;
  if (ffi_prep_cif (&cif, FFI_DEFAULT_ABI, param_count,
		    rtype, argtypes) != FFI_OK)
    {
      // FIXME: throw some kind of VirtualMachineError here.
    }

  char *p = (char *) alloca (size);
  void **values = (void **) alloca (param_count * sizeof (void *));

#define COPY(Where, What, Type) \
  do { \
    Type val = (What); \
    memcpy ((Where), &val, sizeof (Type)); \
    values[i] = (Where); \
    Where += sizeof (Type); \
  } while (0)

  i = 0;
  if (needs_this)
    {
      COPY (p, obj, jobject);
      ++i;
    }

  for (int arg = 0; i < param_count; ++i, ++arg)
    {
      java::lang::Number *num = (java::lang::Number *) argelts[arg];
      if (paramelts[arg] == JvPrimClass (byte))
	COPY (p, num->byteValue(), jbyte);
      else if (paramelts[arg] == JvPrimClass (short))
	COPY (p, num->shortValue(), jshort);
      else if (paramelts[arg] == JvPrimClass (int))
	COPY (p, num->intValue(), jint);
      else if (paramelts[arg] == JvPrimClass (long))
	COPY (p, num->longValue(), jlong);
      else if (paramelts[arg] == JvPrimClass (float))
	COPY (p, num->floatValue(), jfloat);
      else if (paramelts[arg] == JvPrimClass (double))
	COPY (p, num->doubleValue(), jdouble);
      else if (paramelts[arg] == JvPrimClass (boolean))
	COPY (p, ((java::lang::Boolean *) argelts[arg])->booleanValue(),
	      jboolean);
      else if (paramelts[arg] == JvPrimClass (char))
	COPY (p, ((java::lang::Character *) argelts[arg])->charValue(), jchar);
      else
	{
	  JvAssert (! paramelts[arg]->isPrimitive());
	  COPY (p, argelts[arg], jobject);
	}
    }

  // FIXME: initialize class here.

  // Largest possible value.  Hopefully it is aligned!
  jdouble ret_value;
  java::lang::Throwable *ex;
  using namespace java::lang;
  using namespace java::lang::reflect;
  ex = Method::hack_trampoline ((gnu::gcj::RawData *) &cif,
				(gnu::gcj::RawData *) meth->ncode,
				(gnu::gcj::RawData *) &ret_value,
				(gnu::gcj::RawData *) values);

  if (ex)
    JvThrow (new InvocationTargetException (ex));

  jobject r;
#define VAL(Wrapper, Type)  (new Wrapper (* (Type *) &ret_value))
  if (is_constructor)
    r = obj;
  else if (return_type == JvPrimClass (byte))
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
      JvAssert (return_type == NULL || ! return_type->isPrimitive());
      r = * (Object **) &ret_value;
    }

  return r;
}
