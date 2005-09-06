// natMethod.cc - Native code for Method class.

/* Copyright (C) 1998, 1999, 2000, 2001 , 2002, 2003, 2004, 2005 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <jni.h>
#include <java-stack.h>

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
#include <java/lang/IllegalAccessException.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/VirtualMachineError.h>
#include <java/lang/Class.h>
#include <gcj/method.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/NoClassDefFoundError.h>

#include <stdlib.h>

#if USE_LIBFFI
#include <ffi.h>
#else
#include <java/lang/UnsupportedOperationException.h>
#endif

struct cpair
{
  jclass prim;
  jclass wrap;
};

// This is used to determine when a primitive widening conversion is
// allowed.
static cpair primitives[] =
{
#define BOOLEAN 0
  { JvPrimClass (boolean), &java::lang::Boolean::class$ },
  { JvPrimClass (byte), &java::lang::Byte::class$ },
#define SHORT 2
  { JvPrimClass (short), &java::lang::Short::class$ },
#define CHAR 3
  { JvPrimClass (char), &java::lang::Character::class$ },
  { JvPrimClass (int), &java::lang::Integer::class$ },
  { JvPrimClass (long), &java::lang::Long::class$ },
  { JvPrimClass (float), &java::lang::Float::class$ },
  { JvPrimClass (double), &java::lang::Double::class$ },
  { NULL, NULL }
};

static inline jboolean
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
  // Boolean arguments may not be widened.
  if (fromx == BOOLEAN && tox != BOOLEAN)
    return false;
  // Nothing promotes to char.
  if (tox == CHAR && fromx != CHAR)
    return false;

  return fromx <= tox;
}

#ifdef USE_LIBFFI
static inline ffi_type *
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
#endif // USE_LIBFFI

jobject
java::lang::reflect::Method::invoke (jobject obj, jobjectArray args)
{
  using namespace java::lang::reflect;
  jclass iface = NULL;
  
  if (parameter_types == NULL)
    getType ();
    
  jmethodID meth = _Jv_FromReflectedMethod (this);

  if (Modifier::isStatic(meth->accflags))
    {
      // We have to initialize a static class.  It is safe to do this
      // here and not in _Jv_CallAnyMethodA because JNI initializes a
      // class whenever a method lookup is done.
      _Jv_InitClass (declaringClass);
    }
  else
    {
      jclass objClass = JV_CLASS (obj);
      if (! _Jv_IsAssignableFrom (objClass, declaringClass))
        throw new java::lang::IllegalArgumentException;
    }

  // Check accessibility, if required.
  if (! (Modifier::isPublic (meth->accflags) || this->isAccessible()))
    {
      Class *caller = _Jv_StackTrace::GetCallingClass (&Method::class$);
      if (! _Jv_CheckAccess(caller, declaringClass, meth->accflags))
	throw new IllegalAccessException;
    }

  if (declaringClass->isInterface())
    iface = declaringClass;
  
  return _Jv_CallAnyMethodA (obj, return_type, meth, false,
			     parameter_types, args, iface);
}

jint
java::lang::reflect::Method::getModifiers ()
{
  // Ignore all unknown flags.
  return _Jv_FromReflectedMethod (this)->accflags & Modifier::ALL_FLAGS;
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
  _Jv_Method *method = _Jv_FromReflectedMethod (this);
  _Jv_GetTypesFromSignature (method,
			     declaringClass,
			     &parameter_types,
			     &return_type);

  int count = 0;
  if (method->throws != NULL)
    {
      while (method->throws[count] != NULL)
	++count;
    }

  exception_types
    = (JArray<jclass> *) JvNewObjectArray (count, &java::lang::Class::class$,
					   NULL);
  jclass *elts = elements (exception_types);
  for (int i = 0; i < count; ++i)
    elts[i] = _Jv_FindClass (method->throws[i],
			     declaringClass->getClassLoaderInternal ());
}

void
_Jv_GetTypesFromSignature (jmethodID method,
			   jclass declaringClass,
			   JArray<jclass> **arg_types_out,
			   jclass *return_type_out)
{

  _Jv_Utf8Const* sig = method->signature;
  java::lang::ClassLoader *loader = declaringClass->getClassLoaderInternal();
  char *ptr = sig->chars();
  int numArgs = 0;
  /* First just count the number of parameters. */
  // FIXME: should do some validation here, e.g., that there is only
  // one return type.
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
    JvNewObjectArray (numArgs, &java::lang::Class::class$, NULL);
  jclass* argPtr = elements (args);
  for (ptr = sig->chars(); *ptr != '\0'; ptr++)
    {
      if (*ptr == '(')
	continue;
      if (*ptr == ')')
	{
	  argPtr = return_type_out;
	  continue;
	}

      char *end_ptr;
      jclass type = _Jv_FindClassFromSignature (ptr, loader, &end_ptr);
      if (type == NULL)
	// FIXME: This isn't ideal.
	throw new java::lang::NoClassDefFoundError (sig->toString());

      // ARGPTR can be NULL if we are processing the return value of a
      // call from Constructor.
      if (argPtr)
	*argPtr++ = type;

      ptr = end_ptr;
    }
  *arg_types_out = args;
}

// This is a very rough analog of the JNI CallNonvirtual<type>MethodA
// functions.  It handles both Methods and Constructors, and it can
// handle any return type.  In the Constructor case, the `obj'
// argument is unused and should be NULL; also, the `return_type' is
// the class that the constructor will construct.  RESULT is a pointer
// to a `jvalue' (see jni.h); for a void method this should be NULL.
// This function returns an exception (if one was thrown), or NULL if
// the call went ok.
void
_Jv_CallAnyMethodA (jobject obj,
		    jclass return_type,
		    jmethodID meth,
		    jboolean is_constructor,
		    jboolean is_virtual_call,
		    JArray<jclass> *parameter_types,
		    jvalue *args,
		    jvalue *result,
		    jboolean is_jni_call,
		    jclass iface)
{
  using namespace java::lang::reflect;
  
#ifdef USE_LIBFFI
  JvAssert (! is_constructor || ! obj);
  JvAssert (! is_constructor || return_type);

  // See whether call needs an object as the first argument.  A
  // constructor does need a `this' argument, but it is one we create.
  jboolean needs_this = false;
  if (is_constructor
      || ! Modifier::isStatic(meth->accflags))
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
  ffi_type **argtypes = (ffi_type **) __builtin_alloca (param_count
							* sizeof (ffi_type *));

  jclass *paramelts = elements (parameter_types);

  // Special case for the `this' argument of a constructor.  Note that
  // the JDK 1.2 docs specify that the new object must be allocated
  // before argument conversions are done.
  if (is_constructor)
    obj = _Jv_AllocObject (return_type);

  const int size_per_arg = sizeof(jvalue);
  ffi_cif cif;

  char *p = (char *) __builtin_alloca (param_count * size_per_arg);
		// Overallocate to get correct alignment.
  void **values = (void **)
			__builtin_alloca (param_count * sizeof (void *));

  int i = 0;
  if (needs_this)
    {
      // The `NULL' type is `Object'.
      argtypes[i] = get_ffi_type (NULL);
      values[i] = p;
      memcpy (p, &obj, sizeof (jobject));
      p += size_per_arg;
      ++i;
    }

  for (int arg = 0; i < param_count; ++i, ++arg)
    {
      int tsize;

      argtypes[i] = get_ffi_type (paramelts[arg]);
      if (paramelts[arg]->isPrimitive())
	tsize = paramelts[arg]->size();
      else
	tsize = sizeof (jobject);

      // Copy appropriate bits from the jvalue into the ffi array.
      // FIXME: we could do this copying all in one loop, above, by
      // over-allocating a bit.
      // How do we do this without breaking big-endian platforms?
      values[i] = p;
      memcpy (p, &args[arg], tsize);
      p += size_per_arg;
    }

  if (ffi_prep_cif (&cif, FFI_DEFAULT_ABI, param_count,
		    rtype, argtypes) != FFI_OK)
    throw new java::lang::VirtualMachineError(JvNewStringLatin1("internal error: ffi_prep_cif failed"));

  using namespace java::lang;
  using namespace java::lang::reflect;

  union
  {
    ffi_arg i;
    jobject o;
    jlong l;
    jfloat f;
    jdouble d;
  } ffi_result;

  switch (rtype->type)
    {
    case FFI_TYPE_VOID:
      break;
    case FFI_TYPE_SINT8:
      result->b = 0;
      break;
    case FFI_TYPE_SINT16:
      result->s = 0;
      break;
    case FFI_TYPE_UINT16:
      result->c = 0;
      break;
    case FFI_TYPE_SINT32:
      result->i = 0;
      break;
    case FFI_TYPE_SINT64:
      result->j = 0;
      break;
    case FFI_TYPE_FLOAT:
      result->f = 0;
      break;
    case FFI_TYPE_DOUBLE:
      result->d = 0;
      break;
    case FFI_TYPE_POINTER:
      result->l = 0;
      break;
    default:
      JvFail ("Unknown ffi_call return type");
      break;
    }

  void *ncode;

  // FIXME: If a vtable index is -1 at this point it is invalid, so we
  // have to use the ncode.  
  //
  // This can happen because methods in final classes don't have
  // vtable entries, but _Jv_isVirtualMethod() doesn't know that.  We
  // could solve this problem by allocating a vtable index for methods
  // in final classes.
  if (is_virtual_call 
      && ! Modifier::isFinal (meth->accflags)
      && (_Jv_ushort)-1 != meth->index)
    {
      _Jv_VTable *vtable = *(_Jv_VTable **) obj;
      if (iface == NULL)
	{
	  if (is_jni_call && Modifier::isAbstract (meth->accflags))
	    {
	      // With JNI we don't know if this is an interface call
	      // or a call to an abstract method.  Look up the method
	      // by name, the slow way.
	      _Jv_Method *concrete_meth
		= _Jv_LookupDeclaredMethod (vtable->clas,
					    meth->name,
					    meth->signature,
					    NULL);
	      if (concrete_meth == NULL
		  || concrete_meth->ncode == NULL
		  || Modifier::isAbstract(concrete_meth->accflags))
		throw new java::lang::IncompatibleClassChangeError
		  (_Jv_GetMethodString (vtable->clas, meth));
	      ncode = concrete_meth->ncode;
	    }
	  else
	    ncode = vtable->get_method (meth->index);
	}
      else
	ncode = _Jv_LookupInterfaceMethodIdx (vtable->clas, iface,
					      meth->index);
    }
  else
    {
      ncode = meth->ncode;
    }

  try
    {
      ffi_call (&cif, (void (*)()) ncode, &ffi_result, values);
    }
  catch (Throwable *ex)
    {
      // For JNI we just throw the real error.  For reflection, we
      // wrap the underlying method's exception in an
      // InvocationTargetException.
      if (! is_jni_call)
	ex = new InvocationTargetException (ex);
      throw ex;
    }

  // Since ffi_call returns integer values promoted to a word, use
  // a narrowing conversion for jbyte, jchar, etc. results.
  // Note that boolean is handled either by the FFI_TYPE_SINT8 or
  // FFI_TYPE_SINT32 case.
  if (is_constructor)
    result->l = obj;
  else
    {
      switch (rtype->type)
	{
	case FFI_TYPE_VOID:
	  break;
	case FFI_TYPE_SINT8:
	  result->b = (jbyte)ffi_result.i;
	  break;
	case FFI_TYPE_SINT16:
	  result->s = (jshort)ffi_result.i;
	  break;
	case FFI_TYPE_UINT16:
	  result->c = (jchar)ffi_result.i;
	  break;
	case FFI_TYPE_SINT32:
	  result->i = (jint)ffi_result.i;
	  break;
	case FFI_TYPE_SINT64:
	  result->j = (jlong)ffi_result.l;
	  break;
	case FFI_TYPE_FLOAT:
	  result->f = (jfloat)ffi_result.f;
	  break;
	case FFI_TYPE_DOUBLE:
	  result->d = (jdouble)ffi_result.d;
	  break;
	case FFI_TYPE_POINTER:
	  result->l = (jobject)ffi_result.o;
	  break;
	default:
	  JvFail ("Unknown ffi_call return type");
	  break;
	}
    }
#else
  throw new java::lang::UnsupportedOperationException(JvNewStringLatin1("reflection not available in this build"));
#endif // USE_LIBFFI
}

// This is another version of _Jv_CallAnyMethodA, but this one does
// more checking and is used by the reflection (and not JNI) code.
jobject
_Jv_CallAnyMethodA (jobject obj,
		    jclass return_type,
		    jmethodID meth,
		    jboolean is_constructor,
		    JArray<jclass> *parameter_types,
		    jobjectArray args,
		    jclass iface)
{
  if (parameter_types->length == 0 && args == NULL)
    {
      // The JDK accepts this, so we do too.
    }
  else if (parameter_types->length != args->length)
    throw new java::lang::IllegalArgumentException;

  int param_count = parameter_types->length;

  jclass *paramelts = elements (parameter_types);
  jobject *argelts = args == NULL ? NULL : elements (args);
  jvalue argvals[param_count];

#define COPY(Where, What, Type) \
  do { \
    Type val = (What); \
    memcpy ((Where), &val, sizeof (Type)); \
  } while (0)

  for (int i = 0; i < param_count; ++i)
    {
      jclass k = argelts[i] ? argelts[i]->getClass() : NULL;
      if (paramelts[i]->isPrimitive())
	{
	  if (! argelts[i]
	      || ! k
	      || ! can_widen (k, paramelts[i]))
	    throw new java::lang::IllegalArgumentException;
	    
	  if (paramelts[i] == JvPrimClass (boolean))
	    COPY (&argvals[i],
		  ((java::lang::Boolean *) argelts[i])->booleanValue(),
		  jboolean);
	  else if (paramelts[i] == JvPrimClass (char))
	    COPY (&argvals[i],
		  ((java::lang::Character *) argelts[i])->charValue(),
		  jchar);
          else
	    {
	      java::lang::Number *num = (java::lang::Number *) argelts[i];
	      if (paramelts[i] == JvPrimClass (byte))
		COPY (&argvals[i], num->byteValue(), jbyte);
	      else if (paramelts[i] == JvPrimClass (short))
		COPY (&argvals[i], num->shortValue(), jshort);
	      else if (paramelts[i] == JvPrimClass (int))
		COPY (&argvals[i], num->intValue(), jint);
	      else if (paramelts[i] == JvPrimClass (long))
		COPY (&argvals[i], num->longValue(), jlong);
	      else if (paramelts[i] == JvPrimClass (float))
		COPY (&argvals[i], num->floatValue(), jfloat);
	      else if (paramelts[i] == JvPrimClass (double))
		COPY (&argvals[i], num->doubleValue(), jdouble);
	    }
	}
      else
	{
	  if (argelts[i] && ! paramelts[i]->isAssignableFrom (k))
	    throw new java::lang::IllegalArgumentException;
	  COPY (&argvals[i], argelts[i], jobject);
	}
    }

  jvalue ret_value;
  _Jv_CallAnyMethodA (obj, return_type, meth, is_constructor,
  		      _Jv_isVirtualMethod (meth),
		      parameter_types, argvals, &ret_value,
		      false, iface);

  jobject r;
#define VAL(Wrapper, Field)  (new Wrapper (ret_value.Field))
  if (is_constructor)
    r = ret_value.l;
  else  if (return_type == JvPrimClass (byte))
    r = VAL (java::lang::Byte, b);
  else if (return_type == JvPrimClass (short))
    r = VAL (java::lang::Short, s);
  else if (return_type == JvPrimClass (int))
    r = VAL (java::lang::Integer, i);
  else if (return_type == JvPrimClass (long))
    r = VAL (java::lang::Long, j);
  else if (return_type == JvPrimClass (float))
    r = VAL (java::lang::Float, f);
  else if (return_type == JvPrimClass (double))
    r = VAL (java::lang::Double, d);
  else if (return_type == JvPrimClass (boolean))
    r = VAL (java::lang::Boolean, z);
  else if (return_type == JvPrimClass (char))
    r = VAL (java::lang::Character, c);
  else if (return_type == JvPrimClass (void))
    r = NULL;
  else
    {
      JvAssert (return_type == NULL || ! return_type->isPrimitive());
      r = ret_value.l;
    }

  return r;
}
