// jni.cc - JNI implementation, including the jump table.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdio.h>
#include <stddef.h>
#include <string.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-assert.h>
#include <jni.h>
#ifdef ENABLE_JVMPI
#include <jvmpi.h>
#endif
#ifdef INTERPRETER
#include <jvmti.h>
#include "jvmti-int.h"
#endif
#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Throwable.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/StringIndexOutOfBoundsException.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/UnsatisfiedLinkError.h>
#include <java/lang/InstantiationException.h>
#include <java/lang/NoSuchFieldError.h>
#include <java/lang/NoSuchMethodError.h>
#include <java/lang/reflect/Constructor.h>
#include <java/lang/reflect/Method.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/OutOfMemoryError.h>
#include <java/lang/Integer.h>
#include <java/lang/ThreadGroup.h>
#include <java/lang/Thread.h>
#include <java/lang/IllegalAccessError.h>
#include <java/nio/Buffer.h>
#include <java/nio/DirectByteBufferImpl.h>
#include <java/nio/DirectByteBufferImpl$ReadWrite.h>
#include <java/util/IdentityHashMap.h>
#include <gnu/gcj/RawData.h>
#include <java/lang/ClassNotFoundException.h>

#include <gcj/method.h>
#include <gcj/field.h>

#include <java-interp.h>
#include <java-threads.h>

using namespace gcj;

// This enum is used to select different template instantiations in
// the invocation code.
enum invocation_type
{
  normal,
  nonvirtual,
  static_type,
  constructor
};

// Forward declarations.
extern struct JNINativeInterface_ _Jv_JNIFunctions;
extern struct JNIInvokeInterface_ _Jv_JNI_InvokeFunctions;

// Number of slots in the default frame.  The VM must allow at least
// 16.
#define FRAME_SIZE 16

// Mark value indicating this is an overflow frame.
#define MARK_NONE    0
// Mark value indicating this is a user frame.
#define MARK_USER    1
// Mark value indicating this is a system frame.
#define MARK_SYSTEM  2

// This structure is used to keep track of local references.
struct _Jv_JNI_LocalFrame
{
  // This is one of the MARK_ constants.
  unsigned char marker;

  // Flag to indicate some locals were allocated.
  bool allocated_p;

  // Number of elements in frame.
  int size;

  // The class loader of the JNI method that allocated this frame.
  ::java::lang::ClassLoader *loader;

  // Next frame in chain.
  _Jv_JNI_LocalFrame *next;

  // The elements.  These are allocated using the C "struct hack".
  jobject vec[0];
};

// This holds a reference count for all local references.
static java::util::IdentityHashMap *local_ref_table;
// This holds a reference count for all global references.
static java::util::IdentityHashMap *global_ref_table;

// The only VM.
JavaVM *_Jv_the_vm;

#ifdef ENABLE_JVMPI
// The only JVMPI interface description.
static JVMPI_Interface _Jv_JVMPI_Interface;

static jint
jvmpiEnableEvent (jint event_type, void *)
{
  switch (event_type)
    {
    case JVMPI_EVENT_OBJECT_ALLOC:
      _Jv_JVMPI_Notify_OBJECT_ALLOC = _Jv_JVMPI_Interface.NotifyEvent;
      break;

    case JVMPI_EVENT_THREAD_START:
      _Jv_JVMPI_Notify_THREAD_START = _Jv_JVMPI_Interface.NotifyEvent;
      break;

    case JVMPI_EVENT_THREAD_END:
      _Jv_JVMPI_Notify_THREAD_END = _Jv_JVMPI_Interface.NotifyEvent;
      break;

    default:
      return JVMPI_NOT_AVAILABLE;
    }

  return JVMPI_SUCCESS;
}

static jint
jvmpiDisableEvent (jint event_type, void *)
{
  switch (event_type)
    {
    case JVMPI_EVENT_OBJECT_ALLOC:
      _Jv_JVMPI_Notify_OBJECT_ALLOC = NULL;
      break;

    default:
      return JVMPI_NOT_AVAILABLE;
    }

  return JVMPI_SUCCESS;
}
#endif



void
_Jv_JNI_Init (void)
{
  local_ref_table = new java::util::IdentityHashMap;
  global_ref_table = new java::util::IdentityHashMap;

#ifdef ENABLE_JVMPI
  _Jv_JVMPI_Interface.version = 1;
  _Jv_JVMPI_Interface.EnableEvent = &jvmpiEnableEvent;
  _Jv_JVMPI_Interface.DisableEvent = &jvmpiDisableEvent;
  _Jv_JVMPI_Interface.EnableGC = &_Jv_EnableGC;
  _Jv_JVMPI_Interface.DisableGC = &_Jv_DisableGC;
  _Jv_JVMPI_Interface.RunGC = &_Jv_RunGC;
#endif
}

// Tell the GC that a certain pointer is live.
static void
mark_for_gc (jobject obj, java::util::IdentityHashMap *ref_table)
{
  JvSynchronize sync (ref_table);

  using namespace java::lang;
  Integer *refcount = (Integer *) ref_table->get (obj);
  jint val = (refcount == NULL) ? 0 : refcount->intValue ();
  // FIXME: what about out of memory error?
  ref_table->put (obj, new Integer (val + 1));
}

// Unmark a pointer.
static void
unmark_for_gc (jobject obj, java::util::IdentityHashMap *ref_table)
{
  JvSynchronize sync (ref_table);

  using namespace java::lang;
  Integer *refcount = (Integer *) ref_table->get (obj);
  JvAssert (refcount);
  jint val = refcount->intValue () - 1;
  JvAssert (val >= 0);
  if (val == 0)
    ref_table->remove (obj);
  else
    // FIXME: what about out of memory error?
    ref_table->put (obj, new Integer (val));
}

// "Unwrap" some random non-reference type.  This exists to simplify
// other template functions.
template<typename T>
static T
unwrap (T val)
{
  return val;
}

// Unwrap a weak reference, if required.
template<typename T>
static T *
unwrap (T *obj)
{
  using namespace gnu::gcj::runtime;
  // We can compare the class directly because JNIWeakRef is `final'.
  // Doing it this way is much faster.
  if (obj == NULL || obj->getClass () != &JNIWeakRef::class$)
    return obj;
  JNIWeakRef *wr = reinterpret_cast<JNIWeakRef *> (obj);
  return reinterpret_cast<T *> (wr->get ());
}

jobject
_Jv_UnwrapJNIweakReference (jobject obj)
{
  return unwrap (obj);
}



static jobject JNICALL
_Jv_JNI_NewGlobalRef (JNIEnv *, jobject obj)
{
  // This seems weird but I think it is correct.
  obj = unwrap (obj);
  mark_for_gc (obj, global_ref_table);
  return obj;
}

static void JNICALL
_Jv_JNI_DeleteGlobalRef (JNIEnv *, jobject obj)
{
  // This seems weird but I think it is correct.
  obj = unwrap (obj);
  
  // NULL is ok here -- the JNI specification doesn't say so, but this
  // is a no-op.
  if (! obj)
    return;

  unmark_for_gc (obj, global_ref_table);
}

static void JNICALL
_Jv_JNI_DeleteLocalRef (JNIEnv *env, jobject obj)
{
  _Jv_JNI_LocalFrame *frame;

  // This seems weird but I think it is correct.
  obj = unwrap (obj);

  // NULL is ok here -- the JNI specification doesn't say so, but this
  // is a no-op.
  if (! obj)
    return;

  for (frame = env->locals; frame != NULL; frame = frame->next)
    {
      for (int i = 0; i < frame->size; ++i)
	{
	  if (frame->vec[i] == obj)
	    {
	      frame->vec[i] = NULL;
	      unmark_for_gc (obj, local_ref_table);
	      return;
	    }
	}

      // Don't go past a marked frame.
      JvAssert (frame->marker == MARK_NONE);
    }

  JvAssert (0);
}

static jint JNICALL
_Jv_JNI_EnsureLocalCapacity (JNIEnv *env, jint size)
{
  // It is easier to just always allocate a new frame of the requested
  // size.  This isn't the most efficient thing, but for now we don't
  // care.  Note that _Jv_JNI_PushLocalFrame relies on this right now.

  _Jv_JNI_LocalFrame *frame;
  try
    {
      frame = (_Jv_JNI_LocalFrame *) _Jv_Malloc (sizeof (_Jv_JNI_LocalFrame)
						 + size * sizeof (jobject));
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return JNI_ERR;
    }

  frame->marker = MARK_NONE;
  frame->size = size;
  frame->allocated_p = false;
  memset (&frame->vec[0], 0, size * sizeof (jobject));
  frame->loader = env->locals->loader;
  frame->next = env->locals;
  env->locals = frame;

  return 0;
}

static jint JNICALL
_Jv_JNI_PushLocalFrame (JNIEnv *env, jint size)
{
  jint r = _Jv_JNI_EnsureLocalCapacity (env, size);
  if (r < 0)
    return r;

  // The new frame is on top.
  env->locals->marker = MARK_USER;

  return 0;
}

static jobject JNICALL
_Jv_JNI_NewLocalRef (JNIEnv *env, jobject obj)
{
  // This seems weird but I think it is correct.
  obj = unwrap (obj);

  // Try to find an open slot somewhere in the topmost frame.
  _Jv_JNI_LocalFrame *frame = env->locals;
  bool done = false, set = false;
  for (; frame != NULL && ! done; frame = frame->next)
    {
      for (int i = 0; i < frame->size; ++i)
	{
	  if (frame->vec[i] == NULL)
	    {
	      set = true;
	      done = true;
	      frame->vec[i] = obj;
	      frame->allocated_p = true;
	      break;
	    }
	}

      // If we found a slot, or if the frame we just searched is the
      // mark frame, then we are done.
      if (done || frame == NULL || frame->marker != MARK_NONE)
	break;
    }

  if (! set)
    {
      // No slots, so we allocate a new frame.  According to the spec
      // we could just die here.  FIXME: return value.
      _Jv_JNI_EnsureLocalCapacity (env, 16);
      // We know the first element of the new frame will be ok.
      env->locals->vec[0] = obj;
      env->locals->allocated_p = true;
    }

  mark_for_gc (obj, local_ref_table);
  return obj;
}

static jobject JNICALL
_Jv_JNI_PopLocalFrame (JNIEnv *env, jobject result, int stop)
{
  _Jv_JNI_LocalFrame *rf = env->locals;

  bool done = false;
  while (rf != NULL && ! done)
    {
      for (int i = 0; i < rf->size; ++i)
	if (rf->vec[i] != NULL)
	  unmark_for_gc (rf->vec[i], local_ref_table);

      // If the frame we just freed is the marker frame, we are done.
      done = (rf->marker == stop);

      _Jv_JNI_LocalFrame *n = rf->next;
      // When N==NULL, we've reached the reusable bottom_locals, and we must
      // not free it.  However, we must be sure to clear all its elements.
      if (n == NULL)
	{
	  if (rf->allocated_p)
	    memset (&rf->vec[0], 0, rf->size * sizeof (jobject));
	  rf->allocated_p = false;
	  rf = NULL;
	  break;
	}

      _Jv_Free (rf);
      rf = n;
    }

  // Update the local frame information.
  env->locals = rf;

  return result == NULL ? NULL : _Jv_JNI_NewLocalRef (env, result);
}

static jobject JNICALL
_Jv_JNI_PopLocalFrame (JNIEnv *env, jobject result)
{
  return _Jv_JNI_PopLocalFrame (env, result, MARK_USER);
}

// Make sure an array's type is compatible with the type of the
// destination.
template<typename T>
static bool
_Jv_JNI_check_types (JNIEnv *env, JArray<T> *array, jclass K)
{
  jclass klass = array->getClass()->getComponentType();
  if (__builtin_expect (klass != K, false))
    {
      env->ex = new java::lang::IllegalAccessError ();
      return false;
    }
  else
    return true;
}

// Pop a `system' frame from the stack.  This is `extern "C"' as it is
// used by the compiler.
extern "C" void
_Jv_JNI_PopSystemFrame (JNIEnv *env)
{
  // Only enter slow path when we're not at the bottom, or there have been
  // allocations. Usually this is false and we can just null out the locals
  // field.

  if (__builtin_expect ((env->locals->next 
			 || env->locals->allocated_p), false))
    _Jv_JNI_PopLocalFrame (env, NULL, MARK_SYSTEM);
  else
    env->locals = NULL;

#ifdef INTERPRETER
  if (__builtin_expect (env->ex != NULL, false))
    {
      jthrowable t = env->ex;
      env->ex = NULL;
      if (JVMTI_REQUESTED_EVENT (Exception))
	_Jv_ReportJVMTIExceptionThrow (t);
      throw t;
    }
#endif
}

template<typename T> T extract_from_jvalue(jvalue const & t);
template<> jboolean extract_from_jvalue(jvalue const & jv) { return jv.z; }
template<> jbyte    extract_from_jvalue(jvalue const & jv) { return jv.b; }
template<> jchar    extract_from_jvalue(jvalue const & jv) { return jv.c; }
template<> jshort   extract_from_jvalue(jvalue const & jv) { return jv.s; }
template<> jint     extract_from_jvalue(jvalue const & jv) { return jv.i; }
template<> jlong    extract_from_jvalue(jvalue const & jv) { return jv.j; }
template<> jfloat   extract_from_jvalue(jvalue const & jv) { return jv.f; }
template<> jdouble  extract_from_jvalue(jvalue const & jv) { return jv.d; }
template<> jobject  extract_from_jvalue(jvalue const & jv) { return jv.l; }


// This function is used from other template functions.  It wraps the
// return value appropriately; we specialize it so that object returns
// are turned into local references.
template<typename T>
static T
wrap_value (JNIEnv *, T value)
{
  return value;
}

// This specialization is used for jobject, jclass, jstring, jarray,
// etc.
template<typename R, typename T>
static T *
wrap_value (JNIEnv *env, T *value)
{
  return (value == NULL
	  ? value
	  : (T *) _Jv_JNI_NewLocalRef (env, (jobject) value));
}



static jint JNICALL
_Jv_JNI_GetVersion (JNIEnv *)
{
  return JNI_VERSION_1_4;
}

static jclass JNICALL
_Jv_JNI_DefineClass (JNIEnv *env, const char *name, jobject loader,
		     const jbyte *buf, jsize bufLen)
{
  try
    {
      loader = unwrap (loader);

      jstring sname = JvNewStringUTF (name);
      jbyteArray bytes = JvNewByteArray (bufLen);

      jbyte *elts = elements (bytes);
      memcpy (elts, buf, bufLen * sizeof (jbyte));

      java::lang::ClassLoader *l
	= reinterpret_cast<java::lang::ClassLoader *> (loader);

      jclass result = l->defineClass (sname, bytes, 0, bufLen);
      return (jclass) wrap_value (env, result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static jclass JNICALL
_Jv_JNI_FindClass (JNIEnv *env, const char *name)
{
  // FIXME: assume that NAME isn't too long.
  int len = strlen (name);
  char s[len + 1];
  for (int i = 0; i <= len; ++i)
    s[i] = (name[i] == '/') ? '.' : name[i];

  jclass r = NULL;
  try
    {
      // This might throw an out of memory exception.
      jstring n = JvNewStringUTF (s);

      java::lang::ClassLoader *loader = NULL;
      if (env->locals->loader != NULL)
	loader = env->locals->loader;

      if (loader == NULL)
	{
	  // FIXME: should use getBaseClassLoader, but we don't have that
	  // yet.
	  loader = java::lang::ClassLoader::getSystemClassLoader ();
	}

      r = loader->loadClass (n);
      _Jv_InitClass (r);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return (jclass) wrap_value (env, r);
}

static jclass JNICALL
_Jv_JNI_GetSuperclass (JNIEnv *env, jclass clazz)
{
  return (jclass) wrap_value (env, unwrap (clazz)->getSuperclass ());
}

static jboolean JNICALL
_Jv_JNI_IsAssignableFrom (JNIEnv *, jclass clazz1, jclass clazz2)
{
  return unwrap (clazz2)->isAssignableFrom (unwrap (clazz1));
}

static jint JNICALL
_Jv_JNI_Throw (JNIEnv *env, jthrowable obj)
{
  // We check in case the user did some funky cast.
  obj = unwrap (obj);
  JvAssert (obj != NULL && java::lang::Throwable::class$.isInstance (obj));
  env->ex = obj;
  return 0;
}

static jint JNICALL
_Jv_JNI_ThrowNew (JNIEnv *env, jclass clazz, const char *message)
{
  using namespace java::lang::reflect;

  clazz = unwrap (clazz);
  JvAssert (java::lang::Throwable::class$.isAssignableFrom (clazz));

  int r = JNI_OK;
  try
    {
      JArray<jclass> *argtypes
	= (JArray<jclass> *) JvNewObjectArray (1, &java::lang::Class::class$,
					       NULL);

      jclass *elts = elements (argtypes);
      elts[0] = &java::lang::String::class$;

      Constructor *cons = clazz->getConstructor (argtypes);

      jobjectArray values = JvNewObjectArray (1, &java::lang::String::class$,
					      NULL);
      jobject *velts = elements (values);
      velts[0] = JvNewStringUTF (message);

      jobject obj = cons->newInstance (values);

      env->ex = reinterpret_cast<jthrowable> (obj);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      r = JNI_ERR;
    }

  return r;
}

static jthrowable JNICALL
_Jv_JNI_ExceptionOccurred (JNIEnv *env)
{
  return (jthrowable) wrap_value (env, env->ex);
}

static void JNICALL
_Jv_JNI_ExceptionDescribe (JNIEnv *env)
{
  if (env->ex != NULL)
    env->ex->printStackTrace();
}

static void JNICALL
_Jv_JNI_ExceptionClear (JNIEnv *env)
{
  env->ex = NULL;
}

static jboolean JNICALL
_Jv_JNI_ExceptionCheck (JNIEnv *env)
{
  return env->ex != NULL;
}

static void JNICALL
_Jv_JNI_FatalError (JNIEnv *, const char *message)
{
  JvFail (message);
}



static jboolean JNICALL
_Jv_JNI_IsSameObject (JNIEnv *, jobject obj1, jobject obj2)
{
  return unwrap (obj1) == unwrap (obj2);
}

static jobject JNICALL
_Jv_JNI_AllocObject (JNIEnv *env, jclass clazz)
{
  jobject obj = NULL;
  using namespace java::lang::reflect;

  try
    {
      clazz = unwrap (clazz);
      JvAssert (clazz && ! clazz->isArray ());
      if (clazz->isInterface() || Modifier::isAbstract(clazz->getModifiers()))
	env->ex = new java::lang::InstantiationException ();
      else
	obj = _Jv_AllocObject (clazz);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return wrap_value (env, obj);
}

static jclass JNICALL
_Jv_JNI_GetObjectClass (JNIEnv *env, jobject obj)
{
  obj = unwrap (obj);
  JvAssert (obj);
  return (jclass) wrap_value (env, obj->getClass());
}

static jboolean JNICALL
_Jv_JNI_IsInstanceOf (JNIEnv *, jobject obj, jclass clazz)
{
  return unwrap (clazz)->isInstance(unwrap (obj));
}



//
// This section concerns method invocation.
//

template<jboolean is_static>
static jmethodID JNICALL
_Jv_JNI_GetAnyMethodID (JNIEnv *env, jclass clazz,
			const char *name, const char *sig)
{
  try
    {
      clazz = unwrap (clazz);
      _Jv_InitClass (clazz);

      _Jv_Utf8Const *name_u = _Jv_makeUtf8Const ((char *) name, -1);

      // FIXME: assume that SIG isn't too long.
      int len = strlen (sig);
      char s[len + 1];
      for (int i = 0; i <= len; ++i)
	s[i] = (sig[i] == '/') ? '.' : sig[i];
      _Jv_Utf8Const *sig_u = _Jv_makeUtf8Const ((char *) s, -1);

      JvAssert (! clazz->isPrimitive());

      using namespace java::lang::reflect;

      while (clazz != NULL)
	{
	  jint count = JvNumMethods (clazz);
	  jmethodID meth = JvGetFirstMethod (clazz);

	  for (jint i = 0; i < count; ++i)
	    {
	      if (((is_static && Modifier::isStatic (meth->accflags))
		   || (! is_static && ! Modifier::isStatic (meth->accflags)))
		  && _Jv_equalUtf8Consts (meth->name, name_u)
		  && _Jv_equalUtf8Consts (meth->signature, sig_u))
		return meth;

	      meth = meth->getNextMethod();
	    }

	  clazz = clazz->getSuperclass ();
	}

      java::lang::StringBuffer *name_sig =
        new java::lang::StringBuffer (JvNewStringUTF (name));
      name_sig->append ((jchar) ' ');
      name_sig->append (JvNewStringUTF (s));
      env->ex = new java::lang::NoSuchMethodError (name_sig->toString ());
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return NULL;
}

// This is a helper function which turns a va_list into an array of
// `jvalue's.  It needs signature information in order to do its work.
// The array of values must already be allocated.
static void
array_from_valist (jvalue *values, JArray<jclass> *arg_types, va_list vargs)
{
  jclass *arg_elts = elements (arg_types);
  for (int i = 0; i < arg_types->length; ++i)
    {
      // Here we assume that sizeof(int) >= sizeof(jint), because we
      // use `int' when decoding the varargs.  Likewise for
      // float, and double.  Also we assume that sizeof(jlong) >=
      // sizeof(int), i.e. that jlong values are not further
      // promoted.
      JvAssert (sizeof (int) >= sizeof (jint));
      JvAssert (sizeof (jlong) >= sizeof (int));
      JvAssert (sizeof (double) >= sizeof (jfloat));
      JvAssert (sizeof (double) >= sizeof (jdouble));
      if (arg_elts[i] == JvPrimClass (byte))
	values[i].b = (jbyte) va_arg (vargs, int);
      else if (arg_elts[i] == JvPrimClass (short))
	values[i].s = (jshort) va_arg (vargs, int);
      else if (arg_elts[i] == JvPrimClass (int))
	values[i].i = (jint) va_arg (vargs, int);
      else if (arg_elts[i] == JvPrimClass (long))
	values[i].j = (jlong) va_arg (vargs, jlong);
      else if (arg_elts[i] == JvPrimClass (float))
	values[i].f = (jfloat) va_arg (vargs, double);
      else if (arg_elts[i] == JvPrimClass (double))
	values[i].d = (jdouble) va_arg (vargs, double);
      else if (arg_elts[i] == JvPrimClass (boolean))
	values[i].z = (jboolean) va_arg (vargs, int);
      else if (arg_elts[i] == JvPrimClass (char))
	values[i].c = (jchar) va_arg (vargs, int);
      else
	{
	  // An object.
	  values[i].l = unwrap (va_arg (vargs, jobject));
	}
    }
}

// This can call any sort of method: virtual, "nonvirtual", static, or
// constructor.
template<typename T, invocation_type style>
static T JNICALL
_Jv_JNI_CallAnyMethodV (JNIEnv *env, jobject obj, jclass klass,
			jmethodID id, va_list vargs)
{
  obj = unwrap (obj);
  klass = unwrap (klass);

  jclass decl_class = klass ? klass : obj->getClass ();
  JvAssert (decl_class != NULL);

  jclass return_type;
  JArray<jclass> *arg_types;

  try
    {
      _Jv_GetTypesFromSignature (id, decl_class,
				 &arg_types, &return_type);

      jvalue args[arg_types->length];
      array_from_valist (args, arg_types, vargs);

      // For constructors we need to pass the Class we are instantiating.
      if (style == constructor)
	return_type = klass;

      jvalue result;
      _Jv_CallAnyMethodA (obj, return_type, id,
			  style == constructor,
			  style == normal,
			  arg_types, args, &result);

      return wrap_value (env, extract_from_jvalue<T>(result));
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return wrap_value (env, (T) 0);
}

template<typename T, invocation_type style>
static T JNICALL
_Jv_JNI_CallAnyMethod (JNIEnv *env, jobject obj, jclass klass,
		       jmethodID method, ...)
{
  va_list args;
  T result;

  va_start (args, method);
  result = _Jv_JNI_CallAnyMethodV<T, style> (env, obj, klass, method, args);
  va_end (args);

  return result;
}

template<typename T, invocation_type style>
static T JNICALL
_Jv_JNI_CallAnyMethodA (JNIEnv *env, jobject obj, jclass klass,
			jmethodID id, const jvalue *args)
{
  obj = unwrap (obj);
  klass = unwrap (klass);

  jclass decl_class = klass ? klass : obj->getClass ();
  JvAssert (decl_class != NULL);

  jclass return_type;
  JArray<jclass> *arg_types;
  try
    {
      _Jv_GetTypesFromSignature (id, decl_class,
				 &arg_types, &return_type);

      // For constructors we need to pass the Class we are instantiating.
      if (style == constructor)
	return_type = klass;

      // Unwrap arguments as required.  Eww.
      jclass *type_elts = elements (arg_types);
      jvalue arg_copy[arg_types->length];
      for (int i = 0; i < arg_types->length; ++i)
	{
	  if (type_elts[i]->isPrimitive ())
	    arg_copy[i] = args[i];
	  else
	    arg_copy[i].l = unwrap (args[i].l);
	}

      jvalue result;
      _Jv_CallAnyMethodA (obj, return_type, id,
			  style == constructor,
			  style == normal,
			  arg_types, arg_copy, &result);

      return wrap_value (env, extract_from_jvalue<T>(result));
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return wrap_value (env, (T) 0);
}

template<invocation_type style>
static void JNICALL
_Jv_JNI_CallAnyVoidMethodV (JNIEnv *env, jobject obj, jclass klass,
			    jmethodID id, va_list vargs)
{
  obj = unwrap (obj);
  klass = unwrap (klass);

  jclass decl_class = klass ? klass : obj->getClass ();
  JvAssert (decl_class != NULL);

  jclass return_type;
  JArray<jclass> *arg_types;
  try
    {
      _Jv_GetTypesFromSignature (id, decl_class,
				 &arg_types, &return_type);

      jvalue args[arg_types->length];
      array_from_valist (args, arg_types, vargs);

      // For constructors we need to pass the Class we are instantiating.
      if (style == constructor)
	return_type = klass;

      _Jv_CallAnyMethodA (obj, return_type, id,
			  style == constructor,
			  style == normal,
			  arg_types, args, NULL);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
}

template<invocation_type style>
static void JNICALL
_Jv_JNI_CallAnyVoidMethod (JNIEnv *env, jobject obj, jclass klass,
			   jmethodID method, ...)
{
  va_list args;

  va_start (args, method);
  _Jv_JNI_CallAnyVoidMethodV<style> (env, obj, klass, method, args);
  va_end (args);
}

template<invocation_type style>
static void JNICALL
_Jv_JNI_CallAnyVoidMethodA (JNIEnv *env, jobject obj, jclass klass,
			    jmethodID id, const jvalue *args)
{
  jclass decl_class = klass ? klass : obj->getClass ();
  JvAssert (decl_class != NULL);

  jclass return_type;
  JArray<jclass> *arg_types;
  try
    {
      _Jv_GetTypesFromSignature (id, decl_class,
				 &arg_types, &return_type);

      // Unwrap arguments as required.  Eww.
      jclass *type_elts = elements (arg_types);
      jvalue arg_copy[arg_types->length];
      for (int i = 0; i < arg_types->length; ++i)
	{
	  if (type_elts[i]->isPrimitive ())
	    arg_copy[i] = args[i];
	  else
	    arg_copy[i].l = unwrap (args[i].l);
	}

      _Jv_CallAnyMethodA (obj, return_type, id,
			  style == constructor,
			  style == normal,
			  arg_types, args, NULL);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
}

// Functions with this signature are used to implement functions in
// the CallMethod family.
template<typename T>
static T JNICALL
_Jv_JNI_CallMethodV (JNIEnv *env, jobject obj, 
		     jmethodID id, va_list args)
{
  return _Jv_JNI_CallAnyMethodV<T, normal> (env, obj, NULL, id, args);
}

// Functions with this signature are used to implement functions in
// the CallMethod family.
template<typename T>
static T JNICALL
_Jv_JNI_CallMethod (JNIEnv *env, jobject obj, jmethodID id, ...)
{
  va_list args;
  T result;

  va_start (args, id);
  result = _Jv_JNI_CallAnyMethodV<T, normal> (env, obj, NULL, id, args);
  va_end (args);

  return result;
}

// Functions with this signature are used to implement functions in
// the CallMethod family.
template<typename T>
static T JNICALL
_Jv_JNI_CallMethodA (JNIEnv *env, jobject obj, 
		     jmethodID id, const jvalue *args)
{
  return _Jv_JNI_CallAnyMethodA<T, normal> (env, obj, NULL, id, args);
}

static void JNICALL
_Jv_JNI_CallVoidMethodV (JNIEnv *env, jobject obj, 
			 jmethodID id, va_list args)
{
  _Jv_JNI_CallAnyVoidMethodV<normal> (env, obj, NULL, id, args);
}

static void JNICALL
_Jv_JNI_CallVoidMethod (JNIEnv *env, jobject obj, jmethodID id, ...)
{
  va_list args;

  va_start (args, id);
  _Jv_JNI_CallAnyVoidMethodV<normal> (env, obj, NULL, id, args);
  va_end (args);
}

static void JNICALL
_Jv_JNI_CallVoidMethodA (JNIEnv *env, jobject obj, 
			 jmethodID id, const jvalue *args)
{
  _Jv_JNI_CallAnyVoidMethodA<normal> (env, obj, NULL, id, args);
}

// Functions with this signature are used to implement functions in
// the CallStaticMethod family.
template<typename T>
static T JNICALL
_Jv_JNI_CallStaticMethodV (JNIEnv *env, jclass klass,
			   jmethodID id, va_list args)
{
  JvAssert (((id->accflags) & java::lang::reflect::Modifier::STATIC));
  JvAssert (java::lang::Class::class$.isInstance (unwrap (klass)));

  return _Jv_JNI_CallAnyMethodV<T, static_type> (env, NULL, klass, id, args);
}

// Functions with this signature are used to implement functions in
// the CallStaticMethod family.
template<typename T>
static T JNICALL
_Jv_JNI_CallStaticMethod (JNIEnv *env, jclass klass, 
			  jmethodID id, ...)
{
  va_list args;
  T result;

  JvAssert (((id->accflags) & java::lang::reflect::Modifier::STATIC));
  JvAssert (java::lang::Class::class$.isInstance (unwrap (klass)));

  va_start (args, id);
  result = _Jv_JNI_CallAnyMethodV<T, static_type> (env, NULL, klass,
						   id, args);
  va_end (args);

  return result;
}

// Functions with this signature are used to implement functions in
// the CallStaticMethod family.
template<typename T>
static T JNICALL
_Jv_JNI_CallStaticMethodA (JNIEnv *env, jclass klass, jmethodID id,
			   const jvalue *args)
{
  JvAssert (((id->accflags) & java::lang::reflect::Modifier::STATIC));
  JvAssert (java::lang::Class::class$.isInstance (unwrap (klass)));

  return _Jv_JNI_CallAnyMethodA<T, static_type> (env, NULL, klass, id, args);
}

static void JNICALL
_Jv_JNI_CallStaticVoidMethodV (JNIEnv *env, jclass klass, 
			       jmethodID id, va_list args)
{
  _Jv_JNI_CallAnyVoidMethodV<static_type> (env, NULL, klass, id, args);
}

static void JNICALL
_Jv_JNI_CallStaticVoidMethod (JNIEnv *env, jclass klass, 
			      jmethodID id, ...)
{
  va_list args;

  va_start (args, id);
  _Jv_JNI_CallAnyVoidMethodV<static_type> (env, NULL, klass, id, args);
  va_end (args);
}

static void JNICALL
_Jv_JNI_CallStaticVoidMethodA (JNIEnv *env, jclass klass, 
			       jmethodID id, const jvalue *args)
{
  _Jv_JNI_CallAnyVoidMethodA<static_type> (env, NULL, klass, id, args);
}

static jobject JNICALL
_Jv_JNI_NewObjectV (JNIEnv *env, jclass klass,
		    jmethodID id, va_list args)
{
  JvAssert (klass && ! klass->isArray ());
  JvAssert (! strcmp (id->name->chars(), "<init>")
	    && id->signature->len() > 2
	    && id->signature->chars()[0] == '('
	    && ! strcmp (&id->signature->chars()[id->signature->len() - 2],
			 ")V"));

  return _Jv_JNI_CallAnyMethodV<jobject, constructor> (env, NULL, klass,
						       id, args);
}

static jobject JNICALL
_Jv_JNI_NewObject (JNIEnv *env, jclass klass, jmethodID id, ...)
{
  JvAssert (klass && ! klass->isArray ());
  JvAssert (! strcmp (id->name->chars(), "<init>")
	    && id->signature->len() > 2
	    && id->signature->chars()[0] == '('
	    && ! strcmp (&id->signature->chars()[id->signature->len() - 2],
			 ")V"));

  va_list args;
  jobject result;

  va_start (args, id);
  result = _Jv_JNI_CallAnyMethodV<jobject, constructor> (env, NULL, klass,
							 id, args);
  va_end (args);

  return result;
}

static jobject JNICALL
_Jv_JNI_NewObjectA (JNIEnv *env, jclass klass, jmethodID id,
		    const jvalue *args)
{
  JvAssert (klass && ! klass->isArray ());
  JvAssert (! strcmp (id->name->chars(), "<init>")
	    && id->signature->len() > 2
	    && id->signature->chars()[0] == '('
	    && ! strcmp (&id->signature->chars()[id->signature->len() - 2],
			 ")V"));

  return _Jv_JNI_CallAnyMethodA<jobject, constructor> (env, NULL, klass,
						       id, args);
}



template<typename T>
static T JNICALL
_Jv_JNI_GetField (JNIEnv *env, jobject obj, jfieldID field)
{
  obj = unwrap (obj);
  JvAssert (obj);
  T *ptr = (T *) ((char *) obj + field->getOffset ());
  return wrap_value (env, *ptr);
}

template<typename T>
static void JNICALL
_Jv_JNI_SetField (JNIEnv *, jobject obj, jfieldID field, T value)
{
  obj = unwrap (obj);
  value = unwrap (value);

  JvAssert (obj);
  T *ptr = (T *) ((char *) obj + field->getOffset ());
  *ptr = value;
}

template<jboolean is_static>
static jfieldID JNICALL
_Jv_JNI_GetAnyFieldID (JNIEnv *env, jclass clazz,
		       const char *name, const char *sig)
{
  try
    {
      clazz = unwrap (clazz);

      _Jv_InitClass (clazz);

      _Jv_Utf8Const *a_name = _Jv_makeUtf8Const ((char *) name, -1);

      // FIXME: assume that SIG isn't too long.
      int len = strlen (sig);
      char s[len + 1];
      for (int i = 0; i <= len; ++i)
	s[i] = (sig[i] == '/') ? '.' : sig[i];
      java::lang::ClassLoader *loader = clazz->getClassLoaderInternal ();
      jclass field_class = _Jv_FindClassFromSignature ((char *) s, loader);
      if (! field_class)
	throw new java::lang::ClassNotFoundException(JvNewStringUTF(s));

      while (clazz != NULL)
	{
	  // We acquire the class lock so that fields aren't resolved
	  // while we are running.
	  JvSynchronize sync (clazz);

	  jint count = (is_static
			? JvNumStaticFields (clazz)
			: JvNumInstanceFields (clazz));
	  jfieldID field = (is_static
			    ? JvGetFirstStaticField (clazz)
			    : JvGetFirstInstanceField (clazz));
	  for (jint i = 0; i < count; ++i)
	    {
	      _Jv_Utf8Const *f_name = field->getNameUtf8Const(clazz);

	      // The field might be resolved or it might not be.  It
	      // is much simpler to always resolve it.
	      _Jv_Linker::resolve_field (field, loader);
	      if (_Jv_equalUtf8Consts (f_name, a_name)
		  && field->getClass() == field_class)
		return field;

	      field = field->getNextField ();
	    }

	  clazz = clazz->getSuperclass ();
	}

      env->ex = new java::lang::NoSuchFieldError ();
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
  return NULL;
}

template<typename T>
static T JNICALL
_Jv_JNI_GetStaticField (JNIEnv *env, jclass, jfieldID field)
{
  T *ptr = (T *) field->u.addr;
  return wrap_value (env, *ptr);
}

template<typename T>
static void JNICALL
_Jv_JNI_SetStaticField (JNIEnv *, jclass, jfieldID field, T value)
{
  value = unwrap (value);
  T *ptr = (T *) field->u.addr;
  *ptr = value;
}

static jstring JNICALL
_Jv_JNI_NewString (JNIEnv *env, const jchar *unichars, jsize len)
{
  try
    {
      jstring r = _Jv_NewString (unichars, len);
      return (jstring) wrap_value (env, r);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static jsize JNICALL
_Jv_JNI_GetStringLength (JNIEnv *, jstring string)
{
  return unwrap (string)->length();
}

static const jchar * JNICALL
_Jv_JNI_GetStringChars (JNIEnv *, jstring string, jboolean *isCopy)
{
  string = unwrap (string);
  jchar *result = _Jv_GetStringChars (string);
  mark_for_gc (string, global_ref_table);
  if (isCopy)
    *isCopy = false;
  return (const jchar *) result;
}

static void JNICALL
_Jv_JNI_ReleaseStringChars (JNIEnv *, jstring string, const jchar *)
{
  unmark_for_gc (unwrap (string), global_ref_table);
}

static jstring JNICALL
_Jv_JNI_NewStringUTF (JNIEnv *env, const char *bytes)
{
  try
    {
      // For compatibility with the JDK.
      if (!bytes)
	return NULL;
      jstring result = JvNewStringUTF (bytes);
      return (jstring) wrap_value (env, result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static jsize JNICALL
_Jv_JNI_GetStringUTFLength (JNIEnv *, jstring string)
{
  return JvGetStringUTFLength (unwrap (string));
}

static const char * JNICALL
_Jv_JNI_GetStringUTFChars (JNIEnv *env, jstring string, 
			   jboolean *isCopy)
{
  try
    {
      string = unwrap (string);
      if (string == NULL)
	return NULL;
      jsize len = JvGetStringUTFLength (string);
      char *r = (char *) _Jv_Malloc (len + 1);
      JvGetStringUTFRegion (string, 0, string->length(), r);
      r[len] = '\0';

      if (isCopy)
	*isCopy = true;

      return (const char *) r;
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static void JNICALL
_Jv_JNI_ReleaseStringUTFChars (JNIEnv *, jstring, const char *utf)
{
  _Jv_Free ((void *) utf);
}

static void JNICALL
_Jv_JNI_GetStringRegion (JNIEnv *env, jstring string, jsize start, 
			 jsize len, jchar *buf)
{
  string = unwrap (string);
  jchar *result = _Jv_GetStringChars (string);
  if (start < 0 || start > string->length ()
      || len < 0 || start + len > string->length ())
    {
      try
	{
	  env->ex = new java::lang::StringIndexOutOfBoundsException ();
	}
      catch (jthrowable t)
	{
	  env->ex = t;
	}
    }
  else
    memcpy (buf, &result[start], len * sizeof (jchar));
}

static void JNICALL
_Jv_JNI_GetStringUTFRegion (JNIEnv *env, jstring str, jsize start,
			    jsize len, char *buf)
{
  str = unwrap (str);
    
  if (start < 0 || start > str->length ()
      || len < 0 || start + len > str->length ())
    {
      try
	{
	  env->ex = new java::lang::StringIndexOutOfBoundsException ();
	}
      catch (jthrowable t)
	{
	  env->ex = t;
	}
    }
  else
    _Jv_GetStringUTFRegion (str, start, len, buf);
}

static const jchar * JNICALL
_Jv_JNI_GetStringCritical (JNIEnv *, jstring str, jboolean *isCopy)
{
  jchar *result = _Jv_GetStringChars (unwrap (str));
  if (isCopy)
    *isCopy = false;
  return result;
}

static void JNICALL
_Jv_JNI_ReleaseStringCritical (JNIEnv *, jstring, const jchar *)
{
  // Nothing.
}

static jsize JNICALL
_Jv_JNI_GetArrayLength (JNIEnv *, jarray array)
{
  return unwrap (array)->length;
}

static jobjectArray JNICALL
_Jv_JNI_NewObjectArray (JNIEnv *env, jsize length, 
			jclass elementClass, jobject init)
{
  try
    {
      elementClass = unwrap (elementClass);
      init = unwrap (init);

      _Jv_CheckCast (elementClass, init);
      jarray result = JvNewObjectArray (length, elementClass, init);
      return (jobjectArray) wrap_value (env, result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static jobject JNICALL
_Jv_JNI_GetObjectArrayElement (JNIEnv *env, jobjectArray array, 
			       jsize index)
{
  if ((unsigned) index >= (unsigned) array->length)
    _Jv_ThrowBadArrayIndex (index);
  jobject *elts = elements (unwrap (array));
  return wrap_value (env, elts[index]);
}

static void JNICALL
_Jv_JNI_SetObjectArrayElement (JNIEnv *env, jobjectArray array, 
			       jsize index, jobject value)
{
  try
    {
      array = unwrap (array);
      value = unwrap (value);

      _Jv_CheckArrayStore (array, value);
      if ((unsigned) index >= (unsigned) array->length)
	_Jv_ThrowBadArrayIndex (index);
      jobject *elts = elements (array);
      elts[index] = value;
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
}

template<typename T, jclass K>
static JArray<T> * JNICALL
_Jv_JNI_NewPrimitiveArray (JNIEnv *env, jsize length)
{
  try
    {
      return (JArray<T> *) wrap_value (env, _Jv_NewPrimArray (K, length));
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

template<typename T, jclass K>
static T * JNICALL
_Jv_JNI_GetPrimitiveArrayElements (JNIEnv *env, JArray<T> *array,
				   jboolean *isCopy)
{
  array = unwrap (array);
  if (! _Jv_JNI_check_types (env, array, K))
    return NULL;
  T *elts = elements (array);
  if (isCopy)
    {
      // We elect never to copy.
      *isCopy = false;
    }
  mark_for_gc (array, global_ref_table);
  return elts;
}

template<typename T, jclass K>
static void JNICALL
_Jv_JNI_ReleasePrimitiveArrayElements (JNIEnv *env, JArray<T> *array,
				       T *, jint /* mode */)
{
  array = unwrap (array);
  _Jv_JNI_check_types (env, array, K);
  // Note that we ignore MODE.  We can do this because we never copy
  // the array elements.  My reading of the JNI documentation is that
  // this is an option for the implementor.
  unmark_for_gc (array, global_ref_table);
}

template<typename T, jclass K>
static void JNICALL
_Jv_JNI_GetPrimitiveArrayRegion (JNIEnv *env, JArray<T> *array,
				 jsize start, jsize len,
				 T *buf)
{
  array = unwrap (array);
  if (! _Jv_JNI_check_types (env, array, K))
    return;

  // The cast to unsigned lets us save a comparison.
  if (start < 0 || len < 0
      || (unsigned long) (start + len) > (unsigned long) array->length)
    {
      try
	{
	  // FIXME: index.
	  env->ex = new java::lang::ArrayIndexOutOfBoundsException ();
	}
      catch (jthrowable t)
	{
	  // Could have thown out of memory error.
	  env->ex = t;
	}
    }
  else
    {
      T *elts = elements (array) + start;
      memcpy (buf, elts, len * sizeof (T));
    }
}

template<typename T, jclass K>
static void JNICALL
_Jv_JNI_SetPrimitiveArrayRegion (JNIEnv *env, JArray<T> *array,
				 jsize start, jsize len, const T *buf)
{
  array = unwrap (array);
  if (! _Jv_JNI_check_types (env, array, K))
    return;

  // The cast to unsigned lets us save a comparison.
  if (start < 0 || len < 0
      || (unsigned long) (start + len) > (unsigned long) array->length)
    {
      try
	{
	  // FIXME: index.
	  env->ex = new java::lang::ArrayIndexOutOfBoundsException ();
	}
      catch (jthrowable t)
	{
	  env->ex = t;
	}
    }
  else
    {
      T *elts = elements (array) + start;
      memcpy (elts, buf, len * sizeof (T));
    }
}

static void * JNICALL
_Jv_JNI_GetPrimitiveArrayCritical (JNIEnv *, jarray array,
				   jboolean *isCopy)
{
  array = unwrap (array);
  // FIXME: does this work?
  jclass klass = array->getClass()->getComponentType();
  JvAssert (klass->isPrimitive ());
  char *r = _Jv_GetArrayElementFromElementType (array, klass);
  if (isCopy)
    *isCopy = false;
  return r;
}

static void JNICALL
_Jv_JNI_ReleasePrimitiveArrayCritical (JNIEnv *, jarray, void *, jint)
{
  // Nothing.
}

static jint JNICALL
_Jv_JNI_MonitorEnter (JNIEnv *env, jobject obj)
{
  try
    {
      _Jv_MonitorEnter (unwrap (obj));
      return 0;
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
  return JNI_ERR;
}

static jint JNICALL
_Jv_JNI_MonitorExit (JNIEnv *env, jobject obj)
{
  try
    {
      _Jv_MonitorExit (unwrap (obj));
      return 0;
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
  return JNI_ERR;
}

// JDK 1.2
jobject JNICALL
_Jv_JNI_ToReflectedField (JNIEnv *env, jclass cls, jfieldID fieldID,
			  jboolean)
{
  try
    {
      cls = unwrap (cls);
      java::lang::reflect::Field *field = new java::lang::reflect::Field();
      field->declaringClass = cls;
      field->offset = (char*) fieldID - (char *) cls->fields;
      field->name = _Jv_NewStringUtf8Const (fieldID->getNameUtf8Const (cls));
      return wrap_value (env, field);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
  return NULL;
}

// JDK 1.2
static jfieldID JNICALL
_Jv_JNI_FromReflectedField (JNIEnv *, jobject f)
{
  using namespace java::lang::reflect;

  f = unwrap (f);
  Field *field = reinterpret_cast<Field *> (f);
  return _Jv_FromReflectedField (field);
}

jobject JNICALL
_Jv_JNI_ToReflectedMethod (JNIEnv *env, jclass klass, jmethodID id,
			   jboolean)
{
  using namespace java::lang::reflect;

  jobject result = NULL;
  klass = unwrap (klass);

  try
    {
      if (_Jv_equalUtf8Consts (id->name, init_name))
	{
	  // A constructor.
	  Constructor *cons = new Constructor ();
	  cons->offset = (char *) id - (char *) &klass->methods;
	  cons->declaringClass = klass;
	  result = cons;
	}
      else
	{
	  Method *meth = new Method ();
	  meth->offset = (char *) id - (char *) &klass->methods;
	  meth->declaringClass = klass;
	  result = meth;
	}
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return wrap_value (env, result);
}

static jmethodID JNICALL
_Jv_JNI_FromReflectedMethod (JNIEnv *, jobject method)
{
  using namespace java::lang::reflect;
  method = unwrap (method);
  if (Method::class$.isInstance (method))
    return _Jv_FromReflectedMethod (reinterpret_cast<Method *> (method));
  return
    _Jv_FromReflectedConstructor (reinterpret_cast<Constructor *> (method));
}

// JDK 1.2.
jweak JNICALL
_Jv_JNI_NewWeakGlobalRef (JNIEnv *env, jobject obj)
{
  using namespace gnu::gcj::runtime;
  JNIWeakRef *ref = NULL;

  try
    {
      // This seems weird but I think it is correct.
      obj = unwrap (obj);
      ref = new JNIWeakRef (obj);
      mark_for_gc (ref, global_ref_table);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return reinterpret_cast<jweak> (ref);
}

void JNICALL
_Jv_JNI_DeleteWeakGlobalRef (JNIEnv *, jweak obj)
{
  // JDK compatibility.
  if (obj == NULL)
    return;

  using namespace gnu::gcj::runtime;
  JNIWeakRef *ref = reinterpret_cast<JNIWeakRef *> (obj);
  unmark_for_gc (ref, global_ref_table);
  ref->clear ();
}



// Direct byte buffers.

static jobject JNICALL
_Jv_JNI_NewDirectByteBuffer (JNIEnv *, void *address, jlong length)
{
  using namespace gnu::gcj;
  using namespace java::nio;
  return new DirectByteBufferImpl$ReadWrite
    (reinterpret_cast<RawData *> (address), length);
}

static void * JNICALL
_Jv_JNI_GetDirectBufferAddress (JNIEnv *, jobject buffer)
{
  using namespace java::nio;
  if (! _Jv_IsInstanceOf (buffer, &Buffer::class$))
    return NULL;
  Buffer *tmp = static_cast<Buffer *> (buffer);
  return reinterpret_cast<void *> (tmp->address);
}

static jlong JNICALL
_Jv_JNI_GetDirectBufferCapacity (JNIEnv *, jobject buffer)
{
  using namespace java::nio;
  if (! _Jv_IsInstanceOf (buffer, &Buffer::class$))
    return -1;
  Buffer *tmp = static_cast<Buffer *> (buffer);
  if (tmp->address == NULL)
    return -1;
  return tmp->capacity();
}

static jobjectRefType JNICALL
_Jv_JNI_GetObjectRefType (JNIEnv *, jobject object)
{
  JvFail("GetObjectRefType not implemented");
  return JNIInvalidRefType;
}



struct NativeMethodCacheEntry : public JNINativeMethod
{
  char *className;
};

// Hash table of native methods.
static NativeMethodCacheEntry *nathash;
// Number of slots used.
static int nathash_count = 0;
// Number of slots available.  Must be power of 2.
static int nathash_size = 0;

#define DELETED_ENTRY ((char *) (~0))

// Compute a hash value for a native method descriptor.
static int
hash (const NativeMethodCacheEntry *method)
{
  char *ptr;
  int hash = 0;

  ptr = method->className;
  while (*ptr)
    hash = (31 * hash) + *ptr++;

  ptr = method->name;
  while (*ptr)
    hash = (31 * hash) + *ptr++;

  ptr = method->signature;
  while (*ptr)
    hash = (31 * hash) + *ptr++;

  return hash;
}

// Find the slot where a native method goes.
static NativeMethodCacheEntry *
nathash_find_slot (const NativeMethodCacheEntry *method)
{
  jint h = hash (method);
  int step = (h ^ (h >> 16)) | 1;
  int w = h & (nathash_size - 1);
  int del = -1;

  for (;;)
    {
      NativeMethodCacheEntry *slotp = &nathash[w];
      if (slotp->name == NULL)
	{
	  if (del >= 0)
	    return &nathash[del];
	  else
	    return slotp;
	}
      else if (slotp->name == DELETED_ENTRY)
	del = w;
      else if (! strcmp (slotp->name, method->name)
	       && ! strcmp (slotp->signature, method->signature)
	       && ! strcmp (slotp->className, method->className))
	return slotp;
      w = (w + step) & (nathash_size - 1);
    }
}

// Find a method.  Return NULL if it isn't in the hash table.
static void *
nathash_find (NativeMethodCacheEntry *method)
{
  if (nathash == NULL)
    return NULL;
  NativeMethodCacheEntry *slot = nathash_find_slot (method);
  if (slot->name == NULL || slot->name == DELETED_ENTRY)
    return NULL;
  return slot->fnPtr;
}

static void
natrehash ()
{
  if (nathash == NULL)
    {
      nathash_size = 1024;
      nathash =
	(NativeMethodCacheEntry *) _Jv_AllocBytes (nathash_size
						   * sizeof (NativeMethodCacheEntry));
    }
  else
    {
      int savesize = nathash_size;
      NativeMethodCacheEntry *savehash = nathash;
      nathash_size *= 2;
      nathash =
	(NativeMethodCacheEntry *) _Jv_AllocBytes (nathash_size
						   * sizeof (NativeMethodCacheEntry));

      for (int i = 0; i < savesize; ++i)
	{
	  if (savehash[i].name != NULL && savehash[i].name != DELETED_ENTRY)
	    {
	      NativeMethodCacheEntry *slot = nathash_find_slot (&savehash[i]);
	      *slot = savehash[i];
	    }
	}
    }
}

static void
nathash_add (const NativeMethodCacheEntry *method)
{
  if (3 * nathash_count >= 2 * nathash_size)
    natrehash ();
  NativeMethodCacheEntry *slot = nathash_find_slot (method);
  // If the slot has a real entry in it, then there is no work to do.
  if (slot->name != NULL && slot->name != DELETED_ENTRY)
    return;
  // FIXME: memory leak?
  slot->name = strdup (method->name);
  slot->className = strdup (method->className);
  // This was already strduped in _Jv_JNI_RegisterNatives.
  slot->signature = method->signature;
  slot->fnPtr = method->fnPtr;
}

static jint JNICALL
_Jv_JNI_RegisterNatives (JNIEnv *env, jclass klass,
			 const JNINativeMethod *methods,
			 jint nMethods)
{
  // Synchronize while we do the work.  This must match
  // synchronization in some other functions that manipulate or use
  // the nathash table.
  JvSynchronize sync (global_ref_table);

  NativeMethodCacheEntry dottedMethod;

  // Look at each descriptor given us, and find the corresponding
  // method in the class.
  for (int j = 0; j < nMethods; ++j)
    {
      bool found = false;

      _Jv_Method *imeths = JvGetFirstMethod (klass);
      for (int i = 0; i < JvNumMethods (klass); ++i)
	{
	  _Jv_Method *self = &imeths[i];

	  // Copy this JNINativeMethod and do a slash to dot
	  // conversion on the signature.
	  dottedMethod.name = methods[j].name;
	  // FIXME: we leak a little memory here if the method
	  // is not found.
	  dottedMethod.signature = strdup (methods[j].signature);
	  dottedMethod.fnPtr = methods[j].fnPtr;
	  dottedMethod.className = _Jv_GetClassNameUtf8 (klass)->chars();
	  char *c = dottedMethod.signature;
	  while (*c)
	    {
	      if (*c == '/')
		*c = '.';
	      c++;
	    }

	  if (! strcmp (self->name->chars (), dottedMethod.name)
	      && ! strcmp (self->signature->chars (), dottedMethod.signature))
	    {
	      if (! (self->accflags & java::lang::reflect::Modifier::NATIVE))
		break;

	      // Found a match that is native.
	      found = true;
	      nathash_add (&dottedMethod);

	      break;
	    }
	}

      if (! found)
	{
	  jstring m = JvNewStringUTF (methods[j].name);
	  try
	    {
	      env->ex = new java::lang::NoSuchMethodError (m);
	    }
	  catch (jthrowable t)
	    {
	      env->ex = t;
	    }
	  return JNI_ERR;
	}
    }

  return JNI_OK;
}

static jint JNICALL
_Jv_JNI_UnregisterNatives (JNIEnv *, jclass)
{
  // FIXME -- we could implement this.
  return JNI_ERR;
}



// Add a character to the buffer, encoding properly.
static void
add_char (char *buf, jchar c, int *here)
{
  if (c == '_')
    {
      buf[(*here)++] = '_';
      buf[(*here)++] = '1';
    }
  else if (c == ';')
    {
      buf[(*here)++] = '_';
      buf[(*here)++] = '2';
    }
  else if (c == '[')
    {
      buf[(*here)++] = '_';
      buf[(*here)++] = '3';
    }

  // Also check for `.' here because we might be passed an internal
  // qualified class name like `foo.bar'.
  else if (c == '/' || c == '.')
    buf[(*here)++] = '_';
  else if ((c >= '0' && c <= '9')
	   || (c >= 'a' && c <= 'z')
	   || (c >= 'A' && c <= 'Z'))
    buf[(*here)++] = (char) c;
  else
    {
      // "Unicode" character.
      buf[(*here)++] = '_';
      buf[(*here)++] = '0';
      for (int i = 0; i < 4; ++i)
	{
	  int val = c & 0x0f;
	  buf[(*here) + 3 - i] = (val > 10) ? ('a' + val - 10) : ('0' + val);
	  c >>= 4;
	}
      *here += 4;
    }
}

// Compute a mangled name for a native function.  This computes the
// long name, and also returns an index which indicates where a NUL
// can be placed to create the short name.  This function assumes that
// the buffer is large enough for its results.
static void
mangled_name (jclass klass, _Jv_Utf8Const *func_name,
	      _Jv_Utf8Const *signature, char *buf, int *long_start)
{
  strcpy (buf, "Java_");
  int here = 5;

  // Add fully qualified class name.
  jchar *chars = _Jv_GetStringChars (klass->getName ());
  jint len = klass->getName ()->length ();
  for (int i = 0; i < len; ++i)
    add_char (buf, chars[i], &here);

  // Don't use add_char because we need a literal `_'.
  buf[here++] = '_';

  const unsigned char *fn = (const unsigned char *) func_name->chars ();
  const unsigned char *limit = fn + func_name->len ();
  for (int i = 0; ; ++i)
    {
      int ch = UTF8_GET (fn, limit);
      if (ch < 0)
	break;
      add_char (buf, ch, &here);
    }

  // This is where the long signature begins.
  *long_start = here;
  buf[here++] = '_';
  buf[here++] = '_';

  const unsigned char *sig = (const unsigned char *) signature->chars ();
  limit = sig + signature->len ();
  JvAssert (sig[0] == '(');
  ++sig;
  while (1)
    {
      int ch = UTF8_GET (sig, limit);
      if (ch == ')' || ch < 0)
	break;
      add_char (buf, ch, &here);
    }

  buf[here] = '\0';
}

JNIEnv *
_Jv_GetJNIEnvNewFrameWithLoader (::java::lang::ClassLoader *loader)
{
  JNIEnv *env = _Jv_GetCurrentJNIEnv ();
  if (__builtin_expect (env == NULL, false))
    {
      env = (JNIEnv *) _Jv_MallocUnchecked (sizeof (JNIEnv));
      env->functions = &_Jv_JNIFunctions;
      env->locals = NULL;
      // We set env->ex below.

      // Set up the bottom, reusable frame.
      env->bottom_locals = (_Jv_JNI_LocalFrame *) 
	_Jv_MallocUnchecked (sizeof (_Jv_JNI_LocalFrame)
			     + (FRAME_SIZE
				* sizeof (jobject)));

      env->bottom_locals->marker = MARK_SYSTEM;
      env->bottom_locals->size = FRAME_SIZE;
      env->bottom_locals->next = NULL;
      env->bottom_locals->allocated_p = false;
      // We set the klass field below.
      memset (&env->bottom_locals->vec[0], 0, 
	      env->bottom_locals->size * sizeof (jobject));

      _Jv_SetCurrentJNIEnv (env);
    }

  // If we're in a simple JNI call (non-nested), we can just reuse the
  // locals frame we allocated many calls ago, back when the env was first
  // built, above.

  if (__builtin_expect (env->locals == NULL, true))
    {
      env->locals = env->bottom_locals;
      env->locals->loader = loader;
    }
  else
    {
      // Alternatively, we might be re-entering JNI, in which case we can't
      // reuse the bottom_locals frame, because it is already underneath
      // us. So we need to make a new one.
      _Jv_JNI_LocalFrame *frame
	= (_Jv_JNI_LocalFrame *) _Jv_MallocUnchecked (sizeof (_Jv_JNI_LocalFrame)
						      + (FRAME_SIZE
							 * sizeof (jobject)));

      frame->marker = MARK_SYSTEM;
      frame->size = FRAME_SIZE;
      frame->allocated_p = false;
      frame->next = env->locals;
      frame->loader = loader;

      memset (&frame->vec[0], 0, 
	      frame->size * sizeof (jobject));

      env->locals = frame;
    }

  env->ex = NULL;

  return env;
}

// Return the current thread's JNIEnv; if one does not exist, create
// it.  Also create a new system frame for use.  This is `extern "C"'
// because the compiler calls it.
extern "C" JNIEnv *
_Jv_GetJNIEnvNewFrame (jclass klass)
{
  return _Jv_GetJNIEnvNewFrameWithLoader (klass->getClassLoaderInternal());
}

// Destroy the env's reusable resources. This is called from the thread
// destructor "finalize_native" in natThread.cc
void 
_Jv_FreeJNIEnv (_Jv_JNIEnv *env)
{
  if (env == NULL)
    return;

  if (env->bottom_locals != NULL)
    _Jv_Free (env->bottom_locals);

  _Jv_Free (env);
}

// Return the function which implements a particular JNI method.  If
// we can't find the function, we throw the appropriate exception.
// This is `extern "C"' because the compiler uses it.
extern "C" void *
_Jv_LookupJNIMethod (jclass klass, _Jv_Utf8Const *name,
		     _Jv_Utf8Const *signature, MAYBE_UNUSED int args_size)
{
  int name_length = name->len();
  int sig_length = signature->len();
  char buf[10 + 6 * (name_length + sig_length) + 12];
  int long_start;
  void *function;

  // Synchronize on something convenient.  Right now we use the hash.
  JvSynchronize sync (global_ref_table);

  // First see if we have an override in the hash table.
  strncpy (buf, name->chars (), name_length);
  buf[name_length] = '\0';
  strncpy (buf + name_length + 1, signature->chars (), sig_length);
  buf[name_length + sig_length + 1] = '\0';
  NativeMethodCacheEntry meth;
  meth.name = buf;
  meth.signature = buf + name_length + 1;
  meth.className = _Jv_GetClassNameUtf8(klass)->chars();
  function = nathash_find (&meth);
  if (function != NULL)
    return function;

  // If there was no override, then look in the symbol table.
  buf[0] = '_';
  mangled_name (klass, name, signature, buf + 1, &long_start);
  char c = buf[long_start + 1];
  buf[long_start + 1] = '\0';

  function = _Jv_FindSymbolInExecutable (buf + 1);
#ifdef WIN32
  // On Win32, we use the "stdcall" calling convention (see JNICALL
  // in jni.h).
  // 
  // For a function named 'fooBar' that takes 'nn' bytes as arguments,
  // by default, MinGW GCC exports it as 'fooBar@nn', MSVC exports it
  // as '_fooBar@nn' and Borland C exports it as 'fooBar'. We try to
  // take care of all these variations here.

  char asz_buf[12];    /* '@' + '2147483647' (32-bit INT_MAX) + '\0' */
  char long_nm_sv[11]; /* Ditto, except for the '\0'. */

  if (function == NULL)
    {
      // We have tried searching for the 'fooBar' form (BCC) - now
      // try the others.

      // First, save the part of the long name that will be damaged
      // by appending '@nn'.
      memcpy (long_nm_sv, (buf + long_start + 1 + 1), sizeof (long_nm_sv));

      sprintf (asz_buf, "@%d", args_size);
      strcat (buf, asz_buf);

      // Search for the '_fooBar@nn' form (MSVC).
      function = _Jv_FindSymbolInExecutable (buf);

      if (function == NULL)
        {
          // Search for the 'fooBar@nn' form (MinGW GCC).
          function = _Jv_FindSymbolInExecutable (buf + 1);
        }
    }
#endif /* WIN32 */

  if (function == NULL)
    {
      buf[long_start + 1] = c;
#ifdef WIN32
      // Restore the part of the long name that was damaged by 
      // appending the '@nn'.
      memcpy ((buf + long_start + 1 + 1), long_nm_sv, sizeof (long_nm_sv));
#endif /* WIN32 */
      function = _Jv_FindSymbolInExecutable (buf + 1);
      if (function == NULL)
	{
#ifdef WIN32
          strcat (buf, asz_buf);
          function = _Jv_FindSymbolInExecutable (buf);
          if (function == NULL)
            function = _Jv_FindSymbolInExecutable (buf + 1);

          if (function == NULL)
#endif /* WIN32 */
            {
              jstring str = JvNewStringUTF (name->chars ());
              throw new java::lang::UnsatisfiedLinkError (str);
            }
	}
    }

  return function;
}

#ifdef INTERPRETER

// This function is the stub which is used to turn an ordinary (CNI)
// method call into a JNI call.
void
_Jv_JNIMethod::call (ffi_cif *, void *ret, INTERP_FFI_RAW_TYPE *args,
		     void *__this)
{
  _Jv_JNIMethod* _this = (_Jv_JNIMethod *) __this;

  JNIEnv *env = _Jv_GetJNIEnvNewFrame (_this->defining_class);

  // FIXME: we should mark every reference parameter as a local.  For
  // now we assume a conservative GC, and we assume that the
  // references are on the stack somewhere.

  // We cache the value that we find, of course, but if we don't find
  // a value we don't cache that fact -- we might subsequently load a
  // library which finds the function in question.
  {
    // Synchronize on a convenient object to ensure sanity in case two
    // threads reach this point for the same function at the same
    // time.
    JvSynchronize sync (global_ref_table);
    if (_this->function == NULL)
      {
        int args_size = sizeof (JNIEnv *) + _this->args_raw_size;

        if (_this->self->accflags & java::lang::reflect::Modifier::STATIC)
          args_size += sizeof (_this->defining_class);

        _this->function = _Jv_LookupJNIMethod (_this->defining_class,
                                               _this->self->name,
                                               _this->self->signature,
                                               args_size);
      }
  }

  JvAssert (_this->args_raw_size % sizeof (INTERP_FFI_RAW_TYPE) == 0);
  INTERP_FFI_RAW_TYPE
      real_args[2 + _this->args_raw_size / sizeof (INTERP_FFI_RAW_TYPE)];
  int offset = 0;

  // First argument is always the environment pointer.
  real_args[offset++].ptr = env;

  // For a static method, we pass in the Class.  For non-static
  // methods, the `this' argument is already handled.
  if ((_this->self->accflags & java::lang::reflect::Modifier::STATIC))
    real_args[offset++].ptr = _this->defining_class;

  // In libgcj, the callee synchronizes.
  jobject sync = NULL;
  if ((_this->self->accflags & java::lang::reflect::Modifier::SYNCHRONIZED))
    {
      if ((_this->self->accflags & java::lang::reflect::Modifier::STATIC))
	sync = _this->defining_class;
      else
	sync = (jobject) args[0].ptr;
      _Jv_MonitorEnter (sync);
    }

  // Copy over passed-in arguments.
  memcpy (&real_args[offset], args, _this->args_raw_size);
  
  // Add a frame to the composite (interpreted + JNI) call stack
  java::lang::Thread *thread = java::lang::Thread::currentThread();
  _Jv_NativeFrame nat_frame (_this, thread);

  // The actual call to the JNI function.
#if FFI_NATIVE_RAW_API
  ffi_raw_call (&_this->jni_cif, (void (*)()) _this->function,
		ret, real_args);
#else
  ffi_java_raw_call (&_this->jni_cif, (void (*)()) _this->function,
		     ret, real_args);
#endif

  // We might need to unwrap a JNI weak reference here.
  if (_this->jni_cif.rtype == &ffi_type_pointer)
    {
      _Jv_value *val = (_Jv_value *) ret;
      val->object_value = unwrap (val->object_value);
    }

  if (sync != NULL)
    _Jv_MonitorExit (sync);

  _Jv_JNI_PopSystemFrame (env);
}

#endif /* INTERPRETER */



//
// Invocation API.
//

// An internal helper function.
static jint
_Jv_JNI_AttachCurrentThread (JavaVM *, jstring name, void **penv,
			     void *args, jboolean is_daemon)
{
  JavaVMAttachArgs *attach = reinterpret_cast<JavaVMAttachArgs *> (args);
  java::lang::ThreadGroup *group = NULL;

  if (attach)
    {
      // FIXME: do we really want to support 1.1?
      if (attach->version != JNI_VERSION_1_4
	  && attach->version != JNI_VERSION_1_2
	  && attach->version != JNI_VERSION_1_1)
	return JNI_EVERSION;

      JvAssert (java::lang::ThreadGroup::class$.isInstance (attach->group));
      group = reinterpret_cast<java::lang::ThreadGroup *> (attach->group);
    }

  // Attaching an already-attached thread is a no-op.
  JNIEnv *env = _Jv_GetCurrentJNIEnv ();
  if (env != NULL)
    {
      *penv = reinterpret_cast<void *> (env);
      return 0;
    }

  env = (JNIEnv *) _Jv_MallocUnchecked (sizeof (JNIEnv));
  if (env == NULL)
    return JNI_ERR;
  env->functions = &_Jv_JNIFunctions;
  env->ex = NULL;
  env->bottom_locals
    = (_Jv_JNI_LocalFrame *) _Jv_MallocUnchecked (sizeof (_Jv_JNI_LocalFrame)
						  + (FRAME_SIZE
						     * sizeof (jobject)));
  env->locals = env->bottom_locals;
  if (env->locals == NULL)
    {
      _Jv_Free (env);
      return JNI_ERR;
    }

  env->locals->allocated_p = false;
  env->locals->marker = MARK_SYSTEM;
  env->locals->size = FRAME_SIZE;
  env->locals->loader = NULL;
  env->locals->next = NULL;

  for (int i = 0; i < env->locals->size; ++i)
    env->locals->vec[i] = NULL;

  *penv = reinterpret_cast<void *> (env);

  // This thread might already be a Java thread -- this function might
  // have been called simply to set the new JNIEnv.
  if (_Jv_ThreadCurrent () == NULL)
    {
      try
	{
	  if (is_daemon)
	    _Jv_AttachCurrentThreadAsDaemon (name, group);
	  else
	    _Jv_AttachCurrentThread (name, group);
	}
      catch (jthrowable t)
	{
	  return JNI_ERR;
	}
    }
  _Jv_SetCurrentJNIEnv (env);

  return 0;
}

// This is the one actually used by JNI.
jint JNICALL
_Jv_JNI_AttachCurrentThread (JavaVM *vm, void **penv, void *args)
{
  return _Jv_JNI_AttachCurrentThread (vm, NULL, penv, args, false);
}

static jint JNICALL
_Jv_JNI_AttachCurrentThreadAsDaemon (JavaVM *vm, void **penv, 
				     void *args)
{
  return _Jv_JNI_AttachCurrentThread (vm, NULL, penv, args, true);
}

static jint JNICALL
_Jv_JNI_DestroyJavaVM (JavaVM *vm)
{
  JvAssert (_Jv_the_vm && vm == _Jv_the_vm);

  union
  {
    JNIEnv *env;
    void *env_p;
  };

  if (_Jv_ThreadCurrent () != NULL)
    {
      jstring main_name;
      // This sucks.
      try
	{
	  main_name = JvNewStringLatin1 ("main");
	}
      catch (jthrowable t)
	{
	  return JNI_ERR;
	}

      jint r = _Jv_JNI_AttachCurrentThread (vm, main_name, &env_p,
					    NULL, false);
      if (r < 0)
	return r;
    }
  else
    env = _Jv_GetCurrentJNIEnv ();

  _Jv_ThreadWait ();

  // Docs say that this always returns an error code.
  return JNI_ERR;
}

jint JNICALL
_Jv_JNI_DetachCurrentThread (JavaVM *)
{
  jint code = _Jv_DetachCurrentThread ();
  return code  ? JNI_EDETACHED : 0;
}

static jint JNICALL
_Jv_JNI_GetEnv (JavaVM *, void **penv, jint version)
{
  if (_Jv_ThreadCurrent () == NULL)
    {
      *penv = NULL;
      return JNI_EDETACHED;
    }

#ifdef ENABLE_JVMPI
  // Handle JVMPI requests.
  if (version == JVMPI_VERSION_1)
    {
      *penv = (void *) &_Jv_JVMPI_Interface;
      return 0;
    }
#endif

#ifdef INTERPRETER
  // Handle JVMTI requests
  if (version == JVMTI_VERSION_1_0)
    {
      *penv = (void *) _Jv_GetJVMTIEnv ();
      return 0;
    }
#endif

  // FIXME: do we really want to support 1.1?
  if (version != JNI_VERSION_1_4 && version != JNI_VERSION_1_2
      && version != JNI_VERSION_1_1)
    {
      *penv = NULL;
      return JNI_EVERSION;
    }

  *penv = (void *) _Jv_GetCurrentJNIEnv ();
  return 0;
}

JavaVM *
_Jv_GetJavaVM ()
{
  // FIXME: synchronize
  if (! _Jv_the_vm)
    {
      JavaVM *nvm = (JavaVM *) _Jv_MallocUnchecked (sizeof (JavaVM));
      if (nvm != NULL)
	nvm->functions = &_Jv_JNI_InvokeFunctions;
      _Jv_the_vm = nvm;
    }

  // If this is a Java thread, we want to make sure it has an
  // associated JNIEnv.
  if (_Jv_ThreadCurrent () != NULL)
    {
      void *ignore;
      _Jv_JNI_AttachCurrentThread (_Jv_the_vm, &ignore, NULL);
    }

  return _Jv_the_vm;
}

static jint JNICALL
_Jv_JNI_GetJavaVM (JNIEnv *, JavaVM **vm)
{
  *vm = _Jv_GetJavaVM ();
  return *vm == NULL ? JNI_ERR : JNI_OK;
}



#define RESERVED NULL

struct JNINativeInterface_ _Jv_JNIFunctions =
{
  RESERVED,
  RESERVED,
  RESERVED,
  RESERVED,
  _Jv_JNI_GetVersion,		// GetVersion
  _Jv_JNI_DefineClass,		// DefineClass
  _Jv_JNI_FindClass,		// FindClass
  _Jv_JNI_FromReflectedMethod,	// FromReflectedMethod
  _Jv_JNI_FromReflectedField,	// FromReflectedField
  _Jv_JNI_ToReflectedMethod,	// ToReflectedMethod
  _Jv_JNI_GetSuperclass,	// GetSuperclass
  _Jv_JNI_IsAssignableFrom,	// IsAssignableFrom
  _Jv_JNI_ToReflectedField,	// ToReflectedField
  _Jv_JNI_Throw,		// Throw
  _Jv_JNI_ThrowNew,		// ThrowNew
  _Jv_JNI_ExceptionOccurred,	// ExceptionOccurred
  _Jv_JNI_ExceptionDescribe,	// ExceptionDescribe
  _Jv_JNI_ExceptionClear,	// ExceptionClear
  _Jv_JNI_FatalError,		// FatalError

  _Jv_JNI_PushLocalFrame,	// PushLocalFrame
  _Jv_JNI_PopLocalFrame,	// PopLocalFrame
  _Jv_JNI_NewGlobalRef,		// NewGlobalRef
  _Jv_JNI_DeleteGlobalRef,	// DeleteGlobalRef
  _Jv_JNI_DeleteLocalRef,	// DeleteLocalRef

  _Jv_JNI_IsSameObject,		// IsSameObject

  _Jv_JNI_NewLocalRef,		// NewLocalRef
  _Jv_JNI_EnsureLocalCapacity,	// EnsureLocalCapacity

  _Jv_JNI_AllocObject,		    // AllocObject
  _Jv_JNI_NewObject,		    // NewObject
  _Jv_JNI_NewObjectV,		    // NewObjectV
  _Jv_JNI_NewObjectA,		    // NewObjectA
  _Jv_JNI_GetObjectClass,	    // GetObjectClass
  _Jv_JNI_IsInstanceOf,		    // IsInstanceOf
  _Jv_JNI_GetAnyMethodID<false>,    // GetMethodID

  _Jv_JNI_CallMethod<jobject>,		// CallObjectMethod
  _Jv_JNI_CallMethodV<jobject>,		// CallObjectMethodV
  _Jv_JNI_CallMethodA<jobject>,		// CallObjectMethodA
  _Jv_JNI_CallMethod<jboolean>,		// CallBooleanMethod
  _Jv_JNI_CallMethodV<jboolean>,	// CallBooleanMethodV
  _Jv_JNI_CallMethodA<jboolean>,	// CallBooleanMethodA
  _Jv_JNI_CallMethod<jbyte>,		// CallByteMethod
  _Jv_JNI_CallMethodV<jbyte>,		// CallByteMethodV
  _Jv_JNI_CallMethodA<jbyte>,		// CallByteMethodA
  _Jv_JNI_CallMethod<jchar>,		// CallCharMethod
  _Jv_JNI_CallMethodV<jchar>,		// CallCharMethodV
  _Jv_JNI_CallMethodA<jchar>,		// CallCharMethodA
  _Jv_JNI_CallMethod<jshort>,		// CallShortMethod
  _Jv_JNI_CallMethodV<jshort>,		// CallShortMethodV
  _Jv_JNI_CallMethodA<jshort>,		// CallShortMethodA
  _Jv_JNI_CallMethod<jint>,		// CallIntMethod
  _Jv_JNI_CallMethodV<jint>,		// CallIntMethodV
  _Jv_JNI_CallMethodA<jint>,		// CallIntMethodA
  _Jv_JNI_CallMethod<jlong>,		// CallLongMethod
  _Jv_JNI_CallMethodV<jlong>,		// CallLongMethodV
  _Jv_JNI_CallMethodA<jlong>,		// CallLongMethodA
  _Jv_JNI_CallMethod<jfloat>,		// CallFloatMethod
  _Jv_JNI_CallMethodV<jfloat>,		// CallFloatMethodV
  _Jv_JNI_CallMethodA<jfloat>,		// CallFloatMethodA
  _Jv_JNI_CallMethod<jdouble>,		// CallDoubleMethod
  _Jv_JNI_CallMethodV<jdouble>,		// CallDoubleMethodV
  _Jv_JNI_CallMethodA<jdouble>,		// CallDoubleMethodA
  _Jv_JNI_CallVoidMethod,		// CallVoidMethod
  _Jv_JNI_CallVoidMethodV,		// CallVoidMethodV
  _Jv_JNI_CallVoidMethodA,		// CallVoidMethodA

  // Nonvirtual method invocation functions follow.
  _Jv_JNI_CallAnyMethod<jobject, nonvirtual>,	// CallNonvirtualObjectMethod
  _Jv_JNI_CallAnyMethodV<jobject, nonvirtual>,	// CallNonvirtualObjectMethodV
  _Jv_JNI_CallAnyMethodA<jobject, nonvirtual>,	// CallNonvirtualObjectMethodA
  _Jv_JNI_CallAnyMethod<jboolean, nonvirtual>,	// CallNonvirtualBooleanMethod
  _Jv_JNI_CallAnyMethodV<jboolean, nonvirtual>,	// CallNonvirtualBooleanMethodV
  _Jv_JNI_CallAnyMethodA<jboolean, nonvirtual>,	// CallNonvirtualBooleanMethodA
  _Jv_JNI_CallAnyMethod<jbyte, nonvirtual>,	// CallNonvirtualByteMethod
  _Jv_JNI_CallAnyMethodV<jbyte, nonvirtual>,	// CallNonvirtualByteMethodV
  _Jv_JNI_CallAnyMethodA<jbyte, nonvirtual>,	// CallNonvirtualByteMethodA
  _Jv_JNI_CallAnyMethod<jchar, nonvirtual>,	// CallNonvirtualCharMethod
  _Jv_JNI_CallAnyMethodV<jchar, nonvirtual>,	// CallNonvirtualCharMethodV
  _Jv_JNI_CallAnyMethodA<jchar, nonvirtual>,	// CallNonvirtualCharMethodA
  _Jv_JNI_CallAnyMethod<jshort, nonvirtual>,	// CallNonvirtualShortMethod
  _Jv_JNI_CallAnyMethodV<jshort, nonvirtual>,	// CallNonvirtualShortMethodV
  _Jv_JNI_CallAnyMethodA<jshort, nonvirtual>,	// CallNonvirtualShortMethodA
  _Jv_JNI_CallAnyMethod<jint, nonvirtual>,	// CallNonvirtualIntMethod
  _Jv_JNI_CallAnyMethodV<jint, nonvirtual>,	// CallNonvirtualIntMethodV
  _Jv_JNI_CallAnyMethodA<jint, nonvirtual>,	// CallNonvirtualIntMethodA
  _Jv_JNI_CallAnyMethod<jlong, nonvirtual>,	// CallNonvirtualLongMethod
  _Jv_JNI_CallAnyMethodV<jlong, nonvirtual>,	// CallNonvirtualLongMethodV
  _Jv_JNI_CallAnyMethodA<jlong, nonvirtual>,	// CallNonvirtualLongMethodA
  _Jv_JNI_CallAnyMethod<jfloat, nonvirtual>,	// CallNonvirtualFloatMethod
  _Jv_JNI_CallAnyMethodV<jfloat, nonvirtual>,	// CallNonvirtualFloatMethodV
  _Jv_JNI_CallAnyMethodA<jfloat, nonvirtual>,	// CallNonvirtualFloatMethodA
  _Jv_JNI_CallAnyMethod<jdouble, nonvirtual>,	// CallNonvirtualDoubleMethod
  _Jv_JNI_CallAnyMethodV<jdouble, nonvirtual>,	// CallNonvirtualDoubleMethodV
  _Jv_JNI_CallAnyMethodA<jdouble, nonvirtual>,	// CallNonvirtualDoubleMethodA
  _Jv_JNI_CallAnyVoidMethod<nonvirtual>,	// CallNonvirtualVoidMethod
  _Jv_JNI_CallAnyVoidMethodV<nonvirtual>,	// CallNonvirtualVoidMethodV
  _Jv_JNI_CallAnyVoidMethodA<nonvirtual>,	// CallNonvirtualVoidMethodA

  _Jv_JNI_GetAnyFieldID<false>,	// GetFieldID
  _Jv_JNI_GetField<jobject>,	// GetObjectField
  _Jv_JNI_GetField<jboolean>,	// GetBooleanField
  _Jv_JNI_GetField<jbyte>,	// GetByteField
  _Jv_JNI_GetField<jchar>,	// GetCharField
  _Jv_JNI_GetField<jshort>,	// GetShortField
  _Jv_JNI_GetField<jint>,	// GetIntField
  _Jv_JNI_GetField<jlong>,	// GetLongField
  _Jv_JNI_GetField<jfloat>,	// GetFloatField
  _Jv_JNI_GetField<jdouble>,	// GetDoubleField
  _Jv_JNI_SetField,		// SetObjectField
  _Jv_JNI_SetField,		// SetBooleanField
  _Jv_JNI_SetField,		// SetByteField
  _Jv_JNI_SetField,		// SetCharField
  _Jv_JNI_SetField,		// SetShortField
  _Jv_JNI_SetField,		// SetIntField
  _Jv_JNI_SetField,		// SetLongField
  _Jv_JNI_SetField,		// SetFloatField
  _Jv_JNI_SetField,		// SetDoubleField
  _Jv_JNI_GetAnyMethodID<true>,	// GetStaticMethodID

  _Jv_JNI_CallStaticMethod<jobject>,	  // CallStaticObjectMethod
  _Jv_JNI_CallStaticMethodV<jobject>,	  // CallStaticObjectMethodV
  _Jv_JNI_CallStaticMethodA<jobject>,	  // CallStaticObjectMethodA
  _Jv_JNI_CallStaticMethod<jboolean>,	  // CallStaticBooleanMethod
  _Jv_JNI_CallStaticMethodV<jboolean>,	  // CallStaticBooleanMethodV
  _Jv_JNI_CallStaticMethodA<jboolean>,	  // CallStaticBooleanMethodA
  _Jv_JNI_CallStaticMethod<jbyte>,	  // CallStaticByteMethod
  _Jv_JNI_CallStaticMethodV<jbyte>,	  // CallStaticByteMethodV
  _Jv_JNI_CallStaticMethodA<jbyte>,	  // CallStaticByteMethodA
  _Jv_JNI_CallStaticMethod<jchar>,	  // CallStaticCharMethod
  _Jv_JNI_CallStaticMethodV<jchar>,	  // CallStaticCharMethodV
  _Jv_JNI_CallStaticMethodA<jchar>,	  // CallStaticCharMethodA
  _Jv_JNI_CallStaticMethod<jshort>,	  // CallStaticShortMethod
  _Jv_JNI_CallStaticMethodV<jshort>,	  // CallStaticShortMethodV
  _Jv_JNI_CallStaticMethodA<jshort>,	  // CallStaticShortMethodA
  _Jv_JNI_CallStaticMethod<jint>,	  // CallStaticIntMethod
  _Jv_JNI_CallStaticMethodV<jint>,	  // CallStaticIntMethodV
  _Jv_JNI_CallStaticMethodA<jint>,	  // CallStaticIntMethodA
  _Jv_JNI_CallStaticMethod<jlong>,	  // CallStaticLongMethod
  _Jv_JNI_CallStaticMethodV<jlong>,	  // CallStaticLongMethodV
  _Jv_JNI_CallStaticMethodA<jlong>,	  // CallStaticLongMethodA
  _Jv_JNI_CallStaticMethod<jfloat>,	  // CallStaticFloatMethod
  _Jv_JNI_CallStaticMethodV<jfloat>,	  // CallStaticFloatMethodV
  _Jv_JNI_CallStaticMethodA<jfloat>,	  // CallStaticFloatMethodA
  _Jv_JNI_CallStaticMethod<jdouble>,	  // CallStaticDoubleMethod
  _Jv_JNI_CallStaticMethodV<jdouble>,	  // CallStaticDoubleMethodV
  _Jv_JNI_CallStaticMethodA<jdouble>,	  // CallStaticDoubleMethodA
  _Jv_JNI_CallStaticVoidMethod,		  // CallStaticVoidMethod
  _Jv_JNI_CallStaticVoidMethodV,	  // CallStaticVoidMethodV
  _Jv_JNI_CallStaticVoidMethodA,	  // CallStaticVoidMethodA

  _Jv_JNI_GetAnyFieldID<true>,	       // GetStaticFieldID
  _Jv_JNI_GetStaticField<jobject>,     // GetStaticObjectField
  _Jv_JNI_GetStaticField<jboolean>,    // GetStaticBooleanField
  _Jv_JNI_GetStaticField<jbyte>,       // GetStaticByteField
  _Jv_JNI_GetStaticField<jchar>,       // GetStaticCharField
  _Jv_JNI_GetStaticField<jshort>,      // GetStaticShortField
  _Jv_JNI_GetStaticField<jint>,	       // GetStaticIntField
  _Jv_JNI_GetStaticField<jlong>,       // GetStaticLongField
  _Jv_JNI_GetStaticField<jfloat>,      // GetStaticFloatField
  _Jv_JNI_GetStaticField<jdouble>,     // GetStaticDoubleField
  _Jv_JNI_SetStaticField,	       // SetStaticObjectField
  _Jv_JNI_SetStaticField,	       // SetStaticBooleanField
  _Jv_JNI_SetStaticField,	       // SetStaticByteField
  _Jv_JNI_SetStaticField,	       // SetStaticCharField
  _Jv_JNI_SetStaticField,	       // SetStaticShortField
  _Jv_JNI_SetStaticField,	       // SetStaticIntField
  _Jv_JNI_SetStaticField,	       // SetStaticLongField
  _Jv_JNI_SetStaticField,	       // SetStaticFloatField
  _Jv_JNI_SetStaticField,	       // SetStaticDoubleField
  _Jv_JNI_NewString,		       // NewString
  _Jv_JNI_GetStringLength,	       // GetStringLength
  _Jv_JNI_GetStringChars,	       // GetStringChars
  _Jv_JNI_ReleaseStringChars,	       // ReleaseStringChars
  _Jv_JNI_NewStringUTF,		       // NewStringUTF
  _Jv_JNI_GetStringUTFLength,	       // GetStringUTFLength
  _Jv_JNI_GetStringUTFChars,	       // GetStringUTFChars
  _Jv_JNI_ReleaseStringUTFChars,       // ReleaseStringUTFChars
  _Jv_JNI_GetArrayLength,	       // GetArrayLength
  _Jv_JNI_NewObjectArray,	       // NewObjectArray
  _Jv_JNI_GetObjectArrayElement,       // GetObjectArrayElement
  _Jv_JNI_SetObjectArrayElement,       // SetObjectArrayElement
  _Jv_JNI_NewPrimitiveArray<jboolean, JvPrimClass (boolean)>,
							    // NewBooleanArray
  _Jv_JNI_NewPrimitiveArray<jbyte, JvPrimClass (byte)>,	    // NewByteArray
  _Jv_JNI_NewPrimitiveArray<jchar, JvPrimClass (char)>,	    // NewCharArray
  _Jv_JNI_NewPrimitiveArray<jshort, JvPrimClass (short)>,   // NewShortArray
  _Jv_JNI_NewPrimitiveArray<jint, JvPrimClass (int)>,	    // NewIntArray
  _Jv_JNI_NewPrimitiveArray<jlong, JvPrimClass (long)>,	    // NewLongArray
  _Jv_JNI_NewPrimitiveArray<jfloat, JvPrimClass (float)>,   // NewFloatArray
  _Jv_JNI_NewPrimitiveArray<jdouble, JvPrimClass (double)>, // NewDoubleArray
  _Jv_JNI_GetPrimitiveArrayElements<jboolean, JvPrimClass (boolean)>,	    
					    // GetBooleanArrayElements
  _Jv_JNI_GetPrimitiveArrayElements<jbyte, JvPrimClass (byte)>,	 
					    // GetByteArrayElements
  _Jv_JNI_GetPrimitiveArrayElements<jchar, JvPrimClass (char)>,
					    // GetCharArrayElements
  _Jv_JNI_GetPrimitiveArrayElements<jshort, JvPrimClass (short)>,	    
					    // GetShortArrayElements
  _Jv_JNI_GetPrimitiveArrayElements<jint, JvPrimClass (int)>,		    
					    // GetIntArrayElements
  _Jv_JNI_GetPrimitiveArrayElements<jlong, JvPrimClass (long)>,		    
					    // GetLongArrayElements
  _Jv_JNI_GetPrimitiveArrayElements<jfloat, JvPrimClass (float)>,	    
					    // GetFloatArrayElements
  _Jv_JNI_GetPrimitiveArrayElements<jdouble, JvPrimClass (double)>,	    
					    // GetDoubleArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jboolean, JvPrimClass (boolean)>,    
					    // ReleaseBooleanArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jbyte, JvPrimClass (byte)>,    
					    // ReleaseByteArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jchar, JvPrimClass (char)>,    
					    // ReleaseCharArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jshort, JvPrimClass (short)>,	 
					    // ReleaseShortArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jint, JvPrimClass (int)>,    
					    // ReleaseIntArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jlong, JvPrimClass (long)>,    
					    // ReleaseLongArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jfloat, JvPrimClass (float)>,	 
					    // ReleaseFloatArrayElements
  _Jv_JNI_ReleasePrimitiveArrayElements<jdouble, JvPrimClass (double)>,	   
					    // ReleaseDoubleArrayElements
  _Jv_JNI_GetPrimitiveArrayRegion<jboolean, JvPrimClass (boolean)>,	    
					    // GetBooleanArrayRegion
  _Jv_JNI_GetPrimitiveArrayRegion<jbyte, JvPrimClass (byte)>,	    
					    // GetByteArrayRegion
  _Jv_JNI_GetPrimitiveArrayRegion<jchar, JvPrimClass (char)>,	    
					    // GetCharArrayRegion
  _Jv_JNI_GetPrimitiveArrayRegion<jshort, JvPrimClass (short)>,	    
					    // GetShortArrayRegion
  _Jv_JNI_GetPrimitiveArrayRegion<jint, JvPrimClass (int)>,	    
					    // GetIntArrayRegion
  _Jv_JNI_GetPrimitiveArrayRegion<jlong, JvPrimClass (long)>,	    
					    // GetLongArrayRegion
  _Jv_JNI_GetPrimitiveArrayRegion<jfloat, JvPrimClass (float)>,	    
					    // GetFloatArrayRegion
  _Jv_JNI_GetPrimitiveArrayRegion<jdouble, JvPrimClass (double)>,	    
					    // GetDoubleArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jboolean, JvPrimClass (boolean)>,	    
					    // SetBooleanArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jbyte, JvPrimClass (byte)>,	    
					    // SetByteArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jchar, JvPrimClass (char)>,	    
					    // SetCharArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jshort, JvPrimClass (short)>,	    
					    // SetShortArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jint, JvPrimClass (int)>,	    
					    // SetIntArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jlong, JvPrimClass (long)>,	    
					    // SetLongArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jfloat, JvPrimClass (float)>,	    
					    // SetFloatArrayRegion
  _Jv_JNI_SetPrimitiveArrayRegion<jdouble, JvPrimClass (double)>,	    
					    // SetDoubleArrayRegion
  _Jv_JNI_RegisterNatives,		    // RegisterNatives
  _Jv_JNI_UnregisterNatives,		    // UnregisterNatives
  _Jv_JNI_MonitorEnter,			    // MonitorEnter
  _Jv_JNI_MonitorExit,			    // MonitorExit
  _Jv_JNI_GetJavaVM,			    // GetJavaVM

  _Jv_JNI_GetStringRegion,		    // GetStringRegion
  _Jv_JNI_GetStringUTFRegion,		    // GetStringUTFRegion
  _Jv_JNI_GetPrimitiveArrayCritical,	    // GetPrimitiveArrayCritical
  _Jv_JNI_ReleasePrimitiveArrayCritical,    // ReleasePrimitiveArrayCritical
  _Jv_JNI_GetStringCritical,		    // GetStringCritical
  _Jv_JNI_ReleaseStringCritical,	    // ReleaseStringCritical

  _Jv_JNI_NewWeakGlobalRef,		    // NewWeakGlobalRef
  _Jv_JNI_DeleteWeakGlobalRef,		    // DeleteWeakGlobalRef

  _Jv_JNI_ExceptionCheck,		    // ExceptionCheck

  _Jv_JNI_NewDirectByteBuffer,		    // NewDirectByteBuffer
  _Jv_JNI_GetDirectBufferAddress,	    // GetDirectBufferAddress
  _Jv_JNI_GetDirectBufferCapacity,	    // GetDirectBufferCapacity

  _Jv_JNI_GetObjectRefType		    // GetObjectRefType
};

struct JNIInvokeInterface_ _Jv_JNI_InvokeFunctions =
{
  RESERVED,
  RESERVED,
  RESERVED,

  _Jv_JNI_DestroyJavaVM,
  _Jv_JNI_AttachCurrentThread,
  _Jv_JNI_DetachCurrentThread,
  _Jv_JNI_GetEnv,
  _Jv_JNI_AttachCurrentThreadAsDaemon
};
