// jni.cc - JNI implementation, including the jump table.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stddef.h>
#include <string.h>

// Define this before including jni.h.
#define __GCJ_JNI_IMPL__

#include <gcj/cni.h>
#include <jvm.h>
#include <java-assert.h>
#include <jni.h>
#ifdef ENABLE_JVMPI
#include <jvmpi.h>
#endif

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Throwable.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/StringIndexOutOfBoundsException.h>
#include <java/lang/AbstractMethodError.h>
#include <java/lang/InstantiationException.h>
#include <java/lang/NoSuchFieldError.h>
#include <java/lang/NoSuchMethodError.h>
#include <java/lang/reflect/Constructor.h>
#include <java/lang/reflect/Method.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/OutOfMemoryError.h>
#include <java/util/Hashtable.h>
#include <java/lang/Integer.h>
#include <gnu/gcj/jni/NativeThread.h>

#include <gcj/method.h>
#include <gcj/field.h>

#include <java-interp.h>

#define ClassClass _CL_Q34java4lang5Class
extern java::lang::Class ClassClass;
#define ObjectClass _CL_Q34java4lang6Object
extern java::lang::Class ObjectClass;

#define ThrowableClass _CL_Q34java4lang9Throwable
extern java::lang::Class ThrowableClass;
#define MethodClass _CL_Q44java4lang7reflect6Method
extern java::lang::Class MethodClass;
#define ThreadGroupClass _CL_Q34java4lang11ThreadGroup
extern java::lang::Class ThreadGroupClass;
#define NativeThreadClass _CL_Q43gnu3gcj3jni12NativeThread
extern java::lang::Class ThreadGroupClass;

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
extern struct JNINativeInterface _Jv_JNIFunctions;
extern struct JNIInvokeInterface _Jv_JNI_InvokeFunctions;

// Number of slots in the default frame.  The VM must allow at least
// 16.
#define FRAME_SIZE 32

// Mark value indicating this is an overflow frame.
#define MARK_NONE    0
// Mark value indicating this is a user frame.
#define MARK_USER    1
// Mark value indicating this is a system frame.
#define MARK_SYSTEM  2

// This structure is used to keep track of local references.
struct _Jv_JNI_LocalFrame
{
  // This is true if this frame object represents a pushed frame (eg
  // from PushLocalFrame).
  int marker :  2;

  // Number of elements in frame.
  int size   : 30;

  // Next frame in chain.
  _Jv_JNI_LocalFrame *next;

  // The elements.  These are allocated using the C "struct hack".
  jobject vec[0];
};

// This holds a reference count for all local and global references.
static java::util::Hashtable *ref_table;

// The only VM.
static JavaVM *the_vm;

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
  ref_table = new java::util::Hashtable;
  
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
mark_for_gc (jobject obj)
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
unmark_for_gc (jobject obj)
{
  JvSynchronize sync (ref_table);

  using namespace java::lang;
  Integer *refcount = (Integer *) ref_table->get (obj);
  JvAssert (refcount);
  jint val = refcount->intValue () - 1;
  if (val == 0)
    ref_table->remove (obj);
  else
    // FIXME: what about out of memory error?
    ref_table->put (obj, new Integer (val));
}



static jobject
_Jv_JNI_NewGlobalRef (JNIEnv *, jobject obj)
{
  mark_for_gc (obj);
  return obj;
}

static void
_Jv_JNI_DeleteGlobalRef (JNIEnv *, jobject obj)
{
  unmark_for_gc (obj);
}

static void
_Jv_JNI_DeleteLocalRef (JNIEnv *env, jobject obj)
{
  _Jv_JNI_LocalFrame *frame;

  for (frame = env->locals; frame != NULL; frame = frame->next)
    {
      for (int i = 0; i < FRAME_SIZE; ++i)
	{
	  if (frame->vec[i] == obj)
	    {
	      frame->vec[i] = NULL;
	      unmark_for_gc (obj);
	      return;
	    }
	}

      // Don't go past a marked frame.
      JvAssert (frame->marker == MARK_NONE);
    }

  JvAssert (0);
}

static jint
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
  memset (&frame->vec[0], 0, size * sizeof (jobject));
  frame->next = env->locals;
  env->locals = frame;

  return 0;
}

static jint
_Jv_JNI_PushLocalFrame (JNIEnv *env, jint size)
{
  jint r = _Jv_JNI_EnsureLocalCapacity (env, size);
  if (r < 0)
    return r;

  // The new frame is on top.
  env->locals->marker = MARK_USER;

  return 0;
}

static jobject
_Jv_JNI_NewLocalRef (JNIEnv *env, jobject obj)
{
  // Try to find an open slot somewhere in the topmost frame.
  _Jv_JNI_LocalFrame *frame = env->locals;
  bool done = false, set = false;
  while (frame != NULL && ! done)
    {
      for (int i = 0; i < frame->size; ++i)
	if (frame->vec[i] == NULL)
	  {
	    set = true;
	    done = true;
	    frame->vec[i] = obj;
	    break;
	  }
    }

  if (! set)
    {
      // No slots, so we allocate a new frame.  According to the spec
      // we could just die here.  FIXME: return value.
      _Jv_JNI_EnsureLocalCapacity (env, 16);
      // We know the first element of the new frame will be ok.
      env->locals->vec[0] = obj;
    }

  mark_for_gc (obj);
  return obj;
}

static jobject
_Jv_JNI_PopLocalFrame (JNIEnv *env, jobject result, int stop)
{
  _Jv_JNI_LocalFrame *rf = env->locals;

  bool done = false;
  while (rf != NULL && ! done)
    {  
      for (int i = 0; i < rf->size; ++i)
	if (rf->vec[i] != NULL)
	  unmark_for_gc (rf->vec[i]);

      // If the frame we just freed is the marker frame, we are done.
      done = (rf->marker == stop);

      _Jv_JNI_LocalFrame *n = rf->next;
      // When N==NULL, we've reached the stack-allocated frame, and we
      // must not free it.  However, we must be sure to clear all its
      // elements, since we might conceivably reuse it.
      if (n == NULL)
	{
	  memset (&rf->vec[0], 0, rf->size * sizeof (jobject));
	  break;
	}

      _Jv_Free (rf);
      rf = n;
    }

  return result == NULL ? NULL : _Jv_JNI_NewLocalRef (env, result);
}

static jobject
_Jv_JNI_PopLocalFrame (JNIEnv *env, jobject result)
{
  return _Jv_JNI_PopLocalFrame (env, result, MARK_USER);
}

// Pop a `system' frame from the stack.  This is `extern "C"' as it is
// used by the compiler.
extern "C" void
_Jv_JNI_PopSystemFrame (JNIEnv *env)
{
  _Jv_JNI_PopLocalFrame (env, NULL, MARK_SYSTEM);

  if (env->ex)
    throw env->ex;
}

// This function is used from other template functions.  It wraps the
// return value appropriately; we specialize it so that object returns
// are turned into local references.
template<typename T>
static T
wrap_value (JNIEnv *, T value)
{
  return value;
}

template<>
static jobject
wrap_value (JNIEnv *env, jobject value)
{
  return value == NULL ? value : _Jv_JNI_NewLocalRef (env, value);
}



static jint
_Jv_JNI_GetVersion (JNIEnv *)
{
  return JNI_VERSION_1_2;
}

static jclass
_Jv_JNI_DefineClass (JNIEnv *env, jobject loader, 
		     const jbyte *buf, jsize bufLen)
{
  try
    {
      jbyteArray bytes = JvNewByteArray (bufLen);

      jbyte *elts = elements (bytes);
      memcpy (elts, buf, bufLen * sizeof (jbyte));

      java::lang::ClassLoader *l
	= reinterpret_cast<java::lang::ClassLoader *> (loader);

      jclass result = l->defineClass (bytes, 0, bufLen);
      return (jclass) wrap_value (env, result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static jclass
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

      java::lang::ClassLoader *loader;
      if (env->klass == NULL)
	{
	  // FIXME: should use getBaseClassLoader, but we don't have that
	  // yet.
	  loader = java::lang::ClassLoader::getSystemClassLoader ();
	}
      else
	loader = env->klass->getClassLoader ();

      r = loader->loadClass (n);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return (jclass) wrap_value (env, r);
}

static jclass
_Jv_JNI_GetSuperclass (JNIEnv *env, jclass clazz)
{
  return (jclass) wrap_value (env, clazz->getSuperclass ());
}

static jboolean
_Jv_JNI_IsAssignableFrom(JNIEnv *, jclass clazz1, jclass clazz2)
{
  return clazz1->isAssignableFrom (clazz2);
}

static jint
_Jv_JNI_Throw (JNIEnv *env, jthrowable obj)
{
  // We check in case the user did some funky cast.
  JvAssert (obj != NULL && (&ThrowableClass)->isInstance (obj));
  env->ex = obj;
  return 0;
}

static jint
_Jv_JNI_ThrowNew (JNIEnv *env, jclass clazz, const char *message)
{
  using namespace java::lang::reflect;

  JvAssert ((&ThrowableClass)->isAssignableFrom (clazz));

  int r = JNI_OK;
  try
    {
      JArray<jclass> *argtypes
	= (JArray<jclass> *) JvNewObjectArray (1, &ClassClass, NULL);

      jclass *elts = elements (argtypes);
      elts[0] = &StringClass;

      Constructor *cons = clazz->getConstructor (argtypes);

      jobjectArray values = JvNewObjectArray (1, &StringClass, NULL);
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

static jthrowable
_Jv_JNI_ExceptionOccurred (JNIEnv *env)
{
  return (jthrowable) wrap_value (env, env->ex);
}

static void
_Jv_JNI_ExceptionDescribe (JNIEnv *env)
{
  if (env->ex != NULL)
    env->ex->printStackTrace();
}

static void
_Jv_JNI_ExceptionClear (JNIEnv *env)
{
  env->ex = NULL;
}

static jboolean
_Jv_JNI_ExceptionCheck (JNIEnv *env)
{
  return env->ex != NULL;
}

static void
_Jv_JNI_FatalError (JNIEnv *, const char *message)
{
  JvFail (message);
}



static jboolean
_Jv_JNI_IsSameObject (JNIEnv *, jobject obj1, jobject obj2)
{
  return obj1 == obj2;
}

static jobject
_Jv_JNI_AllocObject (JNIEnv *env, jclass clazz)
{
  jobject obj = NULL;
  using namespace java::lang::reflect;

  try
    {
      JvAssert (clazz && ! clazz->isArray ());
      if (clazz->isInterface() || Modifier::isAbstract(clazz->getModifiers()))
	env->ex = new java::lang::InstantiationException ();
      else
	{
	  // FIXME: will this work for String?
	  obj = JvAllocObject (clazz);
	}
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return wrap_value (env, obj);
}

static jclass
_Jv_JNI_GetObjectClass (JNIEnv *env, jobject obj)
{
  JvAssert (obj);
  return (jclass) wrap_value (env, obj->getClass());
}

static jboolean
_Jv_JNI_IsInstanceOf (JNIEnv *, jobject obj, jclass clazz)
{
  return clazz->isInstance(obj);
}



//
// This section concerns method invocation.
//

template<jboolean is_static>
static jmethodID
_Jv_JNI_GetAnyMethodID (JNIEnv *env, jclass clazz,
			const char *name, const char *sig)
{
  try
    {
      _Jv_InitClass (clazz);

      _Jv_Utf8Const *name_u = _Jv_makeUtf8Const ((char *) name, -1);
      _Jv_Utf8Const *sig_u = _Jv_makeUtf8Const ((char *) sig, -1);

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

      env->ex = new java::lang::NoSuchMethodError ();
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
      if (arg_elts[i] == JvPrimClass (byte))
	values[i].b = va_arg (vargs, jbyte);
      else if (arg_elts[i] == JvPrimClass (short))
	values[i].s = va_arg (vargs, jshort);
      else if (arg_elts[i] == JvPrimClass (int))
	values[i].i = va_arg (vargs, jint);
      else if (arg_elts[i] == JvPrimClass (long))
	values[i].j = va_arg (vargs, jlong);
      else if (arg_elts[i] == JvPrimClass (float))
	values[i].f = va_arg (vargs, jfloat);
      else if (arg_elts[i] == JvPrimClass (double))
	values[i].d = va_arg (vargs, jdouble);
      else if (arg_elts[i] == JvPrimClass (boolean))
	values[i].z = va_arg (vargs, jboolean);
      else if (arg_elts[i] == JvPrimClass (char))
	values[i].c = va_arg (vargs, jchar);
      else
	{
	  // An object.
	  values[i].l = va_arg (vargs, jobject);
	}
    }
}

// This can call any sort of method: virtual, "nonvirtual", static, or
// constructor.
template<typename T, invocation_type style>
static T
_Jv_JNI_CallAnyMethodV (JNIEnv *env, jobject obj, jclass klass,
			jmethodID id, va_list vargs)
{
  if (style == normal)
    id = _Jv_LookupDeclaredMethod (obj->getClass (), id->name, id->signature);

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
      jthrowable ex = _Jv_CallAnyMethodA (obj, return_type, id,
					  style == constructor,
					  arg_types, args, &result);

      if (ex != NULL)
	env->ex = ex;

      // We cheat a little here.  FIXME.
      return wrap_value (env, * (T *) &result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return wrap_value (env, (T) 0);
}

template<typename T, invocation_type style>
static T
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
static T
_Jv_JNI_CallAnyMethodA (JNIEnv *env, jobject obj, jclass klass,
			jmethodID id, jvalue *args)
{
  if (style == normal)
    id = _Jv_LookupDeclaredMethod (obj->getClass (), id->name, id->signature);

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

      jvalue result;
      jthrowable ex = _Jv_CallAnyMethodA (obj, return_type, id,
					  style == constructor,
					  arg_types, args, &result);

      if (ex != NULL)
	env->ex = ex;

      // We cheat a little here.  FIXME.
      return wrap_value (env, * (T *) &result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }

  return wrap_value (env, (T) 0);
}

template<invocation_type style>
static void
_Jv_JNI_CallAnyVoidMethodV (JNIEnv *env, jobject obj, jclass klass,
			    jmethodID id, va_list vargs)
{
  if (style == normal)
    id = _Jv_LookupDeclaredMethod (obj->getClass (), id->name, id->signature);

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

      jthrowable ex = _Jv_CallAnyMethodA (obj, return_type, id,
					  style == constructor,
					  arg_types, args, NULL);

      if (ex != NULL)
	env->ex = ex;
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
}

template<invocation_type style>
static void
_Jv_JNI_CallAnyVoidMethod (JNIEnv *env, jobject obj, jclass klass,
			   jmethodID method, ...)
{
  va_list args;

  va_start (args, method);
  _Jv_JNI_CallAnyVoidMethodV<style> (env, obj, klass, method, args);
  va_end (args);
}

template<invocation_type style>
static void
_Jv_JNI_CallAnyVoidMethodA (JNIEnv *env, jobject obj, jclass klass,
			    jmethodID id, jvalue *args)
{
  if (style == normal)
    id = _Jv_LookupDeclaredMethod (obj->getClass (), id->name, id->signature);

  jclass decl_class = klass ? klass : obj->getClass ();
  JvAssert (decl_class != NULL);

  jclass return_type;
  JArray<jclass> *arg_types;
  try
    {
      _Jv_GetTypesFromSignature (id, decl_class,
				 &arg_types, &return_type);

      jthrowable ex = _Jv_CallAnyMethodA (obj, return_type, id,
					  style == constructor,
					  arg_types, args, NULL);

      if (ex != NULL)
	env->ex = ex;
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
}

// Functions with this signature are used to implement functions in
// the CallMethod family.
template<typename T>
static T
_Jv_JNI_CallMethodV (JNIEnv *env, jobject obj, jmethodID id, va_list args)
{
  return _Jv_JNI_CallAnyMethodV<T, normal> (env, obj, NULL, id, args);
}

// Functions with this signature are used to implement functions in
// the CallMethod family.
template<typename T>
static T
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
static T
_Jv_JNI_CallMethodA (JNIEnv *env, jobject obj, jmethodID id, jvalue *args)
{
  return _Jv_JNI_CallAnyMethodA<T, normal> (env, obj, NULL, id, args);
}

static void
_Jv_JNI_CallVoidMethodV (JNIEnv *env, jobject obj, jmethodID id, va_list args)
{
  _Jv_JNI_CallAnyVoidMethodV<normal> (env, obj, NULL, id, args);
}

static void
_Jv_JNI_CallVoidMethod (JNIEnv *env, jobject obj, jmethodID id, ...)
{
  va_list args;

  va_start (args, id);
  _Jv_JNI_CallAnyVoidMethodV<normal> (env, obj, NULL, id, args);
  va_end (args);
}

static void
_Jv_JNI_CallVoidMethodA (JNIEnv *env, jobject obj, jmethodID id, jvalue *args)
{
  _Jv_JNI_CallAnyVoidMethodA<normal> (env, obj, NULL, id, args);
}

// Functions with this signature are used to implement functions in
// the CallStaticMethod family.
template<typename T>
static T
_Jv_JNI_CallStaticMethodV (JNIEnv *env, jclass klass,
			   jmethodID id, va_list args)
{
  JvAssert (((id->accflags) & java::lang::reflect::Modifier::STATIC));
  JvAssert ((&ClassClass)->isInstance (klass));

  return _Jv_JNI_CallAnyMethodV<T, static_type> (env, NULL, klass, id, args);
}

// Functions with this signature are used to implement functions in
// the CallStaticMethod family.
template<typename T>
static T
_Jv_JNI_CallStaticMethod (JNIEnv *env, jclass klass, jmethodID id, ...)
{
  va_list args;
  T result;

  JvAssert (((id->accflags) & java::lang::reflect::Modifier::STATIC));
  JvAssert ((&ClassClass)->isInstance (klass));

  va_start (args, id);
  result = _Jv_JNI_CallAnyMethodV<T, static_type> (env, NULL, klass,
						   id, args);
  va_end (args);

  return result;
}

// Functions with this signature are used to implement functions in
// the CallStaticMethod family.
template<typename T>
static T
_Jv_JNI_CallStaticMethodA (JNIEnv *env, jclass klass, jmethodID id,
			   jvalue *args)
{
  JvAssert (((id->accflags) & java::lang::reflect::Modifier::STATIC));
  JvAssert ((&ClassClass)->isInstance (klass));

  return _Jv_JNI_CallAnyMethodA<T, static_type> (env, NULL, klass, id, args);
}

static void
_Jv_JNI_CallStaticVoidMethodV (JNIEnv *env, jclass klass, jmethodID id,
			       va_list args)
{
  _Jv_JNI_CallAnyVoidMethodV<static_type> (env, NULL, klass, id, args);
}

static void
_Jv_JNI_CallStaticVoidMethod (JNIEnv *env, jclass klass, jmethodID id, ...)
{
  va_list args;

  va_start (args, id);
  _Jv_JNI_CallAnyVoidMethodV<static_type> (env, NULL, klass, id, args);
  va_end (args);
}

static void
_Jv_JNI_CallStaticVoidMethodA (JNIEnv *env, jclass klass, jmethodID id,
			       jvalue *args)
{
  _Jv_JNI_CallAnyVoidMethodA<static_type> (env, NULL, klass, id, args);
}

static jobject
_Jv_JNI_NewObjectV (JNIEnv *env, jclass klass,
		    jmethodID id, va_list args)
{
  JvAssert (klass && ! klass->isArray ());
  JvAssert (! strcmp (id->name->data, "<init>")
	    && id->signature->length > 2
	    && id->signature->data[0] == '('
	    && ! strcmp (&id->signature->data[id->signature->length - 2],
			 ")V"));

  return _Jv_JNI_CallAnyMethodV<jobject, constructor> (env, NULL, klass,
						       id, args);
}

static jobject
_Jv_JNI_NewObject (JNIEnv *env, jclass klass, jmethodID id, ...)
{
  JvAssert (klass && ! klass->isArray ());
  JvAssert (! strcmp (id->name->data, "<init>")
	    && id->signature->length > 2
	    && id->signature->data[0] == '('
	    && ! strcmp (&id->signature->data[id->signature->length - 2],
			 ")V"));

  va_list args;
  jobject result;

  va_start (args, id);
  result = _Jv_JNI_CallAnyMethodV<jobject, constructor> (env, NULL, klass,
							 id, args);
  va_end (args);

  return result;
}

static jobject
_Jv_JNI_NewObjectA (JNIEnv *env, jclass klass, jmethodID id,
		    jvalue *args)
{
  JvAssert (klass && ! klass->isArray ());
  JvAssert (! strcmp (id->name->data, "<init>")
	    && id->signature->length > 2
	    && id->signature->data[0] == '('
	    && ! strcmp (&id->signature->data[id->signature->length - 2],
			 ")V"));

  return _Jv_JNI_CallAnyMethodA<jobject, constructor> (env, NULL, klass,
						       id, args);
}



template<typename T>
static T
_Jv_JNI_GetField (JNIEnv *env, jobject obj, jfieldID field) 
{
  JvAssert (obj);
  T *ptr = (T *) ((char *) obj + field->getOffset ());
  return wrap_value (env, *ptr);
}

template<typename T>
static void
_Jv_JNI_SetField (JNIEnv *, jobject obj, jfieldID field, T value)
{
  JvAssert (obj);
  T *ptr = (T *) ((char *) obj + field->getOffset ());
  *ptr = value;
}

template<jboolean is_static>
static jfieldID
_Jv_JNI_GetAnyFieldID (JNIEnv *env, jclass clazz,
		       const char *name, const char *sig)
{
  try
    {
      _Jv_InitClass (clazz);

      _Jv_Utf8Const *a_name = _Jv_makeUtf8Const ((char *) name, -1);

      jclass field_class = NULL;
      if (sig[0] == '[')
	field_class = _Jv_FindClassFromSignature ((char *) sig, NULL);
      else
	{
	  _Jv_Utf8Const *sig_u = _Jv_makeUtf8Const ((char *) sig, -1);
	  field_class = _Jv_FindClass (sig_u, NULL);
	}

      // FIXME: what if field_class == NULL?

      while (clazz != NULL)
	{
	  jint count = (is_static
			? JvNumStaticFields (clazz)
			: JvNumInstanceFields (clazz));
	  jfieldID field = (is_static
			    ? JvGetFirstStaticField (clazz)
			    : JvGetFirstInstanceField (clazz));
	  for (jint i = 0; i < count; ++i)
	    {
	      // The field is resolved as a side effect of class
	      // initialization.
	      JvAssert (field->isResolved ());

	      _Jv_Utf8Const *f_name = field->getNameUtf8Const(clazz);

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
static T
_Jv_JNI_GetStaticField (JNIEnv *env, jclass, jfieldID field)
{
  T *ptr = (T *) field->u.addr;
  return wrap_value (env, *ptr);
}

template<typename T>
static void
_Jv_JNI_SetStaticField (JNIEnv *, jclass, jfieldID field, T value)
{
  T *ptr = (T *) field->u.addr;
  *ptr = value;
}

static jstring
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

static jsize
_Jv_JNI_GetStringLength (JNIEnv *, jstring string)
{
  return string->length();
}

static const jchar *
_Jv_JNI_GetStringChars (JNIEnv *, jstring string, jboolean *isCopy)
{
  jchar *result = _Jv_GetStringChars (string);
  mark_for_gc (string);
  if (isCopy)
    *isCopy = false;
  return (const jchar *) result;
}

static void
_Jv_JNI_ReleaseStringChars (JNIEnv *, jstring string, const jchar *)
{
  unmark_for_gc (string);
}

static jstring
_Jv_JNI_NewStringUTF (JNIEnv *env, const char *bytes)
{
  try
    {
      jstring result = JvNewStringUTF (bytes);
      return (jstring) wrap_value (env, result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static jsize
_Jv_JNI_GetStringUTFLength (JNIEnv *, jstring string)
{
  return JvGetStringUTFLength (string);
}

static const char *
_Jv_JNI_GetStringUTFChars (JNIEnv *env, jstring string, jboolean *isCopy)
{
  jsize len = JvGetStringUTFLength (string);
  try
    {
      char *r = (char *) _Jv_Malloc (len + 1);
      JvGetStringUTFRegion (string, 0, len, r);
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

static void
_Jv_JNI_ReleaseStringUTFChars (JNIEnv *, jstring, const char *utf)
{
  _Jv_Free ((void *) utf);
}

static void
_Jv_JNI_GetStringRegion (JNIEnv *env, jstring string, jsize start, jsize len,
			 jchar *buf)
{
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

static void
_Jv_JNI_GetStringUTFRegion (JNIEnv *env, jstring str, jsize start,
			    jsize len, char *buf)
{
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

static const jchar *
_Jv_JNI_GetStringCritical (JNIEnv *, jstring str, jboolean *isCopy)
{
  jchar *result = _Jv_GetStringChars (str);
  if (isCopy)
    *isCopy = false;
  return result;
}

static void
_Jv_JNI_ReleaseStringCritical (JNIEnv *, jstring, const jchar *)
{
  // Nothing.
}

static jsize
_Jv_JNI_GetArrayLength (JNIEnv *, jarray array)
{
  return array->length;
}

static jarray
_Jv_JNI_NewObjectArray (JNIEnv *env, jsize length, jclass elementClass,
			jobject init)
{
  try
    {
      jarray result = JvNewObjectArray (length, elementClass, init);
      return (jarray) wrap_value (env, result);
    }
  catch (jthrowable t)
    {
      env->ex = t;
      return NULL;
    }
}

static jobject
_Jv_JNI_GetObjectArrayElement (JNIEnv *env, jobjectArray array, jsize index)
{
  jobject *elts = elements (array);
  return wrap_value (env, elts[index]);
}

static void
_Jv_JNI_SetObjectArrayElement (JNIEnv *env, jobjectArray array, jsize index,
			       jobject value)
{
  try
    {
      _Jv_CheckArrayStore (array, value);
      jobject *elts = elements (array);
      elts[index] = value;
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
}

template<typename T, jclass K>
static JArray<T> *
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

template<typename T>
static T *
_Jv_JNI_GetPrimitiveArrayElements (JNIEnv *, JArray<T> *array,
				   jboolean *isCopy)
{
  T *elts = elements (array);
  if (isCopy)
    {
      // We elect never to copy.
      *isCopy = false;
    }
  mark_for_gc (array);
  return elts;
}

template<typename T>
static void
_Jv_JNI_ReleasePrimitiveArrayElements (JNIEnv *, JArray<T> *array,
				       T *, jint /* mode */)
{
  // Note that we ignore MODE.  We can do this because we never copy
  // the array elements.  My reading of the JNI documentation is that
  // this is an option for the implementor.
  unmark_for_gc (array);
}

template<typename T>
static void
_Jv_JNI_GetPrimitiveArrayRegion (JNIEnv *env, JArray<T> *array,
				 jsize start, jsize len,
				 T *buf)
{
  if (start < 0 || len >= array->length || start + len >= array->length)
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

template<typename T>
static void
_Jv_JNI_SetPrimitiveArrayRegion (JNIEnv *env, JArray<T> *array, 
				 jsize start, jsize len, T *buf)
{
  if (start < 0 || len >= array->length || start + len >= array->length)
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

static void *
_Jv_JNI_GetPrimitiveArrayCritical (JNIEnv *, jarray array,
				   jboolean *isCopy)
{
  // FIXME: does this work?
  jclass klass = array->getClass()->getComponentType();
  JvAssert (klass->isPrimitive ());
  char *r = _Jv_GetArrayElementFromElementType (array, klass);
  if (isCopy)
    *isCopy = false;
  return r;
}

static void
_Jv_JNI_ReleasePrimitiveArrayCritical (JNIEnv *, jarray, void *, jint)
{
  // Nothing.
}

static jint
_Jv_JNI_MonitorEnter (JNIEnv *env, jobject obj)
{
  try
    {
      return _Jv_MonitorEnter (obj);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
  return JNI_ERR;
}

static jint
_Jv_JNI_MonitorExit (JNIEnv *env, jobject obj)
{
  try
    {
      return _Jv_MonitorExit (obj);
    }
  catch (jthrowable t)
    {
      env->ex = t;
    }
  return JNI_ERR;
}

// JDK 1.2
jobject
_Jv_JNI_ToReflectedField (JNIEnv *env, jclass cls, jfieldID fieldID,
			  jboolean)
{
  try
    {
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
static jfieldID
_Jv_JNI_FromReflectedField (JNIEnv *, jobject f)
{
  using namespace java::lang::reflect;

  Field *field = reinterpret_cast<Field *> (f);
  return _Jv_FromReflectedField (field);
}

jobject
_Jv_JNI_ToReflectedMethod (JNIEnv *env, jclass klass, jmethodID id,
			   jboolean)
{
  using namespace java::lang::reflect;

  // FIXME.
  static _Jv_Utf8Const *init_name = _Jv_makeUtf8Const ("<init>", 6);

  jobject result = NULL;

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

static jmethodID
_Jv_JNI_FromReflectedMethod (JNIEnv *, jobject method)
{
  using namespace java::lang::reflect;
  if ((&MethodClass)->isInstance (method))
    return _Jv_FromReflectedMethod (reinterpret_cast<Method *> (method));
  return
    _Jv_FromReflectedConstructor (reinterpret_cast<Constructor *> (method));
}

static jint
_Jv_JNI_RegisterNatives (JNIEnv *env, jclass k,
			 const JNINativeMethod *methods,
			 jint nMethods)
{
#ifdef INTERPRETER
  // For now, this only matters for interpreted methods.  FIXME.
  if (! _Jv_IsInterpretedClass (k))
    {
      // FIXME: throw exception.
      return JNI_ERR;
    }
  _Jv_InterpClass *klass = reinterpret_cast<_Jv_InterpClass *> (k);

  // Look at each descriptor given us, and find the corresponding
  // method in the class.
  for (int j = 0; j < nMethods; ++j)
    {
      bool found = false;

      _Jv_MethodBase **imeths = _Jv_GetFirstMethod (klass);
      for (int i = 0; i < JvNumMethods (klass); ++i)
	{
	  _Jv_MethodBase *meth = imeths[i];
	  _Jv_Method *self = meth->get_method ();

	  if (! strcmp (self->name->data, methods[j].name)
	      && ! strcmp (self->signature->data, methods[j].signature))
	    {
	      if (! (self->accflags
		     & java::lang::reflect::Modifier::NATIVE))
		break;

	      // Found a match that is native.
	      _Jv_JNIMethod *jmeth = reinterpret_cast<_Jv_JNIMethod *> (meth);
	      jmeth->set_function (methods[i].fnPtr);
	      found = true;
	      break;
	    }
	}

      if (! found)
	{
	  jstring m = JvNewStringUTF (methods[j].name);
	  try
	    {
	      env->ex =new java::lang::NoSuchMethodError (m);
	    }
	  catch (jthrowable t)
	    {
	      env->ex = t;
	    }
	  return JNI_ERR;
	}
    }

  return JNI_OK;
#else /* INTERPRETER */
  return JNI_ERR;
#endif /* INTERPRETER */
}

static jint
_Jv_JNI_UnregisterNatives (JNIEnv *, jclass)
{
  return JNI_ERR;
}



#ifdef INTERPRETER

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
  else if (c == '/')
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
	  buf[(*here) + 4 - i] = (val > 10) ? ('a' + val - 10) : ('0' + val);
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

  const unsigned char *fn = (const unsigned char *) func_name->data;
  const unsigned char *limit = fn + func_name->length;
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

  const unsigned char *sig = (const unsigned char *) signature->data;
  limit = sig + signature->length;
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

// Return the current thread's JNIEnv; if one does not exist, create
// it.  Also create a new system frame for use.  This is `extern "C"'
// because the compiler calls it.
extern "C" JNIEnv *
_Jv_GetJNIEnvNewFrame (jclass klass)
{
  JNIEnv *env = _Jv_GetCurrentJNIEnv ();
  if (env == NULL)
    {
      env = (JNIEnv *) _Jv_MallocUnchecked (sizeof (JNIEnv));
      env->p = &_Jv_JNIFunctions;
      env->ex = NULL;
      env->klass = klass;
      env->locals = NULL;

      _Jv_SetCurrentJNIEnv (env);
    }

  _Jv_JNI_LocalFrame *frame
    = (_Jv_JNI_LocalFrame *) _Jv_MallocUnchecked (sizeof (_Jv_JNI_LocalFrame)
						  + (FRAME_SIZE
						     * sizeof (jobject)));

  frame->marker = MARK_SYSTEM;
  frame->size = FRAME_SIZE;
  frame->next = env->locals;
  env->locals = frame;

  for (int i = 0; i < frame->size; ++i)
    frame->vec[i] = NULL;

  return env;
}

// Return the function which implements a particular JNI method.  If
// we can't find the function, we throw the appropriate exception.
// This is `extern "C"' because the compiler uses it.
extern "C" void *
_Jv_LookupJNIMethod (jclass klass, _Jv_Utf8Const *name,
		     _Jv_Utf8Const *signature)
{
  char buf[10 + 6 * (name->length + signature->length)];
  int long_start;
  void *function;

  mangled_name (klass, name, signature, buf, &long_start);
  char c = buf[long_start];
  buf[long_start] = '\0';
  function = _Jv_FindSymbolInExecutable (buf);
  if (function == NULL)
    {
      buf[long_start] = c;
      function = _Jv_FindSymbolInExecutable (buf);
      if (function == NULL)
	{
	  jstring str = JvNewStringUTF (name->data);
	  JvThrow (new java::lang::AbstractMethodError (str));
	}
    }

  return function;
}

// This function is the stub which is used to turn an ordinary (CNI)
// method call into a JNI call.
void
_Jv_JNIMethod::call (ffi_cif *, void *ret, ffi_raw *args, void *__this)
{
  _Jv_JNIMethod* _this = (_Jv_JNIMethod *) __this;

  JNIEnv *env = _Jv_GetJNIEnvNewFrame (_this->defining_class);

  // FIXME: we should mark every reference parameter as a local.  For
  // now we assume a conservative GC, and we assume that the
  // references are on the stack somewhere.

  // We cache the value that we find, of course, but if we don't find
  // a value we don't cache that fact -- we might subsequently load a
  // library which finds the function in question.
  if (_this->function == NULL)
    _this->function = _Jv_LookupJNIMethod (_this->defining_class,
					   _this->self->name,
					   _this->self->signature);

  JvAssert (_this->args_raw_size % sizeof (ffi_raw) == 0);
  ffi_raw real_args[2 + _this->args_raw_size / sizeof (ffi_raw)];
  int offset = 0;

  // First argument is always the environment pointer.
  real_args[offset++].ptr = env;

  // For a static method, we pass in the Class.  For non-static
  // methods, the `this' argument is already handled.
  if ((_this->self->accflags & java::lang::reflect::Modifier::STATIC))
    real_args[offset++].ptr = _this->defining_class;

  // Copy over passed-in arguments.
  memcpy (&real_args[offset], args, _this->args_raw_size);

  // The actual call to the JNI function.
  ffi_raw_call (&_this->jni_cif, (void (*) (...)) _this->function,
		ret, real_args);

  _Jv_JNI_PopSystemFrame (env);
}

#endif /* INTERPRETER */



//
// Invocation API.
//

// An internal helper function.
static jint
_Jv_JNI_AttachCurrentThread (JavaVM *, jstring name, void **penv, void *args)
{
  JavaVMAttachArgs *attach = reinterpret_cast<JavaVMAttachArgs *> (args);
  java::lang::ThreadGroup *group = NULL;

  if (attach)
    {
      // FIXME: do we really want to support 1.1?
      if (attach->version != JNI_VERSION_1_2
	  && attach->version != JNI_VERSION_1_1)
	return JNI_EVERSION;

      JvAssert ((&ThreadGroupClass)->isInstance (attach->group));
      group = reinterpret_cast<java::lang::ThreadGroup *> (attach->group);
    }

  // Attaching an already-attached thread is a no-op.
  if (_Jv_GetCurrentJNIEnv () != NULL)
    return 0;

  JNIEnv *env = (JNIEnv *) _Jv_MallocUnchecked (sizeof (JNIEnv));
  if (env == NULL)
    return JNI_ERR;
  env->p = &_Jv_JNIFunctions;
  env->ex = NULL;
  env->klass = NULL;
  env->locals
    = (_Jv_JNI_LocalFrame *) _Jv_MallocUnchecked (sizeof (_Jv_JNI_LocalFrame)
						  + (FRAME_SIZE
						     * sizeof (jobject)));
  if (env->locals == NULL)
    {
      _Jv_Free (env);
      return JNI_ERR;
    }
  *penv = reinterpret_cast<void *> (env);

  // This thread might already be a Java thread -- this function might
  // have been called simply to set the new JNIEnv.
  if (_Jv_ThreadCurrent () == NULL)
    {
      try
	{
	  (void) new gnu::gcj::jni::NativeThread (group, name);
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
static jint
_Jv_JNI_AttachCurrentThread (JavaVM *vm, void **penv, void *args)
{
  return _Jv_JNI_AttachCurrentThread (vm, NULL, penv, args);
}

static jint
_Jv_JNI_DestroyJavaVM (JavaVM *vm)
{
  JvAssert (the_vm && vm == the_vm);

  JNIEnv *env;
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

      jint r = _Jv_JNI_AttachCurrentThread (vm,
					    main_name,
					    reinterpret_cast<void **> (&env),
					    NULL);
      if (r < 0)
	return r;
    }
  else
    env = _Jv_GetCurrentJNIEnv ();

  _Jv_ThreadWait ();

  // Docs say that this always returns an error code.
  return JNI_ERR;
}

static jint
_Jv_JNI_DetachCurrentThread (JavaVM *)
{
  java::lang::Thread *t = _Jv_ThreadCurrent ();
  if (t == NULL)
    return JNI_EDETACHED;

  // FIXME: we only allow threads attached via AttachCurrentThread to
  // be detached.  I have no idea how we could implement detaching
  // other threads, given the requirement that we must release all the
  // monitors.  That just seems evil.
  JvAssert ((&NativeThreadClass)->isInstance (t));

  // FIXME: release the monitors.  We'll take this to mean all
  // monitors acquired via the JNI interface.  This means we have to
  // keep track of them.

  gnu::gcj::jni::NativeThread *nt
    = reinterpret_cast<gnu::gcj::jni::NativeThread *> (t);
  nt->finish ();

  return 0;
}

static jint
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

  // FIXME: do we really want to support 1.1?
  if (version != JNI_VERSION_1_2 && version != JNI_VERSION_1_1)
    {
      *penv = NULL;
      return JNI_EVERSION;
    }

  *penv = (void *) _Jv_GetCurrentJNIEnv ();
  return 0;
}

jint
JNI_GetDefaultJavaVMInitArgs (void *args)
{
  jint version = * (jint *) args;
  // Here we only support 1.2.
  if (version != JNI_VERSION_1_2)
    return JNI_EVERSION;

  JavaVMInitArgs *ia = reinterpret_cast<JavaVMInitArgs *> (args);
  ia->version = JNI_VERSION_1_2;
  ia->nOptions = 0;
  ia->options = NULL;
  ia->ignoreUnrecognized = true;

  return 0;
}

jint
JNI_CreateJavaVM (JavaVM **vm, void **penv, void *args)
{
  JvAssert (! the_vm);
  // FIXME: synchronize
  JavaVM *nvm = (JavaVM *) _Jv_MallocUnchecked (sizeof (JavaVM));
  if (nvm == NULL)
    return JNI_ERR;
  nvm->functions = &_Jv_JNI_InvokeFunctions;

  // Parse the arguments.
  if (args != NULL)
    {
      jint version = * (jint *) args;
      // We only support 1.2.
      if (version != JNI_VERSION_1_2)
	return JNI_EVERSION;
      JavaVMInitArgs *ia = reinterpret_cast<JavaVMInitArgs *> (args);
      for (int i = 0; i < ia->nOptions; ++i)
	{
	  if (! strcmp (ia->options[i].optionString, "vfprintf")
	      || ! strcmp (ia->options[i].optionString, "exit")
	      || ! strcmp (ia->options[i].optionString, "abort"))
	    {
	      // We are required to recognize these, but for now we
	      // don't handle them in any way.  FIXME.
	      continue;
	    }
	  else if (! strncmp (ia->options[i].optionString,
			      "-verbose", sizeof ("-verbose") - 1))
	    {
	      // We don't do anything with this option either.  We
	      // might want to make sure the argument is valid, but we
	      // don't really care all that much for now.
	      continue;
	    }
	  else if (! strncmp (ia->options[i].optionString, "-D", 2))
	    {
	      // FIXME.
	      continue;
	    }
	  else if (ia->ignoreUnrecognized)
	    {
	      if (ia->options[i].optionString[0] == '_'
		  || ! strncmp (ia->options[i].optionString, "-X", 2))
		continue;
	    }

	  return JNI_ERR;
	}
    }

  jint r =_Jv_JNI_AttachCurrentThread (nvm, penv, NULL);
  if (r < 0)
    return r;

  the_vm = nvm;
  *vm = the_vm;
  return 0;
}

jint
JNI_GetCreatedJavaVMs (JavaVM **vm_buffer, jsize buf_len, jsize *n_vms)
{
  if (buf_len <= 0)
    return JNI_ERR;

  // We only support a single VM.
  if (the_vm != NULL)
    {
      vm_buffer[0] = the_vm;
      *n_vms = 1;
    }
  else
    *n_vms = 0;
  return 0;
}

JavaVM *
_Jv_GetJavaVM ()
{
  // FIXME: synchronize
  if (! the_vm)
    {
      JavaVM *nvm = (JavaVM *) _Jv_MallocUnchecked (sizeof (JavaVM));
      if (nvm != NULL)
	nvm->functions = &_Jv_JNI_InvokeFunctions;
      the_vm = nvm;
    }

  // If this is a Java thread, we want to make sure it has an
  // associated JNIEnv.
  if (_Jv_ThreadCurrent () != NULL)
    {
      void *ignore;
      _Jv_JNI_AttachCurrentThread (the_vm, &ignore, NULL);
    }

  return the_vm;
}

static jint
_Jv_JNI_GetJavaVM (JNIEnv *, JavaVM **vm)
{
  *vm = _Jv_GetJavaVM ();
  return *vm == NULL ? JNI_ERR : JNI_OK;
}



#define NOT_IMPL NULL
#define RESERVED NULL

struct JNINativeInterface _Jv_JNIFunctions =
{
  RESERVED,
  RESERVED,
  RESERVED,
  RESERVED,
  _Jv_JNI_GetVersion,
  _Jv_JNI_DefineClass,
  _Jv_JNI_FindClass,
  _Jv_JNI_FromReflectedMethod,
  _Jv_JNI_FromReflectedField,
  _Jv_JNI_ToReflectedMethod,
  _Jv_JNI_GetSuperclass,
  _Jv_JNI_IsAssignableFrom,
  _Jv_JNI_ToReflectedField,
  _Jv_JNI_Throw,
  _Jv_JNI_ThrowNew,
  _Jv_JNI_ExceptionOccurred,
  _Jv_JNI_ExceptionDescribe,
  _Jv_JNI_ExceptionClear,
  _Jv_JNI_FatalError,

  _Jv_JNI_PushLocalFrame,
  _Jv_JNI_PopLocalFrame,
  _Jv_JNI_NewGlobalRef,
  _Jv_JNI_DeleteGlobalRef,
  _Jv_JNI_DeleteLocalRef,

  _Jv_JNI_IsSameObject,

  _Jv_JNI_NewLocalRef,
  _Jv_JNI_EnsureLocalCapacity,

  _Jv_JNI_AllocObject,
  _Jv_JNI_NewObject,
  _Jv_JNI_NewObjectV,
  _Jv_JNI_NewObjectA,
  _Jv_JNI_GetObjectClass,
  _Jv_JNI_IsInstanceOf,
  _Jv_JNI_GetAnyMethodID<false>,

  _Jv_JNI_CallMethod<jobject>,
  _Jv_JNI_CallMethodV<jobject>,
  _Jv_JNI_CallMethodA<jobject>,
  _Jv_JNI_CallMethod<jboolean>,
  _Jv_JNI_CallMethodV<jboolean>,
  _Jv_JNI_CallMethodA<jboolean>,
  _Jv_JNI_CallMethod<jbyte>,
  _Jv_JNI_CallMethodV<jbyte>,
  _Jv_JNI_CallMethodA<jbyte>,
  _Jv_JNI_CallMethod<jchar>,
  _Jv_JNI_CallMethodV<jchar>,
  _Jv_JNI_CallMethodA<jchar>,
  _Jv_JNI_CallMethod<jshort>,
  _Jv_JNI_CallMethodV<jshort>,
  _Jv_JNI_CallMethodA<jshort>,
  _Jv_JNI_CallMethod<jint>,
  _Jv_JNI_CallMethodV<jint>,
  _Jv_JNI_CallMethodA<jint>,
  _Jv_JNI_CallMethod<jlong>,
  _Jv_JNI_CallMethodV<jlong>,
  _Jv_JNI_CallMethodA<jlong>,
  _Jv_JNI_CallMethod<jfloat>,
  _Jv_JNI_CallMethodV<jfloat>,
  _Jv_JNI_CallMethodA<jfloat>,
  _Jv_JNI_CallMethod<jdouble>,
  _Jv_JNI_CallMethodV<jdouble>,
  _Jv_JNI_CallMethodA<jdouble>,
  _Jv_JNI_CallVoidMethod,
  _Jv_JNI_CallVoidMethodV,
  _Jv_JNI_CallVoidMethodA,

  // Nonvirtual method invocation functions follow.
  _Jv_JNI_CallAnyMethod<jobject, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jobject, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jobject, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jboolean, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jboolean, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jboolean, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jbyte, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jbyte, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jbyte, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jchar, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jchar, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jchar, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jshort, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jshort, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jshort, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jint, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jint, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jint, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jlong, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jlong, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jlong, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jfloat, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jfloat, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jfloat, nonvirtual>,
  _Jv_JNI_CallAnyMethod<jdouble, nonvirtual>,
  _Jv_JNI_CallAnyMethodV<jdouble, nonvirtual>,
  _Jv_JNI_CallAnyMethodA<jdouble, nonvirtual>,
  _Jv_JNI_CallAnyVoidMethod<nonvirtual>,
  _Jv_JNI_CallAnyVoidMethodV<nonvirtual>,
  _Jv_JNI_CallAnyVoidMethodA<nonvirtual>,

  _Jv_JNI_GetAnyFieldID<false>,
  _Jv_JNI_GetField<jobject>,
  _Jv_JNI_GetField<jboolean>,
  _Jv_JNI_GetField<jbyte>,
  _Jv_JNI_GetField<jchar>,
  _Jv_JNI_GetField<jshort>,
  _Jv_JNI_GetField<jint>,
  _Jv_JNI_GetField<jlong>,
  _Jv_JNI_GetField<jfloat>,
  _Jv_JNI_GetField<jdouble>,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_SetField,
  _Jv_JNI_GetAnyMethodID<true>,

  _Jv_JNI_CallStaticMethod<jobject>,
  _Jv_JNI_CallStaticMethodV<jobject>,
  _Jv_JNI_CallStaticMethodA<jobject>,
  _Jv_JNI_CallStaticMethod<jboolean>,
  _Jv_JNI_CallStaticMethodV<jboolean>,
  _Jv_JNI_CallStaticMethodA<jboolean>,
  _Jv_JNI_CallStaticMethod<jbyte>,
  _Jv_JNI_CallStaticMethodV<jbyte>,
  _Jv_JNI_CallStaticMethodA<jbyte>,
  _Jv_JNI_CallStaticMethod<jchar>,
  _Jv_JNI_CallStaticMethodV<jchar>,
  _Jv_JNI_CallStaticMethodA<jchar>,
  _Jv_JNI_CallStaticMethod<jshort>,
  _Jv_JNI_CallStaticMethodV<jshort>,
  _Jv_JNI_CallStaticMethodA<jshort>,
  _Jv_JNI_CallStaticMethod<jint>,
  _Jv_JNI_CallStaticMethodV<jint>,
  _Jv_JNI_CallStaticMethodA<jint>,
  _Jv_JNI_CallStaticMethod<jlong>,
  _Jv_JNI_CallStaticMethodV<jlong>,
  _Jv_JNI_CallStaticMethodA<jlong>,
  _Jv_JNI_CallStaticMethod<jfloat>,
  _Jv_JNI_CallStaticMethodV<jfloat>,
  _Jv_JNI_CallStaticMethodA<jfloat>,
  _Jv_JNI_CallStaticMethod<jdouble>,
  _Jv_JNI_CallStaticMethodV<jdouble>,
  _Jv_JNI_CallStaticMethodA<jdouble>,
  _Jv_JNI_CallStaticVoidMethod,
  _Jv_JNI_CallStaticVoidMethodV,
  _Jv_JNI_CallStaticVoidMethodA,

  _Jv_JNI_GetAnyFieldID<true>,
  _Jv_JNI_GetStaticField<jobject>,
  _Jv_JNI_GetStaticField<jboolean>,
  _Jv_JNI_GetStaticField<jbyte>,
  _Jv_JNI_GetStaticField<jchar>,
  _Jv_JNI_GetStaticField<jshort>,
  _Jv_JNI_GetStaticField<jint>,
  _Jv_JNI_GetStaticField<jlong>,
  _Jv_JNI_GetStaticField<jfloat>,
  _Jv_JNI_GetStaticField<jdouble>,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_SetStaticField,
  _Jv_JNI_NewString,
  _Jv_JNI_GetStringLength,
  _Jv_JNI_GetStringChars,
  _Jv_JNI_ReleaseStringChars,
  _Jv_JNI_NewStringUTF,
  _Jv_JNI_GetStringUTFLength,
  _Jv_JNI_GetStringUTFChars,
  _Jv_JNI_ReleaseStringUTFChars,
  _Jv_JNI_GetArrayLength,
  _Jv_JNI_NewObjectArray,
  _Jv_JNI_GetObjectArrayElement,
  _Jv_JNI_SetObjectArrayElement,
  _Jv_JNI_NewPrimitiveArray<jboolean, JvPrimClass (boolean)>,
  _Jv_JNI_NewPrimitiveArray<jbyte, JvPrimClass (byte)>,
  _Jv_JNI_NewPrimitiveArray<jchar, JvPrimClass (char)>,
  _Jv_JNI_NewPrimitiveArray<jshort, JvPrimClass (short)>,
  _Jv_JNI_NewPrimitiveArray<jint, JvPrimClass (int)>,
  _Jv_JNI_NewPrimitiveArray<jlong, JvPrimClass (long)>,
  _Jv_JNI_NewPrimitiveArray<jfloat, JvPrimClass (float)>,
  _Jv_JNI_NewPrimitiveArray<jdouble, JvPrimClass (double)>,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_ReleasePrimitiveArrayElements,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_GetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_SetPrimitiveArrayRegion,
  _Jv_JNI_RegisterNatives,
  _Jv_JNI_UnregisterNatives,
  _Jv_JNI_MonitorEnter,
  _Jv_JNI_MonitorExit,
  _Jv_JNI_GetJavaVM,

  _Jv_JNI_GetStringRegion,
  _Jv_JNI_GetStringUTFRegion,
  _Jv_JNI_GetPrimitiveArrayCritical,
  _Jv_JNI_ReleasePrimitiveArrayCritical,
  _Jv_JNI_GetStringCritical,
  _Jv_JNI_ReleaseStringCritical,

  NOT_IMPL /* newweakglobalref */,
  NOT_IMPL /* deleteweakglobalref */,

  _Jv_JNI_ExceptionCheck
};

struct JNIInvokeInterface _Jv_JNI_InvokeFunctions =
{
  RESERVED,
  RESERVED,
  RESERVED,

  _Jv_JNI_DestroyJavaVM,
  _Jv_JNI_AttachCurrentThread,
  _Jv_JNI_DetachCurrentThread,
  _Jv_JNI_GetEnv
};
