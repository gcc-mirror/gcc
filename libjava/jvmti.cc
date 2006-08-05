// jvmti.cc - JVMTI implementation

/* Copyright (C) 2006 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <jvm.h>
#include <java-threads.h>
#include <java-gc.h>
#include <jvmti.h>

#include <gcj/method.h>

#include <gnu/classpath/SystemProperties.h>
#include <gnu/gcj/runtime/BootClassLoader.h>
#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Thread.h>
#include <java/lang/Throwable.h>
#include <java/lang/VMClassLoader.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/reflect/Modifier.h>
#include <java/util/Collection.h>
#include <java/util/HashMap.h>
#include <java/net/URL.h>

extern struct JNINativeInterface _Jv_JNIFunctions;

struct _Jv_rawMonitorID
{
  _Jv_Mutex_t mutex;
  _Jv_ConditionVariable_t condition;
};

// Some commonly-used checks

#define THREAD_DEFAULT_TO_CURRENT(jthread)				\
  if (jthread == NULL) jthread = java::lang::Thread::currentThread ();

#define THREAD_CHECK_VALID(jthread)					\
  if (!java::lang::Thread::class$.isAssignableFrom (&(jthread->class$))) \
    return JVMTI_ERROR_INVALID_THREAD;

#define THREAD_CHECK_IS_ALIVE(thread)				\
  if (!thread->isAlive ()) return JVMTI_ERROR_THREAD_NOT_ALIVE;

// FIXME: if current phase is not set in Phases,
// return JVMTI_ERROR_WRONG_PHASE
#define REQUIRE_PHASE(Env, Phases)

#define NULL_CHECK(Ptr)					\
  if (Ptr == NULL) return JVMTI_ERROR_NULL_POINTER;

static jvmtiError JNICALL
_Jv_JVMTI_SuspendThread (MAYBE_UNUSED jvmtiEnv *env, jthread thread)
{
  using namespace java::lang;

  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);

  Thread *t = reinterpret_cast<Thread *> (thread);
  THREAD_CHECK_IS_ALIVE (t);

  _Jv_Thread_t *data = _Jv_ThreadGetData (t);
  _Jv_SuspendThread (data);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_ResumeThread (MAYBE_UNUSED jvmtiEnv *env, jthread thread)
{
  using namespace java::lang;

  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);

  Thread *t = reinterpret_cast<Thread *> (thread);
  THREAD_CHECK_IS_ALIVE (t);

  _Jv_Thread_t *data = _Jv_ThreadGetData (t);
  _Jv_ResumeThread (data);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_InterruptThread (MAYBE_UNUSED jvmtiEnv *env, jthread thread)
{
  using namespace java::lang;

  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
  // FIXME: capability handling?  'can_signal_thread'
  if (thread == NULL)
    return JVMTI_ERROR_INVALID_THREAD;
  THREAD_CHECK_VALID (thread);
  Thread *real_thread = reinterpret_cast<Thread *> (thread);
  THREAD_CHECK_IS_ALIVE (real_thread);
  real_thread->interrupt();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_CreateRawMonitor (MAYBE_UNUSED jvmtiEnv *env, const char *name,
			    jrawMonitorID *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD | JVMTI_PHASE_LIVE);
  NULL_CHECK (name);
  NULL_CHECK (result);
  *result = (jrawMonitorID) _Jv_Malloc (sizeof (_Jv_rawMonitorID));
  _Jv_MutexInit (&(*result)->mutex);
  _Jv_CondInit (&(*result)->condition);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_DestroyRawMonitor (MAYBE_UNUSED jvmtiEnv *env, jrawMonitorID monitor)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD | JVMTI_PHASE_LIVE);
  // Note we have no better way of knowing whether this object is
  // really a raw monitor.
  if (monitor == NULL)
    return JVMTI_ERROR_INVALID_MONITOR;
  // FIXME: perform checks on monitor, release it if this thread owns
  // it.
#ifdef _Jv_HaveMutexDestroy
  _Jv_MutexDestroy (&monitor->mutex);
#endif
  _Jv_Free (monitor);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_RawMonitorEnter (MAYBE_UNUSED jvmtiEnv *env, jrawMonitorID monitor)
{
  if (monitor == NULL)
    return JVMTI_ERROR_INVALID_MONITOR;
  _Jv_MutexLock (&monitor->mutex);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_RawMonitorExit (MAYBE_UNUSED jvmtiEnv *env, jrawMonitorID monitor)
{
  if (monitor == NULL)
    return JVMTI_ERROR_INVALID_MONITOR;
  if (_Jv_MutexUnlock (&monitor->mutex))
    return JVMTI_ERROR_NOT_MONITOR_OWNER;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_RawMonitorWait (MAYBE_UNUSED jvmtiEnv *env, jrawMonitorID monitor)
{
  if (monitor == NULL)
    return JVMTI_ERROR_INVALID_MONITOR;
  int r = _Jv_CondWait (&monitor->condition, &monitor->mutex, 0, 0);
  if (r == _JV_NOT_OWNER)
    return JVMTI_ERROR_NOT_MONITOR_OWNER;
  if (r == _JV_INTERRUPTED)
    return JVMTI_ERROR_INTERRUPT;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_RawMonitorNotify (MAYBE_UNUSED jvmtiEnv *env, jrawMonitorID monitor)
{
  if (monitor == NULL)
    return JVMTI_ERROR_INVALID_MONITOR;
  if (_Jv_CondNotify (&monitor->condition, &monitor->mutex) == _JV_NOT_OWNER)
    return JVMTI_ERROR_NOT_MONITOR_OWNER;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_RawMonitorNotifyAll (MAYBE_UNUSED jvmtiEnv *env,
			       jrawMonitorID monitor)
{
  if (monitor == NULL)
    return JVMTI_ERROR_INVALID_MONITOR;
  if (_Jv_CondNotifyAll (&monitor->condition, &monitor->mutex)
      == _JV_NOT_OWNER)
    return JVMTI_ERROR_NOT_MONITOR_OWNER;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_Allocate (MAYBE_UNUSED jvmtiEnv *env, jlong size,
		    unsigned char **result)
{
  if (size < 0)
    return JVMTI_ERROR_ILLEGAL_ARGUMENT;
  NULL_CHECK (result);
  if (size == 0)
    *result = NULL;
  else
    {
      *result = (unsigned char *) _Jv_MallocUnchecked (size);
      if (*result == NULL)
	return JVMTI_ERROR_OUT_OF_MEMORY;
    }
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_Deallocate (MAYBE_UNUSED jvmtiEnv *env, unsigned char *mem)
{
  if (mem != NULL)
    _Jv_Free (mem);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetClassModifiers (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
			     jint *mods)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  // Don't bother checking KLASS' type.
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;
  NULL_CHECK (mods);
  *mods = klass->getModifiers();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetClassMethods (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
			   jint *count_ptr, jmethodID **methods_ptr)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  // FIXME: capability can_maintain_original_method_order
  // Don't bother checking KLASS' type.
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;
  NULL_CHECK (count_ptr);
  NULL_CHECK (methods_ptr);
  *count_ptr = JvNumMethods(klass);

  *methods_ptr = (jmethodID *) _Jv_Malloc (*count_ptr * sizeof (jmethodID));
  jmethodID start = JvGetFirstMethod (klass);
  for (jint i = 0; i < *count_ptr; ++i)
    // FIXME: correct?
    (*methods_ptr)[i] = start + i;

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_IsInterface (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
		       jboolean *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;
  NULL_CHECK (result);
  *result = klass->isInterface();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_IsArrayClass (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
			jboolean *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;
  NULL_CHECK (result);
  *result = klass->isArray();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetClassLoader (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
			  jobject *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;
  NULL_CHECK (result);
  *result = klass->getClassLoaderInternal();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetObjectHashCode (MAYBE_UNUSED jvmtiEnv *env, jobject obj,
			     jint *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (obj == NULL)
    return JVMTI_ERROR_INVALID_OBJECT;
  NULL_CHECK (result);
  *result = _Jv_HashCode (obj);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetFieldModifiers (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
			     jfieldID field, jint *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;
  if (field == NULL)
    return JVMTI_ERROR_INVALID_FIELDID;
  NULL_CHECK (result);
  *result = field->getModifiers();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_IsFieldSynthetic (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
			    jfieldID field, jboolean *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;
  if (field == NULL)
    return JVMTI_ERROR_INVALID_FIELDID;
  NULL_CHECK (result);

  // FIXME: capability can_get_synthetic_attribute
  *result = ((field->getModifiers() & java::lang::reflect::Modifier::SYNTHETIC)
	     != 0);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetMethodModifiers (MAYBE_UNUSED jvmtiEnv *env, jmethodID method,
			      jint *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (method == NULL)
    return JVMTI_ERROR_INVALID_METHODID;
  NULL_CHECK (result);

  // FIXME: mask off some internal bits...
  *result = method->accflags;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_IsMethodNative (MAYBE_UNUSED jvmtiEnv *env, jmethodID method,
			  jboolean *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (method == NULL)
    return JVMTI_ERROR_INVALID_METHODID;
  NULL_CHECK (result);

  *result = ((method->accflags & java::lang::reflect::Modifier::NATIVE) != 0);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_IsMethodSynthetic (MAYBE_UNUSED jvmtiEnv *env, jmethodID method,
			     jboolean *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (method == NULL)
    return JVMTI_ERROR_INVALID_METHODID;
  NULL_CHECK (result);

  // FIXME capability can_get_synthetic_attribute

  *result = ((method->accflags & java::lang::reflect::Modifier::SYNTHETIC)
	     != 0);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetClassLoaderClasses (MAYBE_UNUSED jvmtiEnv *env,
				 jobject init_loader,
				 jint *count_ptr,
				 jclass **result_ptr)
{
  using namespace java::lang;
  using namespace java::util;

  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
  NULL_CHECK (count_ptr);
  NULL_CHECK (result_ptr);

  ClassLoader *loader = (ClassLoader *) init_loader;
  if (loader == NULL)
    loader = VMClassLoader::bootLoader;

  Collection *values = loader->loadedClasses->values();
  jobjectArray array = values->toArray();
  *count_ptr = array->length;
  jobject *elts = elements (array);
  jclass *result = (jclass *) _Jv_Malloc (*count_ptr * sizeof (jclass));
  // FIXME: JNI references...
  memcpy (result, elts, *count_ptr * sizeof (jclass));

  *result_ptr = result;

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_ForceGarbageCollection (MAYBE_UNUSED jvmtiEnv *env)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
  _Jv_RunGC();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetJNIFunctionTable (MAYBE_UNUSED jvmtiEnv *env,
			       const jniNativeInterface *function_table)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  NULL_CHECK (function_table);
  memcpy (&_Jv_JNIFunctions, function_table, sizeof (jniNativeInterface));
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetJNIFunctionTable (MAYBE_UNUSED jvmtiEnv *env,
			       jniNativeInterface **function_table)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  NULL_CHECK (function_table);
  *function_table
    = (jniNativeInterface *) _Jv_Malloc (sizeof (jniNativeInterface));
  memcpy (*function_table, &_Jv_JNIFunctions, sizeof (jniNativeInterface));
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_DisposeEnvironment (jvmtiEnv *env)
{
  // All we need to do is free memory allocated by _Jv_GetJVMTIEnv
  _Jv_Free (env);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetSystemProperty (MAYBE_UNUSED jvmtiEnv *env, const char *property,
			     char **result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD | JVMTI_PHASE_LIVE);
  NULL_CHECK (property);
  NULL_CHECK (result);

  jstring name = JvNewStringUTF(property);
  jstring result_str = gnu::classpath::SystemProperties::getProperty(name);

  if (result_str == NULL)
    return JVMTI_ERROR_NOT_AVAILABLE;

  int len = JvGetStringUTFLength (result_str);
  *result = (char *) _Jv_Malloc (len + 1);
  JvGetStringUTFRegion (result_str, 0, result_str->length(), *result);
  (*result)[len] = '\0';

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetSystemProperty (MAYBE_UNUSED jvmtiEnv *env, const char *property,
			     const char *value)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD);

  NULL_CHECK (property);
  if (value == NULL)
    {
      // FIXME: When would a property not be writeable?
      return JVMTI_ERROR_NONE;
    }

  jstring prop_str = JvNewStringUTF(property);
  jstring value_str = JvNewStringUTF(value);
  gnu::classpath::SystemProperties::setProperty(prop_str, value_str);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetTime (MAYBE_UNUSED jvmtiEnv *env, jlong *nanos_ptr)
{
  NULL_CHECK (nanos_ptr);
  *nanos_ptr = _Jv_platform_nanotime();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetAvailableProcessors (MAYBE_UNUSED jvmtiEnv *env,
				  jint *nprocessors_ptr)
{
  NULL_CHECK (nprocessors_ptr);
#ifdef _SC_NPROCESSORS_ONLN
  *nprocessors_ptr = sysconf(_SC_NPROCESSORS_ONLN);
#else
  *nprocessors_ptr = 1;
#endif
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_AddToBootstrapClassLoaderSearch (MAYBE_UNUSED jvmtiEnv *env,
					   const char *segment)
{
  using namespace java::lang;
  using namespace java::net;
  using namespace gnu::gcj::runtime;

  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD);
  NULL_CHECK (segment);

  jstring str_segment = JvNewStringUTF(segment);
  URL *url;
  try
    {
      url = new URL(JvNewStringUTF("file"), NULL, str_segment);
    }
  catch (jthrowable ignore)
    {
      return JVMTI_ERROR_ILLEGAL_ARGUMENT;
    }

  BootClassLoader *loader = VMClassLoader::bootLoader;
  // Don't call this too early.
  // assert (loader != NULL);
  loader->addURL(url);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetVerboseFlag (MAYBE_UNUSED jvmtiEnv *env, jvmtiVerboseFlag flag,
			  jboolean value)
{
  switch (flag)
    {
    case JVMTI_VERBOSE_OTHER:
    case JVMTI_VERBOSE_GC:
    case JVMTI_VERBOSE_JNI:
      // Ignore.
      break;
    case JVMTI_VERBOSE_CLASS:
      gcj::verbose_class_flag = value;
      break;
    default:
      return JVMTI_ERROR_ILLEGAL_ARGUMENT;
    }
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetObjectSize (MAYBE_UNUSED jvmtiEnv *env, jobject object,
			 jlong *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  if (object == NULL)
    return JVMTI_ERROR_INVALID_OBJECT;
  NULL_CHECK (result);

  jclass klass = object->getClass();
  if (klass->isArray())
    {
      jclass comp = klass->getComponentType();
      jint base
	= (jint) (_Jv_uintptr_t) _Jv_GetArrayElementFromElementType(NULL,
								    klass->getComponentType());
      // FIXME: correct for primitive types?
      jint compSize = comp->size();
      __JArray *array = (__JArray *) object;
      *result = base + array->length * compSize;
    }
  else
    {
      // Note that if OBJECT is a String then it may (if
      // str->data==str) take more space.  Do we care?
      *result = klass->size();
    }
  return JVMTI_ERROR_NONE;
}

#define RESERVED NULL
#define UNIMPLEMENTED NULL

struct _Jv_jvmtiEnv _Jv_JVMTI_Interface =
{
  RESERVED,			// reserved1
  UNIMPLEMENTED,		// SetEventNotification
  RESERVED,			// reserved3
  UNIMPLEMENTED,		// GetAllThreads
  _Jv_JVMTI_SuspendThread,	// SuspendThread
  _Jv_JVMTI_ResumeThread,	// ResumeThread
  UNIMPLEMENTED,		// StopThread
  _Jv_JVMTI_InterruptThread,	// InterruptThread
  UNIMPLEMENTED,		// GetThreadInfo
  UNIMPLEMENTED,		// GetOwnedMonitorInfo
  UNIMPLEMENTED,		// GetCurrentContendedMonitor
  UNIMPLEMENTED,		// RunAgentThread
  UNIMPLEMENTED,		// GetTopThreadGroups
  UNIMPLEMENTED,		// GetThreadGroupInfo
  UNIMPLEMENTED,		// GetThreadGroupChildren
  UNIMPLEMENTED,		// GetFrameCount
  UNIMPLEMENTED,		// GetThreadState
  RESERVED,			// reserved18
  UNIMPLEMENTED,		// GetFrameLocation
  UNIMPLEMENTED,		// NotifyPopFrame
  UNIMPLEMENTED,		// GetLocalObject
  UNIMPLEMENTED,		// GetLocalInt
  UNIMPLEMENTED,		// GetLocalLong
  UNIMPLEMENTED,		// GetLocalFloat
  UNIMPLEMENTED,		// GetLocalDouble
  UNIMPLEMENTED,		// SetLocalObject
  UNIMPLEMENTED,		// SetLocalInt
  UNIMPLEMENTED,		// SetLocalLong
  UNIMPLEMENTED,		// SetLocalFloat
  UNIMPLEMENTED,		// SetLocalDouble
  _Jv_JVMTI_CreateRawMonitor,	// CreateRawMonitor
  _Jv_JVMTI_DestroyRawMonitor,	// DestroyRawMonitor
  _Jv_JVMTI_RawMonitorEnter,	// RawMonitorEnter
  _Jv_JVMTI_RawMonitorExit,	// RawMonitorExit
  _Jv_JVMTI_RawMonitorWait,	// RawMonitorWait
  _Jv_JVMTI_RawMonitorNotify,	// RawMonitorNotify
  _Jv_JVMTI_RawMonitorNotifyAll, // RawMonitorNotifyAll
  UNIMPLEMENTED,		// SetBreakpoint
  UNIMPLEMENTED,		// ClearBreakpoint
  RESERVED,			// reserved40
  UNIMPLEMENTED,		// SetFieldAccessWatch
  UNIMPLEMENTED,		// ClearFieldAccessWatch
  UNIMPLEMENTED,		// SetFieldModificationWatch
  UNIMPLEMENTED,		// ClearFieldModificationWatch
  RESERVED,			// reserved45
  _Jv_JVMTI_Allocate,		// Allocate
  _Jv_JVMTI_Deallocate,		// Deallocate
  UNIMPLEMENTED,		// GetClassSignature
  UNIMPLEMENTED,		// GetClassStatus
  UNIMPLEMENTED,		// GetSourceFileName
  _Jv_JVMTI_GetClassModifiers,	// GetClassModifiers
  _Jv_JVMTI_GetClassMethods,	// GetClassMethods
  UNIMPLEMENTED,		// GetClassFields
  UNIMPLEMENTED,		// GetImplementedInterfaces
  _Jv_JVMTI_IsInterface,	// IsInterface
  _Jv_JVMTI_IsArrayClass,	// IsArrayClass
  _Jv_JVMTI_GetClassLoader,	// GetClassLoader
  _Jv_JVMTI_GetObjectHashCode,	// GetObjectHashCode
  UNIMPLEMENTED,		// GetObjectMonitorUsage
  UNIMPLEMENTED,		// GetFieldName
  UNIMPLEMENTED,		// GetFieldDeclaringClass
  _Jv_JVMTI_GetFieldModifiers,	// GetFieldModifiers
  _Jv_JVMTI_IsFieldSynthetic,	// IsFieldSynthetic
  UNIMPLEMENTED,		// GetMethodName
  UNIMPLEMENTED,		// GetMethodDeclaringClass
  _Jv_JVMTI_GetMethodModifiers,	// GetMethodModifers
  RESERVED,			// reserved67
  UNIMPLEMENTED,		// GetMaxLocals
  UNIMPLEMENTED,		// GetArgumentsSize
  UNIMPLEMENTED,		// GetLineNumberTable
  UNIMPLEMENTED,		// GetMethodLocation
  UNIMPLEMENTED,		// GetLocalVariableTable
  RESERVED,			// reserved73
  RESERVED,			// reserved74
  UNIMPLEMENTED,		// GetBytecodes
  _Jv_JVMTI_IsMethodNative,	// IsMethodNative
  _Jv_JVMTI_IsMethodSynthetic,	// IsMethodSynthetic
  UNIMPLEMENTED,		// GetLoadedClasses
  _Jv_JVMTI_GetClassLoaderClasses, // GetClassLoaderClasses
  UNIMPLEMENTED,		// PopFrame
  RESERVED,			// reserved81
  RESERVED,			// reserved82
  RESERVED,			// reserved83
  RESERVED,			// reserved84
  RESERVED,			// reserved85
  RESERVED,			// reserved86
  UNIMPLEMENTED,		// RedefineClasses
  UNIMPLEMENTED,		// GetVersionNumber
  UNIMPLEMENTED,		// GetCapabilities
  UNIMPLEMENTED,		// GetSourceDebugExtension
  UNIMPLEMENTED,		// IsMethodObsolete
  UNIMPLEMENTED,		// SuspendThreadList
  UNIMPLEMENTED,		// ResumeThreadList
  RESERVED,			// reserved94
  RESERVED,			// reserved95
  RESERVED,			// reserved96
  RESERVED,			// reserved97
  RESERVED,			// reserved98
  RESERVED,			// reserved99
  UNIMPLEMENTED,		// GetAllStackTraces
  UNIMPLEMENTED,		// GetThreadListStackTraces
  UNIMPLEMENTED,		// GetThreadLocalStorage
  UNIMPLEMENTED,		// SetThreadLocalStorage
  UNIMPLEMENTED,		// GetStackTrace
  RESERVED,			// reserved105
  UNIMPLEMENTED,		// GetTag
  UNIMPLEMENTED,		// SetTag
  _Jv_JVMTI_ForceGarbageCollection, // ForceGarbageCollection
  UNIMPLEMENTED,		// IterateOverObjectsReachable
  UNIMPLEMENTED,		// IterateOverReachableObjects
  UNIMPLEMENTED,		// IterateOverHeap
  UNIMPLEMENTED,		// IterateOverInstanceOfClass
  RESERVED,			// reserved113
  UNIMPLEMENTED,		// GetObjectsWithTags
  RESERVED,			// reserved115
  RESERVED,			// reserved116
  RESERVED,			// reserved117
  RESERVED,			// reserved118
  RESERVED,			// reserved119
  _Jv_JVMTI_SetJNIFunctionTable, // SetJNIFunctionTable
  _Jv_JVMTI_GetJNIFunctionTable, // GetJNIFunctionTable
  UNIMPLEMENTED,		// SetEventCallbacks
  UNIMPLEMENTED,		// GenerateEvents
  UNIMPLEMENTED,		// GetExtensionFunctions
  UNIMPLEMENTED,		// GetExtensionEvents
  UNIMPLEMENTED,		// SetExtensionEventCallback
  _Jv_JVMTI_DisposeEnvironment,	// DisposeEnvironment
  UNIMPLEMENTED,		// GetErrorName
  UNIMPLEMENTED,		// GetJLocationFormat
  UNIMPLEMENTED,		// GetSystemProperties
  _Jv_JVMTI_GetSystemProperty,	// GetSystemProperty
  _Jv_JVMTI_SetSystemProperty,	// SetSystemProperty
  UNIMPLEMENTED,		// GetPhase
  UNIMPLEMENTED,		// GetCurrentThreadCpuTimerInfo
  UNIMPLEMENTED,		// GetCurrentThreadCpuTime
  UNIMPLEMENTED,		// GetThreadCpuTimerInfo
  UNIMPLEMENTED,		// GetThreadCpuTime
  UNIMPLEMENTED,		// GetTimerInfo
  _Jv_JVMTI_GetTime,		// GetTime
  UNIMPLEMENTED,		// GetPotentialCapabilities
  RESERVED,			// reserved141
  UNIMPLEMENTED,		// AddCapabilities
  UNIMPLEMENTED,		// RelinquishCapabilities
  _Jv_JVMTI_GetAvailableProcessors, // GetAvailableProcessors
  RESERVED,			// reserved145
  RESERVED,			// reserved146
  UNIMPLEMENTED,		// GetEnvironmentLocalStorage
  UNIMPLEMENTED,		// SetEnvironmentLocalStorage
  _Jv_JVMTI_AddToBootstrapClassLoaderSearch, // AddToBootstrapClassLoaderSearch
  _Jv_JVMTI_SetVerboseFlag,	// SetVerboseFlag
  RESERVED,			// reserved151
  RESERVED,			// reserved152
  RESERVED,			// reserved153
  _Jv_JVMTI_GetObjectSize	// GetObjectSize
};

_Jv_JVMTIEnv *
_Jv_GetJVMTIEnv (void)
{
  _Jv_JVMTIEnv *env
    = (_Jv_JVMTIEnv *) _Jv_MallocUnchecked (sizeof (_Jv_JVMTIEnv));
  env->p = &_Jv_JVMTI_Interface;
  return env;
}
