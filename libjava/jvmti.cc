// jvmti.cc - JVMTI implementation

/* Copyright (C) 2006 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <jvm.h>
#include <java-threads.h>
#include <java-gc.h>
#include <jvmti.h>

#include <java/lang/Thread.h>

// Some commonly-used checks

#define THREAD_DEFAULT_TO_CURRENT(jthread)				\
  if (jthread == NULL) jthread = java::lang::Thread::currentThread ();

#define THREAD_CHECK_VALID(jthread)					\
  if (!java::lang::Thread::class$.isAssignableFrom (&(jthread->class$))) \
    return JVMTI_ERROR_INVALID_THREAD;

#define THREAD_CHECK_IS_ALIVE(thread)				\
  if (!thread->isAlive ()) return JVMTI_ERROR_THREAD_NOT_ALIVE;

static jvmtiError
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

static jvmtiError
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

#define RESERVED NULL
#define UNIMPLEMENTED NULL

static jvmtiError
_Jv_JVMTI_DisposeEnvironment (jvmtiEnv *env)
{
  // All we need to do is free memory allocated by _Jv_GetJVMTIEnv
  _Jv_Free (env);
  return JVMTI_ERROR_NONE;
}

struct _Jv_jvmtiEnv _Jv_JVMTI_Interface =
{
  RESERVED,			// reserved1
  UNIMPLEMENTED,		// SetEventNotification
  RESERVED,			// reserved3
  UNIMPLEMENTED,		// GetAllThreads
  _Jv_JVMTI_SuspendThread,	// SuspendThread
  _Jv_JVMTI_ResumeThread,	// ResumeThread
  UNIMPLEMENTED,		// StopThread
  UNIMPLEMENTED,		// InterruptThread
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
  UNIMPLEMENTED,		// CreateRawMonitor
  UNIMPLEMENTED,		// DestroyRawMonitor
  UNIMPLEMENTED,		// RawMonitorEnter
  UNIMPLEMENTED,		// RawMonitorExit
  UNIMPLEMENTED,		// RawMonitorWait
  UNIMPLEMENTED,		// RawMonitorNotify
  UNIMPLEMENTED,		// RawMonitorNotifyAll
  UNIMPLEMENTED,		// SetBreakpoint
  UNIMPLEMENTED,		// CleareBreakpoint
  RESERVED,			// reserved40
  UNIMPLEMENTED,		// SetFieldAccessWatch
  UNIMPLEMENTED,		// ClearFieldAccessWatch
  UNIMPLEMENTED,		// SetFieldModificationWatch
  UNIMPLEMENTED,		// ClearFieldModificationWatch
  RESERVED,			// reserved45
  UNIMPLEMENTED,		// Allocate
  UNIMPLEMENTED,		// Deallocate
  UNIMPLEMENTED,		// GetClassSignature
  UNIMPLEMENTED,		// GetClassStatus
  UNIMPLEMENTED,		// GetSourceFileName
  UNIMPLEMENTED,		// GetClassModifiers
  UNIMPLEMENTED,		// GetClassMethods
  UNIMPLEMENTED,		// GetClassFields
  UNIMPLEMENTED,		// GetImplementedInterfaces
  UNIMPLEMENTED,		// IsInterface
  UNIMPLEMENTED,		// IsArrayClass
  UNIMPLEMENTED,		// GetClassLoader
  UNIMPLEMENTED,		// GetObjectHashCode
  UNIMPLEMENTED,		// GetObjectMonitorUsage
  UNIMPLEMENTED,		// GetFieldName
  UNIMPLEMENTED,		// GetFieldDeclaringClass
  UNIMPLEMENTED,		// GetFieldModifiers
  UNIMPLEMENTED,		// IsFieldSynthetic
  UNIMPLEMENTED,		// GetMethodName
  UNIMPLEMENTED,		// GetMethodDeclaringClass
  UNIMPLEMENTED,		// GetMethodModiifers
  RESERVED,			// reserved67
  UNIMPLEMENTED,		// GetMaxLocals
  UNIMPLEMENTED,		// GetArgumentsSize
  UNIMPLEMENTED,		// GetLineNumberTable
  UNIMPLEMENTED,		// GetMethodLocation
  UNIMPLEMENTED,		// GetLocalVariableTable
  RESERVED,			// reserved73
  RESERVED,			// reserved74
  UNIMPLEMENTED,		// GetBytecodes
  UNIMPLEMENTED,		// IsMethodNative
  UNIMPLEMENTED,		// IsMethodSynthetic
  UNIMPLEMENTED,		// GetLoadedClasses
  UNIMPLEMENTED,		// GetClassLoaderClasses
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
  UNIMPLEMENTED,		// ForceGarbageCollection
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
  UNIMPLEMENTED,		// SetJNIFunctionTable
  UNIMPLEMENTED,		// GetJNIFunctionTable
  UNIMPLEMENTED,		// SetEventCallbacks
  UNIMPLEMENTED,		// GenerateEvents
  UNIMPLEMENTED,		// GetExtensionFunctions
  UNIMPLEMENTED,		// GetExtensionEvents
  UNIMPLEMENTED,		// SetExtensionEventCallback
  _Jv_JVMTI_DisposeEnvironment,	// DisposeEnvironment
  UNIMPLEMENTED,		// GetErrorName
  UNIMPLEMENTED,		// GetJLocationFormat
  UNIMPLEMENTED,		// GetSystemProperties
  UNIMPLEMENTED,		// GetSystemProperty
  UNIMPLEMENTED,		// SetSystemProperty
  UNIMPLEMENTED,		// GetPhase
  UNIMPLEMENTED,		// GetCurrentThreadCpuTimerInfo
  UNIMPLEMENTED,		// GetCurrentThreadCpuTime
  UNIMPLEMENTED,		// GetThreadCpuTimerInfo
  UNIMPLEMENTED,		// GetThreadCpuTime
  UNIMPLEMENTED,		// GetTimerInfo
  UNIMPLEMENTED,		// GetTime
  UNIMPLEMENTED,		// GetPotentialCapabilities
  RESERVED,			// reserved141
  UNIMPLEMENTED,		// AddCapabilities
  UNIMPLEMENTED,		// RelinquishCapabilities
  UNIMPLEMENTED,		// GetAvailableProcessors
  RESERVED,			// reserved145
  RESERVED,			// reserved146
  UNIMPLEMENTED,		// GetEnvironmentLocalStorage
  UNIMPLEMENTED,		// SetEnvironmentLocalStorage
  UNIMPLEMENTED,		// AddToBootstrapClassLoaderSearch
  UNIMPLEMENTED,		// SetVerboseFlag
  RESERVED,			// reserved151
  RESERVED,			// reserved152
  RESERVED,			// reserved153
  UNIMPLEMENTED			// GetObjectSize
};

_Jv_JVMTIEnv *
_Jv_GetJVMTIEnv (void)
{
  _Jv_JVMTIEnv *env
    = (_Jv_JVMTIEnv *) _Jv_MallocUnchecked (sizeof (_Jv_JVMTIEnv));
  env->p = &_Jv_JVMTI_Interface;
  return env;
}
