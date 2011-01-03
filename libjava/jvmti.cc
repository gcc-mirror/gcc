// jvmti.cc - JVMTI implementation

/* Copyright (C) 2006, 2007, 2010 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <jvm.h>
#include <java-threads.h>
#include <java-gc.h>
#include <java-interp.h>
#include <jvmti.h>
#include "jvmti-int.h"

#include <gcj/method.h>

#include <gnu/classpath/SystemProperties.h>
#include <gnu/gcj/runtime/BootClassLoader.h>
#include <gnu/gcj/jvmti/Breakpoint.h>
#include <gnu/gcj/jvmti/BreakpointManager.h>

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/OutOfMemoryError.h>
#include <java/lang/Thread.h>
#include <java/lang/ThreadGroup.h>
#include <java/lang/Thread$State.h>
#include <java/lang/Throwable.h>
#include <java/lang/VMClassLoader.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/reflect/Modifier.h>
#include <java/util/Collection.h>
#include <java/util/HashMap.h>
#include <java/util/concurrent/locks/Lock.h>
#include <java/util/concurrent/locks/ReentrantReadWriteLock.h>
#include <java/net/URL.h>

static void check_enabled_events (void);
static void check_enabled_event (jvmtiEvent);

namespace JVMTI
{
  // Is JVMTI enabled? (i.e., any jvmtiEnv created?)
  bool enabled;

  // Event notifications
  bool VMInit = false;
  bool VMDeath = false;
  bool ThreadStart = false;
  bool ThreadEnd = false;
  bool ClassFileLoadHook = false;
  bool ClassLoad = false;
  bool ClassPrepare = false;
  bool VMStart = false;
  bool Exception = false;
  bool ExceptionCatch = false;
  bool SingleStep = false;
  bool FramePop = false;
  bool Breakpoint = false;
  bool FieldAccess = false;
  bool FieldModification = false;
  bool MethodEntry = false;
  bool MethodExit = false;
  bool NativeMethodBind = false;
  bool CompiledMethodLoad = false;
  bool CompiledMethodUnload = false;
  bool DynamicCodeGenerated = false;
  bool DataDumpRequest = false;
  bool reserved72 = false;
  bool MonitorWait = false;
  bool MonitorWaited = false;
  bool MonitorContendedEnter = false;
  bool MonitorContendedEntered = false;
  bool reserved77 = false;
  bool reserved78 = false;
  bool reserved79 = false;
  bool reserved80 = false;
  bool GarbageCollectionStart = false;
  bool GarbageCollectionFinish = false;
  bool ObjectFree = false;
  bool VMObjectAlloc = false;
};

extern struct JNINativeInterface _Jv_JNIFunctions;

struct _Jv_rawMonitorID
{
  _Jv_Mutex_t mutex;
  _Jv_ConditionVariable_t condition;
};

/* A simple linked list of all JVMTI environments. Since
   events must be delivered to environments in the order
   in which the environments were created, new environments
   are added to the end of the list. */
struct jvmti_env_list
{
  jvmtiEnv *env;
  struct jvmti_env_list *next;
};
static struct jvmti_env_list *_jvmtiEnvironments = NULL;
static java::util::concurrent::locks::
ReentrantReadWriteLock *_envListLock = NULL;
#define FOREACH_ENVIRONMENT(Ele) \
  for (Ele = _jvmtiEnvironments; Ele != NULL; Ele = Ele->next)

// Some commonly-used checks

#define THREAD_DEFAULT_TO_CURRENT(Ajthread)		\
  do							\
    {							\
      if (Ajthread == NULL)				\
	Ajthread = java::lang::Thread::currentThread ();	\
    }							\
  while (0)

#define THREAD_CHECK_VALID(Athread)					\
  do									\
    {									\
      if (!java::lang::Thread::class$.isAssignableFrom (&(Athread->class$))) \
	return JVMTI_ERROR_INVALID_THREAD;				\
    }									\
  while (0)

#define THREAD_CHECK_IS_ALIVE(Athread)	     \
  do					     \
    {					     \
      if (!Athread->isAlive ())		     \
	return JVMTI_ERROR_THREAD_NOT_ALIVE; \
    }					     \
  while (0)

// FIXME: if current phase is not set in Phases,
// return JVMTI_ERROR_WRONG_PHASE
#define REQUIRE_PHASE(Env, Phases)

#define NULL_CHECK(Ptr)				\
  do						\
    {						\
      if (Ptr == NULL)				\
	return JVMTI_ERROR_NULL_POINTER;	\
    }						\
  while (0)

#define ILLEGAL_ARGUMENT(Cond)			\
  do						\
    {						\
      if ((Cond))				\
	return JVMTI_ERROR_ILLEGAL_ARGUMENT;	\
    }						\
  while (0)

#define CHECK_FOR_NATIVE_METHOD(AjmethodID)	\
  do					\
    {					\
      jboolean is_native;		\
      jvmtiError jerr = env->IsMethodNative (AjmethodID, &is_native);	\
      if (jerr != JVMTI_ERROR_NONE)					\
        return jerr;							\
      if (is_native)							\
        return JVMTI_ERROR_NATIVE_METHOD;			        \
    }									\
  while (0)

static jvmtiError JNICALL
_Jv_JVMTI_SuspendThread (MAYBE_UNUSED jvmtiEnv *env, jthread thread)
{
  using namespace java::lang;

  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);
  THREAD_CHECK_IS_ALIVE (thread);

  _Jv_Thread_t *data = _Jv_ThreadGetData (thread);
  _Jv_SuspendThread (data);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_ResumeThread (MAYBE_UNUSED jvmtiEnv *env, jthread thread)
{
  using namespace java::lang;

  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);
  THREAD_CHECK_IS_ALIVE (thread);

  _Jv_Thread_t *data = _Jv_ThreadGetData (thread);
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
  THREAD_CHECK_IS_ALIVE (thread);
  thread->interrupt();
  return JVMTI_ERROR_NONE;
}

// This method performs the common tasks to get and set variables of all types.
// It is called by the _Jv_JVMTI_Get/SetLocalInt/Object/.... methods.
static jvmtiError
getLocalFrame (jvmtiEnv *env, jthread thread, jint depth, jint slot, char type,
               _Jv_InterpFrame **iframe)
{
  using namespace java::lang;
   
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
   
  ILLEGAL_ARGUMENT (depth < 0);
  
  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);
  THREAD_CHECK_IS_ALIVE (thread);
  
  _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (thread->frame);
  
  for (int i = 0; i < depth; i++)
    {    
      frame = frame->next;
    
      if (frame == NULL)
        return JVMTI_ERROR_NO_MORE_FRAMES; 
    }
  
  if (frame->frame_type == frame_native)
    return JVMTI_ERROR_OPAQUE_FRAME;
  
  jint max_locals;
  jvmtiError jerr = env->GetMaxLocals (reinterpret_cast<jmethodID> 
                                         (frame->self->get_method ()),
                                       &max_locals);
  if (jerr != JVMTI_ERROR_NONE)
    return jerr; 
  
  _Jv_InterpFrame *tmp_iframe = reinterpret_cast<_Jv_InterpFrame *> (frame);
  
  // The second slot taken up by a long type is marked as type 'x' meaning it
  // is not valid for access since it holds only the 4 low bytes of the value.
  if (tmp_iframe->locals_type[slot] == 'x')
    return JVMTI_ERROR_INVALID_SLOT;
  
  if (tmp_iframe->locals_type[slot] != type)
    return JVMTI_ERROR_TYPE_MISMATCH;
  
  // Check for invalid slots, if the type is a long type, we must check that
  // the next slot is valid as well.
  if (slot < 0 || slot >= max_locals 
      || ((type == 'l' || type == 'd') && slot + 1 >= max_locals))
    return JVMTI_ERROR_INVALID_SLOT;
  
  *iframe = tmp_iframe;
  
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetLocalObject (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                          jobject *value)
{
  NULL_CHECK (value);

  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'o', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  *value = frame->locals[slot].o;
  
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetLocalObject (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                          jobject value)
{
  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'o', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  frame->locals[slot].o = value;

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetLocalInt (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                       jint *value)
{
  NULL_CHECK (value);
  
  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'i', &frame);

  if (jerr != JVMTI_ERROR_NONE)
    return jerr;

  *value = frame->locals[slot].i;

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetLocalInt (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                       jint value)
{
  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'i', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  frame->locals[slot].i = value;

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetLocalLong (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                        jlong *value)
{
  NULL_CHECK (value);

  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'l', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;

#if SIZEOF_VOID_P==8
  *value = frame->locals[slot].l;
#else
  _Jv_word2 val;
  val.ia[0] = frame->locals[slot].ia[0];
  val.ia[1] = frame->locals[slot + 1].ia[0];
  *value = val.l;
#endif
  
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetLocalLong (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                        jlong value)
{
  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'l', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;

#if SIZEOF_VOID_P==8
  frame->locals[slot].l = value;
#else
  _Jv_word2 val;
	val.l = value;
	frame->locals[slot].ia[0] = val.ia[0];
	frame->locals[slot + 1].ia[0] = val.ia[1];
#endif

  return JVMTI_ERROR_NONE;
}


static jvmtiError JNICALL
_Jv_JVMTI_GetLocalFloat (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                         jfloat *value)
{
  NULL_CHECK (value);

  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'f', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  *value = frame->locals[slot].f;

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetLocalFloat (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                         jfloat value)
{
  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'f', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  frame->locals[slot].f = value;

  return JVMTI_ERROR_NONE;
}


static jvmtiError JNICALL
_Jv_JVMTI_GetLocalDouble (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                          jdouble *value)
{
  NULL_CHECK (value);

  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'd', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
#if SIZEOF_VOID_P==8
  *value = frame->locals[slot].d;
#else
  _Jv_word2 val;
  val.ia[0] = frame->locals[slot].ia[0];
  val.ia[1] = frame->locals[slot + 1].ia[0];
  *value = val.d;
#endif

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetLocalDouble (jvmtiEnv *env, jthread thread, jint depth, jint slot,
                          jdouble value)
{
  _Jv_InterpFrame *frame;
  jvmtiError jerr = getLocalFrame (env, thread, depth, slot, 'd', &frame);
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
    
#if SIZEOF_VOID_P==8
  frame->locals[slot].d = value;
#else
  _Jv_word2 val;
  val.d = value;
  frame->locals[slot].ia[0] = val.ia[0];
  frame->locals[slot + 1].ia[0] = val.ia[1]; 
#endif

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetAllThreads(MAYBE_UNUSED jvmtiEnv *env, jint *thread_cnt,
                        jthread **threads)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
  NULL_CHECK (thread_cnt);
  NULL_CHECK (threads);
   
  using namespace java::lang;
   
  ThreadGroup *root_grp = ThreadGroup::root;
  jint estimate = root_grp->activeCount ();

  JArray<Thread *> *thr_arr;

  // Allocate some extra space since threads can be created between calls
  try
    { 
      thr_arr = reinterpret_cast<JArray<Thread *> *> (JvNewObjectArray 
						      ((estimate * 2),
						       &Thread::class$, NULL));
    }
  catch (java::lang::OutOfMemoryError *err)
    {
      return JVMTI_ERROR_OUT_OF_MEMORY;
    }
    
  *thread_cnt = root_grp->enumerate (thr_arr);
   
  jvmtiError jerr = env->Allocate ((jlong) ((*thread_cnt) * sizeof (jthread)),
                                   (unsigned char **) threads);
 
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
   
  // Transfer the threads to the result array
  jthread *tmp_arr = reinterpret_cast<jthread *> (elements (thr_arr));
 
  memcpy ((*threads), tmp_arr, (*thread_cnt));
   
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetFrameCount (MAYBE_UNUSED jvmtiEnv *env, jthread thread,
                         jint *frame_count)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
  
  NULL_CHECK (frame_count);
	
  using namespace java::lang;
  
  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);
  THREAD_CHECK_IS_ALIVE (thread);
   
  _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (thread->frame);
  (*frame_count) = frame->depth ();
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetThreadState (MAYBE_UNUSED jvmtiEnv *env, jthread thread,
			  jint *thread_state_ptr)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);

  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);
  NULL_CHECK (thread_state_ptr);

  jint state = 0;
  if (thread->isAlive ())
    {
      state |= JVMTI_THREAD_STATE_ALIVE;

      _Jv_Thread_t *data = _Jv_ThreadGetData (thread);
      if (_Jv_IsThreadSuspended (data))
	state |= JVMTI_THREAD_STATE_SUSPENDED;

      if (thread->isInterrupted ())
	state |= JVMTI_THREAD_STATE_INTERRUPTED;

      _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (thread->frame);
      if (frame != NULL && frame->frame_type == frame_native)
	state |= JVMTI_THREAD_STATE_IN_NATIVE;

      using namespace java::lang;
      Thread$State *ts = thread->getState ();
      if (ts == Thread$State::RUNNABLE)
	state |= JVMTI_THREAD_STATE_RUNNABLE;
      else if (ts == Thread$State::BLOCKED)
	state |= JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER;
      else if (ts == Thread$State::TIMED_WAITING
	       || ts == Thread$State::WAITING)
	{
	  state |= JVMTI_THREAD_STATE_WAITING;
	  state |= ((ts == Thread$State::WAITING)
		    ? JVMTI_THREAD_STATE_WAITING_INDEFINITELY
		    : JVMTI_THREAD_STATE_WAITING_WITH_TIMEOUT);

	  /* FIXME: We don't have a way to tell
	     the caller why the thread is suspended,
	     i.e., JVMTI_THREAD_STATE_SLEEPING,
	     JVMTI_THREAD_STATE_PARKED, and
	     JVMTI_THREAD_STATE_IN_OBJECT_WAIT
	     are never set. */
	}
    }
  else
    {
      using namespace java::lang;
      Thread$State *ts = thread->getState ();
      if (ts == Thread$State::TERMINATED)
	state |= JVMTI_THREAD_STATE_TERMINATED;
    }

  *thread_state_ptr = state;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_CreateRawMonitor (MAYBE_UNUSED jvmtiEnv *env, const char *name,
			    jrawMonitorID *result)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD | JVMTI_PHASE_LIVE);
  NULL_CHECK (name);
  NULL_CHECK (result);
  *result = (jrawMonitorID) _Jv_MallocUnchecked (sizeof (_Jv_rawMonitorID));
  if (*result == NULL)
    return JVMTI_ERROR_OUT_OF_MEMORY;
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
_Jv_JVMTI_RawMonitorWait (MAYBE_UNUSED jvmtiEnv *env, jrawMonitorID monitor,
			  jlong millis)
{
  if (monitor == NULL)
    return JVMTI_ERROR_INVALID_MONITOR;
  int r = _Jv_CondWait (&monitor->condition, &monitor->mutex, millis, 0);
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
_Jv_JVMTI_SetBreakpoint (jvmtiEnv *env, jmethodID method, jlocation location)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);

  using namespace gnu::gcj::jvmti;
  Breakpoint *bp
    = BreakpointManager::getBreakpoint (reinterpret_cast<jlong> (method),
					location);
  if (bp == NULL)
    {
      jclass klass;
      jvmtiError err = env->GetMethodDeclaringClass (method, &klass);
      if (err != JVMTI_ERROR_NONE)
	return err;

      if (!_Jv_IsInterpretedClass (klass))
	return JVMTI_ERROR_INVALID_CLASS;

      _Jv_MethodBase *base = _Jv_FindInterpreterMethod (klass, method);
      if (base == NULL)
	return JVMTI_ERROR_INVALID_METHODID;

      jint flags;
      err = env->GetMethodModifiers (method, &flags);
      if (err != JVMTI_ERROR_NONE)
	return err;

      if (flags & java::lang::reflect::Modifier::NATIVE)
	return JVMTI_ERROR_NATIVE_METHOD;

      _Jv_InterpMethod *imeth = reinterpret_cast<_Jv_InterpMethod *> (base);
      if (imeth->get_insn (location) == NULL)
	return JVMTI_ERROR_INVALID_LOCATION;

      // Now the breakpoint can be safely installed
      bp = BreakpointManager::newBreakpoint (reinterpret_cast<jlong> (method),
					     location);
    }
  else
    {
      // Duplicate breakpoints are not permitted by JVMTI
      return JVMTI_ERROR_DUPLICATE;
    }

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_ClearBreakpoint (MAYBE_UNUSED jvmtiEnv *env, jmethodID method,
			   jlocation location)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);

  using namespace gnu::gcj::jvmti;

  Breakpoint *bp
    = BreakpointManager::getBreakpoint (reinterpret_cast<jlong> (method),
					location);
  if (bp == NULL)
    return JVMTI_ERROR_NOT_FOUND;

  BreakpointManager::deleteBreakpoint (reinterpret_cast<jlong> (method), location);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_Allocate (MAYBE_UNUSED jvmtiEnv *env, jlong size,
		    unsigned char **result)
{
  ILLEGAL_ARGUMENT (size < 0);
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
_Jv_JVMTI_GetClassStatus (MAYBE_UNUSED jvmtiEnv *env, jclass klass,
			  jint *status_ptr)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  NULL_CHECK (status_ptr);
  if (klass == NULL)
    return JVMTI_ERROR_INVALID_CLASS;

  if (klass->isArray ())
    *status_ptr = JVMTI_CLASS_STATUS_ARRAY;
  else if (klass->isPrimitive ())
    *status_ptr  = JVMTI_CLASS_STATUS_PRIMITIVE;
  else
    {
      jbyte state = _Jv_GetClassState (klass);
      *status_ptr = 0;
      if (state >= JV_STATE_LINKED)
	(*status_ptr) |= JVMTI_CLASS_STATUS_VERIFIED;
      if (state >= JV_STATE_PREPARED)
	(*status_ptr) |= JVMTI_CLASS_STATUS_PREPARED;
      if (state == JV_STATE_ERROR || state == JV_STATE_PHANTOM)
	(*status_ptr) |= JVMTI_CLASS_STATUS_ERROR;
      else if (state == JV_STATE_DONE)
	(*status_ptr) |= JVMTI_CLASS_STATUS_INITIALIZED;
    }

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

  *methods_ptr
    = (jmethodID *) _Jv_MallocUnchecked (*count_ptr * sizeof (jmethodID));
  if (*methods_ptr == NULL)
    return JVMTI_ERROR_OUT_OF_MEMORY;

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
_Jv_JVMTI_GetMethodName (MAYBE_UNUSED jvmtiEnv *env, jmethodID method,
			 char **name_ptr, char **signature_ptr,
			 char **generic_ptr)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);

  if (method == NULL)
    return JVMTI_ERROR_INVALID_METHODID;

  if (name_ptr != NULL)
    {
      int len = static_cast<int> (method->name->len ());
      *name_ptr = (char *) _Jv_MallocUnchecked (len + 1);
      if (*name_ptr == NULL)
	return JVMTI_ERROR_OUT_OF_MEMORY;
      strncpy (*name_ptr, method->name->chars (), len);
      (*name_ptr)[len] = '\0';
    }

  if (signature_ptr != NULL)
    {
      int len = static_cast<int> (method->signature->len ());
      *signature_ptr = (char *) _Jv_MallocUnchecked (len + 1);
      if (*signature_ptr == NULL)
	{
	  if (name_ptr != NULL)
	    _Jv_Free (*name_ptr);
	  return JVMTI_ERROR_OUT_OF_MEMORY;
	}
      strncpy (*signature_ptr, method->signature->chars (), len);
      (*signature_ptr)[len] = '\0';
    }

  if (generic_ptr != NULL)
    {
      *generic_ptr = NULL;
    }

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
_Jv_JVMTI_GetLineNumberTable (jvmtiEnv *env, jmethodID method,
			      jint *entry_count_ptr,
			      jvmtiLineNumberEntry **table_ptr)
{
  NULL_CHECK (entry_count_ptr);
  NULL_CHECK (table_ptr);

  jclass klass;
  jvmtiError jerr = env->GetMethodDeclaringClass (method, &klass);
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;

  _Jv_MethodBase *base = _Jv_FindInterpreterMethod (klass, method);
  if (base == NULL)
    return JVMTI_ERROR_INVALID_METHODID;

  if (java::lang::reflect::Modifier::isNative (method->accflags)
      || !_Jv_IsInterpretedClass (klass))
    return JVMTI_ERROR_NATIVE_METHOD;

  _Jv_InterpMethod *imeth = reinterpret_cast<_Jv_InterpMethod *> (base);
  jlong start, end;
  jintArray lines = NULL;
  jlongArray indices = NULL;
  imeth->get_line_table (start, end, lines, indices);
  if (lines == NULL)
    return JVMTI_ERROR_ABSENT_INFORMATION;

  jvmtiLineNumberEntry *table;
  jsize len = lines->length * sizeof (jvmtiLineNumberEntry);
  table = (jvmtiLineNumberEntry *) _Jv_MallocUnchecked (len);
  if (table == NULL)
    return JVMTI_ERROR_OUT_OF_MEMORY;
  
  jint *line = elements (lines);
  jlong *index = elements (indices);
  for (int i = 0; i < lines->length; ++i)
    {
      table[i].start_location = index[i];
      table[i].line_number = line[i];
    }

  *table_ptr = table;
  *entry_count_ptr = lines->length;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetLocalVariableTable (MAYBE_UNUSED jvmtiEnv *env, jmethodID method,
                                 jint *num_locals,
                                 jvmtiLocalVariableEntry **locals)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
  NULL_CHECK (num_locals);
  NULL_CHECK (locals);
  
  CHECK_FOR_NATIVE_METHOD(method);
  
  jclass klass;
  jvmtiError jerr = env->GetMethodDeclaringClass (method, &klass);
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;

  _Jv_InterpMethod *imeth = reinterpret_cast<_Jv_InterpMethod *> 
                              (_Jv_FindInterpreterMethod (klass, method));
  
  if (imeth == NULL)
    return JVMTI_ERROR_INVALID_METHODID;
  
  jerr = env->GetMaxLocals (method, num_locals);
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  jerr = env->Allocate (static_cast<jlong> 
                          ((*num_locals) * sizeof (jvmtiLocalVariableEntry)),
                        reinterpret_cast<unsigned char **> (locals));
  
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  //the slot in the methods local_var_table to get
  int table_slot = 0;
  char *name;
  char *sig;
  char *generic_sig;
  
  while (table_slot < *num_locals 
         && imeth->get_local_var_table (&name, &sig, &generic_sig,
                                 &((((*locals)[table_slot].start_location))),
                                 &((*locals)[table_slot].length), 
                                 &((*locals)[table_slot].slot),
                                 table_slot) 
            >= 0)
    {
      char **str_ptr = &(*locals)[table_slot].name;
      jerr = env->Allocate (static_cast<jlong> (strlen (name) + 1),
                             reinterpret_cast<unsigned char **> (str_ptr));
      if (jerr != JVMTI_ERROR_NONE)
        return jerr;
      strcpy ((*locals)[table_slot].name, name);
      
      str_ptr = &(*locals)[table_slot].signature;
      jerr = env->Allocate (static_cast<jlong> (strlen (sig) + 1),
                               reinterpret_cast<unsigned char **> (str_ptr));
      if (jerr != JVMTI_ERROR_NONE)
        return jerr;
      strcpy ((*locals)[table_slot].signature, sig);
      
      str_ptr = &(*locals)[table_slot].generic_signature;
      jerr = env->Allocate (static_cast<jlong> (strlen (generic_sig) + 1),
                               reinterpret_cast<unsigned char **> (str_ptr));
      if (jerr != JVMTI_ERROR_NONE)
        return jerr;
      strcpy ((*locals)[table_slot].generic_signature, generic_sig);
      
      table_slot++;
    }

  if (table_slot == 0)
    return JVMTI_ERROR_ABSENT_INFORMATION;
  
  // If there are double or long variables in the table, the the table will be
  // smaller than the max number of slots, so correct for this here.
  if ((table_slot) < *num_locals)
    *num_locals = table_slot;
  
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
_Jv_JVMTI_GetMaxLocals (jvmtiEnv *env, jmethodID method, jint *max_locals)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  NULL_CHECK (max_locals);
  
  CHECK_FOR_NATIVE_METHOD (method);
  
  jclass klass;
  jvmtiError jerr = env->GetMethodDeclaringClass (method, &klass);
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;

  _Jv_InterpMethod *imeth = reinterpret_cast<_Jv_InterpMethod *> 
                              (_Jv_FindInterpreterMethod (klass, method));
    
  if (imeth == NULL)
    return JVMTI_ERROR_INVALID_METHODID;
  
  *max_locals = imeth->get_max_locals ();
  
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetArgumentsSize (jvmtiEnv *env, jmethodID method, jint *size)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_START | JVMTI_PHASE_LIVE);
  NULL_CHECK (size);
  
  CHECK_FOR_NATIVE_METHOD (method);
  
  jvmtiError jerr;
  char *sig;
  jint num_slots = 0;
  
  jerr = env->GetMethodName (method, NULL, &sig, NULL);
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  // If the method is non-static add a slot for the "this" pointer.
  if ((method->accflags & java::lang::reflect::Modifier::STATIC) == 0)
    num_slots++;
  
  for (int i = 0; sig[i] != ')'; i++)
    {
      if (sig[i] == 'Z' || sig[i] == 'B' || sig[i] == 'C' || sig[i] == 'S'
          || sig[i] == 'I' || sig[i] == 'F')
        num_slots++;
      else if (sig[i] == 'J' || sig[i] == 'D')
        {
          // If this is an array of wide types it uses a single slot
          if (i > 0 && sig[i - 1] == '[')
            num_slots++;
          else
            num_slots += 2;
        }
      else if (sig[i] == 'L')
        {
          num_slots++;
          while (sig[i] != ';')
            i++;
        }
    }
  
  *size = num_slots;
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetMethodDeclaringClass (MAYBE_UNUSED jvmtiEnv *env,
				   jmethodID method,
				   jclass *declaring_class_ptr)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);
  NULL_CHECK (declaring_class_ptr);

  jclass klass = _Jv_GetMethodDeclaringClass (method);
  if (klass != NULL)
    {
      *declaring_class_ptr = klass;
      return JVMTI_ERROR_NONE;
    }

  return JVMTI_ERROR_INVALID_METHODID;
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
  jclass *result
    = (jclass *) _Jv_MallocUnchecked (*count_ptr * sizeof (jclass));
  if (result == NULL)
    return JVMTI_ERROR_OUT_OF_MEMORY;

  // FIXME: JNI references...
  memcpy (result, elts, *count_ptr * sizeof (jclass));

  *result_ptr = result;

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetStackTrace (MAYBE_UNUSED jvmtiEnv *env, jthread thread,
                         jint start_depth, jint max_frames,
                         jvmtiFrameInfo *frames, jint *frame_count)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_LIVE);

  ILLEGAL_ARGUMENT (max_frames < 0);
  
  NULL_CHECK (frames);
  NULL_CHECK (frame_count);
	
  using namespace java::lang;
  
  THREAD_DEFAULT_TO_CURRENT (thread);
  THREAD_CHECK_VALID (thread);
  THREAD_CHECK_IS_ALIVE (thread);
    
  jvmtiError jerr = env->GetFrameCount (thread, frame_count);
  if (jerr != JVMTI_ERROR_NONE)
    return jerr;
  
  // start_depth can be either a positive number, indicating the depth of the
  // stack at which to begin the trace, or a negative number indicating the
  // number of frames at the bottom of the stack to exclude.  These checks
  // ensure that it is a valid value in either case
  
  ILLEGAL_ARGUMENT (start_depth >= (*frame_count));
  ILLEGAL_ARGUMENT (start_depth < (-(*frame_count)));
  
  _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (thread->frame);

  // If start_depth is negative use this to determine at what depth to start
  // the trace by adding it to the length of the call stack.  This allows the
  // use of the same frame "discarding" mechanism as for a positive start_depth
  if (start_depth < 0)
    start_depth = *frame_count + start_depth;
  
  // If start_depth > 0 "remove" start_depth frames from the beginning
  // of the stack before beginning the trace by moving along the frame list.
  while (start_depth > 0)
    {
      frame = frame->next;
      start_depth--;
      (*frame_count)--;
    }
  
  // Now check to see if the array supplied by the agent is large enough to
  // hold frame_count frames, after adjustment for start_depth.
  if ((*frame_count) > max_frames)
    (*frame_count) = max_frames;
  
  for (int i = 0; i < (*frame_count); i++)
    {
      frames[i].method = frame->self->get_method ();
      
      // Set the location in the frame, native frames have location = -1
      if (frame->frame_type == frame_interpreter)
        {
          _Jv_InterpMethod *imeth 
            = static_cast<_Jv_InterpMethod *> (frame->self);
          _Jv_InterpFrame *interp_frame 
            = static_cast<_Jv_InterpFrame *> (frame);
          frames[i].location = imeth->insn_index (interp_frame->get_pc ());
        }
      else
        frames[i].location = -1;
        
      frame = frame->next;
    }
    
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
    = (jniNativeInterface *) _Jv_MallocUnchecked (sizeof (jniNativeInterface));
  if (*function_table == NULL)
    return JVMTI_ERROR_OUT_OF_MEMORY;
  memcpy (*function_table, &_Jv_JNIFunctions, sizeof (jniNativeInterface));
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_DisposeEnvironment (jvmtiEnv *env)
{
  NULL_CHECK (env);

  if (_jvmtiEnvironments == NULL)
    return JVMTI_ERROR_INVALID_ENVIRONMENT;
  else
    {
      _envListLock->writeLock ()->lock ();
      if (_jvmtiEnvironments->env == env)
	{
	  struct jvmti_env_list *next = _jvmtiEnvironments->next;
	  _Jv_Free (_jvmtiEnvironments);
	  _jvmtiEnvironments = next;
	}
      else
	{
	  struct jvmti_env_list *e = _jvmtiEnvironments; 
	  while (e->next != NULL && e->next->env != env)
	    e = e->next;
	  if (e->next == NULL)
	    {
	      _envListLock->writeLock ()->unlock ();
	      return JVMTI_ERROR_INVALID_ENVIRONMENT;
	    }

	  struct jvmti_env_list *next = e->next->next;
	  _Jv_Free (e->next);
	  e->next = next;
	}
      _envListLock->writeLock ()->unlock ();
    }

  _Jv_Free (env);

  check_enabled_events ();

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
  *result = (char *) _Jv_MallocUnchecked (len + 1);
  if (*result == NULL)
    return JVMTI_ERROR_OUT_OF_MEMORY;
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

/* An event is enabled only if it has both an event handler
   and it is enabled in the environment. */
static void
check_enabled_event (jvmtiEvent type)
{
  bool *enabled;
  int offset;

#define GET_OFFSET(Event)				\
  do							\
    {							\
      enabled = &JVMTI::Event;				\
      offset = offsetof (jvmtiEventCallbacks, Event);	\
    }							\
  while (0)

  switch (type)
    {
    case JVMTI_EVENT_VM_INIT:
      GET_OFFSET (VMInit);
      break;

    case JVMTI_EVENT_VM_DEATH:
      GET_OFFSET (VMDeath);
      break;

    case JVMTI_EVENT_THREAD_START:
      GET_OFFSET (ThreadStart);
      break;

    case JVMTI_EVENT_THREAD_END:
      GET_OFFSET (ThreadEnd);
      break;

    case JVMTI_EVENT_CLASS_FILE_LOAD_HOOK:
      GET_OFFSET (ClassFileLoadHook);
      break;

    case JVMTI_EVENT_CLASS_LOAD:
      GET_OFFSET (ClassLoad);
      break;

    case JVMTI_EVENT_CLASS_PREPARE:
      GET_OFFSET (ClassPrepare);
      break;

    case JVMTI_EVENT_VM_START:
      GET_OFFSET (VMStart);
      break;

    case JVMTI_EVENT_EXCEPTION:
      GET_OFFSET (Exception);
      break;

    case JVMTI_EVENT_EXCEPTION_CATCH:
      GET_OFFSET (ExceptionCatch);
      break;

    case JVMTI_EVENT_SINGLE_STEP:
      GET_OFFSET (SingleStep);
      break;

    case JVMTI_EVENT_FRAME_POP:
      GET_OFFSET (FramePop);
      break;

    case JVMTI_EVENT_BREAKPOINT:
      GET_OFFSET (Breakpoint);
      break;

    case JVMTI_EVENT_FIELD_ACCESS:
      GET_OFFSET (FieldAccess);
      break;

    case JVMTI_EVENT_FIELD_MODIFICATION:
      GET_OFFSET (FieldModification);
      break;

    case JVMTI_EVENT_METHOD_ENTRY:
      GET_OFFSET (MethodEntry);
      break;

    case JVMTI_EVENT_METHOD_EXIT:
      GET_OFFSET (MethodExit);
      break;

    case JVMTI_EVENT_NATIVE_METHOD_BIND:
      GET_OFFSET (NativeMethodBind);
      break;

    case JVMTI_EVENT_COMPILED_METHOD_LOAD:
      GET_OFFSET (CompiledMethodLoad);
      break;

    case JVMTI_EVENT_COMPILED_METHOD_UNLOAD:
      GET_OFFSET (CompiledMethodUnload);
      break;

    case JVMTI_EVENT_DYNAMIC_CODE_GENERATED:
      GET_OFFSET (DynamicCodeGenerated);
      break;

    case JVMTI_EVENT_DATA_DUMP_REQUEST:
      GET_OFFSET (DataDumpRequest);
      break;

    case JVMTI_EVENT_MONITOR_WAIT:
      GET_OFFSET (MonitorWait);
      break;

    case JVMTI_EVENT_MONITOR_WAITED:
      GET_OFFSET (MonitorWaited);
      break;

    case JVMTI_EVENT_MONITOR_CONTENDED_ENTER:
      GET_OFFSET (MonitorContendedEnter);
      break;

    case JVMTI_EVENT_MONITOR_CONTENDED_ENTERED:
      GET_OFFSET (MonitorContendedEntered);
      break;

    case JVMTI_EVENT_GARBAGE_COLLECTION_START:
      GET_OFFSET (GarbageCollectionStart);
      break;

    case JVMTI_EVENT_GARBAGE_COLLECTION_FINISH:
      GET_OFFSET (GarbageCollectionFinish);
      break;

    case JVMTI_EVENT_OBJECT_FREE:
      GET_OFFSET (ObjectFree);
      break;

    case JVMTI_EVENT_VM_OBJECT_ALLOC:
      GET_OFFSET (VMObjectAlloc);
      break;

    default:
      fprintf (stderr,
	       "libgcj: check_enabled_event for unknown JVMTI event (%d)\n",
	       (int) type);
      return;
    }
#undef GET_OFFSET

  int index = EVENT_INDEX (type); // safe since caller checks this

  if (_jvmtiEnvironments != NULL)
    {
      _envListLock->readLock ()->lock ();
      struct jvmti_env_list *e;
      FOREACH_ENVIRONMENT (e)
	{
	  char *addr
	    = reinterpret_cast<char *> (&e->env->callbacks) + offset;
	  void **callback = reinterpret_cast<void **> (addr);
	  if (e->env->enabled[index] && *callback != NULL)
	    {
	      *enabled = true;
	      _envListLock->readLock ()->unlock ();
	      return;
	    }
	}

      _envListLock->readLock ()->unlock ();
    }

  *enabled = false;
}

static void
check_enabled_events ()
{
  check_enabled_event (JVMTI_EVENT_VM_INIT);
  check_enabled_event (JVMTI_EVENT_VM_DEATH);
  check_enabled_event (JVMTI_EVENT_THREAD_START);
  check_enabled_event (JVMTI_EVENT_THREAD_END);
  check_enabled_event (JVMTI_EVENT_CLASS_FILE_LOAD_HOOK);
  check_enabled_event (JVMTI_EVENT_CLASS_LOAD);
  check_enabled_event (JVMTI_EVENT_CLASS_PREPARE);
  check_enabled_event (JVMTI_EVENT_VM_START);
  check_enabled_event (JVMTI_EVENT_EXCEPTION);
  check_enabled_event (JVMTI_EVENT_EXCEPTION_CATCH);
  check_enabled_event (JVMTI_EVENT_SINGLE_STEP);
  check_enabled_event (JVMTI_EVENT_FRAME_POP);
  check_enabled_event (JVMTI_EVENT_BREAKPOINT);
  check_enabled_event (JVMTI_EVENT_FIELD_ACCESS);
  check_enabled_event (JVMTI_EVENT_FIELD_MODIFICATION);
  check_enabled_event (JVMTI_EVENT_METHOD_ENTRY);
  check_enabled_event (JVMTI_EVENT_METHOD_EXIT);
  check_enabled_event (JVMTI_EVENT_NATIVE_METHOD_BIND);
  check_enabled_event (JVMTI_EVENT_COMPILED_METHOD_LOAD);
  check_enabled_event (JVMTI_EVENT_COMPILED_METHOD_UNLOAD);
  check_enabled_event (JVMTI_EVENT_DYNAMIC_CODE_GENERATED);
  check_enabled_event (JVMTI_EVENT_DATA_DUMP_REQUEST);
  check_enabled_event (JVMTI_EVENT_MONITOR_WAIT);
  check_enabled_event (JVMTI_EVENT_MONITOR_WAITED);
  check_enabled_event (JVMTI_EVENT_MONITOR_CONTENDED_ENTER);
  check_enabled_event (JVMTI_EVENT_MONITOR_CONTENDED_ENTERED);
  check_enabled_event (JVMTI_EVENT_GARBAGE_COLLECTION_START);
  check_enabled_event (JVMTI_EVENT_GARBAGE_COLLECTION_FINISH);
  check_enabled_event (JVMTI_EVENT_OBJECT_FREE);
  check_enabled_event (JVMTI_EVENT_VM_OBJECT_ALLOC);
}

static jvmtiError JNICALL
_Jv_JVMTI_SetEventNotificationMode (jvmtiEnv *env, jvmtiEventMode mode,
				    jvmtiEvent type, jthread event_thread, ...)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD | JVMTI_PHASE_LIVE);

  if (event_thread != NULL)
    {
      THREAD_CHECK_VALID (event_thread);
      THREAD_CHECK_IS_ALIVE (event_thread);
    }

  bool enabled;
  switch (mode)
    {
    case JVMTI_DISABLE:
      enabled = false;
      break;
    case JVMTI_ENABLE:
      enabled = true;
      break;

    default:
      return JVMTI_ERROR_ILLEGAL_ARGUMENT;
    }

  switch (type)
    {
    case JVMTI_EVENT_VM_INIT:
    case JVMTI_EVENT_VM_DEATH:
    case JVMTI_EVENT_THREAD_START:
    case JVMTI_EVENT_VM_START:
    case JVMTI_EVENT_COMPILED_METHOD_LOAD:
    case JVMTI_EVENT_COMPILED_METHOD_UNLOAD:
    case JVMTI_EVENT_DYNAMIC_CODE_GENERATED:
    case JVMTI_EVENT_DATA_DUMP_REQUEST:
      ILLEGAL_ARGUMENT (event_thread != NULL);
      break;

    case JVMTI_EVENT_THREAD_END:
    case JVMTI_EVENT_CLASS_FILE_LOAD_HOOK:
    case JVMTI_EVENT_CLASS_LOAD:
    case JVMTI_EVENT_CLASS_PREPARE:
    case JVMTI_EVENT_EXCEPTION:
    case JVMTI_EVENT_EXCEPTION_CATCH:
    case JVMTI_EVENT_SINGLE_STEP:
    case JVMTI_EVENT_FRAME_POP:
    case JVMTI_EVENT_BREAKPOINT:
    case JVMTI_EVENT_FIELD_ACCESS:
    case JVMTI_EVENT_FIELD_MODIFICATION:
    case JVMTI_EVENT_METHOD_ENTRY:
    case JVMTI_EVENT_METHOD_EXIT:
    case JVMTI_EVENT_NATIVE_METHOD_BIND:
    case JVMTI_EVENT_MONITOR_WAIT:
    case JVMTI_EVENT_MONITOR_WAITED:
    case JVMTI_EVENT_MONITOR_CONTENDED_ENTER:
    case JVMTI_EVENT_MONITOR_CONTENDED_ENTERED:
    case JVMTI_EVENT_GARBAGE_COLLECTION_START:
    case JVMTI_EVENT_GARBAGE_COLLECTION_FINISH:
    case JVMTI_EVENT_OBJECT_FREE:
    case JVMTI_EVENT_VM_OBJECT_ALLOC:
      break;

    default:
      return JVMTI_ERROR_INVALID_EVENT_TYPE;
    }

  env->thread[EVENT_INDEX(type)] = event_thread;
  env->enabled[EVENT_INDEX(type)] = enabled;
  check_enabled_event (type);
  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_SetEventCallbacks (jvmtiEnv *env,
			     const jvmtiEventCallbacks *callbacks,
			     jint size_of_callbacks)
{
  REQUIRE_PHASE (env, JVMTI_PHASE_ONLOAD | JVMTI_PHASE_LIVE);
  ILLEGAL_ARGUMENT (size_of_callbacks < 0);

  // Copy the list of callbacks into the environment
  memcpy (&env->callbacks, callbacks, sizeof (jvmtiEventCallbacks));

  /* Check which events are now enabeld (JVMTI makes no requirements
     about the order in which SetEventCallbacks and SetEventNotifications
     are called. So we must check all events here. */
  check_enabled_events ();

  return JVMTI_ERROR_NONE;
}

static jvmtiError JNICALL
_Jv_JVMTI_GetErrorName (MAYBE_UNUSED jvmtiEnv *env, jvmtiError error,
			char **name_ptr)
{
  NULL_CHECK (name_ptr);

  const char *name;
  switch (error)
    {
    case JVMTI_ERROR_NONE:
      name = "none";
      break;

    case JVMTI_ERROR_NULL_POINTER:
      name = "null pointer";
      break;

    case JVMTI_ERROR_OUT_OF_MEMORY:
      name = "out of memory";
      break;

    case JVMTI_ERROR_ACCESS_DENIED:
      name = "access denied";
      break;

    case JVMTI_ERROR_WRONG_PHASE:
      name = "wrong phase";
      break;

    case JVMTI_ERROR_INTERNAL:
      name = "internal error";
      break;

    case JVMTI_ERROR_UNATTACHED_THREAD:
      name = "unattached thread";
      break;

    case JVMTI_ERROR_INVALID_ENVIRONMENT:
      name = "invalid environment";
      break;

    case JVMTI_ERROR_INVALID_PRIORITY:
      name = "invalid priority";
      break;

    case JVMTI_ERROR_THREAD_NOT_SUSPENDED:
      name = "thread not suspended";
      break;

    case JVMTI_ERROR_THREAD_SUSPENDED:
      name = "thread suspended";
      break;

    case JVMTI_ERROR_THREAD_NOT_ALIVE:
      name = "thread not alive";
      break;

    case JVMTI_ERROR_CLASS_NOT_PREPARED:
      name = "class not prepared";
      break;

    case JVMTI_ERROR_NO_MORE_FRAMES:
      name = "no more frames";
      break;

    case JVMTI_ERROR_OPAQUE_FRAME:
      name = "opaque frame";
      break;

    case JVMTI_ERROR_DUPLICATE:
      name = "duplicate";
      break;

    case JVMTI_ERROR_NOT_FOUND:
      name = "not found";
      break;

    case JVMTI_ERROR_NOT_MONITOR_OWNER:
      name = "not monitor owner";
      break;

    case JVMTI_ERROR_INTERRUPT:
      name = "interrupted";
      break;

    case JVMTI_ERROR_UNMODIFIABLE_CLASS:
      name = "unmodifiable class";
      break;

    case JVMTI_ERROR_NOT_AVAILABLE:
      name = "not available";
      break;

    case JVMTI_ERROR_ABSENT_INFORMATION:
      name = "absent information";
      break;

    case JVMTI_ERROR_INVALID_EVENT_TYPE:
      name = "invalid event type";
      break;

    case JVMTI_ERROR_NATIVE_METHOD:
      name = "native method";
      break;

    case JVMTI_ERROR_INVALID_THREAD:
      name = "invalid thread";
      break;

    case JVMTI_ERROR_INVALID_THREAD_GROUP:
      name = "invalid thread group";
      break;

    case JVMTI_ERROR_INVALID_OBJECT:
      name = "invalid object";
      break;

    case JVMTI_ERROR_INVALID_CLASS:
      name = "invalid class";
      break;

    case JVMTI_ERROR_INVALID_METHODID:
      name = "invalid method ID";
      break;

    case JVMTI_ERROR_INVALID_LOCATION:
      name = "invalid location";
      break;

    case JVMTI_ERROR_INVALID_FIELDID:
      name = "invalid field ID";
      break;

    case JVMTI_ERROR_TYPE_MISMATCH:
      name = "type mismatch";
      break;

    case JVMTI_ERROR_INVALID_SLOT:
      name = "invalid slot";
      break;

    case JVMTI_ERROR_INVALID_MONITOR:
      name = "invalid monitor";
      break;

    case JVMTI_ERROR_INVALID_CLASS_FORMAT:
      name = "invalid class format";
      break;

    case JVMTI_ERROR_CIRCULAR_CLASS_DEFINITION:
      name = "circular class definition";
      break;

    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_ADDED:
      name = "unsupported redefinition: method added";
      break;

    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_SCHEMA_CHANGED:
      name = "unsupported redefinition: schema changed";
      break;

    case JVMTI_ERROR_INVALID_TYPESTATE:
      name = "invalid type state";
      break;

    case JVMTI_ERROR_FAILS_VERIFICATION:
      name = "fails verification";
      break;

    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_HIERARCHY_CHANGED:
      name = "unsupported redefinition: hierarchy changed";
      break;

    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_DELETED:
      name = "unsupported redefinition: method deleted";
      break;

    case JVMTI_ERROR_UNSUPPORTED_VERSION:
      name = "unsupported version";
      break;

    case JVMTI_ERROR_NAMES_DONT_MATCH:
      name = "names do not match";
      break;

    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_CLASS_MODIFIERS_CHANGED:
      name = "unsupported redefinition: class modifiers changed";
      break;

    case JVMTI_ERROR_UNSUPPORTED_REDEFINITION_METHOD_MODIFIERS_CHANGED:
      name = "unsupported redefinition: method modifiers changed";
      break;

    case JVMTI_ERROR_MUST_POSSESS_CAPABILITY:
      name = "must possess capability";
      break;

    case JVMTI_ERROR_ILLEGAL_ARGUMENT:
      name = "illegal argument";
      break;

    default:
      return JVMTI_ERROR_ILLEGAL_ARGUMENT;
    }

  *name_ptr = (char *) _Jv_MallocUnchecked (strlen (name) + 1);
  if (*name_ptr == NULL)
    return JVMTI_ERROR_OUT_OF_MEMORY;

  strcpy (*name_ptr, name);
  return JVMTI_ERROR_NONE;
}

#define RESERVED NULL
#define UNIMPLEMENTED NULL

struct _Jv_jvmtiEnv _Jv_JVMTI_Interface =
{
  RESERVED,			// reserved1
  _Jv_JVMTI_SetEventNotificationMode, // SetEventNotificationMode
  RESERVED,			// reserved3
  _Jv_JVMTI_GetAllThreads,		// GetAllThreads
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
  _Jv_JVMTI_GetFrameCount,		// GetFrameCount
  _Jv_JVMTI_GetThreadState,	// GetThreadState
  RESERVED,			// reserved18
  UNIMPLEMENTED,		// GetFrameLocation
  UNIMPLEMENTED,		// NotifyPopFrame
  _Jv_JVMTI_GetLocalObject,		// GetLocalObject
  _Jv_JVMTI_GetLocalInt,		// GetLocalInt
  _Jv_JVMTI_GetLocalLong,		// GetLocalLong
  _Jv_JVMTI_GetLocalFloat,		// GetLocalFloat
  _Jv_JVMTI_GetLocalDouble,		// GetLocalDouble
  _Jv_JVMTI_SetLocalObject,		// SetLocalObject
  _Jv_JVMTI_SetLocalInt,		// SetLocalInt
  _Jv_JVMTI_SetLocalLong,		// SetLocalLong
  _Jv_JVMTI_SetLocalFloat,		// SetLocalFloat
  _Jv_JVMTI_SetLocalDouble,		// SetLocalDouble
  _Jv_JVMTI_CreateRawMonitor,	// CreateRawMonitor
  _Jv_JVMTI_DestroyRawMonitor,	// DestroyRawMonitor
  _Jv_JVMTI_RawMonitorEnter,	// RawMonitorEnter
  _Jv_JVMTI_RawMonitorExit,	// RawMonitorExit
  _Jv_JVMTI_RawMonitorWait,	// RawMonitorWait
  _Jv_JVMTI_RawMonitorNotify,	// RawMonitorNotify
  _Jv_JVMTI_RawMonitorNotifyAll, // RawMonitorNotifyAll
  _Jv_JVMTI_SetBreakpoint,	// SetBreakpoint
  _Jv_JVMTI_ClearBreakpoint,	// ClearBreakpoint
  RESERVED,			// reserved40
  UNIMPLEMENTED,		// SetFieldAccessWatch
  UNIMPLEMENTED,		// ClearFieldAccessWatch
  UNIMPLEMENTED,		// SetFieldModificationWatch
  UNIMPLEMENTED,		// ClearFieldModificationWatch
  RESERVED,			// reserved45
  _Jv_JVMTI_Allocate,		// Allocate
  _Jv_JVMTI_Deallocate,		// Deallocate
  UNIMPLEMENTED,		// GetClassSignature
  _Jv_JVMTI_GetClassStatus,	// GetClassStatus
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
  _Jv_JVMTI_GetMethodName,	// GetMethodName
  _Jv_JVMTI_GetMethodDeclaringClass,  // GetMethodDeclaringClass
  _Jv_JVMTI_GetMethodModifiers,	// GetMethodModifers
  RESERVED,			// reserved67
  _Jv_JVMTI_GetMaxLocals,		// GetMaxLocals
  _Jv_JVMTI_GetArgumentsSize,		// GetArgumentsSize
  _Jv_JVMTI_GetLineNumberTable,	// GetLineNumberTable
  UNIMPLEMENTED,		// GetMethodLocation
  _Jv_JVMTI_GetLocalVariableTable,		// GetLocalVariableTable
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
  _Jv_JVMTI_GetStackTrace,		// GetStackTrace
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
  _Jv_JVMTI_SetEventCallbacks,	// SetEventCallbacks
  UNIMPLEMENTED,		// GenerateEvents
  UNIMPLEMENTED,		// GetExtensionFunctions
  UNIMPLEMENTED,		// GetExtensionEvents
  UNIMPLEMENTED,		// SetExtensionEventCallback
  _Jv_JVMTI_DisposeEnvironment,	// DisposeEnvironment
  _Jv_JVMTI_GetErrorName,	// GetErrorName
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
  memset (env, 0, sizeof (_Jv_JVMTIEnv));
  env->p = &_Jv_JVMTI_Interface;
  struct jvmti_env_list *element
    = (struct jvmti_env_list *) _Jv_MallocUnchecked (sizeof (struct jvmti_env_list));
  element->env = env;
  element->next = NULL;

  _envListLock->writeLock ()->lock ();
  if (_jvmtiEnvironments == NULL)
    _jvmtiEnvironments = element;
  else
    {
      struct jvmti_env_list *e;
      for (e = _jvmtiEnvironments; e->next != NULL; e = e->next)
	;
      e->next = element;
    }
  _envListLock->writeLock ()->unlock ();

  /* Mark JVMTI active. This is used to force the interpreter
     to use either debugging or non-debugging code. Once JVMTI
     has been enabled, the non-debug interpreter cannot be used. */
  JVMTI::enabled = true;
  return env;
}

void
_Jv_JVMTI_Init ()
{
  _jvmtiEnvironments = NULL;
  _envListLock
    = new java::util::concurrent::locks::ReentrantReadWriteLock ();

  // No environments, so this should set all JVMTI:: members to false
  check_enabled_events ();
}

static void
post_event (jvmtiEnv *env, jvmtiEvent type, jthread event_thread, va_list args)
{
#define ARG(Type,Name) Type Name = (Type) va_arg (args, Type)

#define GET_BOOLEAN_ARG(Name)			\
  ARG (int, b);					\
  jboolean Name = (b == 0) ? false : true

#define GET_CHAR_ARG(Name)			\
  ARG (int, c);					\
  char Name = static_cast<char> (c)

  switch (type)
    {
    case JVMTI_EVENT_VM_INIT:
      if (env->callbacks.VMInit != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  env->callbacks.VMInit (env, jni_env, event_thread);
	}
      break;

    case JVMTI_EVENT_VM_DEATH:
      if (env->callbacks.VMDeath != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  env->callbacks.VMDeath (env, jni_env);
	}
      break;

    case JVMTI_EVENT_THREAD_START:
      if (env->callbacks.ThreadStart != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  env->callbacks.ThreadStart (env, jni_env, event_thread);
	}
      break;

    case JVMTI_EVENT_THREAD_END:
      if (env->callbacks.ThreadEnd != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  env->callbacks.ThreadEnd (env, jni_env, event_thread);
	}
      break;

    case JVMTI_EVENT_CLASS_FILE_LOAD_HOOK:
      if (env->callbacks.ClassFileLoadHook != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jclass, class_being_redefined);
	  ARG (jobject, loader);
	  ARG (const char *, name);
	  ARG (jobject, protection_domain);
	  ARG (jint, class_data_len);
	  ARG (const unsigned char *, class_data);
	  ARG (jint *, new_class_data_len);
	  ARG (unsigned char **, new_class_data);
	  env->callbacks.ClassFileLoadHook (env, jni_env,
					    class_being_redefined, loader,
					    name, protection_domain,
					    class_data_len, class_data,
					    new_class_data_len,
					    new_class_data);
	}
      break;

    case JVMTI_EVENT_CLASS_LOAD:
      if (env->callbacks.ClassLoad != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jclass, klass);
	  env->callbacks.ClassLoad (env, jni_env, event_thread, klass);
	}
      break;

    case JVMTI_EVENT_CLASS_PREPARE:
      if (env->callbacks.ClassPrepare != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jclass, klass);
	  env->callbacks.ClassPrepare (env, jni_env, event_thread, klass);
	}
      break;

    case JVMTI_EVENT_VM_START:
      if (env->callbacks.VMStart != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  env->callbacks.VMStart (env, jni_env);
	}
      break;

    case JVMTI_EVENT_EXCEPTION:
      if (env->callbacks.Exception != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  ARG (jlocation, location);
	  ARG (jobject, exception);
	  ARG (jmethodID, catch_method);
	  ARG (jlocation, catch_location);
	  env->callbacks.Exception (env, jni_env, event_thread, method,
				    location, exception, catch_method,
				    catch_location);
	}
      break;

    case JVMTI_EVENT_EXCEPTION_CATCH:
      if (env->callbacks.ExceptionCatch != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  ARG (jlocation, location);
	  ARG (jobject, exception);
	  env->callbacks.ExceptionCatch (env, jni_env, event_thread, method,
					 location, exception);
	}
      break;

    case JVMTI_EVENT_SINGLE_STEP:
      if (env->callbacks.SingleStep != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  ARG (jlocation, location);
	  env->callbacks.SingleStep (env, jni_env, event_thread, method,
				     location);
	}
      break;

    case JVMTI_EVENT_FRAME_POP:
      if (env->callbacks.FramePop != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  GET_BOOLEAN_ARG (was_popped_by_exception);
	  env->callbacks.FramePop (env, jni_env, event_thread, method,
				   was_popped_by_exception);
	}
      break;

    case JVMTI_EVENT_BREAKPOINT:
      if (env->callbacks.Breakpoint != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  ARG (jlocation, location);
	  env->callbacks.Breakpoint (env, jni_env, event_thread, method,
				     location);
	}
      break;

    case JVMTI_EVENT_FIELD_ACCESS:
      if (env->callbacks.FieldAccess != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  ARG (jlocation, location);
	  ARG (jclass, field_class);
	  ARG (jobject, object);
	  ARG (jfieldID, field);
	  env->callbacks.FieldAccess (env, jni_env, event_thread, method,
				      location, field_class, object, field);
	}
      break;

    case JVMTI_EVENT_FIELD_MODIFICATION:
      if (env->callbacks.FieldModification != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  ARG (jlocation, location);
	  ARG (jclass, field_class);
	  ARG (jobject, object);
	  ARG (jfieldID, field);
	  GET_CHAR_ARG (signature_type);
	  ARG (jvalue, new_value);
	  env->callbacks.FieldModification (env, jni_env, event_thread, method,
					    location, field_class, object,
					    field, signature_type, new_value);
	}
      break;

    case JVMTI_EVENT_METHOD_ENTRY:
      if (env->callbacks.MethodEntry != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  env->callbacks.MethodEntry (env, jni_env, event_thread, method);
	}
      break;

    case JVMTI_EVENT_METHOD_EXIT:
      if (env->callbacks.MethodExit != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  GET_BOOLEAN_ARG (was_popped_by_exception);
	  ARG (jvalue, return_value);
	  env->callbacks.MethodExit (env, jni_env, event_thread, method,
				     was_popped_by_exception, return_value);
	}
      break;

    case JVMTI_EVENT_NATIVE_METHOD_BIND:
      if (env->callbacks.NativeMethodBind != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jmethodID, method);
	  ARG (void *, address);
	  ARG (void **, new_address_ptr);
	  env->callbacks.NativeMethodBind (env, jni_env, event_thread, method,
					   address, new_address_ptr);
	}
      break;

    case JVMTI_EVENT_COMPILED_METHOD_LOAD:
      if (env->callbacks.CompiledMethodLoad != NULL)
	{
	  ARG (jmethodID, method);
	  ARG (jint, code_size);
	  ARG (const void *, code_addr);
	  ARG (jint, map_length);
	  ARG (const jvmtiAddrLocationMap *, map);
	  ARG (const void *, compile_info);
	  env->callbacks.CompiledMethodLoad (env, method, code_size, code_addr,
					     map_length, map, compile_info);
	}
      break;

    case JVMTI_EVENT_COMPILED_METHOD_UNLOAD:
      if (env->callbacks.CompiledMethodUnload != NULL)
	{
	  ARG (jmethodID, method);
	  ARG (const void *, code_addr);
	  env->callbacks.CompiledMethodUnload (env, method, code_addr);
	}
      break;

    case JVMTI_EVENT_DYNAMIC_CODE_GENERATED:
      if (env->callbacks.DynamicCodeGenerated != NULL)
	{
	  ARG (const char *, name);
	  ARG (const void *, address);
	  ARG (jint, length);
	  env->callbacks.DynamicCodeGenerated (env, name, address, length);
	}
      break;

    case JVMTI_EVENT_DATA_DUMP_REQUEST:
      if (env->callbacks.DataDumpRequest != NULL)
	{
	  env->callbacks.DataDumpRequest (env);
	}
      break;

    case JVMTI_EVENT_MONITOR_WAIT:
      if (env->callbacks.MonitorWait != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jobject, object);
	  ARG (jlong, timeout);
	  env->callbacks.MonitorWait (env, jni_env, event_thread, object,
				      timeout);
	}
      break;

    case JVMTI_EVENT_MONITOR_WAITED:
      if (env->callbacks.MonitorWaited != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jobject, object);
	  GET_BOOLEAN_ARG (timed_out);
	  env->callbacks.MonitorWaited (env, jni_env, event_thread, object,
					timed_out);
	}
      break;

    case JVMTI_EVENT_MONITOR_CONTENDED_ENTER:
      if (env->callbacks.MonitorContendedEnter != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jobject, object);
	  env->callbacks.MonitorContendedEnter (env, jni_env, event_thread,
						object);
	}
      break;

    case JVMTI_EVENT_MONITOR_CONTENDED_ENTERED:
      if (env->callbacks.MonitorContendedEntered != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jobject, object);
	  env->callbacks.MonitorContendedEntered (env, jni_env, event_thread,
						  object);
	}
      break;

    case JVMTI_EVENT_GARBAGE_COLLECTION_START:
      if (env->callbacks.GarbageCollectionStart != NULL)
	{
	  env->callbacks.GarbageCollectionStart (env);
	}
      break;

    case JVMTI_EVENT_GARBAGE_COLLECTION_FINISH:
      if (env->callbacks.GarbageCollectionFinish != NULL)
	{
	  env->callbacks.GarbageCollectionFinish (env);
	}
      break;

    case JVMTI_EVENT_OBJECT_FREE:
      if (env->callbacks.ObjectFree != NULL)
	{
	  ARG (jlong, tag);
	  env->callbacks.ObjectFree (env, tag);
	}
      break;

    case JVMTI_EVENT_VM_OBJECT_ALLOC:
      if (env->callbacks.VMObjectAlloc != NULL)
	{
	  ARG (JNIEnv *, jni_env);
	  ARG (jobject, object);
	  ARG (jclass, object_class);
	  ARG (jlong, size);
	  env->callbacks.VMObjectAlloc (env, jni_env, event_thread,
					object, object_class, size);
	}
      break;

    default:
      fprintf (stderr, "libgcj: post of unknown JVMTI event (%d)\n",
	       (int) type);
      break;
    }
  va_end (args);
#undef ARG
#undef GET_BOOLEAN_ARG
#undef GET_CHAR_ARG
}

/* Post an event to requesting JVMTI environments
 *
 * This function should not be called without consulting the
 * JVMTI_REQUESTED_EVENT macro first (for speed). It does no real
 * harm (other than kill speed), since this function will still
 * only send the event if it was properly requested by an environment.
 */ 
void
_Jv_JVMTI_PostEvent (jvmtiEvent type, jthread event_thread, ...)
{
  va_list args;
  va_start (args, event_thread);

  _envListLock->readLock ()->lock ();
  struct jvmti_env_list *e;
  FOREACH_ENVIRONMENT (e)
    {
      /* Events are only posted if the event was explicitly enabled,
	 it has a registered event handler, and the event thread
	 matches (either globally or restricted to a specific thread).
	 Here we check all but the event handler, which will be handled
	 by post_event. */
      if (e->env->enabled[EVENT_INDEX(type)]
	  && (e->env->thread[EVENT_INDEX(type)] == NULL
	      || e->env->thread[EVENT_INDEX(type)] == event_thread))
	{
	  post_event (e->env, type, event_thread, args);
	}
    }
  _envListLock->readLock ()->unlock ();
  va_end (args);
}
