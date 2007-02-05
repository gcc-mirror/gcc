// natVMVirtualMachine.cc - native support for VMVirtualMachine

/* Copyright (C) 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License. Please consult the file "LIBGCJ_LICENSE" for
details. */

#include <config.h>
#include <gcj/cni.h>
#include <java-assert.h>
#include <jvm.h>
#include <jvmti.h>

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Integer.h>
#include <java/lang/String.h>
#include <java/lang/StringBuilder.h>
#include <java/lang/Thread.h>
#include <java/nio/ByteBuffer.h>
#include <java/util/ArrayList.h>
#include <java/util/Collection.h>
#include <java/util/Hashtable.h>
#include <java/util/Iterator.h>

#include <gnu/classpath/jdwp/Jdwp.h>
#include <gnu/classpath/jdwp/VMFrame.h>
#include <gnu/classpath/jdwp/VMMethod.h>
#include <gnu/classpath/jdwp/VMVirtualMachine.h>
#include <gnu/classpath/jdwp/event/ClassPrepareEvent.h>
#include <gnu/classpath/jdwp/event/EventManager.h>
#include <gnu/classpath/jdwp/event/EventRequest.h>
#include <gnu/classpath/jdwp/event/ThreadEndEvent.h>
#include <gnu/classpath/jdwp/event/ThreadStartEvent.h>
#include <gnu/classpath/jdwp/event/VmDeathEvent.h>
#include <gnu/classpath/jdwp/event/VmInitEvent.h>
#include <gnu/classpath/jdwp/event/filters/IEventFilter.h>
#include <gnu/classpath/jdwp/event/filters/LocationOnlyFilter.h>
#include <gnu/classpath/jdwp/exception/InvalidLocationException.h>
#include <gnu/classpath/jdwp/exception/InvalidMethodException.h>
#include <gnu/classpath/jdwp/exception/JdwpInternalErrorException.h>
#include <gnu/classpath/jdwp/util/Location.h>
#include <gnu/classpath/jdwp/util/MethodResult.h>
#include <gnu/gcj/jvmti/Breakpoint.h>
#include <gnu/gcj/jvmti/BreakpointManager.h>

using namespace java::lang;
using namespace gnu::classpath::jdwp::event;
using namespace gnu::classpath::jdwp::util;

// Forward declarations
static Location *get_request_location (EventRequest *);
static void JNICALL jdwpClassPrepareCB (jvmtiEnv *, JNIEnv *, jthread, jclass);
static void JNICALL jdwpThreadEndCB (jvmtiEnv *, JNIEnv *, jthread);
static void JNICALL jdwpThreadStartCB (jvmtiEnv *, JNIEnv *, jthread);
static void JNICALL jdwpVMDeathCB (jvmtiEnv *, JNIEnv *);
static void JNICALL jdwpVMInitCB (jvmtiEnv *, JNIEnv *, jthread);
static void throw_jvmti_error (jvmtiError);

#define DEFINE_CALLBACK(Cb,Event) Cb.Event = jdwp ## Event ## CB
#define ENABLE_EVENT(Event,Thread)					\
  _jdwp_jvmtiEnv->SetEventNotificationMode (JVMTI_ENABLE,		\
					    JVMTI_EVENT_ ## Event, Thread)
// JVMTI environment
static jvmtiEnv *_jdwp_jvmtiEnv;

jvmtiEnv *
_Jv_GetJDWP_JVMTIEnv (void)
{
  return _jdwp_jvmtiEnv;
}

void
gnu::classpath::jdwp::VMVirtualMachine::initialize ()
{
  _jdwp_suspend_counts = new ::java::util::Hashtable ();
  JavaVM *vm = _Jv_GetJavaVM ();
  vm->GetEnv (reinterpret_cast<void **> (&_jdwp_jvmtiEnv), JVMTI_VERSION_1_0);

  // Wait for VM_INIT to do more initialization
  jvmtiEventCallbacks callbacks;
  DEFINE_CALLBACK (callbacks, VMInit);
  _jdwp_jvmtiEnv->SetEventCallbacks (&callbacks, sizeof (callbacks));
  ENABLE_EVENT (VM_INIT, NULL);
}

void
gnu::classpath::jdwp::VMVirtualMachine::suspendThread (Thread *thread)
{
  jint value;
  Integer *count;
  {
    JvSynchronize dummy (_jdwp_suspend_counts);
    count = reinterpret_cast<Integer *> (_jdwp_suspend_counts->get (thread));
    if (count == NULL)
      {
	// New -- suspend thread
	value = 0;
      }
    else
      {
	// Thread already suspended
	value = count->intValue ();
      }

    count = Integer::valueOf (++value);
    _jdwp_suspend_counts->put (thread, count);
  }

  if (value == 1)
    {
      // Suspend the thread
      jvmtiError err = _jdwp_jvmtiEnv->SuspendThread (thread);
      if (err != JVMTI_ERROR_NONE)
	{
	  using namespace gnu::gcj::runtime;
	  using namespace gnu::classpath::jdwp::exception;
	  char *reason;
	  _jdwp_jvmtiEnv->GetErrorName (err, &reason);
	  String *txt = JvNewStringLatin1 ("could not suspend thread: ");
	  StringBuilder *msg = new StringBuilder (txt);
	  msg->append (JvNewStringLatin1 (reason));
	  _jdwp_jvmtiEnv->Deallocate ((unsigned char *) reason);
	  throw new JdwpInternalErrorException (msg->toString ());
	}
    }
}

void
gnu::classpath::jdwp::VMVirtualMachine::resumeThread (Thread *thread)
{
  jint value;
  {
    JvSynchronize dummy (_jdwp_suspend_counts);
    Integer *count
      = reinterpret_cast<Integer *> (_jdwp_suspend_counts->get (thread));
    if (count == NULL)
      {
	// Thread not suspended: ThreadReference.Resume says to ignore it.
	return;
      }
    else
      {
	// Decrement suspend count
	value = count->intValue () - 1;
      }

    if (value == 0)
      {
	// Thread will be resumed, remove from table
	_jdwp_suspend_counts->remove (thread);
      }
    else
      {
	// Thread stays suspended: record new suspend count
	count = Integer::valueOf (value);
	_jdwp_suspend_counts->put (thread, count);
      }
  }

  if (value == 0)
    {
      jvmtiError err = _jdwp_jvmtiEnv->ResumeThread (thread);
      if (err != JVMTI_ERROR_NONE)
	{
	  using namespace gnu::gcj::runtime;
	  using namespace gnu::classpath::jdwp::exception;
	  char *reason;
	  _jdwp_jvmtiEnv->GetErrorName (err, &reason);
	  String *txt = JvNewStringLatin1 ("could not resume thread: ");
	  StringBuilder *msg = new StringBuilder (txt);
	  msg->append (JvNewStringLatin1 (reason));
	  _jdwp_jvmtiEnv->Deallocate ((unsigned char *) reason);
	  throw new JdwpInternalErrorException (msg->toString ());
	}
    }
}

jint
gnu::classpath::jdwp::VMVirtualMachine::getSuspendCount (Thread *thread)
{
  jint suspensions = 0;
  Integer *count
    = reinterpret_cast<Integer *> (_jdwp_suspend_counts->get (thread));
  if (count != NULL)
    suspensions = count->intValue ();
  return suspensions;
}

void
gnu::classpath::jdwp::VMVirtualMachine::registerEvent (EventRequest *request)
{
  switch (request->getEventKind ())
    {
    case EventRequest::EVENT_SINGLE_STEP:
      break;

    case EventRequest::EVENT_BREAKPOINT:
      {
	using namespace ::gnu::gcj::jvmti;
	Location *loc = get_request_location (request);
	if (loc == NULL)
	  {
	    using namespace gnu::classpath::jdwp::exception;
	    throw new InvalidLocationException ();
	  }

	jlong method = loc->getMethod ()->getId ();
	jlocation index = loc->getIndex ();
	Breakpoint  *bp = BreakpointManager::getBreakpoint (method, index);
	if (bp == NULL)
	  {
	    // Breakpoint not in interpreter yet
	    bp = BreakpointManager::newBreakpoint (method, index);
	  }
	else
	  {
	    // Ignore the duplicate
	  }
      }
     break;

    case EventRequest::EVENT_FRAME_POP:
      break;

    case EventRequest::EVENT_EXCEPTION:
      break;

    case EventRequest::EVENT_USER_DEFINED:
      break;

    case EventRequest::EVENT_THREAD_START:
      break;

    case EventRequest::EVENT_THREAD_END:
      break;

    case EventRequest::EVENT_CLASS_PREPARE:
      break;

    case EventRequest::EVENT_CLASS_LOAD:
      break;

    case EventRequest::EVENT_CLASS_UNLOAD:
      break;

    case EventRequest::EVENT_FIELD_ACCESS:
      break;

    case EventRequest::EVENT_FIELD_MODIFY:
      break;

    case EventRequest::EVENT_METHOD_ENTRY:
      break;

    case EventRequest::EVENT_METHOD_EXIT:
      break;

    case EventRequest::EVENT_VM_INIT:
      break;

    case EventRequest::EVENT_VM_DEATH:
      break;
    }
}

void
gnu::classpath::jdwp::VMVirtualMachine::unregisterEvent (EventRequest *request)
{
  switch (request->getEventKind ())
    {
    case EventRequest::EVENT_SINGLE_STEP:
      break;

    case EventRequest::EVENT_BREAKPOINT:
      {
	using namespace gnu::gcj::jvmti;
	::java::util::Collection *breakpoints;
	EventManager *em = EventManager::getDefault ();
	breakpoints = em->getRequests (EventRequest::EVENT_BREAKPOINT);

	// Check for duplicates
	int matches = 0;
	Location *the_location = get_request_location (request);

	// This should not be possible: we REQUIRE a Location
	// to install a breakpoint
	JvAssert (the_location != NULL);

	::java::util::Iterator *iter = breakpoints->iterator ();
	while (iter->hasNext ())
	  {
	    EventRequest *er
	      = reinterpret_cast<EventRequest *> (iter->next ());
	    Location *loc = get_request_location (er);
	    JvAssert (loc != NULL);
	    if (loc->equals (the_location) && ++matches == 2)
	      {
		// Short-circuit: already more than one breakpoint
		return;
	      }
	  }

	if (matches == 0)
	  {
	    using namespace gnu::classpath::jdwp::exception;
	    jstring msg
	      = JvNewStringLatin1 ("attempt to remove unknown breakpoint");
	    throw new JdwpInternalErrorException (msg);
	  }

	jlong methodId = the_location->getMethod ()->getId ();
	BreakpointManager::deleteBreakpoint (methodId,
					     the_location->getIndex ());
      }
      break;

    case EventRequest::EVENT_FRAME_POP:
      break;

    case EventRequest::EVENT_EXCEPTION:
      break;

    case EventRequest::EVENT_USER_DEFINED:
      break;

    case EventRequest::EVENT_THREAD_START:
      break;

    case EventRequest::EVENT_THREAD_END:
      break;

    case EventRequest::EVENT_CLASS_PREPARE:
      break;

    case EventRequest::EVENT_CLASS_LOAD:
      break;

    case EventRequest::EVENT_CLASS_UNLOAD:
      break;

    case EventRequest::EVENT_FIELD_ACCESS:
      break;

    case EventRequest::EVENT_FIELD_MODIFY:
      break;

    case EventRequest::EVENT_METHOD_ENTRY:
      break;

    case EventRequest::EVENT_METHOD_EXIT:
      break;

    case EventRequest::EVENT_VM_INIT:
      break;

    case EventRequest::EVENT_VM_DEATH:
      break;
    }
}

void
gnu::classpath::jdwp::VMVirtualMachine::clearEvents (MAYBE_UNUSED jbyte kind)
{
}

jint
gnu::classpath::jdwp::VMVirtualMachine::getAllLoadedClassesCount (void)
{
  return 0;
}

java::util::Iterator *
gnu::classpath::jdwp::VMVirtualMachine::getAllLoadedClasses (void)
{
  return NULL;
}

jint
gnu::classpath::jdwp::VMVirtualMachine::
getClassStatus (MAYBE_UNUSED jclass klass)
{
  return 0;
}

JArray<gnu::classpath::jdwp::VMMethod *> *
gnu::classpath::jdwp::VMVirtualMachine::
getAllClassMethods (jclass klass)
{
  jint count;
  jmethodID *methods;
  jvmtiError err = _jdwp_jvmtiEnv->GetClassMethods (klass, &count, &methods);
  if (err != JVMTI_ERROR_NONE)
    throw_jvmti_error (err);

  JArray<VMMethod *> *result
    = (JArray<VMMethod *> *) JvNewObjectArray (count,
					       &VMMethod::class$, NULL);
  VMMethod **rmeth = elements (result);
  for (int i = 0; i < count; ++i)
    {
      jlong id = reinterpret_cast<jlong> (methods[i]);
      rmeth[i] = getClassMethod (klass, id);
    }

  _jdwp_jvmtiEnv->Deallocate ((unsigned char *) methods);
  return result;
}

gnu::classpath::jdwp::VMMethod *
gnu::classpath::jdwp::VMVirtualMachine::
getClassMethod (jclass klass, jlong id)
{
  jmethodID method = reinterpret_cast<jmethodID> (id);
  _Jv_MethodBase *bmeth = _Jv_FindInterpreterMethod (klass, method);
  if (bmeth != NULL)
    return new gnu::classpath::jdwp::VMMethod (klass, id);

  throw new gnu::classpath::jdwp::exception::InvalidMethodException (id);
}

java::util::ArrayList *
gnu::classpath::jdwp::VMVirtualMachine::getFrames (MAYBE_UNUSED Thread *thread,
						   MAYBE_UNUSED jint start,
						   MAYBE_UNUSED jint length)
{
  return NULL;
}

gnu::classpath::jdwp::VMFrame *
gnu::classpath::jdwp::VMVirtualMachine::
getFrame (MAYBE_UNUSED Thread *thread, MAYBE_UNUSED::java::nio::ByteBuffer *bb)
{
  return NULL;
}

jint
gnu::classpath::jdwp::VMVirtualMachine::
getFrameCount (Thread *thread)
{
  jint frame_count;
  
  jvmtiError jerr = _jdwp_jvmtiEnv->GetFrameCount (thread, &frame_count);
  
  if (jerr != JVMTI_ERROR_NONE)
    throw_jvmti_error (jerr);
  
  return frame_count;
}

jint
gnu::classpath::jdwp::VMVirtualMachine::
getThreadStatus (MAYBE_UNUSED Thread *thread)
{
  return 0;
}

java::util::ArrayList *
gnu::classpath::jdwp::VMVirtualMachine::
getLoadRequests (MAYBE_UNUSED ClassLoader *cl)
{
  return NULL;
}

MethodResult *
gnu::classpath::jdwp::VMVirtualMachine::
executeMethod (MAYBE_UNUSED jobject obj, MAYBE_UNUSED Thread *thread,
	       MAYBE_UNUSED jclass clazz, MAYBE_UNUSED reflect::Method *method,
	       MAYBE_UNUSED jobjectArray values,
	       MAYBE_UNUSED jboolean nonVirtual)
{
  return NULL;
}

jstring
gnu::classpath::jdwp::VMVirtualMachine::
getSourceFile (MAYBE_UNUSED jclass clazz)
{
  return NULL;
}

static Location *
get_request_location (EventRequest *request)
{
  Location *loc = NULL;
  ::java::util::Collection *filters = request->getFilters ();
  ::java::util::Iterator *iter = filters->iterator ();
  while (iter->hasNext ())
    {
      using namespace gnu::classpath::jdwp::event::filters;
      IEventFilter *filter = (IEventFilter *) iter->next ();
      if (filter->getClass () == &LocationOnlyFilter::class$)
	{
	  LocationOnlyFilter *lof
	    = reinterpret_cast<LocationOnlyFilter *> (filter);
	  loc = lof->getLocation ();
	}
    }

  return loc;
}

static void
throw_jvmti_error (jvmtiError err)
{
  char *error;
  jstring msg;
  if (_jdwp_jvmtiEnv->GetErrorName (err, &error) == JVMTI_ERROR_NONE)
    {
      msg = JvNewStringLatin1 (error);
      _jdwp_jvmtiEnv->Deallocate ((unsigned char *) error);
    }
  else
    msg = JvNewStringLatin1 ("out of memory");

  using namespace gnu::classpath::jdwp::exception;
  throw new JdwpInternalErrorException (msg);
}

static void JNICALL
jdwpClassPrepareCB (jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env,
		    jthread thread, jclass klass)
{
  using namespace gnu::classpath::jdwp;

  jint flags = 0;
  jvmtiError err = env->GetClassStatus (klass, &flags);
  if (err != JVMTI_ERROR_NONE)
    throw_jvmti_error (err);

  using namespace gnu::classpath::jdwp::event;
  jint status = 0;
  if (flags & JVMTI_CLASS_STATUS_VERIFIED)
    status |= ClassPrepareEvent::STATUS_VERIFIED;
  if (flags & JVMTI_CLASS_STATUS_PREPARED)
    status |= ClassPrepareEvent::STATUS_PREPARED;
  if (flags & JVMTI_CLASS_STATUS_ERROR)
    status |= ClassPrepareEvent::STATUS_ERROR;
  if (flags & JVMTI_CLASS_STATUS_INITIALIZED)
    status |= ClassPrepareEvent::STATUS_INITIALIZED;

  event::ClassPrepareEvent *event
    = new event::ClassPrepareEvent (thread, klass, status);
  Jdwp::notify (event);
}

static void JNICALL
jdwpThreadEndCB (MAYBE_UNUSED jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env,
		 jthread thread)
{
  using namespace gnu::classpath::jdwp::event;

  ThreadEndEvent *e = new ThreadEndEvent (thread);
  gnu::classpath::jdwp::Jdwp::notify (e);
}

static void JNICALL
jdwpThreadStartCB (MAYBE_UNUSED jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env,
		   jthread thread)
{
  using namespace gnu::classpath::jdwp::event;

  ThreadStartEvent *e = new ThreadStartEvent (thread);
  gnu::classpath::jdwp::Jdwp::notify (e);
}

static void JNICALL
jdwpVMDeathCB (MAYBE_UNUSED jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env)
{
  using namespace gnu::classpath::jdwp::event;
  gnu::classpath::jdwp::Jdwp::notify (new VmDeathEvent ());
}

static void JNICALL
jdwpVMInitCB (MAYBE_UNUSED jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env,
	      jthread thread)
{
  // The VM is now initialized, add our callbacks
  jvmtiEventCallbacks callbacks;
  DEFINE_CALLBACK (callbacks, ClassPrepare);
  DEFINE_CALLBACK (callbacks, ThreadEnd);
  DEFINE_CALLBACK (callbacks, ThreadStart);
  DEFINE_CALLBACK (callbacks, VMDeath);
  _jdwp_jvmtiEnv->SetEventCallbacks (&callbacks, sizeof (callbacks));

  // Enable callbacks
  ENABLE_EVENT (CLASS_PREPARE, NULL);
  ENABLE_EVENT (THREAD_END, NULL);
  ENABLE_EVENT (THREAD_START, NULL);
  ENABLE_EVENT (VM_DEATH, NULL);

  // Send JDWP VMInit
  using namespace gnu::classpath::jdwp::event;
  gnu::classpath::jdwp::Jdwp::notify (new VmInitEvent (thread));
}
