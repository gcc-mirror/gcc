// natVMVirtualMachine.cc - native support for VMVirtualMachine

/* Copyright (C) 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License. Please consult the file "LIBGCJ_LICENSE" for
details. */

#include <config.h>
#include <gcj/cni.h>
#include <java-assert.h>
#include <java-interp.h>
#include <jvm.h>
#include <jvmti.h>

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Integer.h>
#include <java/lang/String.h>
#include <java/lang/StringBuilder.h>
#include <java/lang/Thread.h>
#include <java/lang/Throwable.h>
#include <java/nio/ByteBuffer.h>
#include <java/nio/ByteBufferImpl.h>
#include <java/util/ArrayList.h>
#include <java/util/Collection.h>
#include <java/util/Hashtable.h>
#include <java/util/Iterator.h>

#include <gnu/classpath/jdwp/Jdwp.h>
#include <gnu/classpath/jdwp/JdwpConstants$StepDepth.h>
#include <gnu/classpath/jdwp/JdwpConstants$StepSize.h>
#include <gnu/classpath/jdwp/JdwpConstants$ThreadStatus.h>
#include <gnu/classpath/jdwp/VMFrame.h>
#include <gnu/classpath/jdwp/VMMethod.h>
#include <gnu/classpath/jdwp/VMVirtualMachine.h>
#include <gnu/classpath/jdwp/event/BreakpointEvent.h>
#include <gnu/classpath/jdwp/event/ClassPrepareEvent.h>
#include <gnu/classpath/jdwp/event/ExceptionEvent.h>
#include <gnu/classpath/jdwp/event/EventManager.h>
#include <gnu/classpath/jdwp/event/EventRequest.h>
#include <gnu/classpath/jdwp/event/SingleStepEvent.h>
#include <gnu/classpath/jdwp/event/ThreadEndEvent.h>
#include <gnu/classpath/jdwp/event/ThreadStartEvent.h>
#include <gnu/classpath/jdwp/event/VmDeathEvent.h>
#include <gnu/classpath/jdwp/event/VmInitEvent.h>
#include <gnu/classpath/jdwp/event/filters/IEventFilter.h>
#include <gnu/classpath/jdwp/event/filters/LocationOnlyFilter.h>
#include <gnu/classpath/jdwp/event/filters/StepFilter.h>
#include <gnu/classpath/jdwp/exception/AbsentInformationException.h>
#include <gnu/classpath/jdwp/exception/InvalidFrameException.h>
#include <gnu/classpath/jdwp/exception/InvalidLocationException.h>
#include <gnu/classpath/jdwp/exception/InvalidMethodException.h>
#include <gnu/classpath/jdwp/exception/JdwpInternalErrorException.h>
#include <gnu/classpath/jdwp/id/ThreadId.h>
#include <gnu/classpath/jdwp/util/Location.h>
#include <gnu/classpath/jdwp/util/MethodResult.h>
#include <gnu/gcj/jvmti/Breakpoint.h>
#include <gnu/gcj/jvmti/BreakpointManager.h>

using namespace java::lang;
using namespace gnu::classpath::jdwp::event;
using namespace gnu::classpath::jdwp::util;

// Stepping information
struct step_info
{
  jint size;   // See gnu.classpath.jdwp.JdwpConstants.StepSize
  jint depth;  // See gnu.classpath.jdwp.JdwpConstants.StepDepth
  int stack_depth;  // stack depth at start of stepping
  jmethodID method; // method in which we are stepping
};

// Forward declarations
static jvmtiError get_linetable (jvmtiEnv *, jmethodID, jint *,
				 jvmtiLineNumberEntry **);
static Location *get_request_location (EventRequest *);
static gnu::classpath::jdwp::event::filters::StepFilter *
get_request_step_filter (EventRequest *);
static void handle_single_step (jvmtiEnv *, struct step_info *, jthread,
				jmethodID, jlocation);
static void JNICALL jdwpBreakpointCB (jvmtiEnv *, JNIEnv *, jthread,
				      jmethodID, jlocation);
static void JNICALL jdwpClassPrepareCB (jvmtiEnv *, JNIEnv *, jthread, jclass);
static void JNICALL jdwpExceptionCB (jvmtiEnv *, JNIEnv *jni_env, jthread,
				     jmethodID, jlocation, jobject,
				     jmethodID, jlocation);
static void JNICALL jdwpSingleStepCB (jvmtiEnv *, JNIEnv *, jthread,
				      jmethodID, jlocation);
static void JNICALL jdwpThreadEndCB (jvmtiEnv *, JNIEnv *, jthread);
static void JNICALL jdwpThreadStartCB (jvmtiEnv *, JNIEnv *, jthread);
static void JNICALL jdwpVMDeathCB (jvmtiEnv *, JNIEnv *);
static void JNICALL jdwpVMInitCB (jvmtiEnv *, JNIEnv *, jthread);
static void throw_jvmti_error (jvmtiError);

#define DEFINE_CALLBACK(Cb,Event) Cb.Event = jdwp ## Event ## CB
#define DISABLE_EVENT(Event,Thread)					\
  _jdwp_jvmtiEnv->SetEventNotificationMode (JVMTI_DISABLE,		\
					    JVMTI_EVENT_ ## Event, Thread)
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
  _stepping_threads = new ::java::util::Hashtable ();
  _event_list = new ::java::util::ArrayList ();

  JavaVM *vm = _Jv_GetJavaVM ();
  union
  {
    void *ptr;
    jvmtiEnv *env;
  } foo;
  vm->GetEnv (&(foo.ptr), JVMTI_VERSION_1_0);
  _jdwp_jvmtiEnv = foo.env;

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
      {
	Thread *thread;
	filters::StepFilter *filter = get_request_step_filter (request);
	if (filter == NULL)
	  {
	    // No filter specified: report every step in every
	    // thread.
	    thread = NULL;
	  }
	else
	  {
	    // Add stepping information to list of stepping threads
	    thread = filter->getThread ()->getThread ();
	    _Jv_InterpFrame *frame
	      = reinterpret_cast<_Jv_InterpFrame *> (thread->interp_frame);
	    struct step_info *sinfo
	      = (struct step_info *) JvAllocBytes (sizeof (struct step_info));
	    sinfo->size = filter->getSize ();
	    sinfo->depth = filter->getDepth ();
	    sinfo->stack_depth = frame->depth ();
	    sinfo->method = frame->self->get_method ();
	    _stepping_threads->put (thread, (jobject) sinfo);
	  }

	ENABLE_EVENT (SINGLE_STEP, thread);
      }
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
      {
	Thread *thread;
	filters::StepFilter *filter = get_request_step_filter (request);
	if (filter == NULL)
	  thread = NULL;
	else
	  {
	    thread = filter->getThread ()->getThread ();
	    _stepping_threads->remove (thread);
	  }

	DISABLE_EVENT (SINGLE_STEP, thread);
      }
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

java::util::Collection *
gnu::classpath::jdwp::VMVirtualMachine::getAllLoadedClasses (void)
{
  using namespace ::java::util;
  return (Collection *) new ArrayList ();
}

jint
gnu::classpath::jdwp::VMVirtualMachine::
getClassStatus (jclass klass)
{
  jint flags = 0;
  jvmtiError err = _jdwp_jvmtiEnv->GetClassStatus (klass, &flags);
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

  return status;
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
  jint count;
  jmethodID *methods;
  jvmtiError err = _jdwp_jvmtiEnv->GetClassMethods (klass, &count, &methods);
  if (err != JVMTI_ERROR_NONE)
    throw_jvmti_error (err);

  jmethodID meth_id = reinterpret_cast<jmethodID> (id);

  using namespace gnu::classpath::jdwp;

  // Check if this method is defined for the given class and if so return a
  // VMMethod representing it.
  for (int i = 0; i < count; i++)
    {
      if (methods[i] == meth_id)
        return new VMMethod (klass, reinterpret_cast<jlong> (meth_id));
    }

  throw new exception::InvalidMethodException (id);
}

java::util::ArrayList *
gnu::classpath::jdwp::VMVirtualMachine::getFrames (Thread *thread, jint start,
                                                   jint length)
{
  jint frame_count = getFrameCount (thread);
  ::java::util::ArrayList *frame_list;
  
  // Calculate the max number of frames to be returned.
  jint num_frames = frame_count - start;
  
  // Check if num_frames is valid.
  if (num_frames < 0)
    num_frames = 0;
  
  // Check if there are more than length frames left after start.
  // If length ios -1 return all remaining frames.
  if (length != -1 && num_frames > length)
    num_frames = length;
     
  frame_list = new ::java::util::ArrayList (num_frames);
  
  _Jv_Frame *vm_frame = reinterpret_cast<_Jv_Frame *> (thread->frame);
  
  // Take start frames off the top of the stack
  while (vm_frame != NULL && start > 0)
    {
      start--;
      vm_frame = vm_frame->next;
    }
  
  // Use as a counter for the number of frames returned.
  num_frames = 0;
  
  while (vm_frame != NULL && (num_frames < length || length == -1))
    {  
      jlong frameId = reinterpret_cast<jlong> (vm_frame);
      
      VMFrame *frame = getFrame (thread, frameId);
      frame_list->add (frame);
      vm_frame = vm_frame->next;
      num_frames++;
    }
  
  return frame_list;
}

gnu::classpath::jdwp::VMFrame *
gnu::classpath::jdwp::VMVirtualMachine::
getFrame (Thread *thread, jlong frameID)
{
  using namespace gnu::classpath::jdwp::exception;
  
  _Jv_Frame *vm_frame = (_Jv_Frame *) thread->frame;
  jint depth = 0;
  _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (frameID); 
  
  // We need to find the stack depth of the frame, so search through the call
  // stack to find it.  This also checks for a valid frameID.
  while (vm_frame != frame)
    {
      vm_frame = vm_frame->next;
      depth++;
      if (vm_frame == NULL)
        throw new InvalidFrameException (frameID);
    }
  
  Location *loc = NULL;
  jvmtiFrameInfo info;
  jvmtiError jerr;
  jint num_frames;
  jclass klass;
  
  // Get the info for the frame of interest
  jerr = _jdwp_jvmtiEnv->GetStackTrace (thread, depth, 1, &info, &num_frames);
   
  if (jerr != JVMTI_ERROR_NONE)
    throw_jvmti_error (jerr);
  
  jerr = _jdwp_jvmtiEnv->GetMethodDeclaringClass (info.method, &klass);
      
  if (jerr != JVMTI_ERROR_NONE)
    throw_jvmti_error (jerr);

  VMMethod *meth 
    = getClassMethod (klass, reinterpret_cast<jlong> (info.method));
  
  jobject this_obj;
  
  if (info.location == -1)
    {
      loc = new Location (meth, 0);
      this_obj = NULL;
    }
  else
    {
      loc = new Location (meth, info.location);
      _Jv_InterpFrame *iframe = reinterpret_cast<_Jv_InterpFrame *> (vm_frame);
      this_obj = iframe->get_this_ptr ();
    }
  
  return new VMFrame (thread, reinterpret_cast<jlong> (vm_frame), loc,
                      this_obj); 
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
getThreadStatus (Thread *thread)
{
  jint thr_state, status;
  
  jvmtiError jerr = _jdwp_jvmtiEnv->GetThreadState (thread, &thr_state);
  if (jerr != JVMTI_ERROR_NONE)
    throw_jvmti_error (jerr);
  
  if (thr_state & JVMTI_THREAD_STATE_SLEEPING)
    status = gnu::classpath::jdwp::JdwpConstants$ThreadStatus::SLEEPING;
  else if (thr_state & JVMTI_THREAD_STATE_RUNNABLE)
    status = gnu::classpath::jdwp::JdwpConstants$ThreadStatus::RUNNING;
  else if (thr_state & JVMTI_THREAD_STATE_WAITING)
    {
      if (thr_state & (JVMTI_THREAD_STATE_IN_OBJECT_WAIT
                       | JVMTI_THREAD_STATE_BLOCKED_ON_MONITOR_ENTER))
        status = gnu::classpath::jdwp::JdwpConstants$ThreadStatus::MONITOR;
      else
        status = gnu::classpath::jdwp::JdwpConstants$ThreadStatus::WAIT;
    }
  else
    {
      // The thread is not SLEEPING, MONITOR, or WAIT.  It may, however, be
      // alive but not yet started.
      if (!(thr_state & (JVMTI_THREAD_STATE_ALIVE 
                         | JVMTI_THREAD_STATE_TERMINATED)))
        status = gnu::classpath::jdwp::JdwpConstants$ThreadStatus::RUNNING;
      status = gnu::classpath::jdwp::JdwpConstants$ThreadStatus::ZOMBIE;     
    }   

  return status;
}

java::util::ArrayList *
gnu::classpath::jdwp::VMVirtualMachine::
getLoadRequests (MAYBE_UNUSED ClassLoader *cl)
{
  return new ::java::util::ArrayList ();
}

MethodResult *
gnu::classpath::jdwp::VMVirtualMachine::
executeMethod (MAYBE_UNUSED jobject obj, MAYBE_UNUSED Thread *thread,
	       MAYBE_UNUSED jclass clazz, MAYBE_UNUSED VMMethod *method,
	       MAYBE_UNUSED JArray<value::Value *> *values,
	       MAYBE_UNUSED jint options)
{
  return NULL;
}

jstring
gnu::classpath::jdwp::VMVirtualMachine::
getSourceFile (jclass clazz)
{
  jstring file = _Jv_GetInterpClassSourceFile (clazz);
  
  // Check if the source file was found.
  if (file == NULL)
    throw new exception::AbsentInformationException (
                           _Jv_NewStringUTF("Source file not found"));
  
  return file;
}

void
gnu::classpath::jdwp::VMVirtualMachine::
redefineClasses (MAYBE_UNUSED JArray<jclass> *types,
		 MAYBE_UNUSED JArray<jbyteArray> *bytecodes)
{
}

void
gnu::classpath::jdwp::VMVirtualMachine::
setDefaultStratum (MAYBE_UNUSED jstring stratum)
{
}

jstring
gnu::classpath::jdwp::VMVirtualMachine::
getSourceDebugExtension (MAYBE_UNUSED jclass klass)
{
  return NULL;
}

jbyteArray
gnu::classpath::jdwp::VMVirtualMachine::
getBytecodes (MAYBE_UNUSED gnu::classpath::jdwp::VMMethod *method)
{
  return NULL;
}

gnu::classpath::jdwp::util::MonitorInfo *
gnu::classpath::jdwp::VMVirtualMachine::
getMonitorInfo (MAYBE_UNUSED jobject obj)
{
  return NULL;
}

jobjectArray
gnu::classpath::jdwp::VMVirtualMachine::
getOwnedMonitors (MAYBE_UNUSED ::java::lang::Thread *thread)
{
  return NULL;
}

jobject
gnu::classpath::jdwp::VMVirtualMachine::
getCurrentContendedMonitor (MAYBE_UNUSED ::java::lang::Thread *thread)
{
  return NULL;
}

void
gnu::classpath::jdwp::VMVirtualMachine::
popFrames (MAYBE_UNUSED ::java::lang::Thread *thread,
	   MAYBE_UNUSED jlong frameId)
{
}

// A simple caching function used while single-stepping
static jvmtiError
get_linetable (jvmtiEnv *env, jmethodID method, jint *count_ptr,
	       jvmtiLineNumberEntry **table_ptr)
{
  static jint last_count = 0;
  static jvmtiLineNumberEntry *last_table = NULL;
  static jmethodID last_method = 0;

  if (method == last_method)
    {
      *count_ptr = last_count;
      *table_ptr = last_table;
      return JVMTI_ERROR_NONE;
    }

  jvmtiError err;
  jint count;
  jvmtiLineNumberEntry *table;
  err = env->GetLineNumberTable (method, &count, &table);
  if (err != JVMTI_ERROR_NONE)
    {
      // Keep last table in cache
      return err;
    }

  env->Deallocate ((unsigned char *) last_table);
  last_table = *table_ptr = table;
  last_count = *count_ptr = count;
  return JVMTI_ERROR_NONE;
}

static gnu::classpath::jdwp::event::filters::StepFilter *
get_request_step_filter (EventRequest *request)
{
  ::java::util::Collection *filters = request->getFilters ();
  ::java::util::Iterator *iter = filters->iterator ();
  filters::StepFilter *filter = NULL;
  while (iter->hasNext ())
    {
      using namespace gnu::classpath::jdwp::event::filters;
      IEventFilter *next = (IEventFilter *) iter->next ();
      if (next->getClass () == &StepFilter::class$)
	{
	  filter = reinterpret_cast<StepFilter *> (next);
	  break;
	}
    }

  return filter;
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
handle_single_step (jvmtiEnv *env, struct step_info *sinfo, jthread thread,
		    jmethodID method, jlocation location)
{
  using namespace gnu::classpath::jdwp;

  if (sinfo == NULL || sinfo->size == JdwpConstants$StepSize::MIN)
    {
      // Stop now
      goto send_notification;
    }
  else
    {
      // Check if we're on a new source line
      /* This is a little inefficient when we're stepping OVER,
	 but this must be done when stepping INTO. */
      jint count;
      jvmtiLineNumberEntry *table;
      if (get_linetable (env, method, &count, &table) == JVMTI_ERROR_NONE)
	{
	  jint i;
	  for (i = 0; i < count; ++i)
	    {
	      if (table[i].start_location == location)
		{
		  // This is the start of a new line -- stop
		  goto send_notification;
		}
	    }

	  // Not at a new source line -- just keep stepping
	  return;
	}
      else
	{
	  /* Something went wrong: either "absent information"
	     or "out of memory" ("invalid method id" and "native
	     method" aren't possible -- those are validated before
	     single stepping is enabled).

	     Do what gdb does: just keep going. */
	  return;
	}
    }

 send_notification:
  jclass klass;
  jvmtiError err = env->GetMethodDeclaringClass (method, &klass);
  if (err != JVMTI_ERROR_NONE)
    {
      fprintf (stderr, "libgcj: internal error: could not find class for method while single stepping -- continuing\n");
      return;
    }

  VMMethod *vmmethod = new VMMethod (klass, reinterpret_cast<jlong> (method));
  Location *loc = new Location (vmmethod, location);
  JvAssert (thread->frame.frame_type == frame_interpreter);
  _Jv_InterpFrame *iframe
    = reinterpret_cast<_Jv_InterpFrame *> (thread->interp_frame);  
  jobject instance = iframe->get_this_ptr ();
  event::SingleStepEvent *event
    = new event::SingleStepEvent (thread, loc, instance);

  // We only want to send the notification (and consequently
  // suspend) if we are not about to execute a breakpoint.
  _Jv_InterpMethod *im = reinterpret_cast<_Jv_InterpMethod *> (iframe->self);
  if (im->breakpoint_at (location))
    {
      // Next insn is a breakpoint -- record event and
      // wait for the JVMTI breakpoint notification to
      // enforce a suspension policy.
      VMVirtualMachine::_event_list->add (event);
    }
  else
    {
      // Next insn is not a breakpoint, so send notification
      // and enforce the suspend policy.
      Jdwp::notify (event);
    }
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
jdwpBreakpointCB (jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env,
		  jthread thread, jmethodID method, jlocation location)
{
  jclass klass;
  jvmtiError err;
  err = env->GetMethodDeclaringClass (method, &klass);
  JvAssert (err == JVMTI_ERROR_NONE);

  using namespace gnu::classpath::jdwp;
  using namespace gnu::classpath::jdwp::event;

  jlong methodId = reinterpret_cast<jlong> (method);
  VMMethod *meth = VMVirtualMachine::getClassMethod (klass, methodId);
  Location *loc = new Location (meth, location);
  JvAssert (thread->frame.frame_type == frame_interpreter);
  _Jv_InterpFrame *iframe
    = reinterpret_cast<_Jv_InterpFrame *> (thread->interp_frame);
  jobject instance = iframe->get_this_ptr ();
  BreakpointEvent *event = new BreakpointEvent (thread, loc, instance);
  
  VMVirtualMachine::_event_list->add (event);
  JArray<Event *> *events
    = ((JArray<Event *> *)
       JvNewObjectArray (VMVirtualMachine::_event_list->size (),
			 &Event::class$, NULL));
  VMVirtualMachine::_event_list->toArray ((jobjectArray) events);
  VMVirtualMachine::_event_list->clear ();
  Jdwp::notify (events);
}

static void JNICALL
jdwpClassPrepareCB (MAYBE_UNUSED jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env,
		    jthread thread, jclass klass)
{
  using namespace gnu::classpath::jdwp;

  jint status = VMVirtualMachine::getClassStatus (klass);
  event::ClassPrepareEvent *event
    = new event::ClassPrepareEvent (thread, klass, status);
  Jdwp::notify (event);
}

static void JNICALL
jdwpExceptionCB (jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env, jthread thread,
		 jmethodID method, jlocation location, jobject exception,
		 jmethodID catch_method, jlocation catch_location)
{
  using namespace gnu::classpath::jdwp;
  jclass throw_klass;
  jvmtiError err = env->GetMethodDeclaringClass (method, &throw_klass);
  if (err != JVMTI_ERROR_NONE)
    {
      fprintf (stderr, "libgcj: internal error: could not find class for ");
      fprintf (stderr, "method throwing exception -- continuing\n");
      return;
    }

  VMMethod *vmmethod = new VMMethod (throw_klass,
				     reinterpret_cast<jlong> (method));
  Location *throw_loc = new Location (vmmethod, location);  
  Location *catch_loc = NULL;
  if (catch_method == 0)
    catch_loc = Location::getEmptyLocation ();
  else
    {
      jclass catch_klass;
      err = env->GetMethodDeclaringClass (catch_method, &catch_klass);
      if (err != JVMTI_ERROR_NONE)
	{
	  fprintf (stderr,
		   "libgcj: internal error: could not find class for ");
	  fprintf (stderr,
		   "method catching exception -- ignoring\n");
	}
      else
	{
	  vmmethod = new VMMethod (catch_klass,
				   reinterpret_cast<jlong> (catch_method));
	  catch_loc = new Location (vmmethod, catch_location);
	}
    }

  _Jv_InterpFrame *iframe
    = reinterpret_cast<_Jv_InterpFrame *> (thread->interp_frame);
  jobject instance = (iframe == NULL) ? NULL : iframe->get_this_ptr ();
  Throwable *throwable = reinterpret_cast<Throwable *> (exception);
  event::ExceptionEvent *e = new ExceptionEvent (throwable, thread,
						 throw_loc, catch_loc,
						 throw_klass, instance);
  Jdwp::notify (e);
}

static void JNICALL
jdwpSingleStepCB (jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env, jthread thread,
		  jmethodID method, jlocation location)
{
  jobject si =
    gnu::classpath::jdwp::VMVirtualMachine::_stepping_threads->get (thread);
  struct step_info *sinfo = reinterpret_cast<struct step_info *> (si);

  if (sinfo == NULL)
    {
      // no step filter for this thread - simply report it
      handle_single_step (env, NULL, thread, method, location);
    }
  else
    {
      // A step filter exists for this thread
      using namespace gnu::classpath::jdwp;

      _Jv_InterpFrame *frame
	= reinterpret_cast<_Jv_InterpFrame *> (thread->interp_frame);

      switch (sinfo->depth)
	{
	case JdwpConstants$StepDepth::INTO:
	  /* This is the easy case. We ignore the method and
	     simply stop at either the next insn, or the next source
	     line. */
	  handle_single_step (env, sinfo, thread, method, location);
	  break;

	case JdwpConstants$StepDepth::OVER:
	  /* This is also a pretty easy case. We just make sure that
	     the methods are the same and that we are at the same
	     stack depth, but we should also stop on the next
	     insn/line if the stack depth is LESS THAN it was when
	     we started stepping. */
	  if (method == sinfo->method)
	    {
	      // Still in the same method -- must be at same stack depth
	      // to avoid confusion with recursive methods.
	      if (frame->depth () == sinfo->stack_depth)
		handle_single_step (env, sinfo, thread, method, location);
	    }
	  else if (frame->depth () < sinfo->stack_depth)
	    {
	      // The method in which we were stepping was popped off
	      // the stack. We simply need to stop at the next insn/line.
	      handle_single_step (env, sinfo, thread, method, location);
	    }
	  break;

	case JdwpConstants$StepDepth::OUT:
	  // All we need to do is check the stack depth
	  if (sinfo->stack_depth > frame->depth ())
	    handle_single_step (env, sinfo, thread, method, location);
	  break;

	default:
	  /* This should not happen. The JDWP back-end should have
	     validated the StepFilter. */
	  fprintf (stderr,
		   "libgcj: unknown step depth while single stepping\n");
	  return;
	}
    }
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
  DEFINE_CALLBACK (callbacks, Breakpoint);
  DEFINE_CALLBACK (callbacks, ClassPrepare);
  DEFINE_CALLBACK (callbacks, Exception);
  DEFINE_CALLBACK (callbacks, SingleStep);
  DEFINE_CALLBACK (callbacks, ThreadEnd);
  DEFINE_CALLBACK (callbacks, ThreadStart);
  DEFINE_CALLBACK (callbacks, VMDeath);
  _jdwp_jvmtiEnv->SetEventCallbacks (&callbacks, sizeof (callbacks));

  // Enable callbacks
  ENABLE_EVENT (BREAKPOINT, NULL);
  ENABLE_EVENT (CLASS_PREPARE, NULL);
  ENABLE_EVENT (EXCEPTION, NULL);
  // SingleStep is enabled only when needed
  ENABLE_EVENT (THREAD_END, NULL);
  ENABLE_EVENT (THREAD_START, NULL);
  ENABLE_EVENT (VM_DEATH, NULL);

  // Send JDWP VMInit
  using namespace gnu::classpath::jdwp::event;
  gnu::classpath::jdwp::Jdwp::notify (new VmInitEvent (thread));
}
