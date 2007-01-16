// natVMVirtualMachine.cc - native support for VMVirtualMachine

/* Copyright (C) 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License. Please consult the file "LIBGCJ_LICENSE" for
details. */

#include <config.h>
#include <gcj/cni.h>
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
#include <java/util/Hashtable.h>
#include <java/util/Iterator.h>

#include <gnu/classpath/jdwp/Jdwp.h>
#include <gnu/classpath/jdwp/VMFrame.h>
#include <gnu/classpath/jdwp/VMMethod.h>
#include <gnu/classpath/jdwp/VMVirtualMachine.h>
#include <gnu/classpath/jdwp/event/EventRequest.h>
#include <gnu/classpath/jdwp/event/VmInitEvent.h>
#include <gnu/classpath/jdwp/exception/JdwpInternalErrorException.h>
#include <gnu/classpath/jdwp/util/MethodResult.h>

using namespace java::lang;
using namespace gnu::classpath::jdwp::event;
using namespace gnu::classpath::jdwp::util;

// Forward declarations
static void jdwpVMInitCB (jvmtiEnv *env, JNIEnv *jni_env, jthread thread);

#define DEFINE_CALLBACK(Cb,Event) Cb.Event = jdwp ## Event ## CB
#define ENABLE_EVENT(Event,Thread)					\
  _jdwp_jvmtiEnv->SetEventNotificationMode (JVMTI_ENABLE,		\
					    JVMTI_EVENT_ ## Event, Thread)
// JVMTI environment
static jvmtiEnv *_jdwp_jvmtiEnv;

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
gnu::classpath::jdwp::VMVirtualMachine ::suspendThread (Thread *thread)
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
gnu::classpath::jdwp::VMVirtualMachine::clearEvents (jbyte kind)
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
gnu::classpath::jdwp::VMVirtualMachine::getClassStatus (jclass klass)
{
  return 0;
}

JArray<gnu::classpath::jdwp::VMMethod *> *
gnu::classpath::jdwp::VMVirtualMachine::getAllClassMethods (jclass klass)
{
  return NULL;
}

gnu::classpath::jdwp::VMMethod *
gnu::classpath::jdwp::VMVirtualMachine::getClassMethod (jclass klass, jlong id)
{
  return NULL;
}

java::util::ArrayList *
gnu::classpath::jdwp::VMVirtualMachine::getFrames (Thread *thread,
						   jint start,
						   jint length)
{
  return NULL;
}

gnu::classpath::jdwp::VMFrame *
gnu::classpath::jdwp::VMVirtualMachine::getFrame (Thread *thread,
						  ::java::nio::ByteBuffer *bb)
{
  return NULL;
}

jint
gnu::classpath::jdwp::VMVirtualMachine::getFrameCount (Thread *thread)
{
  return 0;
}

jint
gnu::classpath::jdwp::VMVirtualMachine::getThreadStatus (Thread *thread)
{
  return 0;
}

java::util::ArrayList *
gnu::classpath::jdwp::VMVirtualMachine::getLoadRequests (ClassLoader *cl)
{
  return NULL;
}

MethodResult *
gnu::classpath::jdwp::VMVirtualMachine::executeMethod (jobject obj,
						       Thread *thread,
						       jclass clazz,
						       reflect::Method *method,
						       jobjectArray values,
						       jboolean nonVirtual)
{
  return NULL;
}

jstring
gnu::classpath::jdwp::VMVirtualMachine::getSourceFile (jclass clazz)
{
  return NULL;
}

static void
jdwpVMInitCB (MAYBE_UNUSED jvmtiEnv *env, MAYBE_UNUSED JNIEnv *jni_env,
	      jthread thread)
{
  // Send JDWP VMInit
  using namespace gnu::classpath::jdwp::event;
  Thread *init_thread = reinterpret_cast<Thread *> (thread);
  gnu::classpath::jdwp::Jdwp::notify (new VmInitEvent (init_thread));
}
