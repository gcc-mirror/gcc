// natVMVirtualMachine.cc - native support for VMVirtualMachine

/* Copyright (C) 2006 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License. Please consult the file "LIBGCJ_LICENSE" for
details. */

#include <config.h>
#include <gcj/cni.h>

#include <java/lang/Class.h>
#include <java/lang/ClassLoader.h>
#include <java/lang/Thread.h>
#include <java/nio/ByteBuffer.h>
#include <java/util/ArrayList.h>
#include <java/util/Iterator.h>

#include <gnu/classpath/jdwp/VMFrame.h>
#include <gnu/classpath/jdwp/VMMethod.h>
#include <gnu/classpath/jdwp/VMVirtualMachine.h>
#include <gnu/classpath/jdwp/event/EventRequest.h>
#include <gnu/classpath/jdwp/util/MethodResult.h>

using namespace java::lang;
using namespace gnu::classpath::jdwp::event;
using namespace gnu::classpath::jdwp::util;

void
gnu::classpath::jdwp::VMVirtualMachine ::suspendThread (Thread *thread)
{
}

void
gnu::classpath::jdwp::VMVirtualMachine::resumeThread (Thread *thread)
{
}

jint
gnu::classpath::jdwp::VMVirtualMachine::getSuspendCount (Thread *thread)
{
  return 0;
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
