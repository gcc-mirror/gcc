// natFrame.cc -- native support for VMFrame.java

/* Copyright (C) 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License. Please consult the file "LIBGCJ_LICENSE" for
details. */

#include <config.h>
#include <gcj/cni.h>
#include <jvm.h>
#include <jvmti.h>
#include "jvmti-int.h"

#include <java-interp.h>

#include <gnu/classpath/jdwp/VMFrame.h>
#include <gnu/classpath/jdwp/VMVirtualMachine.h>
#include <gnu/classpath/jdwp/exception/InvalidFrameException.h>
#include <gnu/classpath/jdwp/exception/InvalidSlotException.h>
#include <gnu/classpath/jdwp/exception/InvalidThreadException.h>
#include <gnu/classpath/jdwp/exception/JdwpInternalErrorException.h>
#include <gnu/classpath/jdwp/exception/TypeMismatchException.h>
#include <gnu/classpath/jdwp/util/NullObject.h>
#include <gnu/classpath/jdwp/value/ArrayValue.h>
#include <gnu/classpath/jdwp/value/ByteValue.h>
#include <gnu/classpath/jdwp/value/BooleanValue.h>
#include <gnu/classpath/jdwp/value/CharValue.h>
#include <gnu/classpath/jdwp/value/DoubleValue.h>
#include <gnu/classpath/jdwp/value/FloatValue.h>
#include <gnu/classpath/jdwp/value/IntValue.h>
#include <gnu/classpath/jdwp/value/LongValue.h>
#include <gnu/classpath/jdwp/value/ObjectValue.h>
#include <gnu/classpath/jdwp/value/ShortValue.h>
#include <gnu/classpath/jdwp/value/Value.h>
#include <gnu/classpath/jdwp/value/VoidValue.h>

using namespace java::lang;
using namespace gnu::classpath::jdwp;
using namespace gnu::classpath::jdwp::exception;


// All the jvmti GetLocalXX and SetLocalXX functions return the same potential
// errors, so this function handles them all and throws the appropriate JDWP
// exception.
static void
checkJVMTIError (jvmtiEnv *env, jthread thread, jvmtiError jerr, jint slot,
                 jbyte sig)
{
  if (jerr != JVMTI_ERROR_NONE)
    {
      char *error;
      env->GetErrorName (jerr, &error);
      String *msg = reinterpret_cast<String *> (JvNewStringUTF (error));
      env->Deallocate ((unsigned char *) error);
      
      if (jerr == JVMTI_ERROR_INVALID_THREAD)
        throw new InvalidThreadException ((jlong) thread);
      else if (jerr == JVMTI_ERROR_NO_MORE_FRAMES)
        throw new InvalidFrameException (msg);
      else if (jerr == JVMTI_ERROR_INVALID_SLOT)
        throw new InvalidSlotException (slot);
      else if (jerr == JVMTI_ERROR_TYPE_MISMATCH)
        throw new TypeMismatchException (sig);
      else
        throw new JdwpInternalErrorException (msg);
    }
}


static jobject
getObjectJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, jbyte sig)
{
  jobject value;
  jvmtiError jerr = env->GetLocalObject (thread, depth, slot, &value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
  
  return value;
}

static void
setObjectJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth,
                jbyte sig, jobject value)
{
  if (value->getClass ()->isAssignableFrom (&util::NullObject::class$))
    value = NULL;
	
  jvmtiError jerr = env->SetLocalObject (thread, depth, slot, value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
}

static jint
getIntJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, jbyte sig)
{
  jint value;
  jvmtiError jerr = env->GetLocalInt (thread, depth, slot, &value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
  return value;
}

static void
setIntJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, jbyte sig,
             jint value)
{
  jvmtiError jerr = env->SetLocalInt (thread, depth, slot, value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
}

static jlong
getLongJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, jbyte sig)
{
  jlong value;
  jvmtiError jerr = env->GetLocalLong (thread, depth, slot, &value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
  
  return value;
}

static void
setLongJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, jbyte sig,
              jlong value)
{
  jvmtiError jerr = env->SetLocalLong (thread, depth, slot, value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
}

static jfloat
getFloatJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, jbyte sig)
{
  jfloat value;
  jvmtiError jerr = env->GetLocalFloat (thread, depth, slot, &value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
  
  return value;
}

static void
setFloatJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, jbyte sig,
               jfloat value)
{
  jvmtiError jerr = env->SetLocalFloat (thread, depth, slot, value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
}

static jdouble
getDoubleJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth,
                jbyte sig)
{
  jdouble value;
  jvmtiError jerr = env->GetLocalDouble (thread, depth, slot, &value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
  
  return value;
}

static void
setDoubleJVMTI (jvmtiEnv *env, jthread thread, jint slot, jint depth, 
                jbyte sig, jdouble value)
{
  jvmtiError jerr = env->SetLocalDouble (thread, depth, slot, value);
  
  checkJVMTIError (env, thread, jerr, slot, sig);
}

// This is necessary since JVMTI requires a stack depth as a parameter in all
// its local variable functions.  Since JDWP needs frameids, we have to run
// through the call stack to translate these ids into the parameters JVMTI
// wants.
static jint
getFrameDepth (_Jv_Frame *frame)
{
  jint depth = 0;
  _Jv_Frame *top_frame = (_Jv_Frame *) frame->thread->frame;
  jint num_frames = VMVirtualMachine::getFrameCount (frame->thread);
  
  while (frame != top_frame)
    {
      top_frame = top_frame->next;
      depth++;
      
      if (depth >= num_frames || top_frame == NULL)
        throw new InvalidFrameException ((jlong) frame);
    }
  
  return depth;
}

using namespace gnu::classpath::jdwp::value;

Value *
gnu::classpath::jdwp::VMFrame::getValue (jint slot, jbyte sig)
{
  _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (id);
  jint depth = getFrameDepth (frame);
  jthread thread = reinterpret_cast<jthread> (frame->thread);
  jvmtiEnv *env = _Jv_GetJDWP_JVMTIEnv ();
  
  Value *value = NULL;

  switch (sig)
    {
    case 'B':
      value = new ByteValue ((jbyte) getIntJVMTI (env, thread, slot, depth, 
                                                  sig));
      break;
    case 'Z':
      value = new BooleanValue ((jboolean) getIntJVMTI (env, thread, slot,
                                                        depth, sig));
      break;
    case 'C':
      value = new CharValue ((jchar) getIntJVMTI (env, thread, slot, depth,
                                                  sig));
      break;
    case 'S':
      value = new ShortValue ((jshort) getIntJVMTI (env, thread, slot, depth,
                                                    sig));
      break;
    case 'I':
      value = new IntValue (getIntJVMTI (env, thread, slot, depth, sig));
      break;
    case 'J':
      value = new LongValue (getLongJVMTI (env, thread, slot, depth, sig));
      break;
    case 'F':
      value = new FloatValue (getFloatJVMTI (env, thread, slot, depth, sig));
      break;
    case 'D':
      value = new DoubleValue (getDoubleJVMTI (env, thread, slot, depth, sig));
      break;
    case 'V':
      value = new VoidValue ();
      break;
    case '[':
      {
        Object *obj = getObjectJVMTI (env, thread, slot, depth, sig);
        if (obj == NULL)
          obj = new util::NullObject ();
        value = new ArrayValue (obj);
        break;
      }
    default:
      Object *obj = getObjectJVMTI (env, thread, slot, depth, sig);
      if (obj == NULL)
        obj = new util::NullObject ();
      value = new ObjectValue (obj);
      break;
    }
  
  return value;
}

void
gnu::classpath::jdwp::VMFrame::setValue (jint slot, Value* value)
{	
  jbyte sig = value->getTag ();
  
  _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (id);
  jint depth = getFrameDepth (frame);
  jthread thread = reinterpret_cast<jthread> (frame->thread);
  jvmtiEnv *env = _Jv_GetJDWP_JVMTIEnv ();
  
  switch (sig)
    {
    case 'B':
      {
        ByteValue *val = reinterpret_cast<ByteValue *> (value);
        setIntJVMTI (env, thread, slot, depth, sig, (jint) val->getValue ());
        break;
      }
    case 'Z':
      {
        BooleanValue *val = reinterpret_cast<BooleanValue *> (value);
        setIntJVMTI (env, thread, slot, depth, sig, (jint) val->getValue ());
        break;
      }
    case 'C':
      {
        CharValue *val = reinterpret_cast<CharValue *> (value);
        setIntJVMTI (env, thread, slot, depth, sig, (jint) val->getValue ());
        break;
      }
    case 'S':
      {
        ShortValue *val = reinterpret_cast<ShortValue *> (value);
        setIntJVMTI (env, thread, slot, depth, sig, (jint) val->getValue ());
        break;
      }
    case 'I':
      {
        IntValue *val = reinterpret_cast<IntValue *> (value);
        setIntJVMTI (env, thread, slot, depth, sig, val->getValue ());
        break;
      }
    case 'J':
      {
        LongValue *val = reinterpret_cast<LongValue *> (value);
        setLongJVMTI (env, thread, slot, depth, sig, val->getValue ());
        break;
      }
    case 'F':
      {
        FloatValue *val = reinterpret_cast<FloatValue *> (value);
        setFloatJVMTI (env, thread, slot, depth, sig, val->getValue ());
        break;
      }
    case 'D':
      {
        DoubleValue *val = reinterpret_cast<DoubleValue *> (value);
        setDoubleJVMTI (env, thread, slot, depth, sig, val->getValue ());
        break;
      }
    case 'V':
      break;
    case '[':
      {
        ArrayValue *val = reinterpret_cast<ArrayValue *> (value);
        setObjectJVMTI (env, thread, slot, depth, sig, val->getObject ());
        break;
      }
    default:
      {
        ObjectValue *val = reinterpret_cast<ObjectValue *> (value);
        setObjectJVMTI (env, thread, slot, depth, sig, val->getObject());
        break;
      }
    }
}
