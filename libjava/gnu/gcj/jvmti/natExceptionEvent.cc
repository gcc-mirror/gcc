// natExceptionEvent.cc - C++ code for JVMTI Exception events

/* Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <gcj/method.h>
#include <java-interp.h>
#include <java-insns.h>
#include <java-assert.h>
#include <jvmti.h>
#include <jvmti-int.h>

#include <gnu/gcj/jvmti/ExceptionEvent.h>

void
gnu::gcj::jvmti::ExceptionEvent::sendEvent ()
{
  // Check if the exception is caught somewhere in the interpreted call stack
  if (_catchMeth == 0 || _catchLoc == 0)
    checkCatch ();
    
  JNIEnv *jni = _Jv_GetCurrentJNIEnv ();

  _Jv_JVMTI_PostEvent (JVMTI_EVENT_EXCEPTION, _thread, jni,
                       reinterpret_cast<jmethodID> (_throwMeth),
                       static_cast<jlocation> (_throwLoc), _ex,
                       reinterpret_cast<jmethodID> (_catchMeth),
                       static_cast<jlocation> (_catchLoc)); 
}

// This method looks up the interpreted call stack to see if the exception will
// eventually be caught by some java method.
void
gnu::gcj::jvmti::ExceptionEvent::checkCatch ()
{
  _Jv_InterpFrame *frame 
    = reinterpret_cast<_Jv_InterpFrame *> (_thread->interp_frame);
  
  while ((frame = frame->next_interp))
    {
	  _Jv_InterpMethod *meth 
	    = reinterpret_cast<_Jv_InterpMethod *> (frame->self);
	  pc_t pc = frame->pc;
		
	  if (meth->check_handler (&pc, meth, _ex))
	    {
	      _catchMeth = reinterpret_cast<jlong> (meth->get_method ());
	      _catchLoc = meth->insn_index (pc);
          break;
	    }
    }
}
