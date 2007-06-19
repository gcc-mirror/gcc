// natNormalBreakpoint.cc - C++ side of NormalBreakpoint

/* Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <java-interp.h>
#include <jvmti.h>
#include "jvmti-int.h"

#include <gnu/gcj/jvmti/NormalBreakpoint.h>
#include <java/lang/Thread.h>

void
gnu::gcj::jvmti::NormalBreakpoint::execute ()
{
  using namespace ::java::lang;

  Thread *thread = Thread::currentThread ();
  JNIEnv *jni_env = _Jv_GetCurrentJNIEnv ();
      
  JvAssert (JVMTI_REQUESTED_EVENT (Breakpoint));
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_BREAKPOINT, thread, jni_env,
		       method, location);
}
