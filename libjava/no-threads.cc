// no-thread.cc - Implementation of `no threads' threads.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Thread.h>

java::lang::Thread *_Jv_OnlyThread = NULL;

void
_Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *,
		 _Jv_ThreadStartFunc *meth)
{
  // Don't use JvAssert, since we want this to fail even when compiled
  // without assertions.
  if (_Jv_OnlyThread)
    JvFail ("only thread already running");
  _Jv_OnlyThread = thread;
  (*meth) (thread);
}
