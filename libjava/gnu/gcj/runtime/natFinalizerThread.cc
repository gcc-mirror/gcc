// natFinalizerThread.cc - Implementation of FinalizerThread native methods.

/* Copyright (C) 2001, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/gcj/runtime/FinalizerThread.h>

#include <java-threads.h>

static _Jv_Mutex_t mutex;
static _Jv_ConditionVariable_t condition;

// Initialize lock & condition variable.
void
gnu::gcj::runtime::FinalizerThread::init ()
{
  _Jv_MutexInit (&mutex);
  _Jv_CondInit (&condition);
}

// This is called by the GC when a finalizer is ready to be
// run.  It sets a flag and wakes up the finalizer thread. Note
// that this MUST NOT aquire any Java lock, as this could result in 
// the hash synchronization code being re-entered: the synchronization
// code itself might need to allocate. See PR 16478.
void
gnu::gcj::runtime::FinalizerThread::finalizerReady ()
{
#ifdef __JV_NO_THREADS__
  _Jv_RunFinalizers ();
#else
  _Jv_MutexLock (&mutex);
  finalizer_ready = true;
  _Jv_CondNotify (&condition, &mutex);
  _Jv_MutexUnlock (&mutex);
#endif
}

// Main loop for the finalizer thread. 
void
gnu::gcj::runtime::FinalizerThread::run ()
{
  while (true)
    {
      _Jv_MutexLock (&mutex);
      if (! finalizer_ready)
	_Jv_CondWait (&condition, &mutex, 0, 0);
      finalizer_ready = false;
      _Jv_MutexUnlock (&mutex);
      _Jv_RunFinalizers ();
    }
}
