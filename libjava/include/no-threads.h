// -*- c++ -*-
// no-threads.h - Defines for using no threads.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_NO_THREADS__
#define __JV_NO_THREADS__

#include "config.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

//
// Typedefs.
//

typedef int _Jv_ConditionVariable_t;
typedef int _Jv_Mutex_t;
typedef int _Jv_Thread_t;
typedef void _Jv_ThreadStartFunc (java::lang::Thread *);


//
// Condition variables.
//

inline void
_Jv_CondInit (_Jv_ConditionVariable_t *)
{
}

// Waiting is ok provided there is a timeout.  Otherwise we will just
// wait forever.
inline int
_Jv_CondWait (_Jv_ConditionVariable_t *, _Jv_Mutex_t *,
	      jlong millis, jint nanos)
{
  if (millis == 0 && nanos == 0)
    JvFail ("_Jv_CondWait without timeout");

#ifdef HAVE_SLEEP
  int seconds = millis / 1000;
  if (seconds > 0)
    sleep (seconds);
#endif

  return 0;
}

inline int
_Jv_CondNotify (_Jv_ConditionVariable_t *, _Jv_Mutex_t *)
{
  // It is ok to notify -- it just has no effect.
  return 0;
}

inline int
_Jv_CondNotifyAll (_Jv_ConditionVariable_t *, _Jv_Mutex_t *)
{
  // It is ok to notify -- it just has no effect.
  return 0;
}


//
// Mutexes.
//

inline void
_Jv_MutexInit (_Jv_Mutex_t *)
{
}

inline int
_Jv_MutexLock (_Jv_Mutex_t *)
{
  return 0;
}

inline int
_Jv_MutexUnlock (_Jv_Mutex_t *)
{
  return 0;
}


//
// Thread creation and manipulation.
//

inline void
_Jv_InitThreads (void)
{
}

inline void
_Jv_ThreadInitData (_Jv_Thread_t **data, java::lang::Thread *)
{
  *data = NULL;
}

inline java::lang::Thread *
_Jv_ThreadCurrent (void)
{
  extern java::lang::Thread *_Jv_OnlyThread;
  return _Jv_OnlyThread;
}

inline void
_Jv_ThreadYield (void)
{
}

inline void
_Jv_ThreadSetPriority (_Jv_Thread_t *, jint)
{
}

void _Jv_ThreadStart (java::lang::Thread *, _Jv_Thread_t *,
		      _Jv_ThreadStartFunc *meth);

inline void
_Jv_ThreadWait (void)
{
}

inline void
_Jv_ThreadInterrupt (_Jv_Thread_t *)
{
}

#endif /* __JV_NO_THREADS__ */
