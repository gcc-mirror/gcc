// -*- c++ -*-
// quick-threads.h - Defines for using QuickThreads.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_QUICK_THREADS__
#define __JV_QUICK_THREADS__

#include <coop.h>

//
// Typedefs.
//

typedef coop_c _Jv_ConditionVariable_t;
typedef coop_m _Jv_Mutex_t;
typedef coop_t *_Jv_Thread_t;
typedef void _Jv_ThreadStartFunc (java::lang::Thread *);


//
// Condition variables.
//

inline void
_Jv_CondInit (_Jv_ConditionVariable_t *cv)
{
  coop_condition_variable_init (cv);
}

inline int
_Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
	      jlong millis, jint nanos)
{
  long micros = millis * 1000 + nanos / 1000;
  // Don't round to 0 inappropriately.
  if (! micros && (millis || nanos))
    micros = 1;
  return coop_condition_variable_wait (cv, mu, micros);
}

inline int
_Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu)
{
  return coop_condition_variable_signal (cv, mu);
}

inline int
_Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu)
{
  return coop_condition_variable_signal_all (cv, mu);
}


//
// Mutexes.
//

inline void
_Jv_MutexInit (_Jv_Mutex_t *mu)
{
  coop_mutex_init (mu);
}

inline int
_Jv_MutexLock (_Jv_Mutex_t *mu)
{
  coop_mutex_lock (mu);
  return 0;
}

inline int
_Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  return coop_mutex_unlock (mu);
}


//
// Thread creation and manipulation.
//

void _Jv_InitThreads (void);

inline void
_Jv_ThreadInitData (_Jv_Thread_t **data, java::lang::Thread *)
{
  *data = new _Jv_Thread_t;
  **data = (coop_t *) 0;
}

inline java::lang::Thread *
_Jv_ThreadCurrent (void)
{
  extern int _Jv_ThreadKey;
  return (java::lang::Thread *) coop_getspecific (_Jv_ThreadKey);
}

inline void
_Jv_ThreadYield (void)
{
  coop_yield ();
}

inline void
_Jv_ThreadSetPriority (_Jv_Thread_t *, jint)
{
}

inline void
_Jv_ThreadCancel (_Jv_Thread_t *data, void *error)
{
  coop_terminate (*data, error);
}

// Like Cancel, but doesn't run cleanups.
inline void
_Jv_ThreadDestroy (_Jv_Thread_t *data)
{
  coop_terminate (*data, 0);
}

void _Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		      _Jv_ThreadStartFunc *meth);

inline void
_Jv_ThreadWait (void)
{
  coop_start ();
}

inline void
_Jv_ThreadInterrupt (_Jv_Thread_t *)
{
}

#endif /* __JV_QUICK_THREADS__ */
