// -*- c++ -*-
// posix-threads.h - Defines for using POSIX threads.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_POSIX_THREADS__
#define __JV_POSIX_THREADS__

// NOTE: This file may only reference those pthread functions which
// are known not to be overridden by the Boehm GC.  If in doubt, scan
// boehm-gc/gc.h.  This is yucky but lets us avoid including gc.h
// everywhere (which would be truly yucky).

#include <pthread.h>
#include <sched.h>

//
// Typedefs.
//

typedef struct _Jv_Thread_t
{
  // Flag values are defined in implementation.
  int flags;

  // Actual thread id.
  pthread_t thread;
  
  // Java Thread object.
  java::lang::Thread *thread_obj;
  
  // Condition variable and corresponding mutex, used to implement the
  // interruptable wait/notify mechanism.
  pthread_cond_t wait_cond;
  pthread_mutex_t wait_mutex;

  // Next thread for Condition Variable wait-list chain.
  _Jv_Thread_t *next;
  
} _Jv_Thread_t;

typedef void _Jv_ThreadStartFunc (java::lang::Thread *);


// Condition Variables used to implement wait/notify/sleep/interrupt.
typedef struct
{
  // Linked list of Threads that are waiting to be notified.
  _Jv_Thread_t *first;

} _Jv_ConditionVariable_t;

typedef struct
{
  // For compatibility, simplicity, and correctness, we do not use the native
  // pthreads recursive mutex implementation, but simulate them instead.

  // Mutex the thread holds the entire time this mutex is held. 
  pthread_mutex_t mutex;

  // Thread holding this mutex.
  pthread_t owner;

  // Number of times mutex is held (lock depth).  If 0, the lock is not held.
  int count;
} _Jv_Mutex_t;

// This is a convenience function used only by the pthreads thread
// implementation.  This is slow, but that's too bad -- we need to do
// the checks for correctness.  It might be nice to be able to compile
// this out.  Returns 0 if the lock is held by the current thread, and
// 1 otherwise.
inline int
_Jv_PthreadCheckMonitor (_Jv_Mutex_t *mu)
{
  pthread_t self = pthread_self();
  if (mu->owner == self)
    return 0;
  else return 1;
}

//
// Condition variables.
//

int _Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
		  jlong millis, jint nanos);
		  
int _Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu);

int _Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu);

inline void
_Jv_CondInit (_Jv_ConditionVariable_t *cv)
{
  cv->first = NULL;
}

//
// Mutexes.
//

inline void
_Jv_MutexInit (_Jv_Mutex_t *mu)
{
  pthread_mutex_init (&mu->mutex, NULL);

  mu->count = 0;
  mu->owner = 0;
}

inline int
_Jv_MutexLock (_Jv_Mutex_t *mu)
{
  pthread_t self = pthread_self ();
  if (mu->owner == self)
    {
      mu->count++;
    }
  else
    {
      pthread_mutex_lock (&mu->mutex);
      mu->count = 1;
      mu->owner = self;
    }
  return 0;
}

inline int
_Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  if (_Jv_PthreadCheckMonitor (mu))
    return _JV_NOT_OWNER;
    
  mu->count--;

  if (mu->count == 0)
    {
      mu->owner = 0;
      pthread_mutex_unlock (&mu->mutex);
    }
  return 0;
}

#ifndef LINUX_THREADS

// pthread_mutex_destroy does nothing on Linux and it is a win to avoid
// defining this macro.

#define _Jv_HaveMutexDestroy

inline void 
_Jv_MutexDestroy (_Jv_Mutex_t *mu)
{
  pthread_mutex_destroy (&mu->mutex);
}

#endif /* LINUX_THREADS */

//
// Thread creation and manipulation.
//

void _Jv_InitThreads (void);

void _Jv_ThreadInitData (_Jv_Thread_t **data, java::lang::Thread *thread);

inline java::lang::Thread *
_Jv_ThreadCurrent (void)
{
  extern pthread_key_t _Jv_ThreadKey;
  return (java::lang::Thread *) pthread_getspecific (_Jv_ThreadKey);
}

inline _Jv_Thread_t *
_Jv_ThreadCurrentData (void)
{
  extern pthread_key_t _Jv_ThreadDataKey;
  return (_Jv_Thread_t *) pthread_getspecific (_Jv_ThreadDataKey);
}

inline void
_Jv_ThreadYield (void)
{
#ifdef HAVE_SCHED_YIELD
  sched_yield ();
#endif /* HAVE_SCHED_YIELD */
}

void _Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio);

void _Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		      _Jv_ThreadStartFunc *meth);

void _Jv_ThreadWait (void);

void _Jv_ThreadInterrupt (_Jv_Thread_t *data);

#endif /* __JV_POSIX_THREADS__ */
