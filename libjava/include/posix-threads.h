// -*- c++ -*-
// posix-threads.h - Defines for using POSIX threads.

/* Copyright (C) 1998, 1999  Cygnus Solutions

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

#if defined (HAVE_PTHREAD_MUTEXATTR_SETTYPE) || defined (HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)
#  define HAVE_RECURSIVE_MUTEX 1
#endif


//
// Typedefs.
//

typedef pthread_cond_t _Jv_ConditionVariable_t;
#ifdef HAVE_RECURSIVE_MUTEX
typedef pthread_mutex_t _Jv_Mutex_t;
#else
// Some systems do not have recursive mutexes, so we must simulate
// them.  Solaris is one such system.
typedef struct
{
  // Mutex used when locking this structure transiently.
  pthread_mutex_t mutex;
  // Mutex the thread holds the entire time this mutex is held.  This
  // is used to make condition variables work properly.
  pthread_mutex_t mutex2;
  // Condition variable used when waiting for this lock.
  pthread_cond_t cond;
  // Thread holding this mutex.  If COUNT is 0, no thread is holding.
  pthread_t thread;
  // Number of times mutex is held.  If 0, the lock is not held.
  int count;
} _Jv_Mutex_t;
#endif /* HAVE_RECURSIVE_MUTEX */

typedef struct
{
  // Flag values are defined in implementation.
  int flags;

  // Actual thread id.
  pthread_t thread;

  // Exception we want to throw when cancelled.
  void *exception;
} _Jv_Thread_t;
typedef void _Jv_ThreadStartFunc (java::lang::Thread *);


//
// Condition variables.
//

inline void
_Jv_CondInit (_Jv_ConditionVariable_t *cv)
{
  pthread_cond_init (cv, 0);
}

#ifndef LINUX_THREADS

// pthread_cond_destroy does nothing on Linux and it is a win to avoid
// defining this macro.

#define _Jv_HaveCondDestroy

inline void
_Jv_CondDestroy (_Jv_ConditionVariable_t *cv)
{
  pthread_cond_destroy (cv);
}

#endif /* LINUX_THREADS */

int _Jv_CondWait (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *mu,
		  jlong millis, jint nanos);

inline int
_Jv_CondNotify (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *)
{
  // FIXME: check to see if mutex is held by current thread.
  return pthread_cond_signal (cv);
}

inline int
_Jv_CondNotifyAll (_Jv_ConditionVariable_t *cv, _Jv_Mutex_t *)
{
  // FIXME: check to see if mutex is held by current thread.
  return pthread_cond_broadcast (cv);
}


//
// Mutexes.
//

#ifdef RECURSIVE_MUTEX_IS_DEFAULT
inline void
_Jv_MutexInit (_Jv_Mutex_t *mu)
{
  pthread_mutex_init (mu, NULL);
}
#else
void _Jv_MutexInit (_Jv_Mutex_t *mu);
#endif

#ifndef LINUX_THREADS

// pthread_mutex_destroy does nothing on Linux and it is a win to avoid
// defining this macro.

#define _Jv_HaveMutexDestroy

#ifdef HAVE_RECURSIVE_MUTEX

inline void
_Jv_MutexDestroy (_Jv_Mutex_t *mu)
{
  pthread_mutex_destroy (mu);
}

#else /* HAVE_RECURSIVE_MUTEX */

extern void _Jv_MutexDestroy (_Jv_Mutex_t *mu);

#endif /* HAVE_RECURSIVE_MUTEX */
#endif /* LINUX_THREADS */

#ifdef HAVE_RECURSIVE_MUTEX

inline int
_Jv_MutexLock (_Jv_Mutex_t *mu)
{
  return pthread_mutex_lock (mu);
}

inline int
_Jv_MutexUnlock (_Jv_Mutex_t *mu)
{
  return pthread_mutex_unlock (mu);
}

#else /* HAVE_RECURSIVE_MUTEX */

extern int _Jv_MutexLock (_Jv_Mutex_t *mu);
extern int _Jv_MutexUnlock (_Jv_Mutex_t *mu);

#endif /* HAVE_RECURSIVE_MUTEX */


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

inline void
_Jv_ThreadYield (void)
{
#ifdef HAVE_SCHED_YIELD
  sched_yield ();
#endif /* HAVE_SCHED_YIELD */
}

void _Jv_ThreadSetPriority (_Jv_Thread_t *data, jint prio);

void _Jv_ThreadCancel (_Jv_Thread_t *data, void *error);

// Like Cancel, but doesn't run cleanups.
inline void
_Jv_ThreadDestroy (_Jv_Thread_t *)
{
  JvFail ("_Jv_ThreadDestroy");
}

void _Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		      _Jv_ThreadStartFunc *meth);

void _Jv_ThreadWait (void);

void _Jv_ThreadInterrupt (_Jv_Thread_t *data);

#endif /* __JV_POSIX_THREADS__ */
